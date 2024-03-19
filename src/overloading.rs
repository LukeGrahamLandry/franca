use crate::ast::{Expr, FatExpr, Flag, FnType, FuncId, OverloadOption, OverloadSet, Pattern, TargetArch, TypeId, Var};
use crate::bc::{Value, Values};
use crate::compiler::{CErr, Compile, DebugState, ExecTime, Executor, FnWip, Res};
use crate::logging::LogTag::ShowErr;
use crate::logging::{assert_eq, err, ice, outln, unwrap, LogTag, PoolLog};
use crate::pool::Ident;
use std::mem;
use std::ops::DerefMut;

impl<'a, 'p, Exec: Executor<'p>> Compile<'a, 'p, Exec> {
    pub fn maybe_direct_fn(
        &mut self,
        result: &mut FnWip<'p>,
        f: &mut FatExpr<'p>,
        arg: &mut FatExpr<'p>,
        ret: Option<TypeId>,
    ) -> Res<'p, Option<FuncId>> {
        // TODO: more general system for checking if its a constant known expr instead of just for functions?
        Ok(match f.deref_mut() {
            &mut Expr::GetVar(i) => {
                // TODO: only grab here if its a constant, might be a function pointer.
                let id = self.resolve_function(result, i, arg, ret)?; // TODO: error here is probably fine, just return None
                Some(id)
            }
            &mut Expr::Value {
                value: Values::One(Value::GetFn(id)),
                ..
            }
            | &mut Expr::WipFunc(id) => {
                self.named_args_to_tuple(result, arg, id)?;
                Some(id)
            }
            &mut Expr::Value {
                value: Values::One(Value::OverloadSet(i)),
                ..
            } => {
                let id = self.resolve_in_overload_set(result, arg, ret, i)?;
                Some(id)
            }
            Expr::Closure(func) => {
                let id = self.add_func(mem::take(func), &result.constants)?;
                self.named_args_to_tuple(result, arg, id)?;
                Some(id)
            }
            &mut Expr::GetNamed(i) => {
                err!("GetNamed func: {}", self.pool.get(i));
            }
            _ => None,
        })
    }

    // TODO: better error messages
    pub fn resolve_function(
        &mut self,
        result: &mut FnWip<'p>,
        name: Var<'p>,
        arg: &mut FatExpr<'p>,
        mut requested_ret: Option<TypeId>,
    ) -> Res<'p, FuncId> {
        // TODO: probably don't want this here because it means you can't shadow with a const.
        // If there's only one option, we don't care what type it is.
        // if let Some(f) = self.program.find_unique_func(name.0) {
        //     self.named_args_to_tuple(result, arg, f)?;
        //     return Ok(f);
        // }

        let state = DebugState::ResolveFnRef(name);
        self.push_state(&state);

        let value = if let Some((value, _)) = result.constants.get(name) {
            if let Values::One(Value::GetFn(f)) = value {
                self.named_args_to_tuple(result, arg, f)?;
                self.pop_state(state);
                return Ok(f);
            }
            value
        } else {
            err!(CErr::VarNotFound(name))
        };

        // TODO: get rid of any
        if let Some(ty) = requested_ret {
            if ty.is_any() {
                requested_ret = None
            }
        }

        // TODO: combine this with the match up there so its less ugly.
        if let Values::One(Value::OverloadSet(i)) = value {
            let out = self.resolve_in_overload_set(result, arg, requested_ret, i)?;
            self.pop_state(state);
            Ok(out)
        } else {
            err!("Expected function for {} but found {:?}", name.log(self.pool), value);
        }
    }

    pub fn resolve_in_overload_set(
        &mut self,
        result: &mut FnWip<'p>,
        arg: &mut FatExpr<'p>,
        requested_ret: Option<TypeId>,
        i: usize,
    ) -> Res<'p, FuncId> {
        let name = self.program.overload_sets[i].1;
        self.compute_new_overloads(i)?;
        let mut overloads = self.program.overload_sets[i].clone(); // Sad
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            self.prune_overloads_by_named_args(&mut overloads, pattern)?;
        }

        let target = match result.when {
            ExecTime::Comptime => self.program.comptime_arch,
            ExecTime::Runtime => self.program.runtime_arch,
        };

        // TODO: kinda cringe.
        match target {
            TargetArch::Interp => overloads.0.retain(|f| !self.program.funcs[f.func.0].has_tag(Flag::Llvm)),
            TargetArch::Aarch64 => overloads.0.retain(|f| !self.program.funcs[f.func.0].has_tag(Flag::Llvm)),
            TargetArch::Llvm => overloads.0.retain(|f| !self.program.funcs[f.func.0].has_tag(Flag::Aarch64)),
        }
        overloads.0.retain(|f| !self.program.funcs[f.func.0].has_tag(Flag::Forward)); // HACK

        if overloads.0.is_empty() {
            err!("No overload found for {i:?}: {}", self.pool.get(name));
        }
        if overloads.0.len() == 1 {
            let id = overloads.0[0].func;
            self.named_args_to_tuple(result, arg, id)?;
            return Ok(id);
        }

        match self.type_of(result, arg) {
            Ok(Some(arg_ty)) => {
                let accept = |f_arg: TypeId, f_ret: Option<TypeId>| {
                    arg_ty == f_arg && (requested_ret.is_none() || f_ret.is_none() || (requested_ret.unwrap() == f_ret.unwrap()))
                };

                let mut found = None;
                for check in &overloads.0 {
                    if accept(check.arg, check.ret) {
                        found = Some(check.func);
                        break;
                    }
                }

                if let Some(found) = found {
                    self.pop_stat2();
                    return Ok(found);
                }

                let log_goal = |s: &mut Self| {
                    format!(
                        "for fn {}({arg_ty:?}={}) {:?};",
                        s.pool.get(name),
                        s.program.log_type(arg_ty),
                        requested_ret.map(|t| s.program.log_type(t)).unwrap_or_else(|| "??".to_string())
                    )
                };

                // TODO: put the message in the error so !assert_compile_error doesn't print it.
                outln!(ShowErr, "not found {}", log_goal(self));
                for f in overloads.0 {
                    outln!(
                        ShowErr,
                        "- found {:?} fn({:?}={}) {:?}; {:?}",
                        f.func,
                        f.arg,
                        self.program.log_type(f.arg),
                        f.ret.map(|ret| self.program.log_type(ret)),
                        self.program.funcs[f.func.0]
                            .annotations
                            .iter()
                            .map(|a| self.pool.get(a.name))
                            .collect::<Vec<_>>()
                    );
                }
                outln!(ShowErr, "Impls: {:?}", self.program.impls.get(&name));
                outln!(ShowErr, "Maybe you forgot to instantiate a generic?");

                err!(CErr::AmbiguousCall)
            }
            Ok(None) => err!("AmbiguousCall. Unknown type for argument {}", arg.log(self.pool)),
            Err(e) => err!(
                "AmbiguousCall. Unknown type for argument {}. {}",
                arg.log(self.pool),
                e.reason.log(self.program, self.pool)
            ),
        }
    }

    pub fn compute_new_overloads(&mut self, i: usize) -> Res<'p, ()> {
        let overloads = &mut self.program.overload_sets[i];
        let decls = mem::take(&mut overloads.2); // Take any new things found since last time we looked at this function that haven't been typechecked yet.
        if decls.is_empty() {
            return Ok(());
        }
        outln!(LogTag::Generics, "Compute overloads of {} = L{i}", self.pool.get(overloads.1),);
        for f in &decls {
            match self.infer_types(*f) {
                Ok(Some(f_ty)) => {
                    outln!(
                        LogTag::Generics,
                        "- {f:?} is {:?}={} -> {}",
                        f_ty.arg,
                        self.program.log_type(f_ty.arg),
                        self.program.log_type(f_ty.ret)
                    );
                    // TODO: this is probably wrong if you use !assert_compile_error
                    self.program.overload_sets[i].0.push(OverloadOption {
                        arg: f_ty.arg,
                        ret: Some(f_ty.ret),
                        func: *f,
                    });
                }
                e => {
                    if let Some(arg) = self.program.funcs[f.0].finished_arg {
                        self.program.overload_sets[i].0.push(OverloadOption { arg, ret: None, func: *f });
                    } else {
                        println!("ERR: {e:?}")
                    }
                }
            }
        }

        Ok(())
    }

    fn named_args_to_tuple(&mut self, _result: &mut FnWip<'p>, arg: &mut FatExpr<'p>, f: FuncId) -> Res<'p, ()> {
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            let expected = &self.program.funcs[f.0].arg;
            assert_eq!(expected.bindings.len(), pattern.bindings.len());
            let mut parts = vec![];
            for name in expected.flatten_names() {
                let index = unwrap!(
                    pattern.bindings.iter().position(|p| p.name() == Some(name)),
                    "missing named argument {name:?}"
                );
                let value = pattern.bindings.remove(index);
                parts.push(unwrap!(value.ty.expr(), "named arg missing value"));
            }
            debug_assert!(pattern.bindings.is_empty());
            match parts.len() {
                0 => arg.expr = Expr::unit(),
                1 => arg.expr = parts.into_iter().next().unwrap().expr,
                _ => arg.expr = Expr::Tuple(parts),
            }
        }
        Ok(())
    }

    fn prune_overloads_by_named_args(&self, overload_set: &mut OverloadSet<'p>, pattern: &Pattern<'p>) -> Res<'p, ()> {
        let names = pattern.flatten_names();
        overload_set.0.retain(|overload| {
            let mut expected = self.program.funcs[overload.func.0].arg.flatten_names();
            if expected.len() != names.len() {
                // TODO: its sad that ive already allcoated the vec of names.
                return false;
            }

            for name in &names {
                if let Some(i) = expected.iter().position(|c| c == name) {
                    expected.remove(i);
                } else {
                    return false;
                }
            }

            true
        });
        Ok(())
    }
}
