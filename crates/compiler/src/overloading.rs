use crate::ast::{Expr, FatExpr, Flag, FuncId, OverloadOption, OverloadSet, Pattern, Program, TargetArch, TypeId, Var};
use crate::bc::{FuncRef, Value, Values};
use crate::compiler::{CErr, Compile, DebugState, ExecTime, FnWip, Res};
use crate::logging::LogTag::ShowErr;
use crate::logging::{LogTag, PoolLog};
use crate::{assert_eq, err, outln, unwrap};
use std::mem;
use std::ops::DerefMut;

impl<'a, 'p> Compile<'a, 'p> {
    pub fn maybe_direct_fn(
        &mut self,
        result: &mut FnWip<'p>,
        f: &mut FatExpr<'p>,
        arg: &mut FatExpr<'p>,
        ret: Option<TypeId>,
    ) -> Res<'p, Option<FuncRef>> {
        // TODO: more general system for checking if its a constant known expr instead of just for functions?
        Ok(match f.deref_mut() {
            &mut Expr::GetVar(i) => {
                // TODO: only grab here if its a constant, might be a function pointer.
                let id = self.resolve_function(result, i, arg, ret)?; // TODO: error here is probably fine, just return None
                                                                      // println!("Choose {id:?} for {}", f.log(self.pool));
                Some(id)
            }
            &mut Expr::Value {
                value: Values::One(Value::SplitFunc { ct, rt }),
                ..
            } => {
                // TODO: assert no named args
                assert_ne!(ct, rt);
                Some(FuncRef::Split { ct, rt })
            }
            &mut Expr::Value {
                value: Values::One(Value::GetFn(id)),
                ..
            }
            | &mut Expr::WipFunc(id) => {
                self.named_args_to_tuple(result, arg, id)?;
                Some(id.into())
            }
            &mut Expr::Value {
                value: Values::One(Value::OverloadSet(i)),
                ..
            } => {
                let id = self.resolve_in_overload_set(result, arg, ret, i)?;
                Some(id)
            }
            // TODO: this shouldn't be nessisary. Values::Many should collapse. You get here when a macro tries to literal_ast(OverloadSet)!unquote?
            Expr::Value {
                value: Values::Many(vals), ..
            } => {
                if vals.len() == 1 {
                    if let Value::OverloadSet(i) = vals[0] {
                        let id = self.resolve_in_overload_set(result, arg, ret, i)?;
                        return Ok(Some(id));
                    }
                }
                None
            }
            Expr::Closure(_) => {
                let id = self.promote_closure(result, f)?;
                self.named_args_to_tuple(result, arg, id)?;
                Some(id.into())
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
        requested_ret: Option<TypeId>,
    ) -> Res<'p, FuncRef> {
        let state = DebugState::ResolveFnRef(name);
        self.push_state(&state);

        let value = if let Some((value, _)) = result.constants.get(name) {
            if let Values::One(Value::GetFn(f)) = value {
                self.named_args_to_tuple(result, arg, f)?;
                self.pop_state(state);
                return Ok(f.into());
            }
            if let Values::One(Value::SplitFunc { rt, ct }) = value {
                assert_ne!(ct, rt);
                self.pop_state(state);
                return Ok(FuncRef::Split { ct, rt });
            }
            value
        } else {
            // TODO: use self.program.vars[name.1].loc to show the declaration site.
            err!("Missing constant {} (forgot to make a Fn(A, R) 'const'?)", name.log(self.pool))
        };

        // TODO: get rid of any
        if let Some(ty) = requested_ret {
            assert!(!ty.is_any());
            assert!(!ty.is_unknown());
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
    ) -> Res<'p, FuncRef> {
        let name = self.program.overload_sets[i].name;
        self.compute_new_overloads(i)?;
        let mut overloads = self.program.overload_sets[i].clone(); // Sad
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            self.prune_overloads_by_named_args(&mut overloads, pattern)?;

            // TODO: my named args test doesn't work without this
            if overloads.ready.len() == 1 {
                let id = overloads.ready[0].func;
                self.named_args_to_tuple(result, arg, id)?;
                return Ok(id.into());
            }
        }

        overloads.ready.retain(|f| !self.program[f.func].has_tag(Flag::Forward)); // HACK

        if overloads.ready.is_empty() {
            err!("No overload found for {i:?}: {}", self.pool.get(name));
        }

        match self.type_of(result, arg) {
            Ok(Some(arg_ty)) => {
                let accept = |f_arg: TypeId, f_ret: Option<TypeId>| {
                    arg_ty == f_arg && (requested_ret.is_none() || f_ret.is_none() || (requested_ret.unwrap() == f_ret.unwrap()))
                };

                overloads.ready.retain(|check| accept(check.arg, check.ret));

                if overloads.ready.len() == 1 {
                    let id = overloads.ready[0].func;
                    self.named_args_to_tuple(result, arg, id)?;
                    return Ok(id.into());
                }

                let mut ct = overloads.clone();
                filter_arch(self.program, &mut overloads, ExecTime::Runtime);
                filter_arch(self.program, &mut ct, ExecTime::Comptime);

                if ct.ready.len() == 1 && overloads.ready.len() == 1 {
                    // TODO: check that types are exactly the same.
                    let ct = ct.ready[0].func;
                    let rt = overloads.ready[0].func;
                    if ct == rt {
                        // TODO: this should be unreachable
                        self.named_args_to_tuple(result, arg, rt)?;
                        return Ok(FuncRef::Exact(rt));
                    } else {
                        return Ok(FuncRef::Split { ct, rt });
                    }
                }

                let log_goal = |s: &mut Self| {
                    format!(
                        "for fn {}({arg_ty:?}={}) {:?};",
                        s.pool.get(name),
                        s.program.log_type(arg_ty),
                        requested_ret.map(|t| s.program.log_type(t)).unwrap_or_else(|| "??".to_string())
                    )
                };

                // TODO: cleanup include ct vs rt split
                // TODO: put the message in the error so !assert_compile_error doesn't print it.
                outln!(ShowErr, "not found {}", log_goal(self));
                for f in overloads.ready {
                    outln!(
                        ShowErr,
                        "- RT: found {:?} fn({:?}={}) {:?}; {:?}",
                        f.func,
                        f.arg,
                        self.program.log_type(f.arg),
                        f.ret.map(|ret| self.program.log_type(ret)),
                        self.program[f.func].annotations.iter().map(|a| self.pool.get(a.name)).collect::<Vec<_>>()
                    );
                }
                for f in ct.ready {
                    outln!(
                        ShowErr,
                        "- CT: found {:?} fn({:?}={}) {:?}; {:?}",
                        f.func,
                        f.arg,
                        self.program.log_type(f.arg),
                        f.ret.map(|ret| self.program.log_type(ret)),
                        self.program[f.func].annotations.iter().map(|a| self.pool.get(a.name)).collect::<Vec<_>>()
                    );
                }
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
        let decls = mem::take(&mut overloads.pending); // Take any new things found since last time we looked at this function that haven't been typechecked yet.
        if decls.is_empty() {
            return Ok(());
        }
        outln!(LogTag::Generics, "Compute overloads of {} = L{i}", self.pool.get(overloads.name),);
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
                    self.program.overload_sets[i].ready.push(OverloadOption {
                        arg: f_ty.arg,
                        ret: Some(f_ty.ret),
                        func: *f,
                    });
                }
                e => {
                    if let Some(arg) = self.program[*f].finished_arg {
                        self.program.overload_sets[i].ready.push(OverloadOption { arg, ret: None, func: *f });
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
            let expected = &self.program[f].arg;
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
        overload_set.ready.retain(|overload| {
            let mut expected = self.program[overload.func].arg.flatten_names();
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

pub fn has_arch_split<'p>(program: &Program<'p>, overloads: &OverloadSet<'p>) -> bool {
    // TODO
    // if program.comptime_arch == program.runtime_arch {
    //     // Sure but we don't care
    //     return false;
    // }

    for o in &overloads.ready {
        for a in &program[o.func].annotations {
            if let Ok(flag) = a.name.try_into() {
                if matches!(flag, Flag::Llvm | Flag::Interp | Flag::No_Interp | Flag::Aarch64) {
                    return true;
                }
            }
        }
    }

    false
}

pub fn filter_arch<'p>(program: &Program<'p>, overloads: &mut OverloadSet<'p>, when: ExecTime) {
    let target = match when {
        ExecTime::Comptime => program.comptime_arch,
        ExecTime::Runtime => program.runtime_arch,
    };

    // TODO: kinda cringe.
    match target {
        TargetArch::Interp => overloads
            .ready
            .retain(|f| !program[f.func].has_tag(Flag::Llvm) && !program[f.func].has_tag(Flag::No_Interp)),
        TargetArch::Aarch64 => overloads
            .ready
            .retain(|f| !program[f.func].has_tag(Flag::Llvm) && !program[f.func].has_tag(Flag::Interp)),
        TargetArch::Llvm => overloads
            .ready
            .retain(|f| !program[f.func].has_tag(Flag::Aarch64) && !program[f.func].has_tag(Flag::Interp)),
    }
}
