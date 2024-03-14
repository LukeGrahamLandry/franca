use crate::ast::{
    Expr, FatExpr, FnType, FuncId, OverloadOption, OverloadSet, Pattern, TypeId, Var,
};
use crate::bc::{Value, Values};
use crate::compiler::{CErr, Compile, DebugState, Executor, FnWip, Res};
use crate::logging::LogTag::ShowErr;
use crate::logging::{err, ice, outln, unwrap, LogTag, PoolLog};
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
            Expr::Closure(func) => {
                let id = self.add_func(mem::take(func), &result.constants)?;
                self.named_args_to_tuple(result, arg, id)?;
                Some(id)
            }
            &mut Expr::GetNamed(i) => {
                // TODO: from_bit_literal in an @enum gets here.
                self.lookup_unique_func(i)
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
        if let Some(f) = self.lookup_unique_func(name.0) {
            self.named_args_to_tuple(result, arg, f)?;
            return Ok(f);
        }

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
            self.compute_new_overloads(name.0, i)?;
            let mut overloads = self.program.overload_sets[i].clone(); // Sad
            if let Expr::StructLiteralP(pattern) = &mut arg.expr {
                self.prune_overloads_by_named_args(&mut overloads, pattern)?;
            }

            if overloads.0.is_empty() {
                err!("No overload found {name:?}",);
            }
            if overloads.0.len() == 1 {
                let id = overloads.0[0].func;
                self.named_args_to_tuple(result, arg, id)?;
                return Ok(id);
            }

            match self.type_of(result, arg) {
                Ok(Some(arg_ty)) => {
                    let accept = |f_ty: FnType| {
                        arg_ty == f_ty.arg
                            && (requested_ret.is_none() || (requested_ret.unwrap() == f_ty.ret))
                    };

                    let mut found = None;
                    for check in &overloads.0 {
                        if accept(check.ty) {
                            found = Some(check.func);
                            break;
                        }
                    }

                    if let Some(found) = found {
                        self.pop_state(state);
                        return Ok(found);
                    }

                    let log_goal = |s: &mut Self| {
                        format!(
                            "for fn {}({arg_ty:?}={}) {:?};",
                            name.log(s.pool),
                            s.program.log_type(arg_ty),
                            requested_ret
                                .map(|t| s.program.log_type(t))
                                .unwrap_or_else(|| "??".to_string())
                        )
                    };

                    // TODO: put the message in the error so !assert_compile_error doesn't print it.
                    outln!(ShowErr, "not found {}", log_goal(self));
                    let decls = self.program.declarations.get(&name.0).unwrap().clone();
                    for f in decls {
                        if let Ok(Some(f_ty)) = self.infer_types(f) {
                            outln!(
                                ShowErr,
                                "- found {:?} fn({}) {};",
                                f,
                                self.program.log_type(f_ty.arg),
                                self.program.log_type(f_ty.ret),
                            );
                        }
                    }
                    outln!(ShowErr, "Impls: {:?}", self.program.impls.get(&name.0));
                    outln!(ShowErr, "Maybe you forgot to instantiate a generic?");

                    err!(CErr::AmbiguousCall)
                }
                Ok(None) => err!(
                    "AmbiguousCall. Unknown type for argument {}",
                    arg.log(self.pool)
                ),
                Err(e) => err!(
                    "AmbiguousCall. Unknown type for argument {}. {}",
                    arg.log(self.pool),
                    e.reason.log(self.program, self.pool)
                ),
            }
        } else {
            err!(
                "Expected function for {} but found {:?}",
                name.log(self.pool),
                value
            );
        }
    }

    fn compute_new_overloads(&mut self, name: Ident<'p>, i: usize) -> Res<'p, ()> {
        if let Some(decls) = self.program.declarations.get(&name) {
            outln!(
                LogTag::Generics,
                "Compute overloads of {} = L{i}",
                self.pool.get(name),
            );
            let overloads = &self.program.overload_sets[i];
            let decls: Vec<_> = decls
                .iter()
                .copied()
                .filter(|f| !overloads.0.iter().any(|old| old.func == *f))
                .collect();
            for f in &decls {
                if let Ok(Some(f_ty)) = self.infer_types(*f) {
                    outln!(
                        LogTag::Generics,
                        "- {f:?} is {:?}={} -> {}",
                        f_ty.arg,
                        self.program.log_type(f_ty.arg),
                        self.program.log_type(f_ty.ret)
                    );
                    // TODO: this is probably wrong if you use !assert_compile_error
                    self.program.overload_sets[i].0.push(OverloadOption {
                        name,
                        ty: f_ty,
                        func: *f,
                    });
                }
            }
        } else {
            err!(
                "expected declarations for overload set {:?} L{i}",
                self.pool.get(name),
            )
        }

        Ok(())
    }

    fn named_args_to_tuple(
        &mut self,
        _result: &mut FnWip<'p>,
        arg: &mut FatExpr<'p>,
        f: FuncId,
    ) -> Res<'p, ()> {
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

    fn prune_overloads_by_named_args(
        &self,
        overload_set: &mut OverloadSet<'p>,
        pattern: &Pattern<'p>,
    ) -> Res<'p, ()> {
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
