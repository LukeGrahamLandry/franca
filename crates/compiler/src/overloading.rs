use crate::ast::{Expr, FatExpr, Flag, FuncId, OverloadOption, OverloadSet, Pattern, Program, TargetArch, TypeId, Var, VarType};
use crate::bc::{FuncRef, Value, Values};
use crate::compiler::{Compile, DebugState, ExecTime, FnWip, Res};
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
                if i.3 == VarType::Const {
                    let id = self.resolve_function(result, i, arg, ret)?; // TODO: error here is probably fine, just return None
                    Some(id)
                } else {
                    None
                }
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

    // TODO: rename this, it means resolve overloads, not resovle variables which is ambigous now that they happen together sometimes.
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

        let value = if let Some((value, _)) = self.find_const(name)? {
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
            if let Values::One(Value::GetNativeFnPtr(_)) = value {
                err!("TODO: const GetNativeFnPtr?",)
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

        if overloads.ready.is_empty() {
            err!("No overload found for {i:?}: {}", self.pool.get(name));
        }

        match self.type_of(result, arg) {
            Ok(Some(arg_ty)) => {
                // TODO: need to factor this part out so that 'const F: <> = some_overload_set' works properly.

                // TODO: Never needs to not be a special case. Have like an auto cast graph thingy.
                let accept = |f_arg: TypeId, f_ret: Option<TypeId>| {
                    arg_ty == f_arg
                        && (requested_ret.is_none() || f_ret.is_none() || (requested_ret.unwrap() == f_ret.unwrap() || f_ret.unwrap().is_never()))
                };

                let original = overloads.clone();

                // TODO: PROBLEM: this doiesnt do voidptr check. it worked before because i did filter_arch first and there happened to only be one so it didnt even get here and then th e type check passed latter because it knows the rules.
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
                        "for fn {}({arg_ty:?}={}) {}={:?};",
                        s.pool.get(name),
                        s.program.log_type(arg_ty),
                        requested_ret.map(|ret| format!("{ret:?}")).unwrap_or_default(),
                        requested_ret.map(|t| s.program.log_type(t)).unwrap_or_else(|| "??".to_string())
                    )
                };

                // TODO: cleanup include ct vs rt split
                // TODO: put the message in the error so !assert_compile_error doesn't print it.
                let mut msg = String::new();
                use std::fmt::Write;
                writeln!(msg, "not found {}", log_goal(self)).unwrap();
                for f in original.ready {
                    let yes = accept(f.arg, f.ret);
                    let prefix = if yes { "[YES]" } else { "[ NO]" };
                    writeln!(
                        msg,
                        "- {prefix} found {:?} fn({:?}={}) {}={}; {:?}",
                        f.func,
                        f.arg,
                        self.program.log_type(f.arg),
                        f.ret.map(|ret| format!("{ret:?}")).unwrap_or(String::new()),
                        f.ret.map(|ret| self.program.log_type(ret)).unwrap_or(String::new()),
                        self.program[f.func].annotations.iter().map(|a| self.pool.get(a.name)).collect::<Vec<_>>()
                    )
                    .unwrap();
                    if yes {
                        // outln!(ShowErr, "   {}", self.program[f.func].log(self.pool));
                    }
                }
                for f in original.pending {
                    writeln!(
                        msg,
                        "- pending/failed {:?} {:?} uninit={}",
                        f,
                        self.program[f].arg.log(self.pool),
                        self.program[f].evil_uninit
                    )
                    .unwrap()
                }
                writeln!(msg, "Maybe you forgot to instantiate a generic?").unwrap();

                self.last_loc = Some(arg.loc);
                err!("AmbiguousCall: {}\n{}", log_goal(self), msg)
            }
            Ok(None) => {
                self.last_loc = Some(arg.loc);
                err!("AmbiguousCall. Unknown type for argument {}", arg.log(self.pool))
            }
            Err(e) => err!(
                "AmbiguousCall. Unknown type for argument {}. {}",
                arg.log(self.pool),
                e.reason.log(self.program, self.pool)
            ),
        }
    }

    pub fn compute_new_overloads(&mut self, i: usize) -> Res<'p, ()> {
        let overloads = &mut self.program.overload_sets[i];
        // debug_assert!(overloads.just_resolved.is_empty());
        let mut decls = mem::take(&mut overloads.pending); // Take any new things found since last time we looked at this function that haven't been typechecked yet.
        if decls.is_empty() {
            return Ok(());
        }
        outln!(LogTag::Generics, "Compute overloads of {} = L{i}", self.pool.get(overloads.name),);
        while let Some(f) = decls.pop() {
            decls.extend(mem::take(&mut self.program.overload_sets[i].pending));

            if self.program[f].evil_uninit {
                continue;
            }
            if self.ensure_resolved_sign(f).is_err() {
                todo!();
            }
            match self.infer_types(f) {
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
                        func: f,
                    });
                }
                Ok(None) => {
                    if let Some(arg) = self.program[f].finished_arg {
                        self.program.overload_sets[i].ready.push(OverloadOption { arg, ret: None, func: f });
                    } else {
                        todo!()
                    }
                }
                Err(e) => return Err(e),
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
                0 => arg.set(Value::Unit.into(), TypeId::unit()),
                // NOTE: not just taking arg.expr because need to preserve the .ty for int/float literals now that I don't track redundantly in Expr::Value! -- Apr 30
                1 => *arg = parts.into_iter().next().unwrap(),
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
                if matches!(flag, Flag::Llvm | Flag::Aarch64) {
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
        ExecTime::Both => unreachable!(),
    };

    // TODO: kinda cringe.
    match target {
        TargetArch::Aarch64 => overloads.ready.retain(|f| !program[f.func].has_tag(Flag::Llvm)),
        TargetArch::Llvm => overloads.ready.retain(|f| !program[f.func].has_tag(Flag::Aarch64)),
    }
}
