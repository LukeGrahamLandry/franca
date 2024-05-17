use codemap::Span;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};

use crate::ast::{Expr, FatExpr, FuncId, LazyType, OverloadOption, OverloadSet, OverloadSetId, Pattern, TypeId, Var, VarType};
use crate::bc::{Value, Values};
use crate::compiler::{Compile, DebugState, Res};
use crate::logging::PoolLog;
use crate::{assert, assert_eq, err, unwrap};
use std::mem;
use std::ops::DerefMut;

impl<'a, 'p> Compile<'a, 'p> {
    pub fn maybe_direct_fn(&mut self, f: &mut FatExpr<'p>, arg: &mut FatExpr<'p>, ret: Option<TypeId>) -> Res<'p, Option<FuncId>> {
        // TODO: more general system for checking if its a constant known expr instead of just for functions?
        Ok(match f.deref_mut() {
            &mut Expr::GetVar(i) => {
                if i.kind == VarType::Const {
                    let id = self.resolve_function(i, arg, ret)?; // TODO: error here is probably fine, just return None
                    Some(id)
                } else {
                    None
                }
            }
            &mut Expr::Value {
                value: Values::One(Value::GetFn(id)),
                ..
            }
            | &mut Expr::WipFunc(id) => {
                self.named_args_to_tuple(arg, id)?;
                Some(id)
            }
            &mut Expr::Value {
                value: Values::One(Value::OverloadSet(i)),
                ..
            } => {
                let id = self.resolve_in_overload_set(arg, ret, i)?;
                Some(id)
            }
            // TODO: this shouldn't be nessisary. Values::Many should collapse. You get here when a macro tries to literal_ast(OverloadSet)!unquote?
            Expr::Value {
                value: Values::Many(vals), ..
            } => {
                // TODO: revisit when SplitFunc can't fit in one slot anymore.
                debug_assert!(vals.len() != 1);
                None
            }
            Expr::Closure(_) => {
                // This doesn't come up super often. It means you called a closure inline where you declared it for some reason.
                let arg_ty = self.compile_expr(arg, ret)?;
                let id = self.promote_closure(f, Some(arg_ty), ret)?;
                self.named_args_to_tuple(arg, id)?;
                Some(id)
            }
            &mut Expr::GetNamed(i) => {
                err!("GetNamed func: {}", self.pool.get(i));
            }
            _ => None,
        })
    }

    // TODO: rename this, it means resolve overloads, not resovle variables which is ambigous now that they happen together sometimes.
    // TODO: better error messages
    pub fn resolve_function(&mut self, name: Var<'p>, arg: &mut FatExpr<'p>, requested_ret: Option<TypeId>) -> Res<'p, FuncId> {
        let state = DebugState::ResolveFnRef(name);
        self.push_state(&state);

        // TODO: get rid of any
        if let Some(ty) = requested_ret {
            assert!(!ty.is_unknown());
        }

        if let Some((value, _)) = self.find_const(name)? {
            match value {
                Values::One(Value::GetFn(f)) => {
                    self.named_args_to_tuple(arg, f)?;
                    self.pop_state(state);
                    Ok(f)
                }
                Values::One(Value::GetNativeFnPtr(_)) => {
                    err!("TODO: const GetNativeFnPtr?",)
                }
                Values::One(Value::OverloadSet(i)) => {
                    let out = self.resolve_in_overload_set(arg, requested_ret, i)?;
                    self.pop_state(state);
                    Ok(out)
                }
                _ => err!("Expected function for {} but found {:?}", name.log(self.pool), value),
            }
        } else {
            // TODO: use self.program.vars[name.1].loc to show the declaration site.
            err!("Missing constant {} (forgot to make a Fn(A, R) 'const'?)", name.log(self.pool))
        }
    }

    pub fn resolve_in_overload_set(&mut self, arg: &mut FatExpr<'p>, requested_ret: Option<TypeId>, i: OverloadSetId) -> Res<'p, FuncId> {
        let name = self.program[i].name;
        self.compute_new_overloads(i)?;
        let mut overloads = self.program[i].clone(); // Sad
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            self.prune_overloads_by_named_args(&mut overloads, pattern)?;

            // TODO: my named args test doesn't work without this
            if overloads.ready.len() == 1 {
                let id = overloads.ready[0].func;
                self.named_args_to_tuple(arg, id)?;
                return Ok(id);
            }
        }

        if overloads.ready.is_empty() {
            err!("No overload found for {i:?}: {}", self.pool.get(name));
        }

        match self.type_of(arg) {
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

                if overloads.ready.len() > 1 {
                    let special = self.emit_special_body(overloads.ready[0].func)?;
                    if !special {
                        println!("warn: this does not bode well")
                    }

                    // It could be a builtin (like add) that exists for different architectures. Merge them into one.
                    for j in (1..overloads.ready.len()).rev() {
                        assert!(
                            overloads.ready[j].arg == overloads.ready[0].arg && overloads.ready[j].ret == overloads.ready[0].ret,
                            "overload missmatch. unreachable?"
                        );

                        let f = overloads.ready[j].func;
                        // to do the merge, we want jitted_code/llvm_ir to be in thier slots, so have to do that now since it might not be done yet.
                        let special = self.emit_special_body(f)?;
                        if !special {
                            println!("warn: this does not bode well")
                        }

                        macro_rules! merge {
                            ($field:ident) => {{
                                let func = &self.program[f];
                                if let Some(val) = func.$field.clone() {
                                    self.last_loc = Some(self.program[f].loc);
                                    let tags = self.program[f].annotations.clone();
                                    let target = &mut self.program[overloads.ready[0].func];
                                    // assert!(
                                    //     target.$field.is_none(),
                                    //     "tried to merge overwrite. {} {} {:?} <- {:?}",
                                    //     self.pool.get(target.name),
                                    //     stringify!($field),
                                    //     overloads.ready[0].func,
                                    //     overloads.ready[j].func
                                    // );
                                    // TODO: it shouldn't happen twice. i guess it happens on emit_body? but how can that happen before this and still have two of the same overload.
                                    if let Some(prev) = &target.$field {
                                        assert_eq!(
                                            prev,
                                            &val,
                                            "tried to merge overwrite. {} {} {:?} <- {:?}",
                                            self.pool.get(target.name),
                                            stringify!($field),
                                            overloads.ready[0].func,
                                            overloads.ready[j].func
                                        );
                                    }
                                    target.$field = Some(val);
                                    target.annotations.extend(tags);
                                    true
                                } else {
                                    false
                                }
                            }};
                        }

                        if merge!(llvm_ir) | merge!(cl_emit_fn_ptr) | merge!(jitted_code) | merge!(comptime_addr) {
                            // TODO: remove from program as well?
                            overloads.ready.remove(j); // iter rev so its fine
                            self.program[i].ready.retain(|o| o.func != f); // we cloned, so change the original too.
                        }
                    }
                }

                if overloads.ready.len() == 1 {
                    let id = overloads.ready[0].func;
                    self.named_args_to_tuple(arg, id)?;
                    return Ok(id);
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

                fn _where_the_fuck_am_i(comp: &Compile, loc: Span) {
                    let diagnostic = Diagnostic {
                        level: Level::Error,
                        message: String::from("???"),
                        code: None,
                        spans: vec![SpanLabel {
                            span: loc,
                            label: None,
                            style: SpanStyle::Primary,
                        }],
                    };
                    let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&comp.parsing.codemap));
                    emitter.emit(&[diagnostic]);
                }

                // where_the_fuck_am_i(self, arg.loc);
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

    pub fn compute_new_overloads(&mut self, i: OverloadSetId) -> Res<'p, ()> {
        let overloads = &mut self.program[i];
        // debug_assert!(overloads.just_resolved.is_empty());
        let mut decls = mem::take(&mut overloads.pending); // Take any new things found since last time we looked at this function that haven't been typechecked yet.
        if decls.is_empty() {
            return Ok(());
        }
        while let Some(f) = decls.pop() {
            decls.extend(mem::take(&mut self.program[i].pending));

            if self.program[f].evil_uninit {
                continue;
            }
            if self.ensure_resolved_sign(f).is_err() {
                todo!();
            }
            match self.infer_types(f) {
                Ok(Some(f_ty)) => {
                    // TODO: this is probably wrong if you use !assert_compile_error
                    self.program[i].ready.push(OverloadOption {
                        arg: f_ty.arg,
                        ret: Some(f_ty.ret),
                        func: f,
                    });
                }
                Ok(None) => {
                    if let Some(arg) = self.program[f].finished_arg {
                        self.program[i].ready.push(OverloadOption { arg, ret: None, func: f });
                    } else {
                        todo!()
                    }
                }
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    fn named_args_to_tuple(&mut self, arg: &mut FatExpr<'p>, f: FuncId) -> Res<'p, ()> {
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            let expected = &self.program[f].arg;
            assert_eq!(expected.bindings.len(), pattern.bindings.len());
            let names = expected.flatten_names();
            let mut parts = Vec::with_capacity(names.len());
            for name in names {
                let index = unwrap!(
                    pattern.bindings.iter().position(|p| p.name() == Some(name)),
                    "missing named argument {name:?}"
                );
                let value = pattern.bindings.remove(index);
                assert!(matches!(value.ty, LazyType::Infer), "use '=' not ':' for named arg");
                parts.push(unwrap!(value.default, "named arg missing value"));
            }
            debug_assert!(pattern.bindings.is_empty());
            match parts.len() {
                0 => todo!("untested"), // arg.set(Value::Unit.into(), TypeId::unit),
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
