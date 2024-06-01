use codemap::Span;
use codemap_diagnostic::{Diagnostic, Emitter, Level, SpanLabel, SpanStyle};

use crate::ast::{
    garbage_loc, Expr, FatExpr, FnFlag, FuncId, FuncImpl, LazyType, OverloadOption, OverloadSet, OverloadSetId, Pattern, TypeId, TypeInfo, Var,
    VarType,
};
use crate::bc::from_values;
use crate::compiler::{Compile, DebugState, ExecStyle, Res};
use crate::ffi::InterpSend;
use crate::logging::PoolLog;
use crate::{assert, assert_eq, err, unwrap};
use std::mem;
use std::ops::DerefMut;

impl<'a, 'p> Compile<'a, 'p> {
    pub fn maybe_direct_fn(&mut self, f: &mut FatExpr<'p>, arg: &mut FatExpr<'p>, ret: Option<TypeId>) -> Res<'p, Option<FuncId>> {
        let f_ty = f.ty;
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
            Expr::Value { value, .. } => {
                if f_ty == TypeId::overload_set {
                    let i: OverloadSetId = from_values(self.program, value.clone())?;
                    let id = self.resolve_in_overload_set(arg, ret, i)?;
                    Some(id)
                } else if matches!(self.program[f_ty], TypeInfo::Fn(_)) || f_ty == FuncId::get_or_create_type(self.program) {
                    let id = value.unwrap_func_id();
                    self.adjust_call(arg, id)?;
                    Some(id)
                } else {
                    err!("not callable",)
                }
            }
            &mut Expr::WipFunc(id) => {
                self.adjust_call(arg, id)?;
                Some(id)
            }
            Expr::Closure(_) => {
                // This doesn't come up super often. It means you called a closure inline where you declared it for some reason.
                let arg_ty = self.compile_expr(arg, ret)?;
                let id = self.promote_closure(f, Some(arg_ty), ret)?;
                self.adjust_call(arg, id)?;
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

        if let Some((value, ty)) = self.find_const(name)? {
            // TODO: copy paste
            if ty == TypeId::overload_set {
                let i: OverloadSetId = from_values(self.program, value.clone())?;
                let id = self.resolve_in_overload_set(arg, requested_ret, i)?;
                self.pop_state(state);
                Ok(id)
            } else if matches!(self.program[ty], TypeInfo::Fn(_)) || ty == FuncId::get_or_create_type(self.program) {
                let id = value.unwrap_func_id();
                self.adjust_call(arg, id)?;
                self.pop_state(state);
                Ok(id)
            } else {
                err!("Expected function for {} but found {:?}", name.log(self.pool), value);
            }
        } else {
            // TODO: use self.program.vars[name.1].loc to show the declaration site.
            err!("Missing constant {} (forgot to make a Fn(A, R) 'const'?)", name.log(self.pool))
        }
    }

    pub fn resolve_in_overload_set(&mut self, arg: &mut FatExpr<'p>, requested_ret: Option<TypeId>, i: OverloadSetId) -> Res<'p, FuncId> {
        let name = self.program[i].name;
        // This might be a bad idea. its really fucked up if you accidently create an overload with the same types but different arity.
        // should at least have an optional post-check for that.
        let arity = self.arity(arg);
        self.compute_new_overloads(i, Some(arity))?;
        let mut overloads = self.program[i].clone(); // Sad
        overloads.ready.retain(|o| o.arity == arity);
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            self.prune_overloads_by_named_args(&mut overloads, pattern)?;

            // TODO: my named args test doesn't work without this
            if overloads.ready.len() == 1 {
                let id = overloads.ready[0].func;
                self.adjust_call(arg, id)?;
                return Ok(id);
            }
            todo!("not tested. below assumes arg is tuple")
        }

        if overloads.ready.len() == 1 {
            let id = overloads.ready[0].func;
            self.adjust_call(arg, id)?;
            return Ok(id);
        }

        if overloads.ready.is_empty() {
            err!("No overload found for {i:?}: {}", self.pool.get(name));
        }

        let original = overloads.clone();

        if let Some(req) = requested_ret {
            overloads
                .ready
                .retain(|check| check.ret.is_none() || (req == check.ret.unwrap() || check.ret.unwrap().is_never()));
        }

        if arity == 1 {
            match self.type_of(arg) {
                Ok(Some(arg_ty)) => {
                    // TODO: need to factor this part out so that 'const F: <> = some_overload_set' works properly.
                    // TODO: Never needs to not be a special case. Have like an auto cast graph thingy.

                    // TODO: PROBLEM: this doiesnt do voidptr check. it worked before because i did filter_arch first and there happened to only be one so it didnt even get here and then th e type check passed latter because it knows the rules.
                    overloads.ready.retain(|check| check.arg == arg_ty);

                    if overloads.ready.len() > 1 {
                        self.do_merges(&mut overloads.ready, i)?;
                    }
                    if overloads.ready.len() == 1 {
                        let id = overloads.ready[0].func;
                        self.adjust_call(arg, id)?;
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
                    // TODO: do this for the other case too! its probably more common anyway.
                    let mut msg = String::new();
                    use std::fmt::Write;
                    writeln!(msg, "not found {}", log_goal(self)).unwrap();
                    for f in original.ready {
                        let yes = f.arg == arg_ty;
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
                            !self.program[f].get_flag(FnFlag::NotEvilUninit)
                        )
                        .unwrap()
                    }
                    writeln!(msg, "Maybe you forgot to instantiate a generic?").unwrap();

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
        } else {
            // TODO: have a version of type_of that can't fail and if that can do it on the arg cause its simple, don't need to ever do multiple iterations here.
            let Expr::Tuple(parts) = &mut arg.expr else {
                err!("arg of call with multiple arguments should be tuple",)
            };

            for (i, part) in parts.iter_mut().enumerate() {
                match self.type_of(part) {
                    Ok(Some(part_ty)) => {
                        overloads.ready.retain(|check| {
                            let types = self.program.tuple_types(check.arg).expect("arity");
                            debug_assert_eq!(types.len(), check.arity as usize);
                            types[i] == part_ty
                        });
                        if overloads.ready.len() == 1 {
                            break;
                        }
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
            if overloads.ready.len() > 1 {
                self.do_merges(&mut overloads.ready, i)?;
            }
            if overloads.ready.len() == 1 {
                let id = overloads.ready[0].func;
                self.adjust_call(arg, id)?;
                return Ok(id);
            }

            err!("ambigous overload",)
        }
    }

    // TODO: use required_arity to typecheck less things.
    pub fn compute_new_overloads(&mut self, i: OverloadSetId, required_arity: Option<u16>) -> Res<'p, ()> {
        let overloads = &mut self.program[i];
        // debug_assert!(overloads.just_resolved.is_empty());
        let mut decls = mem::take(&mut overloads.pending); // Take any new things found since last time we looked at this function that haven't been typechecked yet.
        if decls.is_empty() {
            return Ok(());
        }
        while let Some(f) = decls.pop() {
            decls.extend(mem::take(&mut self.program[i].pending));

            if !self.program[f].get_flag(FnFlag::NotEvilUninit) {
                continue;
            }
            if self.ensure_resolved_sign(f).is_err() {
                todo!();
            }
            let arity = self.program[f].arg.bindings.len() as u16;
            if let Some(required_arity) = required_arity {
                if required_arity != arity {
                    // TODO
                }
            }
            match self.infer_types(f) {
                Ok(Some(f_ty)) => {
                    // TODO: this is probably wrong if you use !assert_compile_error
                    self.program[i].ready.push(OverloadOption {
                        arity,
                        arg: f_ty.arg,
                        ret: Some(f_ty.ret),
                        func: f,
                    });
                }
                Ok(None) => {
                    if let Some(arg) = self.program[f].finished_arg {
                        self.program[i].ready.push(OverloadOption {
                            arg,
                            ret: None,
                            func: f,
                            arity,
                        });
                    } else {
                        todo!()
                    }
                }
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    pub(crate) fn do_merges(&mut self, overloads: &mut Vec<OverloadOption>, i: OverloadSetId) -> Res<'p, ()> {
        let mut merged = vec![];

        let output = overloads[0].clone();
        self.ensure_compiled(output.func, ExecStyle::Jit)?; // we're hoping its inline asm so ExecStyle doesn't matter

        // It could be a builtin (like add) that exists for different architectures. Merge them into one.
        for opt in overloads.drain(1..) {
            debug_assert!(merged.len() < 10); // wtf
            assert!(opt.arg == output.arg, "overload missmatch. unreachable?");
            assert!(opt.ret == output.ret, "overload missmatch. unreachable? {:?} {:?}", opt.ret, output.ret);

            let f = opt.func;
            // to do the merge, we want jitted_code/llvm_ir to be in thier slots, so have to do that now since it might not be done yet.
            self.ensure_compiled(f, ExecStyle::Jit)?; // we're hoping its inline asm so ExecStyle doesn't matter

            match &self.program[f].body {
                FuncImpl::Redirect(_) => {} // TODO: i'd rather not get here a billion times
                FuncImpl::Empty | FuncImpl::Normal(_) => err!("hmmm {}", self.program[f].body.log(self.pool)),
                FuncImpl::Merged(parts) => {
                    merged.extend(parts.iter().cloned());
                }
                _ => {
                    // let tags = self.program[f].annotations.clone();
                    // target.annotations.extend(tags); // TODO: ?
                    // let mut new = FuncImpl::Empty;
                    let mut new = FuncImpl::Redirect(output.func);
                    mem::swap(&mut self.program[f].body, &mut new);
                    merged.push(new);
                    self.program[f].annotations.clear();

                    // TODO: remove from program as well?
                    // we cloned, so change the original too.
                    self.program[i].ready.retain(|o| o.func != f);
                    // assert!(self.program[f].redirect.is_none());
                    // probably does nothing.
                    // self.program[f].redirect = Some(overloads[0].func);
                }
            }
        }

        // i dare you to make this not an ass copy-paste.
        match &self.program[output.func].body {
            FuncImpl::Redirect(_) | FuncImpl::Empty | FuncImpl::Normal(_) => err!("ambigous overload",),
            FuncImpl::Merged(parts) => {
                merged.extend(parts.iter().cloned());
            }
            _ => {
                // let tags = self.program[f].annotations.clone();
                // target.annotations.extend(tags); // TODO: ?
                // let mut new = FuncImpl::Empty;
                let mut new = FuncImpl::Redirect(output.func);
                mem::swap(&mut self.program[output.func].body, &mut new);
                merged.push(new);
            }
        }

        self.program[output.func].annotations.clear(); // TODO: this prevents it from tring to emit_special body multiple times.
        self.program[output.func].body = FuncImpl::Merged(merged);
        // overloads.push(output.clone());
        // self.program[i].ready.push(output);

        Ok(())
    }

    // - convert named arg to a tuple
    fn adjust_call(&mut self, arg: &mut FatExpr<'p>, f: FuncId) -> Res<'p, ()> {
        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
            let expected = &self.program[f].arg;
            assert_eq!(expected.bindings.len(), pattern.bindings.len());
            let names = expected.flatten_names();
            let mut parts = Vec::with_capacity(names.len());
            for name in names {
                let index = unwrap!(
                    pattern.bindings.iter().position(|p| p.name() == Some(name)),
                    "missing named argument {}",
                    self.pool.get(name)
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

pub fn where_the_fuck_am_i(comp: &Compile, loc: Span) {
    if loc == garbage_loc() {
        println!("called where_the_fuck_am_i on garbage_loc");
        return;
    }
    let diagnostic = Diagnostic {
        level: Level::Note,
        message: String::from("you are here"),
        code: None,
        spans: vec![SpanLabel {
            span: loc,
            label: None,
            style: SpanStyle::Primary,
        }],
    };
    let mut out = vec![];
    let mut emitter = Emitter::vec(&mut out, Some(&comp.parsing.codemap));
    emitter.emit(&[diagnostic]);
    drop(emitter);
    println!("{}", String::from_utf8(out).unwrap())
}
