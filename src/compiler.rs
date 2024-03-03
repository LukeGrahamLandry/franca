//! Converts ASTs into... simpler asts.
//! Type checking, overload resolution, implicit function calls, inlining, monomorphization, etc.
//! Uses the interpreter for comptime evalutation (build scripts, generics, macros, etc).

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use interp_derive::InterpSend;
use std::collections::HashMap;
use std::fmt::Write;
use std::hash::Hash;
use std::mem;
use std::ops::DerefMut;
use std::{ops::Deref, panic::Location};

use crate::ast::{
    garbage_loc, Binding, FatStmt, Field, IntType, Name, OverloadOption, OverloadSet, Pattern, Var, VarInfo, VarType
};

use crate::{bc::*, experiments::builtins};
use crate::ffi::InterpSend;
use crate::logging::{outln, LogTag, PoolLog};
use crate::experiments::reflect::Reflect;
use crate::{
    ast::{Expr, FatExpr, FnType, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};

use crate::logging::{
    assert, assert_eq, err, ice, logln, unwrap,
    LogTag::{ShowErr, ShowPrint},
};

#[derive(Clone)]
pub struct CompileError<'p> {
    pub internal_loc: &'static Location<'static>,
    pub loc: Option<Span>,
    pub reason: CErr<'p>,
    pub trace: String,
    pub value_stack: Vec<Value>,
    pub call_stack: String,
}

#[derive(Clone, Debug)]
pub enum CErr<'p> {
    UndeclaredIdent(Ident<'p>),
    ComptimeCallAtRuntime,
    Ice(&'static str),
    IceFmt(String),
    LeakedValue,
    StackDepthLimit,
    AddrRvalue(FatExpr<'p>),
    TypeError(&'static str, Values),
    TypeCheck(TypeId, TypeId, &'static str),
    Msg(String),
    AmbiguousCall,
    VarNotFound(Var<'p>),
    InterpMsgToCompiler(Ident<'p>, Values, StackAbsoluteRange),
}

pub type Res<'p, T> = Result<T, CompileError<'p>>;

pub struct Compile<'a, 'p, Exec: Executor<'p>> {
    pub pool: &'a StringPool<'p>,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    pub debug_trace: Vec<DebugState<'p>>,
    pub anon_fn_counter: usize,
    currently_inlining: Vec<FuncId>,
    currently_compiling: Vec<FuncId>, // TODO: use this to make recursion work
    last_loc: Option<Span>,
    pub executor: Exec,
    pub program: &'a mut Program<'p>,
    pub jitted: Vec<*const u8>,
    pub save_bootstrap: Vec<FuncId>
}

#[derive(Clone, Debug, PartialEq)]
pub enum DebugState<'p> {
    Compile(FuncId),
    EnsureCompiled(FuncId, ExecTime),
    RunInstLoop(FuncId),
    ComputeCached(FatExpr<'p>),
    ResolveFnType(FuncId),
    EvalConstants(FuncId),
    Msg(String),
    EmitBody(FuncId),
    EmitCapturingCall(FuncId),
    ResolveFnRef(Var<'p>),
}

#[derive(Clone, Debug, InterpSend)]
pub struct FnWip<'p> {
    pub stack_slots: usize,
    pub vars: HashMap<Var<'p>, TypeId>, // TODO: use a vec
    pub when: ExecTime,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub constants: Constants<'p>,
    pub callees: Vec<FuncId>,
}


impl<'a, 'p, Exec: Executor<'p>> Compile<'a, 'p, Exec> {
    pub fn new(pool: &'a StringPool<'p>, program: &'a mut Program<'p>, executor: Exec) -> Self {
        Self {
            pool,
            debug_trace: vec![],
            anon_fn_counter: 0,
            currently_inlining: vec![],
            last_loc: None,
            currently_compiling: vec![],
            program,
            executor,
            jitted: vec![],
            save_bootstrap: vec![],
        }
    }

    #[track_caller]
    pub fn log_trace(&self) -> String {
        let mut out = String::new();
        if !cfg!(feature = "some_log") {
            return out;
        }
        writeln!(out, "=== TRACE ===").unwrap();
        // writeln!(out, "{}", Location::caller()).unwrap();  // Always called from the same place now so this is useless

        for (i, s) in self.debug_trace.iter().enumerate() {
            writeln!(out, "{i} {};", s.log(self.pool, self.program)).unwrap();
        }
        writeln!(out, "=============").unwrap();
        out
    }

    #[track_caller]
    fn push_state(&mut self, s: &DebugState<'p>) {
        self.debug_trace.push(s.clone());
    }

    fn pop_state(&mut self, _s: DebugState<'p>) {
        let _found = self.debug_trace.pop().expect("state stack");
        // debug_assert_eq!(found, s);  // TODO: fix the way i deal with errors. i dont always short circuit so this doesnt work
    }

    pub fn add_declarations(&mut self, ast: Func<'p>) -> Res<'p, FuncId> {
        let f = self.add_func(ast, &Constants::empty())?;
        self.ensure_compiled(f, ExecTime::Comptime)?;
        Ok(f)
    }

    pub fn lookup_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        if let Some(decls) = self.program.declarations.get(&name) {
            if decls.len() == 1 {
                return Some(decls[0]);
            }
        }
        None
    }

    pub fn compile(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        if !add_unique(&mut self.currently_compiling, f) {
            // This makes recursion work. 
            return Ok(());
        }
        let state = DebugState::Compile(f);
        self.push_state(&state);
        debug_assert!(!self.program.funcs[f.0].evil_uninit);
        let mut result = self.ensure_compiled(f, when);
        if result.is_ok() {
            let func = &self.program.funcs[f.0];
            let wip = func.wip.as_ref().unwrap();
            let callees = wip.callees.clone();
            for id in callees {
                self.compile(id, when)?;
            }
            let func = &self.program.funcs[f.0];
            result = self.executor.compile_func(self.program, f);
            if result.is_ok() && func.has_tag(self.pool, "jit") {
                self.jit(f)?;
            }
        }
        
        let result = self.tag_err(result);
        self.pop_state(state);
        self.currently_compiling.retain(|check| *check != f);
        result
    }

    pub fn run(&mut self, f: FuncId, arg: Values, _when: ExecTime) -> Res<'p, Values> {
        let state2 = DebugState::RunInstLoop(f);
        self.push_state(&state2);
        let result = self.executor.run_func(self.program, f, arg);
        let result = self.tag_err(result);
        self.pop_state(state2);
        result
    }

    fn compile_and_run(&mut self, f: FuncId, arg: Values, when: ExecTime) -> Res<'p, Values> {
        self.compile(f, when)?;
        self.run(f, arg, when)
    }

    // This is much less painful than threading it through the macros
    fn tag_err<T>(&self, mut res: Res<'p, T>) -> Res<'p, T> {
        if let Err(err) = &mut res {
            err.trace = self.log_trace();
            err.loc = self.last_loc;
            self.executor.tag_error(err);
        }
        res
    }

    #[track_caller]
    fn empty_fn(
        &mut self,
        when: ExecTime,
        func: FuncId,
        loc: Span,
        parent: Option<Constants<'p>>,
    ) -> FnWip<'p> {
        FnWip {
            stack_slots: 0,
            vars: Default::default(),
            when,
            func,
            why: self.log_trace(),
            last_loc: loc,
            constants: parent
                .unwrap_or_else(|| self.program.funcs[func.0].closed_constants.clone()),
            callees: vec![],
        }
    }

    // Any environment constants must already be in the function.
    fn eval_and_close_local_constants(&mut self, f: FuncId) -> Res<'p, ()> {
        let state = DebugState::EvalConstants(f);
        self.push_state(&state);
        let loc = self.program.funcs[f.0].loc;
        debug_assert!(!self.program.funcs[f.0].evil_uninit);
        debug_assert!(self.program.funcs[f.0].closed_constants.is_valid);

        mut_replace!(
            self.program.funcs[f.0].closed_constants,
            |constants| {
                let mut result = self.empty_fn(
                    ExecTime::Comptime,
                    FuncId(f.0 + 10000000), // TODO: do i even need to pass an index? probably just for debugging
                    loc,
                    Some(constants),
                );

                mut_replace!(
                    self.program.funcs[f.0].local_constants,
                    |mut local_constants| {
                        for stmt in &mut local_constants {
                            self.compile_stmt(&mut result, stmt)?;
                        }
                        Ok((local_constants, ()))
                    }
                );

                self.pop_state(state);

                // Now this includes stuff inherited from the parent, plus any constants pulled up from the function body.
                Ok((result.constants, ()))
            }
        );

        Ok(())
    }

    // Don't pass constants in because the function declaration must have closed over anything it needs.
    fn ensure_compiled(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        let func = &self.program.funcs[f.0];
        if func.wip.is_some() {
            return Ok(());
        }
        debug_assert!(!func.evil_uninit);
        debug_assert!(func.closed_constants.is_valid);
        logln!(
            "BEFORE Closed local consts for {}:\n{}",
            func.synth_name(self.pool),
            self.program.log_consts(&func.closed_constants)
        );

        let state = DebugState::EnsureCompiled(f, when);
        self.push_state(&state);
        self.eval_and_close_local_constants(f)?;

        logln!(
            "Start JIT: {:?} \n{}",
            f,
            self.program.funcs[f.0].log(self.pool)
        );
        self.infer_types(f)?;
        let func = &self.program.funcs[f.0];
        let f_ty = func.unwrap_ty();
        let mut result = self.empty_fn(when, f, func.loc, None);
        let ret = self.emit_body(&mut result, f)?;
        self.type_check_arg(ret.ty(), f_ty.ret, "fn ret")?;
        let func = &mut self.program.funcs[f.0];
        func.wip = Some(result);
        logln!("Done JIT: {:?} {}", f, func.synth_name(self.pool));
        self.pop_state(state);
        Ok(())
    }
    
    /// IMPORTANT: this pulls a little sneaky on ya so you can't access the body of the function inside the main emit handlers.
    fn emit_body(&mut self, result: &mut FnWip<'p>, f: FuncId) -> Res<'p, Structured> {
        let state = DebugState::EmitBody(f);
        self.push_state(&state);
        let has_body = self.program.funcs[f.0].body.is_some();
        debug_assert!(!self.program.funcs[f.0].evil_uninit);

        mut_replace!(self.program.funcs[f.0], |func: Func<'p>| {
            assert!(result.when == ExecTime::Comptime || !func.has_tag(self.pool, "comptime"));
            let arguments = func.arg.flatten();
            for (name, ty) in arguments {
                if let Some(name) = name {
                    let _prev = result.vars.insert(name, ty);
                    // assert!(prev.is_none(), "overwrite arg?");  // TODO: but closures inlined multiple times in the same fn
                }
            }
            Ok((func, ()))
        });

        if !has_body {
            let ret = mut_replace!(self.program.funcs[f.0], |func: Func<'p>| {
                // Functions without a body are always builtins.
                // It's convient to give them a FuncId so you can put them in a variable,
                // TODO: but just force inline call.
                logln!(
                    "builtin shim for {} has constants {}",
                    self.program.funcs[f.0].synth_name(self.pool),
                    self.program.log_consts(&func.closed_constants)
                );
                // func.annotations.push(Annotation {
                //     name: self.pool.intern("inline"),
                //     args: None,
                // });
                let ret_ty = func.ret.unwrap();
                // TODO: this check is what prevents making types comptime only work because you need to pass a type to builtin alloc,
                //       but specializing kills the name. But before that will work anyway i need tonot blindly pass on the shim args to the builtin
                //       since the shim might be specialized so some args are in constants instead of at the base of the stack.
                assert!(func.referencable_name, "fn no body needs name");
                Ok((func, (Structured::RuntimeOnly(ret_ty))))
            });
            self.pop_state(state);
            return Ok(ret);
        }
        
        let fn_ty = self.program.funcs[f.0].unwrap_ty();
        let ret_ty = fn_ty.ret;
        let ret_val = mut_replace!(self.program.funcs[f.0].body, |mut body: Option<
            FatExpr<'p>,
        >| {
            let body_expr = body.as_mut().unwrap();
            
            #[cfg(target_arch = "aarch64")]
            if let Expr::SuffixMacro(name, arg) = body_expr.deref_mut() {
                if *name == self.pool.intern("asm") {
                    let src = arg.log(self.pool);
                    let asm_ty = Vec::<u32>::get_type(self.program);
                    let ops = if let Expr::Tuple(parts) = arg.deref_mut().deref_mut() {
                        let ops: Res<'p, Vec<u32>> = parts.iter().map(|op| {
                            if let Ok((ty, val)) = bit_literal(op, self.pool) {
                                assert_eq!(ty.bit_count, 32);
                                Ok(val as u32)
                            } else {
                                err!("not int",)
                            }
                        }).collect();
                        if let Ok(ops) = ops {
                            ops
                        } else {
                            let ops = self.immediate_eval_expr(&result.constants, *arg.clone(), asm_ty)?;
                            unwrap!(Vec::<u32>::deserialize(&mut ops.vec().into_iter()), "")
                        }
                    } else {
                        let ops = self.immediate_eval_expr(&result.constants, *arg.clone(), asm_ty)?;
                        unwrap!(Vec::<u32>::deserialize(&mut ops.vec().into_iter()), "")
                    };
                    outln!(LogTag::Jitted, "=======\ninline asm\n~~~{src}~~~");
                    for op in &ops {
                        outln!(LogTag::Jitted, "{op:#05x}");
                    }
                    outln!(LogTag::Jitted, "\n=======");
                    let map = builtins::copy_to_mmap_exec(ops.into());
                    let value = Value::CFnPtr { ptr: map.1 as usize, ty: fn_ty };
                        let map = map.0.to_box();
                    let map = Box::leak(map); // TODO: dont leak
                    self.program.funcs[f.0].jitted_code = Some(map.to_vec());
                    self.program.funcs[f.0].jitted_asm = Some(value);
                    let _ = self.program.intern_type(TypeInfo::FnPtr(fn_ty));
                    return Ok((None, Structured::RuntimeOnly(ret_ty)));
                }
            }
            
            let hint = if ret_ty.is_any() { None } else { Some(ret_ty) };
            let res = self.compile_expr(result, body_expr, hint)?;
            Ok((body, res))
        });

        let func = &self.program.funcs[f.0];
        self.type_check_arg(ret_val.ty(), func.ret.unwrap(), "bad return value")?;
        self.pop_state(state);
        Ok(ret_val)
    }

    // This is a normal function call. No comptime args, no runtime captures. TODO WIP
    fn emit_runtime_call(
        &mut self,
        result: &mut FnWip<'p>,
        f: FuncId,
        arg_expr: &mut FatExpr<'p>,
    ) -> Res<'p, Structured> {
        let f_ty = self.program.funcs[f.0].unwrap_ty();
        self.compile_expr(result, arg_expr, Some(f_ty.arg))?;

        // self.last_loc = Some(expr.loc); // TODO: have a stack so i dont have to keep doing this.
        let func = &self.program.funcs[f.0];
        // TODO: some huristic based on how many times called and how big the body is.
        // TODO: pre-intern all these constants so its not a hash lookup everytime
        let force_inline = func.has_tag(self.pool, "inline");
        assert!(func.capture_vars.is_empty());
        assert!(!force_inline);
        add_unique(&mut result.callees, f);
        self.ensure_compiled(f, result.when)?;
        Ok(Structured::RuntimeOnly(f_ty.ret))
    }

    // Replace a call expr with the body of the target function. 
    // The idea is having zero cost (50 cycles) closures :)
    fn emit_capturing_call(&mut self, result: &mut FnWip<'p>, f: FuncId, expr_out: &mut FatExpr<'p>) -> Res<'p, Structured> {
        let loc = expr_out.loc;
        debug_assert_ne!(f, result.func, "recusive inlining?");
        let state = DebugState::EmitCapturingCall(f);
        self.push_state(&state);
        let arg_expr = if let Expr::Call(_, arg) = expr_out.deref_mut() {
            arg
        } else {
            ice!("")
        };
        
        self.infer_types(f)?;
        assert!(
            !self.currently_inlining.contains(&f),
            "Tried to inline recursive function."
        );
        self.currently_inlining.push(f);

        // We close them into f's Func but we need to emit into the caller's body.
        self.eval_and_close_local_constants(f)?;
        let func = &self.program.funcs[f.0];
        let my_consts = &func.closed_constants;
        result.constants.add_all(my_consts);

        // TODO: !quote adds a bunch of random shit to captures of the ast it returns. 
        /* 
        for closed in &func.capture_vars {
            assert!(
                result.vars.contains_key(closed) || result.constants.get(*closed).is_some(),
                "Missing captured var {:?}. \nExisting vars: {:?}\nExisting constants:{:?}",
                closed.log(self.pool),
                result
                    .vars
                    .keys()
                    .map(|v| v.log(self.pool))
                    .collect::<Vec<_>>(),
                result
                    .constants
                    .local
                    .keys()
                    .map(|v| v.log(self.pool))
                    .collect::<Vec<_>>()
            );
        }
        */
        
        let pattern = func.arg.clone();
        let locals = func.arg.collect_vars();
        
        // TODO: is locals supposed to be just the new ones introduced or recursivly bubbled up?
        // TODO: now vars aren't unique if you inline the same thing in multiple places. 
        //       I guess i need to walk the whole tree and rebind them but its hard to think about. have to deal with inner captures too. Feb-24
        // TODO: can I mem::take func.body? 
        let new_expr = Expr::Block { body: vec![
            FatStmt { stmt: Stmt::DeclVarPattern { binding: pattern, value: Some(mem::take(arg_expr)) }, annotations: vec![], loc }
        ], result: Box::new(func.body.as_ref().unwrap().clone()), locals: Some(locals) };
        
        expr_out.expr = new_expr;
        self.currently_inlining.retain(|check| *check != f);
        let f_ty = func.unwrap_ty();
        let res = self.compile_expr(result, expr_out, Some(f_ty.ret))?;
        self.pop_state(state);
        Ok(res)
    }

    // TODO: I should probably implement real runtime closures (even if my version is dumb and slow)
    //       and use those for comptime stuff cause it can't possibly be worse than cloning everything and recompiling.
    //       The cloning is only better for runtime functions where we're trying to output a simpler ast that an optimiser can specialize.
    // Curry a function from fn(a: A, @comptime b: B) to fn(a: A)
    // The argument type is evaluated in the function declaration's scope, the argument value is evaluated in the caller's scope.
    fn bind_const_arg(
        &mut self,
        o_f: FuncId,
        arg_name: Var<'p>,
        arg: Structured,
    ) -> Res<'p, FuncId> {
        let mut new_func = self.program.funcs[o_f.0].clone();
        new_func.referencable_name = false;
        let arg_ty =
            self.get_type_for_arg(&new_func.closed_constants, &mut new_func.arg, arg_name)?;
        self.type_check_arg(arg.ty(), arg_ty, "bind arg")?;
        let arg_value = arg.get()?;

        // TODO: not sure if i actually need this but it seems like i should.
        if let Values::One(Value::GetFn(arg_func)) = &arg_value {
            // TODO: support fns nested in tuples.
            let extra_captures = &self.program.funcs[arg_func.0].capture_vars;
            new_func.capture_vars.extend(extra_captures);
            // TODO: do I need to take its closed closed constants too?
        }
        new_func
            .closed_constants
            .insert(arg_name, (arg_value, arg_ty));
        new_func.arg.remove_named(arg_name);

        let known_type = new_func.finished_type.is_some();
        new_func.finished_type = None;
        let f_id = self.program.add_func(new_func);
        // If it was fully resolved before, we can't leave the wrong answer there.
        // But you might want to call bind_const_arg as part of a resolving a generic signeture so its fine if the type isn't fully known yet.
        if known_type {
            self.infer_types(f_id)?;
        }
        Ok(f_id)
    }

    /// It's fine to call this if the type isn't fully resolved yet.
    /// We just need to be able to finish infering for the referenced argument.
    fn get_type_for_arg(
        &mut self,
        constants: &Constants<'p>,
        arg: &mut Pattern<'p>,
        arg_name: Var<'p>,
    ) -> Res<'p, TypeId> {
        for arg in &mut arg.bindings {
            match arg.name {
                Name::Ident(_) => unreachable!(),
                Name::Var(name) => {
                    if name == arg_name {
                        self.infer_types_progress(constants, &mut arg.ty)?;
                        return Ok(arg.ty.unwrap());
                    }
                }
                Name::None => {}
            }
        }
        err!(CErr::VarNotFound(arg_name))
    }

    // TODO: you only need to call this for generic functions that operate on thier own types.
    //       If you're just doing comptime manipulations of a TypeInfo to be used elsewhere,
    //       it can be interpreted as a normal function so you don't have to clone the ast on every call.
    // TODO: fuse this with bind_const_arg. I have too much of a combinatoric explosion of calling styles going on.
    // TODO: !!! maybe call this @generic instead of @comptime? cause other things can be comptime
    // Return type is allowed to use args.
    fn emit_comptime_call(
        &mut self,
        result: &mut FnWip<'p>,
        template_f: FuncId,
        arg_expr: &mut FatExpr<'p>,
    ) -> Res<'p, (Values, TypeId)> {
        // We don't care about the constants in `result`, we care about the ones that existed when `f` was declared.
        // BUT... the *arguments* to the call need to be evaluated in the caller's scope.

        // This one does need the be a clone because we're about to bake constant arguments into it.
        // If you try to do just the constants or chain them cleverly be careful about the ast rewriting.
        let mut func = self.program.funcs[template_f.0].clone();
        debug_assert!(!func.evil_uninit);
        debug_assert!(func.closed_constants.is_valid);
        func.referencable_name = false;
        func.closed_constants.add_all(&result.constants);

        let arg_value = mut_replace!(func.closed_constants, |constants| {
            let types = self.infer_pattern(&constants, &mut func.arg.bindings)?;
            // TODO: update self.program.funcs[f.0]
            let arg_ty = self.program.tuple_of(types);
            self.compile_expr(result, arg_expr, Some(arg_ty))?;
            let arg_value = self.immediate_eval_expr(&constants, arg_expr.clone(), arg_ty)?;
            Ok((constants, arg_value))
        });

        // TODO
        // if func.body.is_none() {
        //     // TODO: don't re-eval the arg type every time.
        //     let name = func.synth_name(self.pool);
        //     return self.interp.runtime_builtin(name, arg_value, None);
        // }

        // Note: the key is the original function, not our clone of it. TODO: do this check before making the clone.
        let key = (template_f, arg_value.clone()); // TODO: no clone
        let found = self.program.generics_memo.get(&key);
        if let Some(found) = found {
            return Ok(found.clone());
        }

        let arg_ty = self.program.type_of_raw(&arg_value);
        // TODO: self.type_check_arg(arg_ty, arg, "bad comtime arg")?;

        // Bind the arg into my new constants so the ret calculation can use it.
        // also the body constants for generics need this. much cleanup pls.
        // TODO: factor out from normal functions?
        
        let f = self.program.add_func(func);
        let func = &self.program.funcs[f.0];
        let args = func.arg.flatten().into_iter();
        let mut arg_values = arg_value.vec().into_iter();
        for (name, ty) in args {
            let size = self.executor.size_of(self.program, ty);  // TODO: better pattern matching 
            let mut values = vec![];
            for _ in 0..size {
                values.push(unwrap!(arg_values.next(), "ICE: missing arguments"));
            }

            if let Some(var) = name {
                let prev = self.program.funcs[f.0]
                    .closed_constants
                    .insert(var, (values.into(), ty));
                assert!(prev.is_none(), "overwrite arg?");
            }
        }
        assert!(arg_values.next().is_none(), "ICE: unused arguments");

        let ret = mut_replace!(
            self.program.funcs[f.0].closed_constants,
            |constants| {
                let ret = mut_replace!(self.program.funcs[f.0].ret, |mut ret: LazyType<
                    'p,
                >| {
                    assert!(self.infer_types_progress(&constants, &mut ret)?);
                    let ret_ty = ret.unwrap();
                    Ok((ret, ret_ty))
                });
                Ok((constants, ret))
            }
        );

        self.program.funcs[f.0].finished_type = Some(FnType { arg: arg_ty, ret });

        let func = &self.program.funcs[f.0];
        logln!(
            "eval_and_close_local_constants emit_comptime_call of {} with consts: {}",
            func.synth_name(self.pool),
            self
                .program
                .log_consts(&self.program.funcs[f.0].closed_constants)
        );
        // TODO: I think this is the only place that relies on it being a whole function.
        self.eval_and_close_local_constants(f)?;

        // Drop the instantiation of the function specialized to this call's arguments.
        // We cache the result and will never need to call it again.
        let mut func = mem::take(&mut self.program.funcs[f.0]);
        let name = func.name;
        let body = func.body.take();
        let result = if let Some(body) = body {
            self.immediate_eval_expr(&func.closed_constants, body, ret)?
        } else {
            unreachable!("builtin")
        };

        let ty = self.program.type_of_raw(&result);
        self.type_check_arg(ty, ret, "generic result")?;

        outln!(
            LogTag::Generics,
            "{:?}={} of {:?} => {:?}",
            key.0,
            self.pool.get(name),
            key.1,
            result
        );

        self
            .program
            .generics_memo
            .insert(key, (result.clone(), ret));

        Ok((result, ret))
    }

    fn compile_stmt(&mut self, result: &mut FnWip<'p>, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        debug_assert!(result.constants.is_valid);
        self.last_loc = Some(stmt.loc);
        match stmt.deref_mut() {
            Stmt::DoneDeclFunc(_) => unreachable!("compiled twice?"),
            Stmt::Eval(expr) => {
                self.compile_expr(result, expr, None)?;
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                kind,
                ..
            } => {
                let no_type = matches!(ty, LazyType::Infer);
                self.infer_types_progress(&result.constants, ty)?;
                let mut expected_ty = ty.unwrap();

                match kind {
                    VarType::Const => {
                        let mut val = match value {
                            Some(value) => {
                                // You don't need to precompile, immediate_eval_expr will do it for you.
                                // However, we want to update value.ty on our copy to use below to give constant pointers better type inference. 
                                // This makes addr_of const for @enum work
                                self.compile_expr(result, value, if no_type { None } else {Some(expected_ty)})?;
                                
                                self.immediate_eval_expr(
                                &result.constants,
                                value.clone(),
                                expected_ty,
                            )?},
                            None => {
                                let name = self.pool.get(name.0);
                                unwrap!(
                                    self.builtin_constant(name),
                                    "uninit (non-blessed) const: {:?}",
                                    name
                                )
                                .0
                                .into()
                            }
                        };
                        if self.program.types[expected_ty.0] == TypeInfo::Type {
                            // HACK. todo: general overloads for cast()
                            val = Value::Type(self.to_type(val)?).into()
                        }
                        let found_ty = self.program.type_of_raw(&val);
                        if no_type {
                            expected_ty = if let Some(value) = value {
                                if !(value.ty.is_unknown() || value.ty.is_any()) {
                                    // HACK This makes addr_of const for @enum work
                                    value.ty
                                } else {
                                    found_ty
                                }
                            } else {
                                found_ty
                            };
                            *ty = LazyType::Finished(expected_ty);
                        } else {
                            self.type_check_arg(found_ty, expected_ty, "var decl")?;
                        }
                        let val_expr = Expr::Value {
                            ty: expected_ty,
                            value: val.clone(),
                        };
                        if let Some(e) = value {
                            e.expr = val_expr;
                        } else {
                            *value = Some(FatExpr::synthetic(val_expr, garbage_loc()));
                        }
                        result.constants.insert(*name, (val, expected_ty));
                        // println!("{}", self.program.log_consts(&result.constants));
                    }
                    VarType::Let | VarType::Var => {
                        match value {
                            None => {
                                if no_type {
                                    err!("uninit vars require type hint {}", name.log(self.pool));
                                }
                            }
                            Some(value) => {
                                let value = self.compile_expr(result, value, Some(expected_ty))?;
                                self.type_check_arg(value.ty(), expected_ty, "var decl")?;
                                if no_type {
                                    *ty = LazyType::Finished(value.ty());
                                    expected_ty = value.ty();
                                }
                            }
                        };

                        let _prev = result.vars.insert(*name, expected_ty);
                        // TODO: closures break this
                        // assert!(prev.is_none(), "shadow is still new var");
                    }
                }
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclNamed { .. } => {
                ice!("Scope resolution failed {}", stmt.log(self.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                let mut func = mem::take(func);
                let var = func.var_name;
                let for_bootstrap = func.has_tag(self.pool, "bs");
                if for_bootstrap {
                    func.referencable_name = false;
                }

                let id = self.add_func(func, &result.constants)?;
                *stmt.deref_mut() = Stmt::DoneDeclFunc(id);

                if for_bootstrap {
                    self.save_bootstrap.push(id);
                }

                // I thought i dont have to add to constants here because we'll find it on the first call when resolving overloads.
                // But it does need to have an empty entry in the overload pool because that allows it to be closed over so later stuff can find it and share if they compile it.
                if let Some(var) = var {
                    if result.constants.get(var).is_none() {
                        let index = self.program.overload_sets.len();
                        self.program.overload_sets.push(OverloadSet(vec![]));
                        result
                            .constants
                            .insert(var, (Value::OverloadSet(index).into(), TypeId::unknown()));
                    }
                }

                let func = &self.program.funcs[id.0];
                if func.has_tag(self.pool, "impl") {
                    for stmt in &func.local_constants {
                        if let Stmt::DeclFunc(new) = &stmt.stmt {
                            if new.referencable_name {
                                // hey, if you're ever looking for this name, try calling me and I might give you one.
                                insert_multi(&mut self.program.impls, new.name, id);
                            }
                        }
                    }
                }
            }
            // TODO: unify with and DeclVar
            // TODO: don't make the backend deal with the pattern matching but it does it for args anyway rn. 
            Stmt::DeclVarPattern { binding, value } => {
                if let Some(value) = value {
                    let ty = binding.ty(self.program);
                    self.compile_expr(result, value, Some(ty))?;
                }
                let arguments = binding.flatten();
                for (name, ty) in arguments {
                    if let Some(name) = name {
                        let _prev = result.vars.insert(name, ty);
                        // assert!(prev.is_none(), "overwrite arg?");  // TODO: but closures inlined multiple times in the same fn
                    }
                }
            }
        }
        Ok(())
    }

    fn maybe_direct_fn(
        &mut self,
        result: &FnWip<'p>,
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
            } => Some(id),
            Expr::Closure(func) => {
                let id = self.add_func(mem::take(func), &result.constants)?;
                Some(id)
            }
            &mut Expr::GetNamed(i) => {
                // TODO: from_bit_literal in an @enum gets here. 
                self.lookup_unique_func(i)
            }
            _ => None,
        })
    }

    #[track_caller]
    fn func_expr(&mut self, id: FuncId) -> Expr<'p> {
        Expr::Value {
            ty: self.program.func_type(id),
            value: Value::GetFn(id).into(),
        }
    }

    fn compile_expr(
        &mut self,
        result: &mut FnWip<'p>,
        expr: &mut FatExpr<'p>,
        requested: Option<TypeId>,
    ) -> Res<'p, Structured> {
        // TODO: it seems like i shouldn't compile something twice
        // debug_assert!(expr.ty.is_unknown(), "{}", expr.log(self.pool));
        let res = self.compile_expr_inner(result, expr, requested)?;
        expr.ty = res.ty();
        Ok(res)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr_inner(
        &mut self,
        result: &mut FnWip<'p>,
        expr: &mut FatExpr<'p>,
        requested: Option<TypeId>,
    ) -> Res<'p, Structured> {
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);
        let loc = expr.loc;

        Ok(match expr.deref_mut() {
            Expr::Closure(_) => {
                let id = self.promote_closure(&result.constants, expr)?;
                Structured::Const(self.program.func_type(id), Value::GetFn(id).into())
            }
            Expr::Call(f, arg) => {
                // self.compile_expr(result, arg, None)?; // TODO: infer arg type from fn??
                if let Some(ty) = self.type_of(result, f)? {
                    if let TypeInfo::FnPtr(f_ty) = self.program.types[ty.0] {
                        f.ty = ty;
                        self.compile_expr(result, arg, Some(f_ty.arg))?;
                        return Ok(Structured::RuntimeOnly(f_ty.ret));
                    }
                }
                
                if let Some(f_id) = self.maybe_direct_fn(result, f, arg, requested)? {
                    return self.compile_call(result, expr, f_id, requested);
                }
                ice!("function not declared or \nTODO: dynamic call: {}\n\n{expr:?}", expr.log(self.pool))
            }
            Expr::Block {
                body,
                result: value,
                ..
            } => {
                for stmt in body {
                    self.compile_stmt(result, stmt)?;
                }
                // TODO: insert drops for locals
                self.compile_expr(result, value, requested)?
            }
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples");
                let values: Res<'p, Vec<_>> = values
                    .iter_mut()
                    .map(|v| self.compile_expr(result, v, None))
                    .collect();
                let values = values?;
                let types: Vec<_> = values.iter().map(|s| s.ty()).collect();
                let ty = self.program.tuple_of(types);
                self.produce_tuple(values, ty)?
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(var) => {
                if let Some(ty) = result.vars.get(var).cloned() {
                    Structured::RuntimeOnly(ty)
                } else if let Some((value, ty)) = result.constants.get(*var) {
                    Structured::Const(ty, value)
                } else {
                    outln!(ShowErr, "VARS: {:?}", result.vars);
                    outln!(
                        ShowErr,
                        "CONSTANTS: {:?}",
                        self.program.log_consts(&result.constants)
                    );
                    let current_func = &self.program.funcs[result.func.0];
                    outln!(ShowErr, "{}", current_func.log_captures(self.pool));
                    ice!(
                        "Missing resolved variable {:?} '{}'",
                        var,
                        self.pool.get(var.0),
                    )
                }
            }
            Expr::GetNamed(name) => {
                let name = self.pool.get(*name);
                if let Some((val, ty)) = self.builtin_constant(name) {
                    expr.expr = Expr::Value {
                        ty,
                        value: val.into(),
                    };
                    Structured::Const(ty, val.into())
                } else {
                    ice!(
                        "Scope resolution failed {} (in Expr::GetNamed)",
                        expr.log(self.pool)
                    );
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::Value { ty, value } => Structured::Const(*ty, value.clone()),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    "if" => self.emit_call_if(result, arg, requested)?,
                    "while" => self.emit_call_while(result, arg)?,
                    "addr" => self.addr_macro(result, arg)?,
                    "quote" => {
                        let arg: FatExpr<'p> = *arg.clone(); // Take the contents of the box, not the box itself!
                        let mut value = arg.serialize_one();
                        value.make_heap_constant();
                        let ty = FatExpr::get_type(self.program);
                        // TODO: want to do this but then my mutation fucks everything. you really do need to do the clone.
                        //       replace with that constant and a clone. need to impl clone. but deep clone that works on heap ptrs.
                        expr.expr = Expr::Value {
                            ty,
                            value: value.clone(),
                        };
                        Structured::Const(ty, value)
                    }
                    "slice" => {
                        let container = self.compile_expr(result, arg, None)?;
                        let ty = self.program.tuple_types(container.ty());
                        let expect = if let Some(types) = ty {
                            let expect = *unwrap!(types.iter().find(|t| !t.is_any()), "all any");
                            for t in types {
                                self.type_check_arg(*t, expect, "match slice types")?;
                            }
                            expect
                        } else {
                            container.ty()
                        };
                        let ptr_ty = self.program.slice_type(expect);
                        Structured::RuntimeOnly(ptr_ty)
                    }
                    "c_call" => {
                        if let Expr::Call(f, arg) = arg.deref_mut().deref_mut().deref_mut() {
                            if let Expr::GetVar(v) = f.deref_mut().deref_mut() {
                                let name = self.pool.get(v.0);
                                if let Some((func, f_ty)) = self.builtin_constant(name) {
                                    assert!(matches!(func, Value::CFnPtr { .. }));
                                    f.expr = Expr::Value {
                                        ty: f_ty,
                                        value: func.into(),
                                    };
                                    let f_ty = unwrap!(self.program.fn_ty(f_ty), "fn");
                                    let res = self.compile_expr(result, arg, Some(f_ty.arg))?;
                                    self.type_check_arg(res.ty(), f_ty.ret, "c ret")?;
                                    Structured::RuntimeOnly(f_ty.ret)
                                } else {
                                   err!("unresolved ffi func {:?}", v.log(self.pool))
                                }
                            } else {
                                err!("c_call expected Expr:Call(Expr::GetVar, ...args)",)
                            }
                        } else {
                            err!("c_call expected Expr:Call",)
                        }
                    }
                    "deref" => {
                        let ptr = self.compile_expr(result, arg, requested)?;
                        let ty = unwrap!(
                            self.program.unptr_ty(ptr.ty()),
                            "deref not ptr: {}",
                            self.program.log_type(ptr.ty())
                        );
                        Structured::RuntimeOnly(ty)
                    }
                    "first" => self.tuple_access(result, arg, requested, 0),
                    "second" => self.tuple_access(result, arg, requested, 1),
                    "reflect_print" => {
                        self.compile_expr(result, arg, None)?;
                        // TODO: replace expr with fn call
                        Structured::RuntimeOnly(TypeId::unit())
                    }
                    "type" => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects.
                        let ty = unwrap!(self.type_of(result, arg)?, "could not infer yet");
                        expr.expr = Expr::Value {
                            ty: TypeId::ty(),
                            value: Value::Type(ty).into(),
                        };
                        self.program.load_value(Value::Type(ty))
                    }
                    "assert_compile_error" => {
                        // TODO: this can still have side-effects on the vm state tho :(
                        let (saved_res, state) = (result.clone(), self.executor.mark_state());
                        let res = self.compile_expr(result, arg, None);
                        assert!(res.is_err());
                        mem::forget(res); // TODO: dont do this. but for now i like having my drop impl that prints it incase i forget  ot unwrap
                        self.executor.restore_state(state);
                        *result = saved_res;
                        expr.expr = Expr::unit();
                        self.program.load_value(Value::Unit)
                    }
                    "comptime_print" => {
                        outln!(ShowPrint, "EXPR : {}", arg.log(self.pool));
                        let value = self.immediate_eval_expr(
                            &result.constants,
                            *arg.clone(),
                            TypeId::unknown(),
                        );
                        outln!(ShowPrint, "VALUE: {:?}", value);
                        expr.expr = Expr::unit();
                        self.program.load_value(Value::Unit)
                    }
                    "struct" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref_mut().deref_mut() {
                            let ty = self.struct_type(result, pattern)?;
                            let ty = self.program.intern_type(ty);
                            expr.expr = Expr::ty(ty);
                            self.program.load_value(Value::Type(ty))
                        } else {
                            err!(
                                "expected map literal: .{{ name: Type, ... }} but found {:?}",
                                arg
                            );
                        }
                    }
                    "enum" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref_mut().deref_mut() {
                            let ty = self.struct_type(result, pattern)?;
                            let ty = self.program.to_enum(ty);
                            expr.expr = Expr::ty(ty);
                            self.program.load_value(Value::Type(ty))
                        } else {
                            err!(
                                "expected map literal: .{{ name: Type, ... }} but found {:?}",
                                arg
                            );
                        }
                    }
                    "tag" => {
                        // TODO: auto deref and typecheking
                        self.addr_macro(result, arg)?;
                        let ty = self
                            .program
                            .intern_type(TypeInfo::Ptr(TypeId::i64()));
                        Structured::RuntimeOnly(ty)
                    }
                    "symbol" => {
                        // TODO: use match
                        let value = if let Expr::GetNamed(i) = arg.deref_mut().deref_mut() {
                            Value::Symbol(i.0)
                        } else if let Expr::GetVar(v) = arg.deref_mut().deref_mut() {
                            Value::Symbol(v.0 .0)
                        } else {
                            ice!("Expected identifier found {arg:?}")
                        };
                        expr.expr = Expr::Value {
                            ty: Ident::get_type(self.program),
                            value: value.into(),
                        };
                        self.program.load_value(value)
                    }
                    _ => err!(CErr::UndeclaredIdent(*macro_name)),
                }
            }
            Expr::FieldAccess(e, name) => {
                let container_ptr = self.addr_macro(result, e)?;
                self.field_access_expr(result, container_ptr, *name)?
            }
            // TODO: replace these with a more explicit node type?
            Expr::StructLiteralP(pattern) => {
                let requested = unwrap!(requested, "struct literal needs type hint");
                let names: Vec<_> = pattern.flatten_names();
                mut_replace!(*pattern, |mut pattern: Pattern<'p>| {
                    // TODO: why must this suck so bad
                    let values: Option<_> = pattern.flatten_exprs_mut();
                    let mut values: Vec<_> = values.unwrap();
                    assert_eq!(names.len(), values.len());
                    let raw_container_ty = self.program.raw_type(requested);

                    let res = match self.program.types[raw_container_ty.0].clone() {
                        TypeInfo::Struct { fields, .. } => {
                            assert_eq!(
                                fields.len(),
                                values.len(),
                                "Cannot assign {values:?} to type {} = {fields:?}",
                                self.program.log_type(requested)
                            );
                            let all = names.into_iter().zip(values).zip(fields);
                            let mut values = vec![];
                            for ((name, value), field) in all {
                                assert_eq!(name, field.name);
                                let value = self.compile_expr(result, value, Some(field.ty))?;
                                self.type_check_arg(value.ty(), field.ty, "struct field")?;
                                values.push(value.unchecked_cast(field.ty));
                            }

                            Structured::RuntimeOnly(requested)
                        }
                        TypeInfo::Enum {
                            cases,
                            ..
                        } => {
                            assert_eq!(
                                1,
                                values.len(),
                                "{} is an enum, value should have one active varient not {values:?}",
                                self.program.log_type(requested)
                            );
                            let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                            let type_hint = cases[i].1;
                            let value = self.compile_expr(result, values[0], Some(type_hint))?;
                            self.type_check_arg(value.ty(), type_hint, "enum case")?;
                            Structured::RuntimeOnly(requested)
                        }
                        _ => err!("struct literal {pattern:?} but expected {:?}", requested),
                    };
                    Ok((pattern, res))
                })
            }
            &mut Expr::String(i) => {
                let bytes = self.pool.get(i).to_string();
                let mut bytes = bytes.serialize_one();
                bytes.make_heap_constant();
                let ty = String::get_type(self.program);
                expr.expr = Expr::Value {
                    ty,
                    value: bytes.clone(),
                };
                Structured::Const(ty, bytes)
            }
            Expr::PrefixMacro { name, arg, target } => {
                let name_str = self.pool.get(name.0);
                // TODO: this doesn't work in general because it doesnt recalculate closure captures.
                if name_str == "with_var" {
                    if let Expr::Tuple(exprs) = &mut arg.expr {
                        assert_eq!(exprs.len(), 2);
                        if let Expr::GetNamed(name) = exprs[0].expr {
                            let name = Var(name, self.program.vars.len());
                            self.program.vars.push(VarInfo {
                                kind: VarType::Var,
                                loc,
                            });

                            let mut res = mem::take(target);
                            let value = mem::take(&mut exprs[1]);
                            res.walk(&mut |expr| {
                                if let Expr::GetNamed(n) = expr {
                                    if *n == name.0 {
                                        *expr = Expr::GetVar(name);
                                    }
                                }
                            });
                            expr.expr = Expr::Block {
                                body: vec![FatStmt {
                                    stmt: Stmt::DeclVar {
                                        name,
                                        ty: LazyType::Infer,
                                        value: Some(value),
                                        dropping: None,
                                        kind: VarType::Var,
                                    },
                                    annotations: vec![],
                                    loc,
                                }],
                                result: res,
                                locals: Some(vec![name]),
                            };
                            return self.compile_expr(result, expr, requested);
                        }
                    }
                } else if name_str == "enum" {
                    self.compile_expr(result, arg, Some(TypeId::ty()))?;
                    let ty = self.immediate_eval_expr(&result.constants, *arg.clone(), TypeId::ty())?;
                    let ty = self.to_type(ty)?;
                    if let Expr::StructLiteralP(pattern) = target.deref_mut().deref_mut().deref_mut() {
                        let mut the_type = Pattern::empty(loc);
                        let unique_ty = self.program.unique_ty(ty);
                        // Note: we expand target to an anonamus struct literal, not a type. 
                        for b in &mut pattern.bindings {
                            assert!(b.default.is_some());
                            assert_eq!(b.lazy(), &LazyType::Infer);
                            b.ty = LazyType::PendingEval(unwrap!(b.default.take(), ""));
                            the_type.bindings.push(Binding {
                                name: b.name,
                                // ty: LazyType::Finished(unique_ty),
                                ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(unique_ty), loc)),
                                default: None,
                            });
                        }
                        let var = Var(self.pool.intern("T"), self.program.vars.len());
                        self.program.vars.push(VarInfo {
                            kind: VarType::Const,
                            loc,
                        });
                        pattern.bindings.push(Binding {
                            name: Name::Var(var),
                            ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(unique_ty), loc)),
                            default: None,
                        });
                        the_type.bindings.push(Binding {
                            name: Name::Var(var),
                            ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(TypeId::ty()), loc)),
                            default: None,
                        });
                        let the_type = Box::new(FatExpr::synthetic(Expr::StructLiteralP(the_type), loc));
                        let the_type = FatExpr::synthetic(Expr::SuffixMacro(self.pool.intern("struct"), the_type), loc);
                        let the_type = self.immediate_eval_expr(&result.constants, the_type, TypeId::ty())?;
                        let the_type = self.to_type(the_type)?;
                        // *expr = FatExpr::synthetic(Expr::PrefixMacro { name: var, arg, target: mem::take(target) }, loc);
                        let value = self.immediate_eval_expr(&result.constants, mem::take(target), the_type)?;
                        let value = Value::new_box(value.vec(), true).into();
                        let ty = self.program.ptr_type(the_type);
                        *expr = FatExpr::synthetic(Expr::Value { ty, value }, loc);
                        expr.ty = ty;
                        return self.compile_expr(result, expr, Some(ty));
                    }
                    err!("Expected struct literal found {target:?}",)
                } else if name_str == "as" {
                    self.compile_expr(result, arg, Some(TypeId::ty()))?;
                    let ty = self.immediate_eval_expr(&result.constants, *arg.clone(), TypeId::ty())?;
                    let ty = self.to_type(ty)?;
                    *expr = mem::take(target);
                    return self.compile_expr(result, expr, Some(ty));
                    
                }

                outln!(
                    LogTag::Macros,
                    "PrefixMacro: {}\nARG: {}\nTARGET: {}",
                    self.pool.get(name.0),
                    arg.log(self.pool),
                    target.log(self.pool)
                );
                let expr_ty = FatExpr::get_type(self.program);
                let mut values = vec![];
                let arg: &mut FatExpr = arg.deref_mut();
                mem::take(arg).serialize(&mut values);
                let target: &mut FatExpr = target.deref_mut();
                mem::take(target).serialize(&mut values);
                let want = FatExpr::get_type(self.program);
                let full_arg = Expr::Value {
                    ty: self.program.tuple_of(vec![expr_ty, expr_ty]),
                    value: Values::Many(values),
                };
                let mut full_arg = FatExpr::synthetic(full_arg, loc);
                let f = self.resolve_function(result, *name, &mut full_arg, Some(want))?;
                assert!(self.program.funcs[f.0].has_tag(self.pool, "annotation"));
                self.infer_types(f)?;
                let get_func = FatExpr::synthetic(self.func_expr(f), loc);
                let full_call =
                    FatExpr::synthetic(Expr::Call(Box::new(get_func), Box::new(full_arg)), loc);

                let mut response = self.immediate_eval_expr(&result.constants, full_call, want);
                loop {
                    match response {
                        Ok(new_expr) => {
                            // TODO: deserialize should return a meaningful error message
                            *expr = unwrap!(
                                FatExpr::deserialize_one(new_expr.clone()),
                                "macro failed. returned {new_expr:?}"
                            );
                            outln!(LogTag::Macros, "OUTPUT: {}", expr.log(self.pool));
                            outln!(LogTag::Macros, "================\n");
                            // Now evaluate whatever the macro gave us.
                            break self.compile_expr(result, expr, requested)?;
                        }
                        Err(msg) => match msg.reason {
                            CErr::InterpMsgToCompiler(name, arg, _) => {
                                let name = self.pool.get(name);
                                let res = self.handle_macro_msg(result, name, arg)?;
                                response = self.executor.run_continuation(self.program, res);
                            }
                            _ => return Err(msg),
                        },
                    }
                }
            }
        })
    }

    fn handle_macro_msg(&mut self, result: &FnWip<'p>, name: &str, arg: Values) -> Res<'p, Values> {
        match name {
            "infer_raw_deref_type" => {
                let mut expr: FatExpr<'p> = unwrap!(arg.deserialize(), "");
                // TODO: make it less painful to return an option
                let ty = unwrap!(self.type_of(result, &mut expr)?, "could not infer type");
                // TODO: deref. you want to get enum not ptr(enum)
                let ty = self.program.raw_type(ty);
                let ty = self.program.types[ty.0].clone();
                Ok(ty.serialize_one())
            }
            "promote_closure" => {
                let mut expr: FatExpr<'p> = unwrap!(arg.deserialize(), "");
                let id = self.promote_closure(&result.constants, &mut expr)?;
                Ok(id.serialize_one())
            }
            "literal_ast" => {
                let mut args = arg.vec().into_iter();
                let ty = self.to_type(unwrap!(args.next(), "").into())?;
                let mut value = Values::Many(args.collect());
                // TODO: stricter typecheck
                // assert_eq!(self.program.slot_count(ty), value.len());
                if ty == TypeId::ty() {
                    if let Ok(id) = value.clone().single()?.to_int() {
                        value = Values::One(Value::Type(TypeId(id as usize)))
                    }
                }
                let result = FatExpr::synthetic(Expr::Value { ty, value }, garbage_loc());
                Ok(result.serialize_one())
            }
            "intern_type" => {
                let arg: TypeInfo = unwrap!(arg.deserialize(), "");
                Ok(self.program.intern_type(arg).serialize_one())
            }
            "print_ast" => {
                outln!(ShowPrint, "print_ast1: {arg:?}");
                let arg: FatExpr = unwrap!(arg.deserialize(), "");
                outln!(ShowPrint, "show...");
                outln!(ShowPrint, "print_ast2: {arg:?}");
                outln!(ShowPrint, "print_ast3: {}", arg.log(self.pool));
                Ok(Values::One(Value::Unit))
            }
            "clone_ast" => {
                let arg: FatExpr = unwrap!(arg.deserialize(), "");
                Ok(arg.serialize_one())
            }
            "get_type_int" => {
                let mut arg: FatExpr = unwrap!(arg.deserialize(), "");
                match arg.deref() {
                    Expr::Call(_, _) => {
                        if let Ok((int, _)) = bit_literal(&arg, self.pool) {
                            return Ok(int.serialize_one())
                        }
                    },
                    Expr::Value { .. } => err!("todo",),
                    _ => {
                        let ty = unwrap!(self.type_of(result, &mut arg)?,"");
                        let ty = self.program.raw_type(ty);
                        if let TypeInfo::Int(int) = self.program.types[ty.0] {
                            return Ok(int.serialize_one());
                        } 
                        err!("expected expr of int type not {}", self.program.log_type(ty));
                        
                    }
                }
                err!("expected binary literal not {arg:?}",);
            }
            "new_pair_ast" => {
                let (a, b): (FatExpr, FatExpr) = unwrap!(arg.deserialize(), "");
                let loc = a.loc;
                Ok(FatExpr::synthetic(Expr::Tuple(vec![a, b]), loc).serialize_one())
            }
            "new_call_ast" => {
                let (a, b): (FatExpr, FatExpr) = unwrap!(arg.deserialize(), "");
                let loc = a.loc;
                Ok(FatExpr::synthetic(Expr::Call(Box::new(a), Box::new(b)), loc).serialize_one())
            }
            _ => err!("Macro send unknown message: {name} with {arg:?}",),
        }
    }

    fn addr_macro(&mut self, result: &mut FnWip<'p>, arg: &mut FatExpr<'p>) -> Res<'p, Structured> {
        match arg.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                if let Some(value_ty) = result.vars.get(var).cloned() {
                    assert!(
                        !value_ty.is_any(),
                        "took address of Any {}",
                        var.log(self.pool)
                    );
                    let kind = self.program.vars[var.1].kind;
                    if kind != VarType::Var {
                        err!(
                            "Can only take address of vars not {kind:?} {}. TODO: allow read field.",
                            var.log(self.pool)
                        )
                    }
                    let ptr_ty = self.program.ptr_type(value_ty);
                    Ok(Structured::RuntimeOnly(ptr_ty))
                } else if let Some(value) = result.constants.get(*var) {
                    // HACK: this is wrong but it makes constant structs work bette
                    if let TypeInfo::Ptr(_) = self.program.types[value.1.0] {
                        return Ok(value.into())
                    }
                    err!("Took address of constant {}: {:?} ({}) {arg:?}", var.log(self.pool), value.1, self.program.log_type(value.1))
                    
                } else {
                    ice!("Missing var {} (in !addr)", var.log(self.pool))
                }
            }
            Expr::SuffixMacro(macro_name, _) => {
                let name = self.pool.get(*macro_name);
                ice!("Took address of macro {name} not supported")
            }
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::FieldAccess(_, _) => self.compile_expr(result, arg, None),
            &mut Expr::GetNamed(i) => err!(CErr::UndeclaredIdent(i)),
            _ => err!(CErr::AddrRvalue(arg.clone())),
        }
    }

    // TODO: better error messages
    fn resolve_function(
        &mut self,
        result: &FnWip<'p>,
        name: Var<'p>,
        arg: &mut FatExpr<'p>,
        mut requested_ret: Option<TypeId>,
    ) -> Res<'p, FuncId> {
        // If there's only one option, we don't care what type it is.
        if let Some(f) = self.lookup_unique_func(name.0) {
            return Ok(f);
        }

        let state = DebugState::ResolveFnRef(name);
        self.push_state(&state);

        let value = if let Some((value, _)) = result.constants.get(name) {
            if let Values::One(Value::GetFn(f)) = value {
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

        match self.type_of(result, arg) {
            Ok(Some(arg_ty)) => {
                match value {
                    Values::One(Value::GetFn(f)) => Ok(f),
                    Values::One(Value::OverloadSet(i)) => {
                        let overloads = &self.program.overload_sets[i];

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

                        // Compute overloads.
                        if let Some(decls) = self.program.declarations.get(&name.0) {
                            outln!(
                                LogTag::Generics,
                                "Compute overloads of {} = L{i}",
                                name.log(self.pool),
                            );
                            let mut found = None;
                            let decls: Vec<_> = decls
                                .iter()
                                .copied()
                                .filter(|f| !overloads.0.iter().any(|old| old.func == *f))
                                .collect();
                            for f in &decls {
                                if let Ok(f_ty) = self.infer_types(*f) {
                                    outln!(
                                        LogTag::Generics,
                                        "- {f:?} is {:?}={} -> {}",
                                        f_ty.arg,
                                        self.program.log_type(f_ty.arg),
                                        self.program.log_type(f_ty.ret)
                                    );
                                    // Even if it wasn't right, still keep the work.
                                    // TODO: this is probably wrong if you use !assert_compile_error
                                    self.program.overload_sets[i].0.push(OverloadOption {
                                        name: name.0,
                                        ty: f_ty,
                                        func: *f,
                                    });
                                    if accept(f_ty) {
                                        assert!(
                                            found.is_none(),
                                            "AmbiguousCall {:?} vs {:?} \n{}",
                                            found,
                                            (f, f_ty),
                                            log_goal(self)
                                        );
                                        found = Some((*f, f_ty));
                                    }
                                }
                            }
                            match found {
                                Some((func, _)) => {
                                    self.pop_state(state);
                                    Ok(func)
                                }
                                None => {
                                    // TODO: put the message in the error so !assert_compile_error doesn't print it.
                                    outln!(ShowErr, "not found {}", log_goal(self));
                                    for f in &decls {
                                        if let Ok(f_ty) = self.infer_types(*f) {
                                            outln!(
                                                ShowErr,
                                                "- found {:?} fn({}) {};",
                                                f,
                                                self.program.log_type(f_ty.arg),
                                                self.program.log_type(f_ty.ret),
                                            );
                                        }
                                    }
                                    outln!(
                                        ShowErr,
                                        "Impls: {:?}",
                                        self.program.impls.get(&name.0)
                                    );
                                    outln!(ShowErr, "Maybe you forgot to instantiate a generic?");

                                    err!(CErr::AmbiguousCall)
                                }
                            }
                        } else {
                            err!(
                                "expected declarations for overload set {:?}\n{}",
                                name.log(self.pool),
                                log_goal(self)
                            )
                        }
                    }
                    _ => {
                        err!(
                            "Expected function for {} but found {:?}",
                            name.log(self.pool),
                            value
                        )
                    }
                }
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
    }

    // TODO: this is clunky. Err means invalid input, None means couldn't infer type (often just not implemented yet).
    // It's sad that this could mutate expr, but the easiest way to typecheck closures is to promote them to functions,
    // which you have to do anyway eventually so you might as well save that work.
    // But it means you have to be careful that if inference fails, you haven't lost the expr in the process.
    // TODO: maybe this should be more fused with the normal compiling process?
    //       then need to be more careful about what gets put in the result
    //       cause like for blocks too, it would be nice to infer a function's return type by just compiling
    //       the function and seeing what you get.
    fn type_of(&mut self, result: &FnWip<'p>, expr: &mut FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        if !expr.ty.is_unknown() {
            return Ok(Some(expr.ty));
        }
        Ok(Some(match expr.deref_mut() {
            Expr::Value { ty, .. } => *ty,
            Expr::Call(f, arg) => {
                if let Expr::GetVar(i) = f.deref_mut().deref_mut() {
                    if let Some(ty) = result.vars.get(i) {
                        if let TypeInfo::FnPtr(f_ty) = self.program.types[ty.0] {
                            return Ok(Some(f_ty.ret));
                        }
                    }
                    if let Ok(fid) = self.resolve_function(result, *i, arg, None) {
                        if self.infer_types(fid).is_ok() {
                            let f_ty = self.program.funcs[fid.0].unwrap_ty();
                            return Ok(Some(f_ty.ret));
                        }
                    }
                }
                return Ok(None);
            }
            Expr::Block { result: e, .. } => return self.type_of(result, e),
            Expr::Tuple(values) => {
                let types: Res<'p, Vec<_>> =
                    values.iter_mut().map(|v| self.type_of(result, v)).collect();
                let types = types?;
                let before = types.len();
                let types: Vec<_> = types.into_iter().flatten().collect();
                assert_eq!(before, types.len(), "some of tuple not infered");
                self.program.tuple_of(types)
            }
            Expr::FieldAccess(container, name) => {
                if let Some(container_ty) = self.type_of(result, container)? {
                    let container_ptr_ty = self.program.ptr_type(container_ty); // TODO: kinda hacky that you need to do this. should be more consistant
                    self.get_field_type(container_ptr_ty, *name)?
                } else {
                    return Ok(None);
                }
            }
            Expr::Closure(_) => {
                if let Ok(id) = self.promote_closure(&result.constants, expr) {
                    // TODO: this unwraps.
                    self.program.func_type(id)
                } else {
                    ice!("TODO: closure inference failed. need to make promote_closure non-destructive")
                }
            }
            Expr::GetNamed(_)
            | Expr::StructLiteralP(_)
            | Expr::ArrayLiteral(_)
            | Expr::RefType(_)
            | Expr::EnumLiteral(_)
            | Expr::PrefixMacro { .. } => return Ok(None),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    "addr" => match arg.deref_mut().deref_mut() {
                        Expr::GetVar(var) => {
                            let value_ty = *result
                                .vars
                                .get(var)
                                .expect("Missing resolved var (TODO: addr of const?)");
                            self.program.intern_type(TypeInfo::Ptr(value_ty))
                        }
                        &mut Expr::GetNamed(i) => {
                            logln!(
                                "UNDECLARED IDENT: {} (in SuffixMacro::addr)",
                                self.pool.get(i)
                            );
                            err!(CErr::UndeclaredIdent(i))
                        }
                        _ => err!(CErr::AddrRvalue(*arg.clone())),
                    },
                    "type" => self.program.intern_type(TypeInfo::Type),
                    "deref" => {
                        let ptr_ty = self.type_of(result, arg)?;
                        if let Some(ptr_ty) = ptr_ty {
                            return Ok(self.program.unptr_ty(ptr_ty));
                        }
                        return Ok(None);
                    }
                    "symbol" => Ident::get_type(self.program),
                    _ => return Ok(None),
                }
            }
            Expr::GetVar(var) => {
                if let Some(ty) = result.vars.get(var).cloned() {
                    ty
                } else if let Some((_, ty)) = result.constants.get(*var) {
                    ty
                } else {
                    // ice!("type check missing var {:?}", var.log(self.pool))
                    return Ok(None);
                }
            }
            Expr::String(_) => String::get_type(self.program),
        }))
    }

    // TODO: this kinda sucks.
    fn builtin_constant(&mut self, name: &str) -> Option<(Value, TypeId)> {
        use TypeInfo::*;
        let ty = match name {
            "i64" => Some(TypeInfo::Int(IntType { bit_count: 64, signed: true })),
            "f64" => Some(F64),
            "Type" => Some(Type),
            "bool" => Some(Bool),
            "Any" => Some(Any),
            "VoidPtr" => Some(VoidPtr),
            _ => None,
        };
        if let Some(ty) = ty {
            let ty = self.program.intern_type(ty);
            let tyty = self.program.intern_type(TypeInfo::Type);
            return Some((Value::Type(ty), tyty));
        }

        macro_rules! cfn {
            ($fn:expr, $ty:expr) => {{
                #[cfg(not(feature = "interp_c_ffi"))]
                {
                    outln!(ShowErr, "Comptime c ffi is disabled.");
                    return None;
                }
                #[cfg(feature = "interp_c_ffi")]
                {
                    (
                        Value::CFnPtr {
                            ptr: $fn as usize,
                            ty: $ty,
                        },
                        self.program.intern_type(TypeInfo::Fn($ty)),
                    )
                }
            }};
        }

        macro_rules! ffi_type {
            ($ty:ty) => {{
                let ty = <$ty>::get_type(self.program);
                (Value::Type(ty), TypeId::ty())
            }};
        }

        Some(match name {
            "true" => (Value::Bool(true), TypeId::bool()),
            "false" => (Value::Bool(false), TypeId::bool()),
            "Symbol" => ffi_type!(Ident),
            "CmdResult" => ffi_type!(crate::interp::CmdResult),
            "FatExpr" => ffi_type!(FatExpr),
            "getchar" => cfn!(
                libc::getchar,
                FnType {
                    arg: TypeId::unit(),
                    ret: TypeId::i64()
                }
            ),
            "putchar" => cfn!(
                libc::putchar,
                FnType {
                    arg: TypeId::i64(),
                    ret: TypeId::i64()
                }
            ),
            _ => {
                let name = self.pool.intern(name);
                if let Some(ty) = self.program.find_ffi_type(name) {
                    (Value::Type(ty), TypeId::ty())
                } else {
                    return None;
                }
            }
        })
    }

    fn infer_types_progress(
        &mut self,
        constants: &Constants<'p>,
        ty: &mut LazyType<'p>,
    ) -> Res<'p, bool> {
        Ok(mut_replace!(*ty, |mut ty| {
            let done = match ty {
                LazyType::EvilUnit => panic!(),
                LazyType::Infer => {
                    ty = LazyType::Finished(TypeId::any());
                    true
                }
                LazyType::PendingEval(e) => {
                    let value = self.immediate_eval_expr(constants, e.clone(), TypeId::ty())?;
                    let res = self.to_type(value)?;
                    ty = LazyType::Finished(res);
                    true
                }
                LazyType::Finished(_) => true, // easy
                LazyType::Different(mut parts) => {
                    let mut done = true;
                    for p in parts.iter_mut() {
                        done &= self.infer_types_progress(constants, p)?;
                    }
                    if done {
                        let types = parts.iter().map(|p| p.unwrap()).collect();
                        let types = self.program.tuple_of(types);
                        ty = LazyType::Finished(types);
                    } else {
                        ty = LazyType::Different(parts);
                    }
                    done
                }
            };
            Ok((ty, done))
        }))
    }

    fn infer_binding_progress(
        &mut self,
        constants: &Constants<'p>,
        binding: &mut Binding<'p>,
    ) -> Res<'p, bool> {
        self.infer_types_progress(constants, &mut binding.ty)
    }

    fn infer_pattern(
        &mut self,
        constants: &Constants<'p>,
        bindings: &mut [Binding<'p>],
    ) -> Res<'p, Vec<TypeId>> {
        let mut types = vec![];
        for arg in bindings {
            assert!(self.infer_binding_progress(constants, arg)?);
            types.push(arg.unwrap());
        }
        Ok(types)
    }

    // Resolve the lazy types for Arg and Ret
    fn infer_types(&mut self, func: FuncId) -> Res<'p, FnType> {
        let f = &self.program.funcs[func.0];
        // TODO: bad things are going on. it changes behavior if this is a debug_assert. 
        //       oh fuck its because of the type_of where you can backtrack if you couldn't infer. 
        //       so making it work in debug with debug_assert is probably the better outcome.  
        assert!(!f.evil_uninit, "{}", self.pool.get(f.name));
        if let Some(ty) = f.finished_type {
            return Ok(ty);
        }

        Ok(mut_replace!(
            self.program.funcs[func.0],
            |mut f: Func<'p>| {
                let state = DebugState::ResolveFnType(func);
                self.last_loc = Some(f.loc);
                self.push_state(&state);
                logln!("RESOLVE: Arg of {func:?}");
                let types = self.infer_pattern(&f.closed_constants, &mut f.arg.bindings)?;
                logln!("RESOLVE: Ret of {func:?}");
                assert!(self.infer_types_progress(&f.closed_constants, &mut f.ret)?);
                let arg = self.program.tuple_of(types);
                let ty = FnType {
                    arg,
                    ret: f.ret.unwrap(),
                };
                f.finished_type = Some(ty);
                self.pop_state(state);
                Ok((f, ty))
            }
        ))
    }

    // Here we're not in the context of a specific function so the caller has to pass in the constants in the environment.
    fn immediate_eval_expr(
        &mut self,
        constants: &Constants<'p>,
        mut e: FatExpr<'p>,
        ret_ty: TypeId,
    ) -> Res<'p, Values> {
        // println!("- Eval {} as {:?}", e.log(self.pool), ret_ty);
        match e.deref_mut() {
            Expr::Value { value, .. } => return Ok(value.clone()),
            Expr::GetVar(var) => {
                // fast path for builtin type identifiers
                if let Some((value, _)) = constants.get(*var) {
                    // debug_assert_ne!(value, Value::Poison);
                    return Ok(value);
                }
                // fallthrough
            }
            Expr::Tuple(elements) => {
                let types = if ret_ty == TypeId::ty() {
                    vec![TypeId::ty(); elements.len()]
                } else if let Some(types) = self.program.tuple_types(ret_ty) {
                    types.to_vec()
                } else {
                    unreachable!("{}", self.program.log_type(ret_ty))
                };
                let values: Res<'p, Vec<Values>> = elements
                    .iter()
                    .zip(types)
                    .map(|(e, ty)| self.immediate_eval_expr(constants, e.clone(), ty))
                    .collect();
                let mut pls = vec![];
                for v in values? {
                    for v in v.vec() {
                        pls.push(v);
                    }
                }
                return Ok(pls.into());
            }
            _ => {} // fallthrough
        }

        let state = DebugState::ComputeCached(e.clone());
        self.push_state(&state);
        let name = format!(
            "$eval_{}${}$",
            self.anon_fn_counter,
            e.deref().log(self.pool)
        );
        let (arg, ret) = Func::known_args(TypeId::unit(), ret_ty, e.loc);
        let mut fake_func = Func::new(
            self.pool.intern(&name),
            arg,
            ret,
            Some(e.clone()),
            e.loc,
            false,
        );
        fake_func.closed_constants = constants.clone();
        fake_func.finished_type = Some(FnType {
            arg: TypeId::unit(),
            ret: ret_ty,
        });
        self.anon_fn_counter += 1;
        let func_id = self.program.add_func(fake_func);
        logln!(
            "Made anon: {func_id:?} = {}",
            self.program.funcs[func_id.0].log(self.pool)
        );
        let result = self.compile_and_run(func_id, Value::Unit.into(), ExecTime::Comptime)?;
        logln!(
            "COMPUTED: {} -> {:?} under {}",
            e.log(self.pool),
            result,
            self.program.funcs[func_id.0].log(self.pool)
        );
        self.pop_state(state);
        Ok(result)
    }

    #[track_caller]
    fn to_type(&mut self, value: Values) -> Res<'p, TypeId> {
        match value {
            Values::One(Value::Unit) => Ok(self.program.intern_type(TypeInfo::Unit)),
            Values::One(Value::Type(id)) => Ok(id),
            Values::Many(values) => {
                let values: Res<'_, Vec<_>> =
                    values.into_iter().map(|v| self.to_type(v.into())).collect();
                Ok(self.program.tuple_of(values?))
            }
            _ => {
                err!(CErr::TypeError("Type", value))
            }
        }
    }

    fn add_func(&mut self, mut func: Func<'p>, constants: &Constants<'p>) -> Res<'p, FuncId> {
        debug_assert!(func.closed_constants.local.is_empty());
        debug_assert!(func.closed_constants.is_valid);
        func.closed_constants = constants.close(&func.capture_vars_const)?;
        // TODO: make this less trash. it fixes generics where it thinks a cpatured argument is var cause its arg but its actually in consts because generic.
        for capture in &func.capture_vars {
            if let Some(val) = constants.get(*capture) {
                func.closed_constants.insert(*capture, val);
            }
        }
        let id = self.program.add_func(func);
        Ok(id)
    }

    // TODO: calling this in infer is wrong because it might fail and lose the function
    fn promote_closure(
        &mut self,
        constants: &Constants<'p>,
        expr: &mut FatExpr<'p>,
    ) -> Res<'p, FuncId> {
        if let Expr::Closure(func) = expr.deref_mut() {
            let f = self.add_func(mem::take(func), constants)?;
            self.infer_types(f)?;
            expr.expr = self.func_expr(f);
            Ok(f)
        } else {
            ice!("want closure")
        }
    }

    fn emit_call_if(
        &mut self,
        result: &mut FnWip<'p>,
        arg: &mut FatExpr<'p>,
        _requested: Option<TypeId>,  // TODO: allow giving return type to infer
    ) -> Res<'p, Structured> {
        let unit = self.program.intern_type(TypeInfo::Unit);
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        if let Expr::Tuple(parts) = arg.deref_mut() {
            let cond = self.compile_expr(result, &mut parts[0], Some(TypeId::bool()))?;
            self.type_check_arg(cond.ty(), TypeId::bool(), "bool cond")?;
            let true_ty = if let Expr::Closure(_) = parts[1].deref_mut() {
                let if_true =  self.promote_closure(&result.constants, &mut parts[1])?;
                let true_ty = self.infer_types(if_true)?;
                self.type_check_eq(true_ty.arg, unit, sig)?;
                self.emit_call_on_unit(result, if_true, &mut parts[1])?;
                true_ty
            } else {
                ice!("if second arg must be func not {:?}", parts[1]);
            };
            if let Expr::Closure(_) = parts[2].deref_mut() {
                let if_false = self.promote_closure(&result.constants, &mut parts[2])?;
                let false_ty = self.infer_types(if_false)?;
                self.type_check_eq(false_ty.arg, unit, sig)?;
                self.type_check_eq(true_ty.ret, false_ty.ret, sig)?;
                
                self.emit_call_on_unit(result, if_false, &mut parts[2])?;
            } else {
                ice!("if third arg must be func not {:?}", parts[2]);
            }
            // TODO: if the condition is const, don't emit the branch.
            Ok(Structured::RuntimeOnly(true_ty.ret))
        } else {
            ice!("if args must be tuple not {:?}", arg);
        }
    }

    fn emit_call_while(
        &mut self,
        result: &mut FnWip<'p>,
        arg: &mut FatExpr<'p>,
    ) -> Res<'p, Structured> {
        let sig = "while(fn(Unit) bool, fn(Unit) Unit)";
        if let Expr::Tuple(parts) = arg.deref_mut() {
            if let Expr::Closure(_) = parts[0].deref_mut() {
                let cond_fn = self.promote_closure(&result.constants, &mut parts[0])?;
                let cond_ty = self.infer_types(cond_fn)?;
                self.type_check_eq(cond_ty.arg, TypeId::unit(), sig)?;
                self.type_check_eq(cond_ty.ret, TypeId::bool(), sig)?;
                self.emit_call_on_unit(result, cond_fn, &mut parts[0])?;
            } else {
                unwrap!(
                    parts[0].as_fn(),
                    "while first arg must be func not {:?}",
                    parts[0]
                );
                todo!("shouldnt get here twice")
            }
            if let Expr::Closure(_) = parts[1].deref_mut() {
                let body_fn = self.promote_closure(&result.constants, &mut parts[1])?;
                let body_ty = self.infer_types(body_fn)?;
                self.type_check_eq(body_ty.arg, TypeId::unit(), sig)?;
                self.type_check_eq(body_ty.ret, TypeId::unit(), sig)?;
                self.emit_call_on_unit(result, body_fn, &mut parts[1])?;
            } else {
                unwrap!(
                    parts[1].as_fn(),
                    "while second arg must be func not {:?}",
                    parts[1]
                );
                todo!("shouldnt get here twice")
            }
        } else {
            ice!("if args must be tuple not {:?}", arg);
        }

        Ok(Structured::RuntimeOnly(TypeId::unit()))
    }

    #[track_caller]
    fn type_check_eq(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        if found == expected {
            Ok(())
        } else {
            err!(CErr::TypeCheck(found, expected, msg))
        }
    }

    #[track_caller]
    fn type_check_arg(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        // TODO: dont do this. fix ffi types.
        let found = self.program.raw_type(found);
        let expected = self.program.raw_type(expected);

        if found == expected || found.is_any() || expected.is_any() {
            Ok(())
        } else {
            match (
                &self.program.types[found.0],
                &self.program.types[expected.0],
            ) {
                (TypeInfo::Int(a), TypeInfo::Int(b)) => {
                    if a.bit_count == b.bit_count || a.bit_count == 64 || b.bit_count == 64 {
                        return Ok(()); // TODO: not this!
                    }
                }
                (TypeInfo::Tuple(f), TypeInfo::Tuple(e)) => {
                    if f.len() == e.len() {
                        let ok = f
                            .iter()
                            .zip(e.iter())
                            .all(|(f, e)| self.type_check_arg(*f, *e, msg).is_ok());
                        if ok {
                            return Ok(());
                        }
                    }
                }
                // TODO: only allow destructuring in some contexts.
                (&TypeInfo::Struct { as_tuple, .. }, TypeInfo::Tuple(_)) => {
                    return self.type_check_arg(as_tuple, expected, msg);
                }
                (TypeInfo::Tuple(_), &TypeInfo::Struct { as_tuple, .. }) => {
                    return self.type_check_arg(found, as_tuple, msg);
                }
                (&TypeInfo::Ptr(f), &TypeInfo::Ptr(e)) => {
                    if self.type_check_arg(f, e, msg).is_ok() {
                        return Ok(());
                    }
                }
                (TypeInfo::Ptr(_), TypeInfo::VoidPtr) | (TypeInfo::VoidPtr, TypeInfo::Ptr(_)) => {
                    return Ok(())
                }
                (TypeInfo::FnPtr(_), TypeInfo::VoidPtr) | (TypeInfo::VoidPtr, TypeInfo::FnPtr(_)) => {
                    return Ok(())
                }
                (TypeInfo::Tuple(_), TypeInfo::Type) | (TypeInfo::Type, TypeInfo::Tuple(_)) => {
                    return Ok(())
                }
                (&TypeInfo::Fn(f), &TypeInfo::Fn(e)) => {
                    if self.type_check_arg(f.arg, e.arg, msg).is_ok()
                        && self.type_check_arg(f.ret, e.ret, msg).is_ok()
                    {
                        return Ok(());
                    }
                }
                _ => {}
            }
            err!(CErr::TypeCheck(found, expected, msg))
        }
    }

    fn struct_type(
        &mut self,
        result: &FnWip<'p>,
        pattern: &mut Pattern<'p>,
    ) -> Res<'p, TypeInfo<'p>> {
        // TODO: maybe const keyword before name in func/struct lets you be generic.
        let types = self.infer_pattern(&result.constants, &mut pattern.bindings)?;
        let raw_fields = pattern.flatten();

        let as_tuple = self.program.tuple_of(types);
        let mut fields = vec![];
        for (name, ty) in raw_fields {
            fields.push(Field {
                name: unwrap!(name, "field name").0,
                ty,
                ffi_byte_offset: None
            });
        }
        Ok(TypeInfo::simple_struct(
            fields,
            as_tuple,
        ))
    }

    fn set_deref(
        &mut self,
        result: &mut FnWip<'p>,
        place: &mut FatExpr<'p>,
        value: &mut FatExpr<'p>,
    ) -> Res<'p, ()> {
        match place.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                let var_info = self.program.vars[var.1]; // TODO: the type here isn't set.
                assert_eq!(
                    var_info.kind,
                    VarType::Var,
                    "Only 'var' can be reassigned (not let/const). {:?}",
                    place
                );
                let oldty = result.vars.get(var);
                let oldty = *unwrap!(
                    oldty,
                    "SetVar: var must be declared: {}",
                    var.log(self.pool)
                );

                let value = self.compile_expr(result, value, Some(oldty))?;
                self.type_check_arg(value.ty(), oldty, "reassign var")?;
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: type checking
                // TODO: general place expressions.
                let macro_name = self.pool.get(*macro_name);
                if macro_name == "deref" {
                    let ptr = self.compile_expr(result, arg, None)?;
                    let expected_ty = unwrap!(self.program.unptr_ty(ptr.ty()), "not ptr");
                    let value = self.compile_expr(result, value, Some(expected_ty))?;
                    self.type_check_arg(value.ty(), expected_ty, "set ptr")?;
                    return Ok(());
                }
                todo!()
            }
            &mut Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;`"),
        }
    }

    fn field_access_expr(
        &mut self,
        _result: &mut FnWip<'p>,
        container_ptr: Structured,
        name: Ident<'p>,
    ) -> Res<'p, Structured> {
        let mut container_ptr_ty = self.program.raw_type(container_ptr.ty());
        // Auto deref for nested place expressions.
        // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        let depth = self.program.ptr_depth(container_ptr_ty);
        if depth > 1 {
            for _ in 0..(depth - 1) {
                container_ptr_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "");
                container_ptr_ty = self.program.raw_type(container_ptr_ty);
                // TODO: not sure if its better to do the loads here or in backend
            }
        }
        let container_ty = unwrap!(
            self.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.program.log_type(container_ptr_ty)
        );

        let raw_container_ty = self.program.raw_type(container_ty);
        match &self.program.types[raw_container_ty.0] {
            TypeInfo::Struct { fields, .. } => {
                for f in fields {
                    if f.name == name {
                        let f = *f;
                        let ty = self.program.ptr_type(f.ty);
                        return Ok(Structured::RuntimeOnly(ty));
                    }
                }
                err!(
                    "unknown name {} on {:?}",
                    self.pool.get(name),
                    self.program.log_type(container_ty)
                );
            }
            TypeInfo::Enum { cases, .. } => {
                for (_, (f_name, f_ty)) in cases.iter().enumerate() {
                    if *f_name == name {
                        let f_ty = *f_ty;
                        let ty = self.program.ptr_type(f_ty);
                        return Ok(Structured::RuntimeOnly(ty));
                    }
                }
                err!(
                    "unknown name {} on {:?}",
                    self.pool.get(name),
                    self.program.log_type(container_ty)
                );
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.program.log_type(container_ty),
                self.program.log_type(raw_container_ty)
            ),
        }
    }

    // TODO: copy paste from the emit
    fn get_field_type(&mut self, mut container_ptr_ty: TypeId, name: Ident<'_>) -> Res<'p, TypeId> {
        // Auto deref for nested place expressions.
        // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        let depth = self.program.ptr_depth(container_ptr_ty);
        if depth > 1 {
            for _ in 0..(depth - 1) {
                container_ptr_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "");
            }
        }

        let container_ty = unwrap!(
            self.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.program.log_type(container_ptr_ty)
        );

        let unknown_name = || {
            err!(
                "unknown name {} on {:?}",
                self.pool.get(name),
                self.program.log_type(container_ty)
            )
        };

        let raw_container_ty = self.program.raw_type(container_ty);
        match &self.program.types[raw_container_ty.0] {
            TypeInfo::Struct { fields, .. } => {
                for f in fields {
                    if f.name == name {
                        let f = *f;
                        let ty = self.program.ptr_type(f.ty);
                        return Ok(ty);
                    }
                }
                unknown_name()
            }
            TypeInfo::Enum { cases, .. } => {
                for (_, (f_name, f_ty)) in cases.iter().enumerate() {
                    if *f_name == name {
                        let f_ty = *f_ty;
                        let ty = self.program.ptr_type(f_ty);

                        return Ok(ty);
                    }
                }
                unknown_name()
            }
            TypeInfo::Unique(_, _) => unreachable!(),
            _ => err!(
                "only structs support field access but found {}",
                self.program.log_type(container_ty)
            ),
        }
    }

    fn produce_tuple(
        &mut self,
        owned_values: Vec<Structured>,
        tuple_ty: TypeId,
    ) -> Res<'p, Structured> {
        let mut all_stack = true;
        let mut all_const = true;
        for v in &owned_values {
            match v {
                Structured::RuntimeOnly(_) => all_const = false,
                Structured::Const(_, _) => all_stack = false,
                Structured::TupleDifferent(_, _) => {
                    all_const = false;
                    all_stack = false;
                }
                Structured::Emitted(_, _) => unreachable!(),
            }
        }

        assert!(!(all_const && all_stack), "todo empty");
        if all_const {
            let values: Vec<_> = owned_values.into_iter().map(|v| v.get().unwrap()).collect();
            return Ok(Structured::Const(tuple_ty, values.into()));
        }

        if !all_stack {
            return Ok(Structured::TupleDifferent(tuple_ty, owned_values));
        }

        Ok(Structured::RuntimeOnly(tuple_ty))
    }

    fn tuple_access(
        &self,
        _result: &mut FnWip<'p>,
        _container: &FatExpr<'p>,
        _requested: Option<TypeId>,
        _index: i32,
    ) -> Structured {
        todo!()
    }

    fn compile_call(
        &mut self,
        result: &mut FnWip<'p>,
        expr: &mut FatExpr<'p>,
        original_f: FuncId,
        requested: Option<TypeId>,
    ) -> Res<'p, Structured> {
        let (f_expr, arg_expr) = if let Expr::Call(f, arg) = expr.deref_mut() {
            (f, arg)
        } else {
            ice!("")
        };
        let func = &self.program.funcs[original_f.0];
        let is_comptime = func.has_tag(self.pool, "comptime");
        if is_comptime {
            let (ret_val, ret_ty) = self.emit_comptime_call(result, original_f, arg_expr)?;
            let req = requested.unwrap_or(TypeId::any());
            let ty = if req.is_any() || req.is_unknown() {
                ret_ty
            } else {
                requested.unwrap()
            };
            expr.expr = Expr::Value {
                ty,
                value: ret_val.clone(),
            };
            return Ok(Structured::Const(ty, ret_val));
        }
        
        // Note: f will be compiled differently depending on the calling convention.
        // But, we do need to know how many values it returns. If there's no type annotation, this might end up compiling the function to figure it out.
        self.infer_types(original_f)?;

        let f_ty = self.program.funcs[original_f.0].unwrap_ty();

        let mut arg_val = self.compile_expr(result, arg_expr, Some(f_ty.arg))?;
        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
        if arg_val.is_empty() {
            arg_val = self.program.load_value(Value::Unit);
        }
        self.type_check_arg(arg_val.ty(), f_ty.arg, "fn arg")?;

        // TODO: if its a pure function you might want to do the call at comptime
        // TODO: make sure I can handle this as well as Nim: https://news.ycombinator.com/item?id=31160234
        // TODO: seperate function for this
        if self.program.is_comptime_only_type(f_ty.arg) {
            let state = DebugState::Msg(format!("Bake CT Only {original_f:?}"));
            self.push_state(&state);
            // Some part of the argument must be known at comptime.
            // You better compile_expr noticed and didn't put it in a stack slot.
            let func = &self.program.funcs[original_f.0];
            let pattern = func.arg.flatten();
            match arg_val {
                Structured::RuntimeOnly(_) | Structured::Emitted(_, _) => ice!(
                    "{:?} is comptime only but {:?} is only known at runtime.",
                    self.program.log_type(f_ty.arg),
                    arg_val
                ),
                Structured::Const(_, _) => ice!("TODO: fully const comptime arg {:?}. (should be trivial just don't need it yet)", arg_expr),
                Structured::TupleDifferent(_, arg_values) => {
                    assert_eq!(
                        pattern.len(),
                        arg_values.len(),
                        "TODO: non-trivial pattern matching\n{:?} <= {:?} for call to {:?}",
                        pattern,
                        arg_values,
                        func.synth_name(self.pool)
                    );
                    let arg_exprs = if let Expr::Tuple(v) = arg_expr.deref_mut().deref_mut() {
                        v
                    } else {
                        todo!()
                    };
                    assert_eq!(arg_exprs.len(), pattern.len(), "TODO: non-tuple baked args");
                    let mut current_fn = original_f;
                    let mut skipped_args = vec![];  // TODO: need to remove these baked ones from the arg expr as well!
                    let mut skipped_types = vec![];
                    let mut removed = 0;
                    for (i, ((name, ty), arg_value)) in pattern.into_iter().zip(arg_values).enumerate() {
                        if self.program.is_comptime_only_type(ty) {
                            let id = arg_value.clone().get()?.single()?.to_func(); // TODO: not this
                            let name = unwrap!(name, "arg needs name (unreachable?)");
                            current_fn = self.bind_const_arg(current_fn, name, arg_value)?;
                            // TODO: need to do capturing_call to the ast
                            if let Some(id) = id {  
                                let other_captures = self.program.funcs[id.0].capture_vars.clone();
                                if other_captures.is_empty() {
                                    add_unique(&mut result.callees, id);
                                }
                                
                                self.program.funcs[current_fn.0].capture_vars.extend(other_captures)
                            }
                            arg_exprs.remove(i - removed);
                            removed += 1; // TODO: this sucks
                        } else {
                            skipped_args.push(arg_value);
                            skipped_types.push(ty);
                        }
                    }
                    let arg_ty = self.program.tuple_of(skipped_types);
                    debug_assert_ne!(current_fn, original_f);
                    if let Expr::Tuple(v) = arg_expr.deref_mut().deref_mut() {
                        if v.len() == 1 {
                            *arg_expr.deref_mut() = mem::take(v.iter_mut().next().unwrap());
                        }
                    }
                    let ty = self.program.func_type(current_fn);
                    f_expr.expr = Expr::Value {
                        ty,
                        value: Value::GetFn(current_fn).into(),
                    };
                    f_expr.ty = ty;
                    let f_ty = self.program.fn_ty(ty).unwrap();
                    self.type_check_arg(arg_ty, f_ty.arg, "sanity: post bake arg")?;
                    // TODO: only emit_capturing_call if we based a closure
                    let res = self.emit_capturing_call(result, current_fn, expr)?;
                    self.pop_state(state);
                    return Ok(res);
                }
            }
        }

        let func = &self.program.funcs[original_f.0];
        // TODO: some huristic based on how many times called and how big the body is.
        // TODO: pre-intern all these constants so its not a hash lookup everytime
        let force_inline = func.has_tag(self.pool, "inline");
        let deny_inline = func.has_tag(self.pool, "noinline");
        assert!(
            !(force_inline && deny_inline),
            "{original_f:?} is both @inline and @noinline"
        );

        let will_inline = force_inline;
        let func = &self.program.funcs[original_f.0];
        if (!func.capture_vars.is_empty() || will_inline) && func.body.is_some() {
            // TODO: check that you're calling from the same place as the definition.
            assert!(!deny_inline, "capturing calls are always inlined.");
            self.emit_capturing_call(result, original_f, expr)
        } else {
            let res = self.emit_runtime_call(result, original_f, arg_expr)?;
            // Since we've called it, we must know the type by now.
            // TODO: cope with emit_runtime_call baking const args, needs to change the arg expr
            let ty = self.program.func_type(original_f);
            f_expr.expr = Expr::Value {
                ty,
                value: Value::GetFn(original_f).into(),
            };
            f_expr.ty = ty;
            Ok(res)
        }
    }

    fn emit_call_on_unit(&mut self, result: &mut FnWip<'p>, cond_fn: FuncId, expr_out: &mut FatExpr<'p>) -> Res<'p, ()> {
        let get_fn = FatExpr::synthetic(self.func_expr(cond_fn), expr_out.loc);
        let unit = FatExpr::synthetic(Expr::unit(), expr_out.loc);
        expr_out.expr = Expr::Call(Box::new(get_fn), Box::new(unit));
        self.compile_expr(result, expr_out, None)?;
        Ok(())
    }
    
    fn jit(&mut self, _f: FuncId) -> Res<'p, ()> {
        todo!()
        // outln!(LogTag::Jitted, "Try Jit {f:?}");
        // let bc = self.executor.get_bc(f);
        // if bc.is_none() {
        //     return Ok(());
        // }
        // let bc = bc.unwrap();
        // let func = self.lookup_unique_func(self.pool.intern("bc_to_asm"));
        // if func.is_none() {
        //     return Ok(());
        // }
        // let func = func.unwrap();
        // if func == f {
        //     return Ok(());
        // }
        // let ffff = &self.program.funcs[f.0];
        // let wip = ffff.wip.as_ref().unwrap();
        // let callees = wip.callees.clone();
        // for id in callees {
        //     self.jit(id)?;
        // }
        
        // self.compile(func, ExecTime::Comptime)?;
        // let arg = bc.serialize_one();
        // let res = self.executor.run_func(self.program, func, arg)?;
        // let ops: Vec<u32> = unwrap!(res.deserialize(), "");
        // outln!(LogTag::Jitted, "=================\n {f:?}");
        // for op in &ops {
        //     outln!(LogTag::Jitted, "{op}");
        // }
        // outln!(LogTag::Jitted, "\n============");
        // let map = builtins::copy_to_mmap_exec(ops.into());
        // Box::leak(map.0.into()); // TODO: dont leak
        // while self.jitted.len() <= f.0 {
        //     self.jitted.push(null());
        // }
        // self.jitted[f.0] = map.1;
        // Ok(())
    }
}


pub fn insert_multi<K: Hash + Eq, V: Eq>(set: &mut HashMap<K, Vec<V>>, key: K, value: V) {
    if let Some(prev) = set.get_mut(&key) {
        if !prev.contains(&value) {
            prev.push(value);
        }
    } else {
        set.insert(key, vec![value]);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, InterpSend)]
pub enum ExecTime {
    Comptime,
    Runtime,
}

fn add_unique<T: PartialEq>(vec: &mut Vec<T>, new: T) -> bool {
    if !vec.contains(&new) {
        vec.push(new);
        return true;
    }
    false
}

fn bit_literal<'p>(expr: &FatExpr<'p>, pool: &StringPool<'p>) -> Res<'p, (IntType, i64)> {
    if let Expr::Call(f, arg) = &expr.expr {
        if let Some(name) = f.as_ident() {
            if name == pool.intern("from_bit_literal") {
                if let Expr::Tuple(parts) = arg.deref().deref() {
                    if let Expr::Value { value, .. } = parts[0].deref() {
                        let bit_count = value.clone().single()?.to_int()?;
                        if let Expr::Value { value, .. } = parts[1].deref() {
                            let val = value.clone().single()?.to_int()?;
                            return Ok((IntType {
                                bit_count,
                                signed: false,
                            }, val));
                        }
                    }
                }
            }
        }
    }
    err!("not int",)
}

pub trait Executor<'p>: PoolLog<'p> {
    type SavedState;
    fn compile_func(&mut self, program: &Program<'p>, f: FuncId) -> Res<'p, ()>;
    fn run_with_arg<T: Reflect>(&mut self, program: &mut Program<'p>, f: FuncId, arg: &mut T) -> Res<'p, ()>;
    fn run_func(&mut self, program: &mut Program<'p>, f: FuncId, arg: Values) -> Res<'p, Values>;
    fn run_continuation(&mut self, program: &mut Program<'p>, response: Values) -> Res<'p, Values>;
    fn size_of(&mut self, program: &Program<'p>, ty: TypeId) -> usize;
    fn is_ready(&self, f: FuncId) -> bool;
    fn dump_repr(&self, program: &Program<'p>, f: FuncId) -> String;
    fn tag_error(&self, err: &mut CompileError<'p>);
    fn assertion_count(&self) -> usize;
    fn mark_state(&self) -> Self::SavedState;
    fn restore_state(&mut self, state: Self::SavedState);
    fn get_bc(&self, f: FuncId) -> Option<FnBody<'p>>; // asadas
}

// i like when my code is rocks not rice
// its a lot more challenging to eat but at least you can Find it
