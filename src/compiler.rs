//! Converts ASTs into my bytecode-ish format.
//! Type checking, overload resolution, implicit function calls, inlining, monomorphization, etc.
//! Uses the interpreter for comptime evalutation (build scripts, generics, macros, etc).

#![allow(clippy::wrong_self_convention)]
use codemap::Span;
use std::collections::HashMap;
use std::fmt::Write;
use std::hash::Hash;
use std::marker::PhantomData;
use std::mem;
use std::ops::DerefMut;
use std::{ops::Deref, panic::Location};

use crate::ast::{
    Annotation, Binding, FatStmt, Field, OverloadOption, OverloadSet, Pattern, Var, VarType,
};
use crate::bc::*;
use crate::ffi::InterpSend;
use crate::interp::{to_flat_seq, CallFrame, CmdResult, Interp};
use crate::logging::{outln, PoolLog};
use crate::{
    ast::{Expr, FatExpr, FnType, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};

use crate::logging::{assert, assert_eq, err, ice, logln, unwrap};

#[derive(Clone)]
pub struct CompileError<'p> {
    pub internal_loc: &'static Location<'static>,
    pub loc: Option<Span>,
    pub reason: CErr<'p>,
    pub trace: String,
    pub value_stack: Vec<Value>,
    pub call_stack: Vec<CallFrame<'p>>,
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
    TypeError(&'static str, Value),
    TypeCheck(TypeId, TypeId, &'static str),
    Msg(String),
    AmbiguousCall,
    VarNotFound(Var<'p>),
}

pub type Res<'p, T> = Result<T, CompileError<'p>>;

#[derive(Debug, Clone)]
pub struct DebugInfo<'p> {
    pub internal_loc: &'static Location<'static>,
    pub src_loc: Span,
    pub p: PhantomData<&'p str>,
}

//
// TODO:
// - any time you try to call a function it might not be ready yet
//   and compiling it might require running other comptime functions.
// - some types need to be passed between the interpreter and the comptime code.
// - some functions run in the interpreter when bootstraping but then are compiled into the compiler.
// - some functions are written in rust when bootstraping but then are compiled into the compiler.
//
// TODO: bucket array for stack so you can take pointers into it
pub struct Compile<'a, 'p> {
    pub pool: &'a StringPool<'p>,
    pub interp: Interp<'a, 'p>,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    pub debug_trace: Vec<DebugState<'p>>,
    pub anon_fn_counter: usize,
    currently_inlining: Vec<FuncId>,
    last_loc: Option<Span>,
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

#[derive(Clone)]
struct StackHeights<'p> {
    value_stack: usize,
    call_stack: usize,
    debug_trace: usize,
    result: FnBody<'p>,
}

impl<'a, 'p> Compile<'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        Self {
            pool,
            debug_trace: vec![],
            anon_fn_counter: 0,
            currently_inlining: vec![],
            last_loc: None,
            interp: Interp::new(pool, program),
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
            writeln!(out, "{i} {};", s.log(self.pool, self.interp.program)).unwrap();
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
        if let Some(decls) = self.interp.program.declarations.get(&name) {
            if decls.len() == 1 {
                return Some(decls[0]);
            }
        }
        None
    }

    pub fn compile(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        let state = DebugState::Compile(f);
        self.push_state(&state);
        let init_heights = (self.interp.call_stack.len(), self.interp.value_stack.len());
        let result = self.ensure_compiled(f, when);
        if result.is_ok() {
            let end_heights = (self.interp.call_stack.len(), self.interp.value_stack.len());
            assert!(init_heights == end_heights, "bad stack size");
        }
        let result = self.tag_err(result);
        self.pop_state(state);
        result
    }

    pub fn run(&mut self, f: FuncId, arg: Value, when: ExecTime) -> Res<'p, Value> {
        let state2 = DebugState::RunInstLoop(f);
        self.push_state(&state2);
        let result = self.interp.run(f, arg, when);
        let result = self.tag_err(result);
        self.pop_state(state2);
        result
    }

    fn compile_and_run(&mut self, f: FuncId, arg: Value, when: ExecTime) -> Res<'p, Value> {
        self.compile(f, when)?;
        self.run(f, arg, when)
    }

    // This is much less painful than threading it through the macros
    fn tag_err<T>(&self, mut res: Res<'p, T>) -> Res<'p, T> {
        if let Err(err) = &mut res {
            err.trace = self.log_trace();
            err.value_stack = self.interp.value_stack.clone();
            err.call_stack = self.interp.call_stack.clone();
            err.loc = self.interp.last_loc.or(self.last_loc);
        }
        res
    }

    #[track_caller]
    pub fn empty_fn(
        &mut self,
        when: ExecTime,
        func: FuncId,
        loc: Span,
        parent: Option<Constants<'p>>,
    ) -> FnBody<'p> {
        FnBody {
            insts: vec![],
            stack_slots: 0,
            vars: Default::default(),
            when,
            slot_types: vec![],
            func,
            why: self.log_trace(),
            debug: vec![],
            last_loc: loc,
            constants: parent
                .unwrap_or_else(|| self.interp.program.funcs[func.0].closed_constants.clone()),
            to_drop: vec![],
        }
    }

    // Any environment constants must already be in the function.
    fn eval_and_close_local_constants(&mut self, f: FuncId) -> Res<'p, ()> {
        let state = DebugState::EvalConstants(f);
        self.push_state(&state);
        let loc = self.interp.program.funcs[f.0].loc;
        debug_assert!(!self.interp.program.funcs[f.0].evil_uninit);
        debug_assert!(self.interp.program.funcs[f.0].closed_constants.is_valid);

        mut_replace!(
            self.interp.program.funcs[f.0].closed_constants,
            |constants| {
                // println!(
                //     "BEFORE LOCAL CONSTS {}",
                //     self.interp.program.log_consts(&constants)
                // );
                let mut result = self.empty_fn(
                    ExecTime::Comptime,
                    FuncId(f.0 + 10000000), // TODO: do i even need to pass an index? probably just for debugging
                    loc,
                    Some(constants),
                );
                let name = self.interp.program.funcs[f.0].synth_name(self.pool).clone();
                let _msg = format!("locals {}", name);
                let func = &self.interp.program.funcs[f.0];

                let new_constants = func.local_constants.clone();
                for mut stmt in new_constants {
                    // println!("Eval const {}", stmt.log(self.pool));
                    self.compile_stmt(&mut result, &mut stmt)?;
                }

                self.pop_state(state);

                // Now this includes stuff inherited from the parent, plus any constants pulled up from the function body.
                Ok((result.constants, ()))
            }
        );

        Ok(())
    }

    // Don't pass constants in because the function declaration must have closed over anything it needs.
    fn ensure_compiled(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        if let Some(Some(_)) = self.interp.ready.get(f.0) {
            return Ok(());
        }
        let func = &self.interp.program.funcs[f.0];
        debug_assert!(!func.evil_uninit);
        debug_assert!(func.closed_constants.is_valid);
        logln!(
            "BEFORE Closed local consts for {}:\n{}",
            func.synth_name(self.pool),
            self.interp.program.log_consts(&func.closed_constants)
        );

        let state = DebugState::EnsureCompiled(f, when);
        self.push_state(&state);
        self.eval_and_close_local_constants(f)?;

        while self.interp.ready.len() <= f.0 {
            self.interp.ready.push(None);
        }
        logln!(
            "Start JIT: {:?} \n{}",
            f,
            self.interp.program.funcs[f.0].log(self.pool)
        );
        self.infer_types(f)?;
        let func = &self.interp.program.funcs[f.0];
        let f_ty = func.unwrap_ty();
        let mut result = self.empty_fn(when, f, func.loc, None);
        let arg_range = result.reserve_slots(self.interp.program, f_ty.arg)?;
        let return_value = self.emit_body(&mut result, arg_range, f)?;
        let func = &self.interp.program.funcs[f.0];

        result.push(Bc::Ret(return_value));

        logln!("{}", result.log(self.pool));

        assert!(
            result.vars.is_empty(),
            "undropped vars {:?}",
            result
                .vars
                .iter()
                .map(|v| (v.0.log(self.pool), v.1))
                .collect::<Vec<_>>()
        );
        self.interp.ready[f.0] = Some(result);
        logln!("Done JIT: {:?} {}", f, func.synth_name(self.pool));
        self.pop_state(state);
        Ok(())
    }

    // This is used for
    // - emiting normal functions into a fresh FnBody
    /// IMPORTANT: this pulls a little sneaky on ya so you can't access the body of the function inside the main emit handlers.
    fn emit_body(
        &mut self,
        result: &mut FnBody<'p>,
        full_arg_range: StackRange,
        f: FuncId,
    ) -> Res<'p, StackRange> {
        let state = DebugState::EmitBody(f);
        self.push_state(&state);
        let has_body = self.interp.program.funcs[f.0].body.is_some();
        debug_assert!(!self.interp.program.funcs[f.0].evil_uninit);

        // println!(
        //     "emit_body for {} starts with consts:\n{}",
        //     self.interp.program.funcs[f.0].synth_name(self.pool),
        //     self.interp.program.log_consts(&result.constants)
        // );

        let mut args_to_drop = vec![];
        mut_replace!(self.interp.program.funcs[f.0], |func: Func<'p>| {
            assert!(result.when == ExecTime::Comptime || !func.has_tag(self.pool, "comptime"));

            let arguments = func.arg.flatten();
            let mut slot_count = 0;
            for (name, ty) in arguments {
                let size = self.interp.program.slot_count(ty);
                let range = StackRange {
                    first: full_arg_range.offset(slot_count),
                    count: size,
                };
                if let Some(name) = name {
                    let prev = result.vars.insert(name, (range, ty));
                    assert!(prev.is_none(), "overwrite arg?");
                }
                args_to_drop.push((name, range));
                slot_count += size;
            }
            assert_eq!(full_arg_range.count, slot_count);

            Ok((func, ()))
        });

        if !has_body {
            let ret = mut_replace!(self.interp.program.funcs[f.0], |mut func: Func<'p>| {
                // Functions without a body are always builtins.
                // It's convient to give them a FuncId so you can put them in a variable,
                // but just force inline call.
                logln!(
                    "builtin shim for {} has constants {}",
                    self.interp.program.funcs[f.0].synth_name(self.pool),
                    self.interp.program.log_consts(&func.closed_constants)
                );
                func.annotations.push(Annotation {
                    name: self.pool.intern("inline"),
                    args: None,
                });
                let ret = result.reserve_slots(self.interp.program, func.ret.unwrap())?;
                assert!(func.referencable_name, "fn no body needs name");
                result.push(Bc::CallBuiltin {
                    name: func.name,
                    ret,
                    arg: full_arg_range,
                });
                for (var, range) in args_to_drop {
                    if let Some(var) = var {
                        let (slot, _) = unwrap!(result.vars.remove(&var), "lost arg");
                        assert_eq!(range, slot, "moved arg");
                    }
                    // Don't drop, they were moved to the call.
                }

                Ok((func, ret))
            });
            self.pop_state(state);
            return Ok(ret);
        }

        let (ret_val, found_ret_ty) =
            mut_replace!(self.interp.program.funcs[f.0].body, |mut body: Option<
                FatExpr<'p>,
            >| {
                let (ret_val, found_ret_ty) =
                    self.compile_expr(result, body.as_mut().unwrap(), None)?;

                Ok((body, (ret_val, found_ret_ty)))
            });

        let func = &self.interp.program.funcs[f.0];
        self.type_check_arg(found_ret_ty, func.ret.unwrap(), "bad return value")?;
        // We're done with our arguments, get rid of them. Same for other vars.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        result.push(Bc::DebugMarker("drop_args", func.get_name(self.pool)));
        args_to_drop.extend(result.to_drop.drain(0..).map(|(s, _)| (None, s)));
        for (var, range) in args_to_drop {
            if let Some(var) = var {
                let (slot, _) = unwrap!(result.vars.remove(&var), "lost arg");
                assert_eq!(range, slot, "moved arg");
            }
            result.push(Bc::Drop(range));
        }

        self.pop_state(state);
        Ok(ret_val)
    }

    fn emit_runtime_call(
        &mut self,
        result: &mut FnBody<'p>,
        f: FuncId,
        arg: &mut FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        let (mut arg, arg_ty_found) = self.compile_expr(result, arg, None)?;
        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
        if arg.count == 0 {
            arg = result.load_constant(self.interp.program, Value::Unit)?.0;
        }
        // Note: f will be compiled differently depending on the calling convention.
        // But, we do need to know how many values it returns. If there's no type annotation, this might end up compiling the function to figure it out.
        self.infer_types(f)?;
        let func = &self.interp.program.funcs[f.0];
        let f_ty = self.interp.program.funcs[f.0].unwrap_ty();
        // self.last_loc = Some(expr.loc); // TODO: have a stack so i dont have to keep doing this.
        self.type_check_arg(arg_ty_found, f_ty.arg, "bad arg")?;
        debug_assert_eq!(
            self.interp.program.slot_count(arg_ty_found),
            self.interp.program.slot_count(f_ty.arg)
        );
        // TODO: some huristic based on how many times called and how big the body is.
        // TODO: pre-intern all these constants so its not a hash lookup everytime
        let force_inline = func.has_tag(self.pool, "inline");
        let deny_inline = func.has_tag(self.pool, "noinline");
        assert!(
            !(force_inline && deny_inline),
            "{f:?} is both @inline and @noinline"
        );

        for (name, ty) in func.arg.flatten() {
            let info = &self.interp.program.types[ty.0];
            if let TypeInfo::Fn(_) = info {
                logln!(
                    "Pass function {:?}: {} to {:?}",
                    name.map(|v| v.log(self.pool)),
                    self.interp.program.log_type(ty),
                    func.synth_name(self.pool),
                );
            }
        }

        let will_inline = force_inline;
        let func = &self.interp.program.funcs[f.0];
        let ret = if !func.capture_vars.is_empty() {
            // TODO: check that you're calling from the same place as the definition.
            assert!(!deny_inline, "capturing calls are always inlined.");
            self.emit_capturing_call(result, arg, f)?
        } else if will_inline {
            self.emit_inline_call(result, arg, f)?
        } else {
            let ret = result.reserve_slots(self.interp.program, f_ty.ret)?;
            self.ensure_compiled(f, result.when)?;
            result.push(Bc::CallDirect { f, ret, arg });
            ret
        };

        assert_eq!(self.return_stack_slots(f), ret.count);
        assert_eq!(self.interp.program.slot_count(f_ty.ret), ret.count);
        Ok((ret, f_ty.ret))
    }

    fn emit_capturing_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        f: FuncId,
    ) -> Res<'p, StackRange> {
        let state = DebugState::EmitCapturingCall(f);
        self.push_state(&state);
        // TODO: constants!
        let name = self.interp.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker("start:capturing_call", name));
        self.infer_types(f)?;
        assert!(
            !self.currently_inlining.contains(&f),
            "Tried to inline recursive function."
        );
        self.currently_inlining.push(f);
        let return_range = self.emit_body(result, arg, f)?;
        result.push(Bc::DebugMarker("end:capturing_call", name));
        self.currently_inlining.retain(|check| *check != f);
        self.pop_state(state);
        Ok(return_range)
    }

    fn emit_inline_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        f: FuncId,
    ) -> Res<'p, StackRange> {
        assert!(
            !self.currently_inlining.contains(&f),
            "Tried to inline recursive function."
        );
        self.currently_inlining.push(f);
        self.ensure_compiled(f, result.when)?;
        let name = self.interp.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker("start:inline_call", name));
        // This move ensures they end up at the base of the renumbered stack.
        let func = &self.interp.program.funcs[f.0];
        let msg = "capturing calls are already inlined. what are you doing here?";
        assert_eq!(func.capture_vars.len(), 0, "{}", msg);
        let f_ty = func.unwrap_ty();

        let stack_offset = if arg.first.0 == (result.stack_slots - arg.count) {
            // It's already at the top of the stack so don't need to move
            result.stack_slots - arg.count
        } else {
            let stack_offset = result.stack_slots;
            let arg_slots = result.reserve_slots(self.interp.program, f_ty.arg)?; // These are included in the new function's stack
            debug_assert_eq!(arg_slots.count, arg.count);
            result.push(Bc::MoveRange {
                from: arg,
                to: arg_slots,
            });
            stack_offset
        };

        let ip_offset = result.insts.len();
        let func = self.interp.ready[f.0].as_ref().unwrap();
        // TODO: check for recusion somewhere.
        // TODO: put constants somewhere so dont have to clone them each time a function is inlined.
        result.stack_slots += func.stack_slots;
        result.slot_types.extend(func.slot_types.iter());
        let mut ret = None;
        for (i, mut inst) in func.insts.iter().cloned().enumerate() {
            assert!(ret.is_none()); // TODO
            inst.renumber(stack_offset, ip_offset);
            if let Bc::Ret(return_value) = inst {
                ret = Some(return_value);
            } else {
                result.insts.push(inst);
                if cfg!(feature = "some_log") {
                    result.debug.push(result.debug[i].clone());
                }
            }
        }
        assert!(
            ret.is_some(),
            "inline function had no ret instruction. \n{}",
            func.log(self.pool)
        );
        result.push(Bc::DebugMarker("end:inline_call", name));
        self.currently_inlining.retain(|check| *check != f);
        Ok(ret.unwrap())
    }

    // TODO: I should probably implement real runtime closures (even if my version is dumb and slow)
    //       and use those for comptime stuff cause it can't possibly be worse than cloning everything and recompiling.
    //       The cloning is only better for runtime functions where we're trying to output a simpler ast that an optimiser can specialize.
    // Curry a function from fn(a: A, @comptime b: B) to fn(a: A)
    // The argument type is evaluated in the function declaration's scope, the argument value is evaluated in the caller's scope.
    fn bind_const_arg(
        &mut self,
        caller_constants: &Constants<'p>,
        f: FuncId,
        arg_name: Var<'p>,
        arg_expr: FatExpr<'p>,
    ) -> Res<'p, FuncId> {
        let mut new_func = self.interp.program.funcs[f.0].clone();
        new_func.referencable_name = false;
        let arg_ty =
            self.get_type_for_arg(&new_func.closed_constants, &mut new_func.arg, arg_name)?;
        let arg_value = self.immediate_eval_expr(caller_constants, arg_expr, arg_ty)?;
        new_func
            .closed_constants
            .insert(arg_name, (arg_value, arg_ty));
        new_func.arg.remove_named(arg_name);

        let known_type = new_func.finished_type.is_some();
        let f_id = self.interp.program.add_func(new_func);
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
            match arg {
                Binding::Named(_, _) => unreachable!(),
                Binding::Var(name, ty) => {
                    if *name == arg_name {
                        self.infer_types_progress(constants, ty)?;
                        return Ok(ty.unwrap());
                    }
                }
                Binding::Discard(_) => {}
            }
        }
        err!(CErr::VarNotFound(arg_name))
    }

    // TODO: you only need to call this for generic functions that operate on thier own types.
    //       If you're just doing comptime manipulations of a TypeInfo to be used elsewhere,
    //       it can be interpreted as a normal function so you don't have to clone the ast on every call.
    // TODO: fuse this with bind_const_arg. I have too much of a combinatoric explosion of calling styles going on.
    // TODO: maybe call this @generic instead of @comptime? cause other things can be comptime
    // Return type is allowed to use args.
    fn emit_comptime_call(
        &mut self,
        caller_constants: &Constants<'p>,
        template_f: FuncId,
        arg_expr: &FatExpr<'p>,
    ) -> Res<'p, Value> {
        // We don't care about the constants in `result`, we care about the ones that existed when `f` was declared.
        // BUT... the *arguments* to the call need to be evaluated in the caller's scope.

        // This one does need the be a clone because we're about to bake constant arguments into it.
        // If you try to do just the constants or chain them cleverly be careful about the ast rewriting.
        let mut func = self.interp.program.funcs[template_f.0].clone();
        debug_assert!(!func.evil_uninit);
        debug_assert!(func.closed_constants.is_valid);
        func.referencable_name = false;
        func.closed_constants.add_all(caller_constants);
        let constants = func.closed_constants.clone(); // TODO: aaaa

        logln!(
            "emit_comptime_call of {} with consts: {}",
            func.synth_name(self.pool),
            self.interp.program.log_consts(&constants)
        );
        let types = self.infer_pattern(&constants, &mut func.arg.bindings)?;
        // TODO: update self.interp.program.funcs[f.0]
        let arg_ty = self.interp.program.tuple_of(types);
        let arg_value = self.immediate_eval_expr(&constants, arg_expr.clone(), arg_ty)?;

        let f = self.interp.program.add_func(func);

        let func = &self.interp.program.funcs[f.0];
        if func.body.is_none() {
            // TODO: don't re-eval the arg type every time.
            let name = func.synth_name(self.pool);
            return self.interp.runtime_builtin(name, arg_value);
        }

        // Note: the key is the original function, not our clone of it. TODO: do this check before making the clone.
        let key = (template_f, arg_value.clone()); // TODO: no clone
        let found = self.interp.program.generics_memo.get(&key);
        if let Some(found) = found {
            return Ok(found.clone());
        }

        let arg_ty = self.interp.program.type_of(&arg_value);
        // TODO: self.type_check_arg(arg_ty, arg, "bad comtime arg")?;

        // Bind the arg into my new constants so the ret calculation can use it.
        // also the body constants for generics need this. much cleanup pls.
        // TODO: factor out from normal functions?

        let func = &self.interp.program.funcs[f.0];
        let args = func.arg.flatten().into_iter().enumerate();
        let arg_values = to_flat_seq(arg_value);
        for (i, (name, ty)) in args {
            if let Some(var) = name {
                let prev = self.interp.program.funcs[f.0]
                    .closed_constants
                    .insert(var, (arg_values[i].clone(), ty));
                assert!(prev.is_none(), "overwrite arg?");
            }
        }

        let constants = self.interp.program.funcs[f.0].closed_constants.clone(); // TODO: aaaa

        let mut func = self.interp.program.funcs[f.0].clone();
        assert!(self.infer_types_progress(&constants, &mut func.ret)?);
        let ret = func.ret.unwrap();
        self.interp.program.funcs[f.0] = func;

        self.interp.program.funcs[f.0].finished_type = Some(FnType { arg: arg_ty, ret });

        let func = &self.interp.program.funcs[f.0];
        logln!(
            "eval_and_close_local_constants emit_comptime_call of {} with consts: {}",
            func.synth_name(self.pool),
            self.interp
                .program
                .log_consts(&self.interp.program.funcs[f.0].closed_constants)
        );
        self.eval_and_close_local_constants(f)?;

        let constants = &self.interp.program.funcs[f.0].closed_constants.clone(); // TODO: aaaa

        let body = self.interp.program.funcs[f.0].body.clone();
        let result = if let Some(body) = body {
            self.immediate_eval_expr(constants, body, ret)?
        } else {
            unreachable!("builtin")
        };

        let ty = self.interp.program.type_of(&result);
        self.type_check_arg(ty, ret, "generic result")?;

        self.interp
            .program
            .generics_memo
            .insert(key.clone(), result.clone());

        Ok(result)
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        debug_assert!(result.constants.is_valid);
        self.last_loc = Some(stmt.loc);
        match stmt.deref_mut() {
            Stmt::DoneDeclFunc(_) => unreachable!("compiled twice?"),
            Stmt::Eval(expr) => {
                let (value, _) = self.compile_expr(result, expr, None)?;
                result.push(Bc::Drop(value));
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                dropping,
                kind,
            } => {
                // println!(
                //     "BEFORE DeclConst {} = {:?}\n{}",
                //     name.log(self.pool),
                //     value,
                //     self.interp.program.log_consts(&result.constants)
                // );
                let no_type = matches!(ty, LazyType::Infer);
                self.infer_types_progress(&result.constants, ty)?;
                let expected_ty = ty.unwrap();

                match kind {
                    VarType::Const => {
                        let mut value = match value {
                            Some(value) => self.immediate_eval_expr(
                                &result.constants,
                                value.clone(),
                                expected_ty,
                            )?,
                            None => {
                                let name = self.pool.get(name.0);
                                unwrap!(
                                    self.builtin_constant(name),
                                    "uninit (non-blessed) const: {:?}",
                                    name
                                )
                                .0
                            }
                        };
                        if self.interp.program.types[expected_ty.0] == TypeInfo::Type {
                            // HACK. todo: general overloads for cast()
                            value = Value::Type(self.to_type(value)?)
                        }
                        let found_ty = self.interp.program.type_of(&value);
                        self.type_check_arg(found_ty, expected_ty, "var decl")?;
                        // println!("DeclConst {} = {:?}", name.log(self.pool), value);
                        result.constants.insert(*name, (value, found_ty));
                        // println!("{}", self.interp.program.log_consts(&result.constants));
                    }
                    VarType::Let | VarType::Var => {
                        let value = invert(
                            value
                                .as_mut()
                                .map(|expr| self.compile_expr(result, expr, Some(expected_ty))),
                        )?;
                        // self.last_loc = Some(stmt.loc);
                        let value = match value {
                            None => {
                                if no_type {
                                    err!("uninit vars require type hint {}", name.log(self.pool));
                                }
                                (
                                    result.reserve_slots(self.interp.program, expected_ty)?,
                                    expected_ty,
                                )
                            }
                            Some((value, ty)) => {
                                self.type_check_arg(ty, expected_ty, "var decl")?;
                                (value, ty)
                            }
                        };
                        let prev = result.vars.insert(*name, value);
                        assert!(prev.is_none(), "shadow is still new var");

                        // TODO: what if shadow is const? that would be more consistant if did it like rust.
                        if let Some(dropping) = dropping {
                            // Maybe should be like rust and dont call drop on the shadowed thing until the end of scope.
                            // It would be consistant and it mean you can reference its data if you do a chain of transforming something with the same name.
                            // But need to change my debugging check that everything was dropped.
                            // Actually if i did that just put them in the block's list instead of carefully taking them out which i did because i thought i wanted to egarly drop.
                            let (slot, _) = unwrap!(result.vars.remove(dropping), "missing shadow");
                            result.push(Bc::Drop(slot));
                        }
                    }
                }
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclNamed { .. } => {
                ice!("Scope resolution failed {}", stmt.log(self.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                let func = mem::take(func);
                let var = func.var_name;
                let id = self.add_func(func, &result.constants)?;
                *stmt.deref_mut() = Stmt::DoneDeclFunc(id);

                // I thought i dont have to add to constants here because we'll find it on the first call when resolving overloads.
                // But it does need to have an empty entry in the overload pool because that allows it to be closed over so later stuff can find it and share if they compile it.
                if let Some(var) = var {
                    if result.constants.get(var).is_none() {
                        let index = self.interp.program.overload_sets.len();
                        self.interp.program.overload_sets.push(OverloadSet(vec![]));
                        result
                            .constants
                            .insert(var, (Value::OverloadSet(index), TypeId::any()));
                    }
                }
            }
        }
        Ok(())
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        // You must self.infer_types(f); before calling this
        let func = &self.interp.program.funcs[f.0];
        let ty = func.unwrap_ty();
        self.interp.program.slot_count(ty.ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(
        &mut self,
        result: &mut FnBody<'p>,
        expr: &mut FatExpr<'p>,
        requested: Option<TypeId>,
    ) -> Res<'p, (StackRange, TypeId)> {
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        Ok(match expr.deref_mut() {
            Expr::Closure(func) => {
                let id = self.add_func(mem::take(func), &result.constants)?;
                *expr.deref_mut() = Expr::Value(Value::GetFn(id));
                self.infer_types(id)?;
                result.load_constant(self.interp.program, Value::GetFn(id))?
            }
            Expr::Call(f, arg) => {
                // TODO: make this a function
                macro_rules! emit_any_call {
                    ($expr:expr, $result:expr, $f:expr, $arg:expr) => {{
                        let func = &self.interp.program.funcs[$f.0];
                        let is_comptime = func.has_tag(self.pool, "comptime");
                        if is_comptime {
                            let ret = self.emit_comptime_call(&$result.constants, $f, $arg)?;
                            expr.expr = Expr::Value(ret.clone());
                            return result.load_constant(self.interp.program, ret);
                        }

                        return self.emit_runtime_call($result, $f, $arg);
                    }};
                }

                // TODO: more general system for checking if its a constant known expr instead of all these cases.
                if let &Expr::GetVar(i) = f.as_ref().deref() {
                    // TODO: only grab here if its a constant, might be a function pointer.
                    let f_id = self.resolve_function(result, i, arg, requested)?;
                    f.expr = Expr::Value(Value::GetFn(f_id));
                    emit_any_call!(expr, result, f_id, arg)
                }
                if let &Expr::Value(Value::GetFn(f)) = f.deref().deref().deref() {
                    emit_any_call!(expr, result, f, arg)
                }
                if let Expr::Closure(func) = f.deref_mut().deref_mut().deref_mut() {
                    let id = self.add_func(mem::take(func), &result.constants)?;
                    *f.deref_mut().deref_mut() = Expr::Value(Value::GetFn(id));
                    emit_any_call!(expr, result, id, arg)
                }

                // TODO: this will be for calling through a function pointer.
                //       it used to be used for all anon expressions so probably works.
                let (arg, arg_ty_found) = self.compile_expr(result, arg, None)?;
                let (f_slot, func_ty) = self.compile_expr(result, f, None)?;
                logln!(
                    "dynamic {} is {}. {} {:?}",
                    f.log(self.pool),
                    self.interp.program.log_type(func_ty),
                    f.log(self.pool),
                    f
                );
                if !func_ty.is_any() {
                    let ty = unwrap!(self.interp.program.fn_ty(func_ty), "expected fn for call");
                    self.type_check_arg(arg_ty_found, ty.arg, "dyn call bad arg")?;
                }
                let ret_ty = if let TypeInfo::Fn(ty) = &self.interp.program.types[func_ty.0] {
                    ty.ret
                } else {
                    logln!("WARNING: Called Any {:?}", expr.log(self.pool));
                    assert!(func_ty.is_any());
                    TypeId::any() // TODO
                };
                assert_eq!(f_slot.count, 1);
                let ret = result.reserve_slots(self.interp.program, ret_ty)?;
                result.push(Bc::CallDynamic {
                    f: f_slot.first,
                    ret,
                    arg,
                });
                (ret, ret_ty)
            }
            Expr::Block {
                body,
                result: value,
                locals,
            } => {
                for stmt in body {
                    self.compile_stmt(result, stmt)?;
                }
                let ret = self.compile_expr(result, value, requested)?;

                for local in locals.as_ref().expect("resolve failed") {
                    if let Some((slot, _ty)) = result.vars.remove(local) {
                        result.push(Bc::Drop(slot));
                    } else if VarType::Const == self.interp.program.vars[local.1].kind {
                        assert!(
                            result.vars.remove(local).is_none(),
                            "constants are not locals"
                        );
                    } else {
                        ice!("Missing local {local:?}")
                    }
                }

                ret
            }
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples");
                let values: Res<'p, Vec<_>> = values
                    .iter_mut()
                    .map(|v| self.compile_expr(result, v, None))
                    .collect();
                let values = values?;
                let types: Vec<_> = values.iter().map(|(_, ty)| *ty).collect();
                let ty = self.interp.program.tuple_of(types);
                result.produce_tuple(self.interp.program, values, ty)?
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(var) => {
                if let Some((from, ty)) = result.vars.get(var).cloned() {
                    let to = result.reserve_slots(self.interp.program, ty)?;
                    debug_assert_eq!(from.count, to.count);
                    if from.count == 1 {
                        result.push(Bc::Clone {
                            from: from.first,
                            to: to.first,
                        });
                    } else {
                        result.push(Bc::CloneRange { from, to });
                    }
                    (to, ty)
                } else if let Some((value, _)) = result.constants.get(*var) {
                    result.load_constant(self.interp.program, value)?
                } else if let Some(func) = self.interp.program.declarations.get(&var.0) {
                    assert_eq!(func.len(), 1, "ambigous function reference");
                    let func = func[0];
                    self.ensure_compiled(func, ExecTime::Comptime)?;
                    result.load_constant(self.interp.program, Value::GetFn(func))?
                } else {
                    outln!("VARS: {:?}", result.vars);
                    outln!(
                        "CONSTANTS: {:?}",
                        self.interp.program.log_consts(&result.constants)
                    );
                    let current_func = &self.interp.program.funcs[result.func.0];
                    outln!("{}", current_func.log_captures(self.pool));
                    ice!(
                        "Missing resolved variable {:?} '{}'",
                        var,
                        self.pool.get(var.0),
                    )
                }
            }
            Expr::GetNamed(_) => {
                ice!(
                    "Scope resolution failed {} (in Expr::GetNamed)",
                    expr.log(self.pool)
                );
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::Value(value) => result.load_constant(self.interp.program, value.clone())?,
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    "if" => self.emit_call_if(result, arg)?,
                    "while" => self.emit_call_while(result, arg)?,
                    "addr" => self.addr_macro(result, arg)?,
                    "slice" => {
                        let (slot, container_ty) = self.compile_expr(result, arg, None)?;
                        let ty = self.interp.program.tuple_types(container_ty);
                        let expect = if let Some(types) = ty {
                            let expect = *unwrap!(types.iter().find(|t| !t.is_any()), "all any");
                            for t in types {
                                self.type_check_arg(*t, expect, "match slice types")?;
                            }
                            expect
                        } else {
                            container_ty
                        };
                        let ptr_ty = self.interp.program.ptr_type(expect);
                        let ptr = result.reserve_slots(self.interp.program, ptr_ty)?;
                        result.push(Bc::AbsoluteStackAddr {
                            of: slot,
                            to: ptr.single(),
                        });
                        result.to_drop.push((slot, container_ty));
                        (ptr, ptr_ty)
                    }
                    "c_call" => {
                        if let Expr::Call(f, arg) = arg.deref_mut().deref_mut().deref_mut() {
                            if let Expr::GetVar(v) = f.deref_mut().deref_mut() {
                                let name = self.pool.get(v.0);
                                let (func, f_ty) = unwrap!(
                                    self.builtin_constant(name),
                                    "undeclared ffi func: {name:?}",
                                );
                                let f_ty = unwrap!(self.interp.program.fn_ty(f_ty), "fn");
                                let (arg, _todo_arg_ty) =
                                    self.compile_expr(result, arg, Some(f_ty.arg))?;
                                let ret = result.reserve_slots(self.interp.program, f_ty.ret)?;
                                let (f, _) = result.load_constant(self.interp.program, func)?;
                                result.push(Bc::CallC {
                                    f: f.single(),
                                    arg,
                                    ret,
                                });
                                (ret, f_ty.ret)
                            } else {
                                err!("c_call expected Expr:Call(Expr::GetVar, ...args)",)
                            }
                        } else {
                            err!("c_call expected Expr:Call",)
                        }
                    }
                    "deref" => {
                        let (from, ptr_ty) = self.compile_expr(result, arg, requested)?;
                        let ty = unwrap!(self.interp.program.unptr_ty(ptr_ty), "not ptr");
                        let to = result.reserve_slots(self.interp.program, ty)?;
                        result.push(Bc::Load {
                            from: from.single(),
                            to,
                        });
                        (to, ty)
                    }
                    "first" => self.tuple_access(result, arg, requested, 0),
                    "second" => self.tuple_access(result, arg, requested, 1),
                    "reflect_print" => {
                        let (arg, _) = self.compile_expr(result, arg, None)?;
                        let ret = result.reserve_slots(self.interp.program, TypeId::unit())?;
                        result.push(Bc::CallBuiltin {
                            name: *macro_name,
                            ret,
                            arg,
                        });
                        (ret, TypeId::unit())
                    }
                    "type" => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects.
                        let ty = unwrap!(self.type_of(result, arg)?, "could not infer yet");
                        result.load_constant(self.interp.program, Value::Type(ty))?
                    }
                    "assert_compile_error" => {
                        // TODO: this can still have side-effects on the vm state tho :(
                        let state = self.mark_state(result.clone());
                        let res = self.compile_expr(result, arg, None);
                        assert!(res.is_err());
                        mem::forget(res); // TODO: dont do this. but for now i like having my drop impl that prints it incase i forget  ot unwrap
                        *result = self.restore_state(state);
                        result.load_constant(self.interp.program, Value::Unit)?
                    }
                    "comptime_print" => {
                        outln!("EXPR : {}", arg.log(self.pool));
                        let value = self.immediate_eval_expr(
                            &result.constants,
                            *arg.clone(),
                            TypeId::any(),
                        );
                        outln!("VALUE: {:?}", value);
                        result.load_constant(self.interp.program, Value::Unit)?
                    }
                    "struct" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref_mut().deref_mut() {
                            let ty = self.struct_type(result, pattern)?;
                            let ty = Value::Type(self.interp.program.intern_type(ty));
                            result.load_constant(self.interp.program, ty)?
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
                            let ty = Value::Type(self.interp.program.to_enum(ty));
                            result.load_constant(self.interp.program, ty)?
                        } else {
                            err!(
                                "expected map literal: .{{ name: Type, ... }} but found {:?}",
                                arg
                            );
                        }
                    }
                    "tag" => {
                        // TODO: auto deref and typecheking
                        let (addr, _addr_ty) = self.addr_macro(result, arg)?;
                        let ty = self
                            .interp
                            .program
                            .intern_type(TypeInfo::Ptr(TypeId::i64()));
                        let ret = result.reserve_slots(self.interp.program, ty)?;
                        result.push(Bc::SlicePtr {
                            base: addr.single(),
                            offset: 0,
                            count: 1,
                            ret: ret.single(),
                        });
                        (ret, ty)
                    }
                    "symbol" => {
                        // TODO: use match
                        if let Expr::GetNamed(i) = arg.deref_mut().deref_mut() {
                            result.load_constant(self.interp.program, Value::Symbol(i.0))?
                        } else if let Expr::GetVar(v) = arg.deref_mut().deref_mut() {
                            result.load_constant(self.interp.program, Value::Symbol(v.0 .0))?
                        } else {
                            ice!("Expected identifier found {arg:?}")
                        }
                    }
                    _ => err!(CErr::UndeclaredIdent(*macro_name)),
                }
            }
            Expr::FieldAccess(e, name) => {
                let (container_ptr, container_ptr_ty) = self.addr_macro(result, e)?;
                self.field_access_expr(result, container_ptr, container_ptr_ty, *name)?
            }
            Expr::StructLiteralP(pattern) => {
                let requested = unwrap!(requested, "struct literal needs type hint");
                let names: Vec<_> = pattern.flatten_names();
                // TODO: why must this suck so bad
                let values: Option<_> = pattern.flatten_exprs();
                let mut values: Vec<FatExpr<'_>> = values.unwrap();
                assert_eq!(names.len(), values.len());
                let raw_container_ty = self.interp.program.raw_type(requested);

                match self.interp.program.types[raw_container_ty.0].clone() {
                    TypeInfo::Struct {
                        fields, as_tuple, ..
                    } => {
                        assert_eq!(fields.len(), values.len());
                        let all = names.into_iter().zip(values).zip(fields);
                        let mut values = vec![];
                        for ((name, mut value), field) in all {
                            assert_eq!(name, field.name);
                            let (value, found_ty) =
                                self.compile_expr(result, &mut value, Some(field.ty))?;
                            self.type_check_arg(found_ty, field.ty, "struct field")?;
                            values.push((value, field.ty));
                        }

                        let (ret, _) =
                            result.produce_tuple(self.interp.program, values, as_tuple)?;
                        (ret, requested)
                    }
                    TypeInfo::Enum { cases, size } => {
                        assert_eq!(1, values.len());
                        let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                        let type_hint = cases[i].1;
                        let (value, found_ty) =
                            self.compile_expr(result, &mut values[0], Some(type_hint))?;
                        self.type_check_arg(found_ty, type_hint, "enum case")?;
                        if value.count >= size {
                            ice!("Enum value won't fit.")
                        }
                        let ret = result.reserve_slots(self.interp.program, raw_container_ty)?;
                        result.push(Bc::LoadConstant {
                            slot: ret.first,
                            value: Value::I64(i as i64),
                        });
                        result.push(Bc::MoveRange {
                            from: value,
                            to: StackRange {
                                first: ret.offset(1),
                                count: value.count,
                            },
                        });

                        (ret, requested)
                    }
                    _ => err!("struct literal but expected {:?}", requested),
                }
            }
            &mut Expr::String(i) => {
                let bytes = self.pool.get(i);
                let bytes = bytes
                    .as_bytes()
                    .iter()
                    .map(|b| Value::I64(*b as i64))
                    .collect();
                result.load_constant(self.interp.program, Value::new_box(bytes))?
            }
            Expr::GenericArgs(_, _) => {
                todo!()
            }
        })
    }

    fn addr_macro(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &mut FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        match arg.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                    let ptr_ty = self.interp.program.ptr_type(value_ty);
                    let addr_slot = result.reserve_slots(self.interp.program, ptr_ty)?;
                    result.push(Bc::AbsoluteStackAddr {
                        of: stack_slot,
                        to: addr_slot.single(),
                    });
                    Ok((addr_slot, ptr_ty))
                } else if result.constants.get(*var).is_some() {
                    err!("Took address of constant {}", var.log(self.pool))
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

    fn mark_state(&self, result: FnBody<'p>) -> StackHeights<'p> {
        StackHeights {
            value_stack: self.interp.value_stack.len(),
            call_stack: self.interp.call_stack.len(),
            debug_trace: self.debug_trace.len(),
            result,
        }
    }

    fn restore_state(&mut self, state: StackHeights<'p>) -> FnBody<'p> {
        drops(&mut self.interp.value_stack, state.value_stack);
        drops(&mut self.interp.call_stack, state.call_stack);
        drops(&mut self.debug_trace, state.debug_trace);
        state.result
    }

    // TODO: better error messages
    fn resolve_function(
        &mut self,
        result: &mut FnBody<'p>,
        name: Var<'p>,
        arg: &FatExpr<'p>,
        requested_ret: Option<TypeId>,
    ) -> Res<'p, FuncId> {
        // If there's only one option, we don't care what type it is.
        if let Some(f) = self.lookup_unique_func(name.0) {
            return Ok(f);
        }

        let state = DebugState::ResolveFnRef(name);
        self.push_state(&state);

        match self.type_of(result, arg) {
            Ok(Some(arg_ty)) => {
                if let Some((value, _)) = result.constants.get(name) {
                    match value {
                        Value::GetFn(f) => Ok(f),
                        Value::OverloadSet(i) => {
                            let overloads = &self.interp.program.overload_sets[i];

                            let accept = |f_ty: FnType| {
                                arg_ty == f_ty.arg
                                    && (requested_ret.is_none()
                                        || (requested_ret.unwrap() == f_ty.ret))
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
                                    "for fn {}({}) {:?};",
                                    name.log(s.pool),
                                    s.interp.program.log_type(arg_ty),
                                    requested_ret
                                        .map(|t| s.interp.program.log_type(t))
                                        .unwrap_or_else(|| "??".to_string())
                                )
                            };

                            // Compute overloads.
                            if let Some(decls) = self.interp.program.declarations.get(&name.0) {
                                let mut found = None;
                                let decls = decls.clone();
                                for f in &decls {
                                    if let Ok(f_ty) = self.infer_types(*f) {
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
                                    Some((func, ty)) => {
                                        self.interp.program.overload_sets[i].0.push(
                                            OverloadOption {
                                                name: name.0,
                                                ty,
                                                func,
                                            },
                                        );
                                        self.pop_state(state);
                                        Ok(func)
                                    }
                                    None => {
                                        // TODO: put the message in the error so !assert_compile_error doesn't print it.
                                        outln!("not found {}", log_goal(self));
                                        for f in &decls {
                                            if let Ok(f_ty) = self.infer_types(*f) {
                                                outln!(
                                                    "- found {:?} fn({}) {};",
                                                    f,
                                                    self.interp.program.log_type(f_ty.arg),
                                                    self.interp.program.log_type(f_ty.ret),
                                                );
                                            }
                                        }
                                        outln!("Maybe you forgot to instantiate a generic?");

                                        err!(CErr::AmbiguousCall)
                                    }
                                }
                            } else {
                                err!(CErr::VarNotFound(name))
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
                } else {
                    err!(CErr::VarNotFound(name))
                }
            }
            Ok(None) => err!(
                "AmbiguousCall. Unknown type for argument {}",
                arg.log(self.pool)
            ),
            Err(e) => err!(
                "AmbiguousCall. Unknown type for argument {}. {}",
                arg.log(self.pool),
                e.reason.log(self.interp.program, self.pool)
            ),
        }
    }

    // TODO: this is clunky. Err means invalid input, None means couldn't infer type (often just not implemented yet).
    fn type_of(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        if let Some(ty) = expr.ty {
            return Ok(Some(ty));
        }
        Ok(Some(match expr.deref() {
            Expr::Value(v) => self.interp.program.type_of(v),
            Expr::Call(f, arg) => {
                if let Expr::GetVar(i) = f.deref().deref() {
                    if let Ok(fid) = self.resolve_function(result, *i, arg, None) {
                        if self.infer_types(fid).is_ok() {
                            let f_ty = self.interp.program.funcs[fid.0].unwrap_ty();
                            return Ok(Some(f_ty.ret));
                        }
                    }
                }
                return Ok(None);
            }
            Expr::Block { result: e, .. } => return self.type_of(result, e),
            Expr::Tuple(values) => {
                let types: Res<'p, Vec<_>> =
                    values.iter().map(|v| self.type_of(result, v)).collect();
                let types = types?;
                let before = types.len();
                let types: Vec<_> = types.into_iter().flatten().collect();
                assert_eq!(before, types.len());
                self.interp.program.tuple_of(types)
            }
            Expr::FieldAccess(container, name) => {
                if let Some(container_ty) = self.type_of(result, container)? {
                    let container_ptr_ty = self.interp.program.ptr_type(container_ty); // TODO: kinda hacky that you need to do this. should be more consistant
                    self.get_field_type(container_ptr_ty, *name)?
                } else {
                    return Ok(None);
                }
            }
            Expr::GetNamed(_)
            | Expr::StructLiteralP(_)
            | Expr::ArrayLiteral(_)
            | Expr::RefType(_)
            | Expr::EnumLiteral(_)
            | Expr::GenericArgs(_, _)
            | Expr::Closure(_) => return Ok(None),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    "addr" => match arg.deref().deref() {
                        Expr::GetVar(var) => {
                            let (_, value_ty) = *result
                                .vars
                                .get(var)
                                .expect("Missing resolved var (TODO: addr of const?)");
                            self.interp.program.intern_type(TypeInfo::Ptr(value_ty))
                        }
                        &Expr::GetNamed(i) => {
                            logln!(
                                "UNDECLARED IDENT: {} (in SuffixMacro::addr)",
                                self.pool.get(i)
                            );
                            err!(CErr::UndeclaredIdent(i))
                        }
                        _ => err!(CErr::AddrRvalue(*arg.clone())),
                    },
                    "type" => self.interp.program.intern_type(TypeInfo::Type),
                    "deref" => {
                        let ptr_ty = self.type_of(result, arg)?;
                        if let Some(ptr_ty) = ptr_ty {
                            return Ok(self.interp.program.unptr_ty(ptr_ty));
                        } else {
                            return Ok(None);
                        }
                    }
                    _ => return Ok(None),
                }
            }
            Expr::GetVar(var) => {
                if let Some((_, ty)) = result.vars.get(var).cloned() {
                    ty
                } else if let Some((_, ty)) = result.constants.get(*var) {
                    ty
                } else {
                    ice!("type check missing var {var:?}")
                }
            }
            Expr::String(_) => self
                .interp
                .program
                .intern_type(TypeInfo::Ptr(TypeId::i64())),
        }))
    }

    fn builtin_constant(&mut self, name: &str) -> Option<(Value, TypeId)> {
        use TypeInfo::*;
        let ty = match name {
            "i64" => Some(I64),
            "f64" => Some(F64),
            "Type" => Some(Type),
            "bool" => Some(Bool),
            "Any" => Some(Any),
            _ => None,
        };
        if let Some(ty) = ty {
            let ty = self.interp.program.intern_type(ty);
            let tyty = self.interp.program.intern_type(TypeInfo::Type);
            return Some((Value::Type(ty), tyty));
        }

        // It feels like this could trivially be a generic function but somehow no. I dare you to fix it.
        macro_rules! ffi_type {
            ($name:ty) => {{
                let id = unsafe { mem::transmute(std::any::TypeId::of::<$name>()) };
                let ty = self.interp.program.get_ffi_type::<$name>(id);
                (Value::Type(ty), TypeId::ty())
            }};
        }

        macro_rules! cfn {
            ($fn:expr, $ty:expr) => {{
                #[cfg(not(feature = "interp_c_ffi"))]
                {
                    outln!("Comptime c ffi is disabled.");
                    return None;
                }
                #[cfg(feature = "interp_c_ffi")]
                {
                    (
                        Value::CFnPtr {
                            ptr: $fn as usize,
                            ty: $ty,
                        },
                        self.interp.program.intern_type(TypeInfo::Fn($ty)),
                    )
                }
            }};
        }

        Some(match name {
            "true" => (
                Value::Bool(true),
                self.interp.program.intern_type(TypeInfo::Bool),
            ),
            "false" => (
                Value::Bool(false),
                self.interp.program.intern_type(TypeInfo::Bool),
            ),
            "Symbol" => ffi_type!(Ident),
            "CmdResult" => ffi_type!(CmdResult),
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
            _ => return None,
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
                        let types = self.interp.program.tuple_of(types);
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
        Ok(match binding {
            Binding::Named(_, ty) | Binding::Var(_, ty) | Binding::Discard(ty) => {
                self.infer_types_progress(constants, ty)?
            }
        })
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
        let f = &self.interp.program.funcs[func.0];
        debug_assert!(!f.evil_uninit);
        if let Some(ty) = f.finished_type {
            return Ok(ty);
        }

        Ok(mut_replace!(
            self.interp.program.funcs[func.0],
            |mut f: Func<'p>| {
                let state = DebugState::ResolveFnType(func);
                self.last_loc = Some(f.loc);
                self.push_state(&state);
                logln!("RESOLVE: Arg of {func:?}");
                let types = self.infer_pattern(&f.closed_constants, &mut f.arg.bindings)?;
                logln!("RESOLVE: Ret of {func:?}");
                assert!(self.infer_types_progress(&f.closed_constants, &mut f.ret)?);
                let arg = self.interp.program.tuple_of(types);
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
        e: FatExpr<'p>,
        ret_ty: TypeId,
    ) -> Res<'p, Value> {
        match e.deref() {
            Expr::Value(value) => return Ok(value.clone()),
            Expr::GetVar(var) => {
                // fast path for builtin type identifiers
                if let Some((value, _)) = constants.get(*var) {
                    debug_assert_ne!(value, Value::Poison);
                    return Ok(value);
                }
                // fallthrough
            }
            Expr::Tuple(elements) => {
                let types = if ret_ty == TypeId::ty() {
                    vec![TypeId::ty(); elements.len()]
                } else if let Some(types) = self.interp.program.tuple_types(ret_ty) {
                    types.to_vec()
                } else {
                    unreachable!("{}", self.interp.program.log_type(ret_ty))
                };
                let values: Res<'p, Vec<_>> = elements
                    .iter()
                    .zip(types)
                    .map(|(e, ty)| self.immediate_eval_expr(constants, e.clone(), ty))
                    .collect();
                return Ok(Value::Tuple {
                    container_type: TypeId::any(),
                    values: values?,
                });
            }
            Expr::Call(_, _) => {
                // let f = self.resolve_function(result, name, arg)
                // return self.emit_comptime_call(&constants, f, &arg);
            }
            _ => {} // fallthrough
        }
        // println!(
        //     "immediate_eval_expr {} with consts:\n{}",
        //     e.log(self.pool),
        //     self.interp.program.log_consts(constants)
        // );

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
        let func_id = self.interp.program.add_func(fake_func);
        logln!(
            "Made anon: {func_id:?} = {}",
            self.interp.program.funcs[func_id.0].log(self.pool)
        );
        let result = self.compile_and_run(func_id, Value::Unit, ExecTime::Comptime)?;
        logln!(
            "COMPUTED: {} -> {:?} under {}",
            e.log(self.pool),
            result,
            self.interp.program.funcs[func_id.0].log(self.pool)
        );
        self.pop_state(state);
        Ok(result)
    }

    #[track_caller]
    fn to_type(&mut self, value: Value) -> Res<'p, TypeId> {
        if let Value::Type(id) = value {
            Ok(id)
        } else if let Value::Tuple { values, .. } = value {
            let values: Res<'_, Vec<_>> = values.into_iter().map(|v| self.to_type(v)).collect();
            Ok(self.interp.program.tuple_of(values?))
        } else if let Value::Unit = value {
            // This lets you use the literal `()` as a type (i dont parse it as a tuple because reasons). TODO: check if that still true with new parser
            Ok(self.interp.program.intern_type(TypeInfo::Unit))
        } else {
            err!(CErr::TypeError("Type", value))
        }
    }

    fn add_func(&mut self, mut func: Func<'p>, constants: &Constants<'p>) -> Res<'p, FuncId> {
        debug_assert!(func.closed_constants.local.is_empty());
        debug_assert!(func.closed_constants.is_valid);
        // println!("ADD_FUNC {}", func.log_captures(self.pool));
        func.closed_constants = constants.close(&func.capture_vars_const)?;
        // TODO: make this less trash. it fixes generics where it thinks a cpatured argument is var cause its arg but its actually in consts because generic.
        for capture in &func.capture_vars {
            if let Some(val) = constants.get(*capture) {
                func.closed_constants.insert(*capture, val);
            }
        }
        // println!(
        //     "ADD_FUNC CLOSED {}",
        //     self.interp.program.log_consts(&func.closed_constants)
        // );
        let id = self.interp.program.add_func(func);
        Ok(id)
    }

    // TODO: make this not a special case.
    /// This swaps out the closures for function accesses.
    fn emit_call_if(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &mut FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        let ((cond, _), if_true, if_false) = if let Expr::Tuple(parts) = arg.deref_mut() {
            let cond = self.compile_expr(result, &mut parts[0], None)?;
            let if_true = if let Expr::Closure(func) = parts[1].deref_mut() {
                let f = self.add_func(mem::take(func), &result.constants)?;
                *parts[1].deref_mut() = Expr::Value(Value::GetFn(f));
                f
            } else {
                ice!("if second arg must be func not {:?}", parts[1]);
            };
            let if_false = if let Expr::Closure(func) = parts[2].deref_mut() {
                let f = self.add_func(mem::take(func), &result.constants)?;
                *parts[2].deref_mut() = Expr::Value(Value::GetFn(f));
                f
            } else {
                ice!("if third arg must be func not {:?}", parts[2]);
            };
            (cond, if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        let true_ty = self.infer_types(if_true)?;
        let unit = self.interp.program.intern_type(TypeInfo::Unit);
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        self.type_check_eq(true_ty.arg, unit, sig)?;
        let false_ty = self.infer_types(if_false)?;
        self.type_check_eq(false_ty.arg, unit, sig)?;
        self.type_check_eq(true_ty.ret, false_ty.ret, sig)?;

        // TODO: if returning tuples
        let ret = result.reserve_slots(self.interp.program, true_ty.ret)?;

        let arg = result.load_constant(self.interp.program, Value::Unit)?.0; // Note: before you start doing ip stuff!
        let name = self.pool.intern("builtin:if");
        let branch_ip = result.push(Bc::DebugMarker("patch", name));
        let true_ip = result.insts.len();
        let true_ret = self.emit_capturing_call(result, arg, if_true)?;
        result.push(Bc::MoveRange {
            from: true_ret,
            to: ret,
        });
        let jump_over_false = result.push(Bc::DebugMarker("patch", name));
        let false_ip = result.insts.len();
        let false_ret = self.emit_capturing_call(result, arg, if_false)?;
        result.push(Bc::MoveRange {
            from: false_ret,
            to: ret,
        });

        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond.first,
            true_ip,
            false_ip,
        };
        result.insts[jump_over_false] = Bc::Goto {
            ip: result.insts.len(),
        };

        Ok((ret, true_ty.ret))
    }

    fn emit_call_while(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &mut FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        let (cond_fn, body_fn) = if let Expr::Tuple(parts) = arg.deref_mut() {
            let cond = if let Expr::Closure(func) = parts[0].deref_mut() {
                let f = self.add_func(mem::take(func), &result.constants)?;
                *parts[0].deref_mut() = Expr::Value(Value::GetFn(f));
                f
            } else {
                ice!("while first arg must be func not {:?}", parts[0]);
            };
            let body = if let Expr::Closure(func) = parts[1].deref_mut() {
                let f = self.add_func(mem::take(func), &result.constants)?;
                *parts[1].deref_mut() = Expr::Value(Value::GetFn(f));
                f
            } else {
                ice!("while second arg must be func not {:?}", parts[1]);
            };
            (cond, body)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        let cond_ty = self.infer_types(cond_fn)?;
        let body_ty = self.infer_types(body_fn)?;
        let sig = "while(fn(Unit) bool, fn(Unit) Unit)";
        self.type_check_eq(cond_ty.arg, TypeId::unit(), sig)?;
        self.type_check_eq(cond_ty.ret, TypeId::bool(), sig)?;
        self.type_check_eq(body_ty.arg, TypeId::unit(), sig)?;
        self.type_check_eq(body_ty.ret, TypeId::unit(), sig)?;

        let name = self.pool.intern("builtin:while");
        let cond_ip = result.insts.len();
        let unit = result.load_constant(self.interp.program, Value::Unit)?.0;
        let cond_ret = self.emit_capturing_call(result, unit, cond_fn)?;
        let branch_ip = result.push(Bc::DebugMarker("patch", name));

        let body_ip = result.insts.len();
        let unit = result.load_constant(self.interp.program, Value::Unit)?.0;
        let body_ret = self.emit_capturing_call(result, unit, body_fn)?;
        result.push(Bc::Drop(body_ret));
        result.push(Bc::Goto { ip: cond_ip });
        let end_ip = result.insts.len();

        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond_ret.single(),
            true_ip: body_ip,
            false_ip: end_ip,
        };

        result.load_constant(self.interp.program, Value::Unit)
    }

    #[track_caller]
    fn type_check_eq(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        if found == expected {
            Ok(())
        } else {
            err!(CErr::TypeCheck(found, expected, msg))
        }
    }

    // #[track_caller]
    fn type_check_arg(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        if found == expected || found.is_any() || expected.is_any() {
            Ok(())
        } else {
            match (
                &self.interp.program.types[found.0],
                &self.interp.program.types[expected.0],
            ) {
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
                (&TypeInfo::Ptr(f), &TypeInfo::Ptr(e)) => {
                    if self.type_check_arg(f, e, msg).is_ok() {
                        return Ok(());
                    }
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
        result: &FnBody<'p>,
        pattern: &mut Pattern<'p>,
    ) -> Res<'p, TypeInfo<'p>> {
        // TODO: maybe const keyword before name in func/struct lets you be generic.
        let types = self.infer_pattern(&result.constants, &mut pattern.bindings)?;
        let raw_fields = pattern.flatten();

        let as_tuple = self.interp.program.tuple_of(types);
        let mut fields = vec![];
        let mut size = 0;
        for (name, ty) in raw_fields {
            let count = self.interp.program.slot_count(ty);
            fields.push(Field {
                name: unwrap!(name, "field name").0,
                ty,
                first: size,
                count,
            });
            size += count;
        }
        Ok(TypeInfo::Struct {
            fields,
            size,
            as_tuple,
        })
    }

    fn set_deref(
        &mut self,
        result: &mut FnBody<'p>,
        place: &mut FatExpr<'p>,
        value: &mut FatExpr<'p>,
    ) -> Res<'p, ()> {
        match place.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                let var_info = self.interp.program.vars[var.1]; // TODO: the type here isn't set.
                assert_eq!(
                    var_info.kind,
                    VarType::Var,
                    "Only 'var' can be reassigned (not let/const). {:?}",
                    place
                );
                let slot = result.vars.get(var);
                let (slot, oldty) =
                    *unwrap!(slot, "SetVar: var must be declared: {}", var.log(self.pool));

                let (value, new_ty) = self.compile_expr(result, value, Some(oldty))?;
                self.type_check_arg(new_ty, oldty, "reassign var")?;
                result.push(Bc::Drop(slot));
                assert_eq!(value.count, slot.count);
                for i in 0..value.count {
                    result.push(Bc::Move {
                        from: value.offset(i),
                        to: slot.offset(i),
                    });
                }
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: type checking
                // TODO: general place expressions.
                let macro_name = self.pool.get(*macro_name);
                if macro_name == "deref" {
                    let (ptr, ptr_ty) = self.compile_expr(result, arg, None)?;
                    let expected_ty = unwrap!(self.interp.program.unptr_ty(ptr_ty), "not ptr");
                    let (value, new_ty) = self.compile_expr(result, value, Some(expected_ty))?;
                    self.type_check_arg(new_ty, expected_ty, "set ptr")?;
                    result.push(Bc::Store {
                        to: ptr.single(),
                        from: value,
                    });

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
        result: &mut FnBody<'p>,
        mut container_ptr: StackRange,
        mut container_ptr_ty: TypeId,
        name: Ident<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        // Auto deref for nested place expressions.
        // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        let depth = self.interp.program.ptr_depth(container_ptr_ty);
        if depth > 1 {
            for _ in 0..(depth - 1) {
                container_ptr_ty = unwrap!(self.interp.program.unptr_ty(container_ptr_ty), "");
                let ret = result.reserve_slots(self.interp.program, container_ptr_ty)?;
                result.push(Bc::Load {
                    from: container_ptr.single(),
                    to: ret,
                });
                container_ptr = ret;
            }
        }
        let container_ty = unwrap!(
            self.interp.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.interp.program.log_type(container_ptr_ty)
        );

        let raw_container_ty = self.interp.program.raw_type(container_ty);
        match &self.interp.program.types[raw_container_ty.0] {
            TypeInfo::Struct { fields, .. } => {
                for f in fields {
                    if f.name == name {
                        let f = *f;
                        let ty = self.interp.program.ptr_type(f.ty);
                        let ret = result.reserve_slots(self.interp.program, ty)?;

                        result.push(Bc::SlicePtr {
                            base: container_ptr.single(),
                            offset: f.first,
                            count: f.count,
                            ret: ret.single(),
                        });
                        return Ok((ret, ty));
                    }
                }
                err!(
                    "unknown name {} on {:?}",
                    self.pool.get(name),
                    self.interp.program.log_type(container_ty)
                );
            }
            TypeInfo::Enum { cases, .. } => {
                for (i, (f_name, f_ty)) in cases.iter().enumerate() {
                    if *f_name == name {
                        let f_ty = *f_ty;
                        let ty = self.interp.program.ptr_type(f_ty);
                        let ret = result.reserve_slots(self.interp.program, ty)?;
                        let count = self.interp.program.slot_count(f_ty);
                        result.push(Bc::TagCheck {
                            enum_ptr: container_ptr.single(),
                            value: i as i64,
                        });
                        result.push(Bc::SlicePtr {
                            base: container_ptr.single(),
                            offset: 1,
                            count,
                            ret: ret.single(),
                        });
                        return Ok((ret, ty));
                    }
                }
                err!(
                    "unknown name {} on {:?}",
                    self.pool.get(name),
                    self.interp.program.log_type(container_ty)
                );
            }
            _ => err!(
                "only structs support field access but found {}",
                self.interp.program.log_type(container_ty)
            ),
        }
    }

    // TODO: copy paste from the emit
    fn get_field_type(&mut self, mut container_ptr_ty: TypeId, name: Ident<'_>) -> Res<'p, TypeId> {
        // Auto deref for nested place expressions.
        // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        let depth = self.interp.program.ptr_depth(container_ptr_ty);
        if depth > 1 {
            for _ in 0..(depth - 1) {
                container_ptr_ty = unwrap!(self.interp.program.unptr_ty(container_ptr_ty), "");
            }
        }

        let container_ty = unwrap!(
            self.interp.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.interp.program.log_type(container_ptr_ty)
        );

        let unknown_name = || {
            err!(
                "unknown name {} on {:?}",
                self.pool.get(name),
                self.interp.program.log_type(container_ty)
            )
        };

        let raw_container_ty = self.interp.program.raw_type(container_ty);
        match &self.interp.program.types[raw_container_ty.0] {
            TypeInfo::Struct { fields, .. } => {
                for f in fields {
                    if f.name == name {
                        let f = *f;
                        let ty = self.interp.program.ptr_type(f.ty);
                        return Ok(ty);
                    }
                }
                unknown_name()
            }
            TypeInfo::Enum { cases, .. } => {
                for (_, (f_name, f_ty)) in cases.iter().enumerate() {
                    if *f_name == name {
                        let f_ty = *f_ty;
                        let ty = self.interp.program.ptr_type(f_ty);

                        return Ok(ty);
                    }
                }
                unknown_name()
            }
            TypeInfo::Unique(_, _) => unreachable!(),
            _ => err!(
                "only structs support field access but found {}",
                self.interp.program.log_type(container_ty)
            ),
        }
    }

    fn tuple_access(
        &self,
        _result: &mut FnBody<'p>,
        _container: &FatExpr<'p>,
        _requested: Option<TypeId>,
        _index: i32,
    ) -> (StackRange, TypeId) {
        todo!()
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecTime {
    Comptime,
    Runtime,
}

impl<'p> FnBody<'p> {
    fn reserve_slots_raw(
        &mut self,
        program: &Program<'p>,
        count: usize,
        ty: TypeId,
    ) -> Res<'p, StackRange> {
        let first = StackOffset(self.stack_slots);

        if ty.is_any() {
            // debug_assert_eq!(count, 1, "no any tuple");
            for _ in 0..count {
                self.slot_types.push(TypeId::any());
            }
            self.stack_slots += count;
        } else if count == 1 {
            self.slot_types.push(ty);
            self.stack_slots += count;
        } else {
            let types = unwrap!(
                program.tuple_types(ty),
                "expected multiple slots {:?}",
                program.log_type(ty)
            );
            let mut found = 0;
            for ty in types {
                found += self.reserve_slots(program, *ty)?.count;
            }
            debug_assert_eq!(found, count, "bad tuple size");
            // Note: don't bump self.stack_slots here.
        }
        Ok(StackRange { first, count })
    }

    fn reserve_slots(&mut self, program: &Program<'p>, ty: TypeId) -> Res<'p, StackRange> {
        let ty = program.raw_type(ty);
        match &program.types[ty.0] {
            TypeInfo::Enum { size, .. } => {
                let size = *size;
                let first = StackOffset(self.stack_slots);
                self.slot_types.push(TypeId::i64());
                self.stack_slots += size;
                Ok(StackRange { first, count: size })
            }
            _ => {
                let count = program.slot_count(ty);
                self.reserve_slots_raw(program, count, ty)
            }
        }
    }

    fn load_constant(
        &mut self,
        program: &mut Program<'p>,
        value: Value,
    ) -> Res<'p, (StackRange, TypeId)> {
        let ty = program.type_of(&value);
        let to = self.reserve_slots(program, ty)?;
        self.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        Ok((to, ty))
    }

    fn _serialize_constant<T: InterpSend<'p>>(
        &mut self,
        program: &mut Program<'p>,
        value: T,
    ) -> Res<'p, (StackRange, TypeId)> {
        let ty = T::get_type(program);
        let value = value.serialize();
        let to = self.reserve_slots(program, ty)?;
        self.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        Ok((to, ty))
    }

    #[track_caller]
    fn push(&mut self, inst: Bc<'p>) -> usize {
        let ip = self.insts.len();
        self.insts.push(inst);

        #[cfg(feature = "some_log")]
        {
            self.debug.push(DebugInfo {
                internal_loc: Location::caller(),
                src_loc: self.last_loc,
                p: Default::default(),
            });
            debug_assert_eq!(self.insts.len(), self.debug.len(), "lost debug info");
        }
        ip
    }

    pub fn produce_tuple(
        &mut self,
        program: &mut Program<'p>,
        owned_values: Vec<(StackRange, TypeId)>,
        tuple_ty: TypeId,
    ) -> Res<'p, (StackRange, TypeId)> {
        // They might already be consecutive

        debug_assert!(!owned_values.is_empty());
        let mut next = (owned_values[0].0).first;
        let mut ok = true;
        for (r, _) in &owned_values {
            if r.first != next {
                ok = false;
                break;
            }
            next.0 += r.count;
        }
        let ret = if ok {
            StackRange {
                first: (owned_values[0].0).first,
                count: program.slot_count(tuple_ty),
            }
        } else {
            let ret = self.reserve_slots(program, tuple_ty)?;
            // TODO: they might already be consecutive. kinda want to have something to profile before fixing.
            let base = ret.first.0;
            let mut count = 0;
            for (v, _) in owned_values {
                for i in 0..v.count {
                    self.push(Bc::Move {
                        from: StackOffset(v.first.0 + i),
                        to: StackOffset(base + count),
                    });
                    count += 1;
                }
            }
            assert_eq!(count, ret.count);
            ret
        };

        Ok((ret, tuple_ty))
    }
}

/// https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695
fn invert<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    x.map_or(Ok(None), |v| v.map(Some))
}

impl<'p> Bc<'p> {
    // Used for inlining
    fn renumber(&mut self, stack_offset: usize, ip_offset: usize) {
        match self {
            Bc::CallDynamic { f, ret, arg } | Bc::CallC { f, ret, arg } => {
                f.0 += stack_offset;
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallDirect { f: _, ret, arg } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallBuiltin { ret, arg, .. } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::LoadConstant { slot, .. } => slot.0 += stack_offset,
            Bc::JumpIf {
                cond,
                true_ip,
                false_ip,
            } => {
                cond.0 += stack_offset;
                *true_ip += ip_offset;
                *false_ip += ip_offset;
            }
            Bc::Goto { ip } => *ip += ip_offset,
            Bc::CloneCreateTuple { target, .. } | Bc::MoveCreateTuple { target, .. } => {
                target.0 += stack_offset;
            }
            Bc::Drop(arg) | Bc::Ret(arg) => {
                arg.first.0 += stack_offset;
            }

            Bc::Move { from, to } | Bc::Clone { from, to } => {
                from.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                from.first.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::ExpandTuple { from, to } | Bc::Load { from, to } => {
                from.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::AbsoluteStackAddr { of, to } => {
                of.first.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::Store { from, to } => {
                from.first.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::SlicePtr {
                base, offset, ret, ..
            } => {
                base.0 += stack_offset;
                *offset += stack_offset;
                ret.0 += stack_offset;
            }
            Bc::TagCheck { enum_ptr, .. } => {
                enum_ptr.0 += stack_offset;
            }
            Bc::DebugLine(_) | Bc::DebugMarker(_, _) => {}
        }
    }
}

fn drops<T>(vec: &mut Vec<T>, new_len: usize) {
    for _ in 0..(vec.len() - new_len) {
        vec.pop();
    }
}

// i like when my code is rocks not rice
// its a lot more challenging to eat but at least you can Find it
