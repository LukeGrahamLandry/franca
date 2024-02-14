//! Converts ASTs into my bytecode-ish format.
//! Type checking, overload resolution, implicit function calls, inlining, monomorphization, etc.
//! Uses the interpreter for comptime evalutation (build scripts, generics, macros, etc).

#![allow(clippy::wrong_self_convention)]
use codemap::Span;
use std::fmt::Write;
use std::marker::PhantomData;
use std::mem;
use std::rc::Rc;
use std::{ops::Deref, panic::Location};

use crate::ast::{Annotation, FatStmt, Field, Var, VarType};
use crate::bc::*;
use crate::ffi::InterpSend;
use crate::interp::{to_flat_seq, CallFrame, Interp};
use crate::logging::{outln, PoolLog};
use crate::{
    ast::{
        Expr, FatExpr, FnType, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo,
    },
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
    JitToBc(FuncId, ExecTime),
    RunInstLoop(FuncId),
    ComputeCached(FatExpr<'p>),
    ResolveFnType(FuncId, LazyType<'p>, LazyType<'p>),
    EvalConstants(FuncId),
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
        writeln!(out, "{}", Location::caller()).unwrap();

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

    pub fn add_declarations(
        &mut self,
        constants: &SharedConstants<'p>,
        ast: Func<'p>,
    ) -> Res<'p, ()> {
        let f = self.interp.program.add_func(ast);
        self.ensure_compiled(constants, f, ExecTime::Comptime)?;
        Ok(())
    }

    pub fn lookup_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        self.interp.program.declarations.get(&name).map(|decls| {
            debug_assert_eq!(decls.len(), 1);
            decls[0]
        })
    }

    pub fn compile(
        &mut self,
        constants: Option<&SharedConstants<'p>>,
        f: FuncId,
        when: ExecTime,
    ) -> Res<'p, ()> {
        let state = DebugState::Compile(f);
        self.push_state(&state);
        let init_heights = (self.interp.call_stack.len(), self.interp.value_stack.len());
        let constants =
            constants.map_or_else(|| Rc::new(SharedConstants::default()), |c| c.clone().bake());
        let result = self.ensure_compiled(&constants, f, when);
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

    fn compile_and_run(
        &mut self,
        constants: Option<&SharedConstants<'p>>,
        f: FuncId,
        arg: Value,
        when: ExecTime,
    ) -> Res<'p, Value> {
        self.compile(constants, f, when)?;
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
    pub fn empty_fn(&self, when: ExecTime, func: FuncId, loc: Span) -> FnBody<'p> {
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
            constants: Default::default(),
        }
    }

    fn ensure_compiled(
        &mut self,
        constants: &SharedConstants<'p>,
        FuncId(index): FuncId,
        when: ExecTime,
    ) -> Res<'p, ()> {
        if let Some(Some(_)) = self.interp.ready.get(index) {
            return Ok(());
        }
        let state = DebugState::JitToBc(FuncId(index), when);
        self.push_state(&state);
        let func = &self.interp.program.funcs[index];
        let mut constants = constants.clone();
        if !func.local_constants.is_empty() {
            let state = DebugState::EvalConstants(FuncId(index));
            self.push_state(&state);
            let func = &self.interp.program.funcs[index];

            // TODO: pass in comptime known args
            // TODO: do i even need to pass an index? probably just for debugging
            let mut result = self.empty_fn(when, FuncId(index + 10000000), func.loc);
            result.constants.parents.push(constants.clone().bake());
            let new_constants = func.local_constants.clone();
            for stmt in new_constants {
                self.compile_stmt(&mut result, &stmt)?;
            }
            self.pop_state(state);
            constants.parents.push(result.constants.bake())
        }
        while self.interp.ready.len() <= index {
            self.interp.ready.push(None);
        }
        logln!(
            "Start JIT: {:?} \n{}",
            FuncId(index),
            self.interp.program.funcs[index].log(self.pool)
        );
        self.infer_types(&constants, FuncId(index))?;
        let func = &self.interp.program.funcs[index];
        let (arg, _) = func.ty.unwrap();
        let mut result = self.empty_fn(when, FuncId(index), func.loc);
        result.constants = constants;
        let arg_range = result.reserve_slots(self.interp.program, arg);
        let return_value = self.emit_body(&mut result, arg_range, FuncId(index))?;
        let func = &self.interp.program.funcs[index];

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
        self.interp.ready[index] = Some(result);
        logln!(
            "Done JIT: {:?} {}",
            FuncId(index),
            func.synth_name(self.pool)
        );
        self.pop_state(state);
        Ok(())
    }

    // This is used for
    // - emiting normal functions into a fresh FnBody
    // -
    // -
    fn emit_body(
        &mut self,
        result: &mut FnBody<'p>,
        arg_range: StackRange,
        f: FuncId,
    ) -> Res<'p, StackRange> {
        let func = self.interp.program.funcs[f.0].clone();
        let (arg, ret) = func.ty.unwrap();
        let mut to_drop = vec![];
        let arguments = func.arg_vars.as_ref().unwrap();
        if arguments.len() == 1 {
            // if there's one name, it refers to the whole tuple.
            let prev = result.vars.insert(arguments[0], (arg_range, arg));
            assert!(prev.is_none(), "overwrite arg?");
        } else if arguments.len() == arg_range.count {
            let types = self.interp.program.tuple_types(arg).unwrap();
            // if they match, each element has its own name.
            for (i, var) in arguments.iter().enumerate() {
                // This always starts at 0 for normal functions, but for inlined closures, the args could be anywhere because we're sharing the parent's stack frame.
                let range = StackRange {
                    first: arg_range.offset(i),
                    count: 1,
                };
                let prev = result.vars.insert(*var, (range, types[i]));
                assert!(prev.is_none(), "overwrite arg?");
            }
        } else {
            // TODO: pattern match destructuring but for now you just cant refer to the arg.
            to_drop.push(arg_range);
        }
        let return_value = match func.body.as_ref() {
            Some(body) => {
                let (ret_val, found_ret_ty) = self.compile_expr(result, body, None)?;
                self.type_check_arg(found_ret_ty, ret, "bad return value")?;
                // We're done with our arguments, get rid of them. Same for other vars.
                // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
                result.push(Bc::DebugMarker("drop_args", func.get_name(self.pool)));
                for var in arguments {
                    let (slot, _) = unwrap!(result.vars.remove(var), "lost arg");
                    result.push(Bc::Drop(slot));
                }
                for other in to_drop {
                    result.push(Bc::Drop(other));
                }
                ret_val
            }
            None => {
                // Functions without a body are always builtins.
                // It's convient to give them a FuncId so you can put them in a variable,
                // but just force inline call.
                self.interp.program.funcs[f.0].annotations.push(Annotation {
                    name: self.pool.intern("inline"),
                    args: None,
                });
                let ret = result.reserve_slots(self.interp.program, ret);
                let name = unwrap!(func.name, "fn no body needs name");
                result.push(Bc::CallBuiltin {
                    name,
                    ret,
                    arg: arg_range,
                });
                for var in arguments {
                    // TODO: why isn't it always there?
                    result.vars.remove(var);
                    // dont drop. was moved to call
                }
                ret
            }
        };
        Ok(return_value)
    }

    fn emit_capturing_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        f: FuncId,
    ) -> Res<'p, StackRange> {
        let name = self.interp.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker("start:capturing_call", name));
        self.infer_types(&result.constants, f)?;
        assert!(
            !self.currently_inlining.contains(&f),
            "Tried to inline recursive function."
        );
        self.currently_inlining.push(f);
        let return_range = self.emit_body(result, arg, f)?;
        result.push(Bc::DebugMarker("end:capturing_call", name));
        self.currently_inlining.retain(|check| *check != f);
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
        self.ensure_compiled(&result.constants, f, result.when)?;
        let name = self.interp.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker("start:inline_call", name));
        // This move ensures they end up at the base of the renumbered stack.
        let func = &self.interp.program.funcs[f.0];
        let msg = "capturing calls are already inlined. what are you doing here?";
        assert_eq!(func.capture_vars.len(), 0, "{}", msg);
        let (arg_ty, _) = func.ty.unwrap();

        let stack_offset = if arg.first.0 == (result.stack_slots - arg.count) {
            // It's already at the top of the stack so don't need to move
            result.stack_slots - arg.count
        } else {
            let stack_offset = result.stack_slots;
            let arg_slots = result.reserve_slots(self.interp.program, arg_ty); // These are included in the new function's stack
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

    // TODO: do the memo stuff here instead of my crazy other thing.
    // TODO: maybe call this @generic instead of @comptime? cause other things can be comptime
    // Return type is allowed to use args.
    fn emit_comptime_call(
        &mut self,
        result: &mut FnBody<'p>,
        f: FuncId,
        arg_expr: &FatExpr<'p>,
    ) -> Res<'p, Value> {
        let func = self.interp.program.funcs[f.0].clone();
        let arg_ty = match &func.ty {
            &LazyFnType::Finished(arg, _) => arg,
            LazyFnType::Pending { arg, ret: _ } => match arg {
                LazyType::Infer => todo!(),
                LazyType::PendingEval(arg) => {
                    let arg =
                        self.cached_eval_expr(&result.constants, arg.clone(), TypeId::ty())?;
                    self.to_type(arg)?
                }
                &LazyType::Finished(arg) => arg,
            },
        };
        let arg_value = self.cached_eval_expr(&result.constants, arg_expr.clone(), arg_ty)?;
        let arg_ty = self.interp.program.type_of(&arg_value);
        let ret = match &func.ty {
            &LazyFnType::Finished(arg, ret) => {
                self.type_check_arg(arg_ty, arg, "bad comtime arg")?;
                ret
            }
            LazyFnType::Pending { arg, ret } => {
                match arg {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(arg) => {
                        let arg =
                            self.cached_eval_expr(&result.constants, arg.clone(), TypeId::ty())?;
                        let arg = self.to_type(arg)?;
                        self.type_check_arg(arg_ty, arg, "bad comtime arg")?;
                    }
                    &LazyType::Finished(arg) => {
                        self.type_check_arg(arg_ty, arg, "bad comtime arg")?;
                    }
                }
                match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(ret) => {
                        // Bind the arg into my own result so the ret calculation can use it.
                        // TODO: factor out from normal functions?
                        let arguments = self.interp.program.funcs[f.0].arg_vars.as_ref().unwrap();
                        if arguments.len() == 1 {
                            let prev = result.constants.insert(arguments[0], (arg_value, arg_ty));
                            // TODO: remove at the end so can do again.
                            assert!(prev.is_none(), "overwrite comptime arg?");
                        } else {
                            let types = self.interp.program.tuple_types(arg_ty).unwrap();
                            if arguments.len() == types.len() {
                                let arg_values = to_flat_seq(arg_value);
                                // if they match, each element has its own name.
                                for (i, var) in arguments.iter().enumerate() {
                                    let prev = result
                                        .constants
                                        .insert(*var, (arg_values[i].clone(), types[i]));
                                    assert!(prev.is_none(), "overwrite arg?");
                                }
                            } else {
                                todo!()
                            }
                        }

                        let ret =
                            self.cached_eval_expr(&result.constants, ret.clone(), TypeId::ty())?;
                        self.to_type(ret)?
                    }
                    &LazyType::Finished(ret) => ret,
                }
            }
        };

        let result = self.cached_eval_expr(&result.constants, func.body.unwrap(), ret)?;
        let ty = self.interp.program.type_of(&result);
        self.type_check_arg(ty, ret, "generic result")?;

        Ok(result)
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
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
                let expected_ty = invert(
                    ty.clone()
                        .map(|ty| self.cached_eval_expr(&result.constants, ty, TypeId::ty())),
                )?
                .map(|ty| self.to_type(ty).unwrap());
                match kind {
                    VarType::Const => {
                        let mut value = match value {
                            Some(value) => self.cached_eval_expr(
                                &result.constants,
                                value.clone(),
                                expected_ty.unwrap_or(TypeId::any()),
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

                        if let Some(expected_ty) = expected_ty {
                            if self.interp.program.types[expected_ty.0] == TypeInfo::Type {
                                // HACK. todo: general overloads for cast()
                                value = Value::Type(self.to_type(value)?)
                            }
                            let found_ty = self.interp.program.type_of(&value);
                            self.type_check_eq(found_ty, expected_ty, "var decl")?;
                        }
                        let found_ty = self.interp.program.type_of(&value);
                        result.constants.insert(*name, (value, found_ty));
                    }
                    VarType::Let | VarType::Var => {
                        let value = invert(
                            value
                                .as_ref()
                                .map(|expr| self.compile_expr(result, expr, expected_ty)),
                        )?;
                        self.last_loc = Some(stmt.loc);
                        let value = match (value, expected_ty) {
                            (None, Some(expected_ty)) => (
                                result.reserve_slots(self.interp.program, expected_ty),
                                expected_ty,
                            ),
                            (Some(value), None) => value,
                            (Some((value, ty)), Some(expected_ty)) => {
                                self.type_check_arg(ty, expected_ty, "var decl")?;
                                (value, ty)
                            }

                            (None, None) => {
                                err!("{:?} decl with unknown type and no init", name)
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
                self.interp.program.add_func(func.clone());
            }
        }
        Ok(())
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        // You must self.infer_types(f); before calling this
        let func = &self.interp.program.funcs[f.0];
        let (_, ret) = func.ty.unwrap();
        self.interp.program.slot_count(ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(
        &mut self,
        result: &mut FnBody<'p>,
        expr: &FatExpr<'p>,
        requested: Option<TypeId>,
    ) -> Res<'p, (StackRange, TypeId)> {
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        Ok(match expr.deref() {
            Expr::Closure(func) => {
                let id = self.interp.program.add_func(*func.clone());
                self.ensure_compiled(&result.constants, id, result.when)?;
                result.load_constant(self.interp.program, Value::GetFn(id))
            }
            Expr::Call(f, arg) => {
                if let Expr::GetNamed(i) = f.as_ref().deref() {
                    if let Some(f) = self.resolve_function(result, f) {
                        let func = &self.interp.program.funcs[f.0];
                        let is_comptime = func.has_tag(self.pool, "comptime");
                        if is_comptime {
                            let ret = self.emit_comptime_call(result, f, arg)?;
                            return Ok(result.load_constant(self.interp.program, ret));
                        }

                        let (mut arg, arg_ty_found) = self.compile_expr(result, arg, None)?;
                        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
                        if arg.count == 0 {
                            arg = result.load_constant(self.interp.program, Value::Unit).0;
                        }
                        // Note: f will be compiled differently depending on the calling convention.
                        // But, we do need to know how many values it returns. If there's no type annotation, this might end up compiling the function to figure it out.
                        self.infer_types(&result.constants, f)?;
                        let func = &self.interp.program.funcs[f.0];
                        let (arg_ty_expected, ret_ty_expected) = func.ty.unwrap();
                        self.last_loc = Some(expr.loc); // TODO: have a stack so i dont have to keep doing this.
                        self.type_check_arg(arg_ty_found, arg_ty_expected, "bad arg")?;
                        debug_assert_eq!(
                            self.interp.program.slot_count(arg_ty_found),
                            self.interp.program.slot_count(arg_ty_expected)
                        );
                        // TODO: some huristic based on how many times called and how big the body is.
                        // TODO: pre-intern all these constants so its not a hash lookup everytime
                        let force_inline = func.has_tag(self.pool, "inline");
                        let deny_inline = func.has_tag(self.pool, "noinline");
                        assert!(
                            !(force_inline && deny_inline),
                            "{f:?} is both @inline and @noinline"
                        );
                        let will_inline = force_inline;
                        let func = &self.interp.program.funcs[f.0];
                        let ret = if !func.capture_vars.is_empty() {
                            // TODO: check that you're calling from the same place as the definition.
                            assert!(!deny_inline, "capturing calls are always inlined.");
                            self.emit_capturing_call(result, arg, f)?
                        } else if will_inline {
                            self.emit_inline_call(result, arg, f)?
                        } else {
                            let ret = result.reserve_slots(self.interp.program, ret_ty_expected);
                            self.ensure_compiled(&result.constants, f, result.when)?;
                            result.push(Bc::CallDirect { f, ret, arg });
                            ret
                        };

                        assert_eq!(self.return_stack_slots(f), ret.count);
                        assert_eq!(self.interp.program.slot_count(ret_ty_expected), ret.count);
                        return Ok((ret, ret_ty_expected));
                    } else if "if" == self.pool.get(*i) {
                        // TODO: treat this as a normal builtin but need to support general closures?
                        return self.emit_call_if(result, arg);
                    }
                    // else: fallthrough
                }

                let (arg, arg_ty_found) = self.compile_expr(result, arg, None)?;
                let (f_slot, func_ty) = self.compile_expr(result, f, None)?;
                logln!(
                    "dynamic {} is {}",
                    f.log(self.pool),
                    self.interp.program.log_type(func_ty)
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
                let ret = result.reserve_slots(self.interp.program, ret_ty);
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
                    .iter()
                    .map(|v| self.compile_expr(result, v, None))
                    .collect();
                let values = values?;
                let types: Vec<_> = values.iter().map(|(_, ty)| *ty).collect();
                let ty = self.interp.program.intern_type(TypeInfo::Tuple(types));
                result.produce_tuple(self.interp.program, values, ty)?
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(var) => {
                if let Some((from, ty)) = result.vars.get(var).cloned() {
                    let to = result.reserve_slots(self.interp.program, ty);
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
                } else if let Some((value, _)) = result.constants.get(var) {
                    result.load_constant(self.interp.program, value)
                } else {
                    println!("VARS: {:?}", result.vars);
                    println!("GLOBALS: {:?}", result.constants);
                    ice!(
                        "Missing resolved variable {:?} '{}' at {:?}",
                        var,
                        self.pool.get(var.0),
                        expr.loc
                    )
                }
            }
            Expr::GetNamed(i) => {
                if let Some(func) = self.interp.program.declarations.get(i) {
                    assert_eq!(func.len(), 1, "ambigous function reference");
                    let func = func[0];
                    self.ensure_compiled(&result.constants, func, ExecTime::Comptime)?;
                    result.load_constant(self.interp.program, Value::GetFn(func))
                } else {
                    ice!(
                        "Scope resolution failed {} (in Expr::GetNamed)",
                        expr.log(self.pool)
                    );
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::Value(value) => result.load_constant(self.interp.program, value.clone()),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    "addr" => self.addr_macro(result, arg)?,
                    "deref" => {
                        let (from, ptr_ty) = self.compile_expr(result, arg, requested)?;
                        let ty = unwrap!(self.interp.program.unptr_ty(ptr_ty), "not ptr");
                        let to = result.reserve_slots(self.interp.program, ty);
                        result.push(Bc::Load {
                            from: from.single(),
                            to,
                        });
                        (to, ty)
                    }
                    "type" => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects.
                        let ty = self.type_of(result, arg)?;
                        result.load_constant(self.interp.program, Value::Type(ty))
                    }
                    "assert_compile_error" => {
                        // TODO: this can still have side-effects on the vm state tho :(
                        let state = self.mark_state(result.clone());
                        let res = self.compile_expr(result, arg, None);
                        assert!(res.is_err());
                        mem::forget(res); // TODO: dont do this. but for now i like having my drop impl that prints it incase i forget  ot unwrap
                        *result = self.restore_state(state);
                        result.load_constant(self.interp.program, Value::Unit)
                    }
                    "comptime_print" => {
                        outln!("EXPR : {}", arg.log(self.pool));
                        let value =
                            self.cached_eval_expr(&result.constants, *arg.clone(), TypeId::any());
                        outln!("VALUE: {:?}", value);
                        result.load_constant(self.interp.program, Value::Unit)
                    }
                    "struct" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref().deref() {
                            let ty = self.struct_type(result, pattern)?;
                            let ty = Value::Type(self.interp.program.intern_type(ty));
                            result.load_constant(self.interp.program, ty)
                        } else {
                            err!(
                                "expected map literal: .{{ name: Type, ... }} but found {:?}",
                                arg
                            );
                        }
                    }
                    "enum" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref().deref() {
                            let ty = self.struct_type(result, pattern)?;
                            let ty = Value::Type(self.interp.program.to_enum(ty));
                            result.load_constant(self.interp.program, ty)
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
                        let ret = result.reserve_slots(self.interp.program, ty);
                        result.push(Bc::SlicePtr {
                            base: addr.single(),
                            offset: 0,
                            count: 1,
                            ret: ret.single(),
                        });
                        (ret, ty)
                    }
                    "symbol" => {
                        if let Expr::GetNamed(i) = arg.deref().deref() {
                            result.load_constant(self.interp.program, Value::Symbol(i.0))
                        } else {
                            ice!("Expected identifier found {arg:?}")
                        }
                    }
                    _ => err!(CErr::UndeclaredIdent(*macro_name)),
                }
            }
            Expr::FieldAccess(e, name) => {
                let (mut container_ptr, mut container_ptr_ty) = self.addr_macro(result, e)?;
                // Auto deref for nested place expressions.
                // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
                let depth = self.interp.program.ptr_depth(container_ptr_ty);
                if depth > 1 {
                    for _ in 0..(depth - 1) {
                        container_ptr_ty =
                            unwrap!(self.interp.program.unptr_ty(container_ptr_ty), "");
                        let ret = result.reserve_slots(self.interp.program, container_ptr_ty);
                        result.push(Bc::Load {
                            from: container_ptr.single(),
                            to: ret,
                        });
                        container_ptr = ret;
                    }
                }
                let container_ty = unwrap!(
                    self.interp.program.unptr_ty(container_ptr_ty),
                    "unreachable"
                );
                if let TypeInfo::Struct { fields, .. } = &self.interp.program.types[container_ty.0]
                {
                    for f in fields {
                        if f.name == *name {
                            let f = *f;
                            let ty = self.interp.program.ptr_type(f.ty);
                            let ret = result.reserve_slots(self.interp.program, ty);

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
                        self.pool.get(*name),
                        self.interp.program.log_type(container_ty)
                    );
                }
                if let TypeInfo::Enum { cases, .. } = &self.interp.program.types[container_ty.0] {
                    for (i, (f_name, f_ty)) in cases.iter().enumerate() {
                        if f_name == name {
                            let f_ty = *f_ty;
                            let ty = self.interp.program.ptr_type(f_ty);
                            let ret = result.reserve_slots(self.interp.program, ty);
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
                        self.pool.get(*name),
                        self.interp.program.log_type(container_ty)
                    );
                } else {
                    err!(
                        "only structs support field access but found {:?} which is {}",
                        e.log(self.pool),
                        self.interp.program.log_type(container_ty)
                    );
                }
            }
            Expr::StructLiteralP(pattern) => {
                assert!(requested.is_some());
                let names: Vec<_> = pattern.names.iter().map(|n| n.unwrap()).collect();
                // TODO: why must this suck so bad
                let values: Option<_> = pattern.types.iter().cloned().collect();
                let values: Vec<FatExpr<'_>> = values.unwrap();
                assert_eq!(names.len(), values.len());
                let container_ty = requested.unwrap();
                if let TypeInfo::Struct {
                    fields, as_tuple, ..
                } = self.interp.program.types[container_ty.0].clone()
                {
                    assert_eq!(fields.len(), values.len());
                    let all = names.into_iter().zip(values).zip(fields);
                    let mut values = vec![];
                    for ((name, value), field) in all {
                        assert_eq!(name, field.name);
                        let (value, found_ty) =
                            self.compile_expr(result, &value, Some(field.ty))?;
                        self.type_check_arg(found_ty, field.ty, "struct field")?;
                        values.push((value, field.ty));
                    }

                    let (ret, _) = result.produce_tuple(self.interp.program, values, as_tuple)?;
                    (ret, container_ty)
                } else if let TypeInfo::Enum { cases, size } =
                    self.interp.program.types[requested.unwrap().0].clone()
                {
                    assert_eq!(1, values.len());
                    let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                    let type_hint = cases[i].1;
                    let (value, found_ty) =
                        self.compile_expr(result, &values[0], Some(type_hint))?;
                    self.type_check_arg(found_ty, type_hint, "enum case")?;
                    if value.count >= size {
                        ice!("Enum value won't fit.")
                    }
                    let ret = result.reserve_slots(self.interp.program, container_ty);
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

                    (ret, container_ty)
                } else {
                    err!("struct literal but expected {:?}", requested);
                }
            }
            &Expr::String(i) => {
                let bytes = self.pool.get(i);
                let bytes = bytes
                    .as_bytes()
                    .iter()
                    .map(|b| Value::I64(*b as i64))
                    .collect();
                result.load_constant(self.interp.program, Value::new_box(bytes))
            }
        })
    }

    fn addr_macro(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        match arg.deref().deref() {
            Expr::GetVar(var) => {
                if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                    let ptr_ty = self.interp.program.ptr_type(value_ty);
                    let addr_slot = result.reserve_slots(self.interp.program, ptr_ty);
                    result.push(Bc::AbsoluteStackAddr {
                        of: stack_slot,
                        to: addr_slot.first,
                    });
                    Ok((addr_slot, ptr_ty))
                } else if result.constants.get(var).is_some() {
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
            &Expr::GetNamed(i) => err!(CErr::UndeclaredIdent(i)),
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

    fn resolve_function(&self, _result: &FnBody<'p>, expr: &FatExpr<'p>) -> Option<FuncId> {
        match expr.deref() {
            Expr::GetNamed(i) => self.lookup_unique_func(*i),
            _ => None,
        }
    }

    fn type_of(&mut self, result: &FnBody<'p>, expr: &FatExpr<'p>) -> Res<'p, TypeId> {
        if let Some(ty) = expr.ty {
            return Ok(ty);
        }
        Ok(match expr.deref() {
            Expr::Value(v) => self.interp.program.type_of(v),
            Expr::Call(f, _) => {
                let fid = if let Some(f) = self.resolve_function(result, f) {
                    f
                } else {
                    ice!("typecheck failed to resolve function expr {f:?}")
                };
                self.ensure_compiled(&result.constants, fid, ExecTime::Comptime)?;
                let (_, ret) = self.interp.program.funcs[fid.0].ty.unwrap();
                ret
            }
            Expr::Block { result: e, .. } => self.type_of(result, e)?,
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::RefType(_) => todo!(),
            Expr::EnumLiteral(_) => todo!(),
            Expr::Closure(_) => todo!(),
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
                        unwrap!(self.interp.program.unptr_ty(ptr_ty), "")
                    }
                    _ => err!(CErr::UndeclaredIdent(*macro_name)),
                }
            }
            Expr::GetVar(var) => {
                if let Some((_, ty)) = result.vars.get(var).cloned() {
                    ty
                } else if let Some((_, ty)) = result.constants.get(var) {
                    ty
                } else {
                    ice!("type check missing var {var:?}")
                }
            }
            Expr::GetNamed(_) => todo!(),
            Expr::FieldAccess(_, _) => todo!(),
            Expr::StructLiteralP(_) => todo!(),
            Expr::String(_) => self
                .interp
                .program
                .intern_type(TypeInfo::Ptr(TypeId::i64())),
        })
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

        Some(match name {
            "true" => (
                Value::Bool(true),
                self.interp.program.intern_type(TypeInfo::Bool),
            ),
            "false" => (
                Value::Bool(false),
                self.interp.program.intern_type(TypeInfo::Bool),
            ),

            _ => return None,
        })
    }

    // Resolve the lazy types for Arg and Ret
    fn infer_types(&mut self, constants: &SharedConstants<'p>, func: FuncId) -> Res<'p, FnType> {
        let f = &self.interp.program.funcs[func.0];
        self.last_loc = Some(f.loc);
        match f.ty.clone() {
            LazyFnType::Pending { arg, ret } => {
                let state = DebugState::ResolveFnType(func, arg.clone(), ret.clone());
                self.push_state(&state);
                logln!("RESOLVE: Arg of {func:?}");
                let arg = match arg {
                    LazyType::Infer => TypeId::any(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(constants, e.clone(), TypeId::ty())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                // TODO: deal with comptime args that are allowed to be used in return type
                // TODO: copy-n-paste
                logln!("RESOLVE: Ret of {func:?}");
                let ret = match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(constants, e.clone(), TypeId::ty())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                self.interp.program.funcs[func.0].ty = LazyFnType::Finished(arg, ret);
                self.pop_state(state);
            }
            LazyFnType::Finished(_, _) => {} // easy
        }
        // TODO: go find everywhere that calls this and then immediatly unwraps.
        let (arg, ret) = self.interp.program.funcs[func.0].ty.unwrap();
        Ok(FnType { arg, ret })
    }

    fn unit_to(&mut self, ret: TypeId) -> LazyFnType<'p> {
        LazyFnType::Finished(self.interp.program.intern_type(TypeInfo::Unit), ret)
    }

    fn cached_eval_expr(
        &mut self,
        constants: &SharedConstants<'p>,
        e: FatExpr<'p>,
        ret_ty: TypeId,
    ) -> Res<'p, Value> {
        match e.deref() {
            Expr::Value(value) => return Ok(value.clone()),
            Expr::GetVar(var) => {
                // fast path for builtin type identifiers
                if let Some((value, _)) = constants.get(var) {
                    debug_assert_ne!(value, Value::Poison);
                    return Ok(value);
                }
                // fallthrough
            }
            Expr::Call(_, _) => {
                // TODO: fast path for checking the generics cache
            }
            _ => {} // fallthrough
        }
        let state = DebugState::ComputeCached(e.clone());
        self.push_state(&state);
        let name = format!("@eval_{}@", self.anon_fn_counter);
        let fake_func: Func<'p> = Func {
            name: Some(self.pool.intern(&name)),
            ty: self.unit_to(ret_ty),
            body: Some(e.clone()),
            arg_names: vec![],
            annotations: vec![],
            arg_vars: Some(vec![]),
            capture_vars: vec![],
            local_constants: Default::default(),
            loc: e.loc,
            arg_loc: vec![],
        };
        self.anon_fn_counter += 1;
        let func_id = self.interp.program.add_func(fake_func);
        logln!(
            "Made anon: {func_id:?} = {}",
            self.interp.program.funcs[func_id.0].log(self.pool)
        );
        let result =
            self.compile_and_run(Some(constants), func_id, Value::Unit, ExecTime::Comptime)?;
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
            // TODO: fix horible special case. This causes infinite pain for tuple types because you need to put them in a const with a type annotation,
            //       or they're passed as multiple arguments.
            let ty = match values.len() {
                0 => TypeId::unit(),
                1 => self.to_type(values.into_iter().next().unwrap())?,
                _ => {
                    // println!("Type Tuple: {:?}", values);
                    let values: Res<'_, Vec<_>> =
                        values.into_iter().map(|v| self.to_type(v)).collect();
                    self.interp.program.intern_type(TypeInfo::Tuple(values?))
                }
            };
            Ok(ty)
        } else if let Value::Unit = value {
            // This lets you use the literal `()` as a type (i dont parse it as a tuple because reasons). TODO: check if that still true with new parser
            Ok(self.interp.program.intern_type(TypeInfo::Unit))
        } else {
            err!(CErr::TypeError("Type", value))
        }
    }

    // TODO: make this not a special case.
    fn emit_call_if(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        let ((cond, _), if_true, if_false) = if let Expr::Tuple(parts) = arg.deref() {
            let cond = self.compile_expr(result, &parts[0], None)?;
            let if_true = if let Expr::Closure(func) = parts[1].deref() {
                self.interp.program.add_func(*func.clone())
            } else {
                ice!("if args must be tuple");
            };
            let if_false = if let Expr::Closure(func) = parts[2].deref() {
                self.interp.program.add_func(*func.clone())
            } else {
                ice!("if args must be tuple");
            };
            (cond, if_true, if_false)
        } else {
            ice!("if args must be tuple");
        };

        let true_ty = self.infer_types(&result.constants, if_true)?;
        let unit = self.interp.program.intern_type(TypeInfo::Unit);
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        self.type_check_eq(true_ty.arg, unit, sig)?;
        let false_ty = self.infer_types(&result.constants, if_false)?;
        self.type_check_eq(false_ty.arg, unit, sig)?;
        self.type_check_eq(true_ty.ret, false_ty.ret, sig)?;

        // TODO: if returning tuples
        let ret = result.reserve_slots(self.interp.program, true_ty.ret);

        let arg = result.load_constant(self.interp.program, Value::Unit).0; // Note: before you start doing ip stuff!
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
        pattern: &crate::ast::Pattern<'p>,
    ) -> Res<'p, TypeInfo<'p>> {
        let names: Vec<_> = pattern.names.iter().map(|n| n.unwrap()).collect();
        // TODO: why must this suck so bad
        let types: Res<'p, Vec<_>> = pattern
            .types
            .iter()
            .map(|ty| self.cached_eval_expr(&result.constants, ty.clone().unwrap(), TypeId::ty()))
            .collect();
        let types: Res<'p, Vec<_>> = types?.into_iter().map(|ty| self.to_type(ty)).collect();
        let types = types?;
        let as_tuple = self
            .interp
            .program
            .intern_type(TypeInfo::Tuple(types.clone()));
        let mut fields = vec![];
        let mut size = 0;
        for (name, ty) in names.into_iter().zip(types.into_iter()) {
            let count = self.interp.program.slot_count(ty);
            fields.push(Field {
                name,
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
        place: &FatExpr<'p>,
        value: &FatExpr<'p>,
    ) -> Res<'p, ()> {
        match place.deref().deref() {
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
            _ => ice!("TODO: other `place=e;`"),
        }
    }
}

// TODO
pub enum Place<'p> {
    Var(Var<'p>),
    Ptr(StackRange),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecTime {
    Comptime,
    Runtime,
}

impl<'p> FnBody<'p> {
    fn reserve_slots_raw(&mut self, program: &Program<'p>, count: usize, ty: TypeId) -> StackRange {
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
            let types = program.tuple_types(ty).unwrap();
            let mut found = 0;
            for ty in types {
                found += self.reserve_slots(program, *ty).count;
            }
            debug_assert_eq!(found, count, "bad tuple size");
            // Note: don't bump self.stack_slots here.
        }
        StackRange { first, count }
    }

    fn reserve_slots(&mut self, program: &Program<'p>, ty: TypeId) -> StackRange {
        if let TypeInfo::Enum { size, .. } = &program.types[ty.0] {
            let size = *size;
            let first = StackOffset(self.stack_slots);
            self.slot_types.push(TypeId::i64());
            self.stack_slots += size;
            StackRange { first, count: size }
        } else {
            let count = program.slot_count(ty);
            self.reserve_slots_raw(program, count, ty)
        }
    }

    fn load_constant(&mut self, program: &mut Program<'p>, value: Value) -> (StackRange, TypeId) {
        let ty = program.type_of(&value);
        let to = self.reserve_slots(program, ty);
        self.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        (to, ty)
    }

    fn _serialize_constant<T: InterpSend<'p>>(
        &mut self,
        program: &mut Program<'p>,
        value: T,
    ) -> (StackRange, TypeId) {
        let ty = T::get_type(program);
        let value = value.serialize();
        let to = self.reserve_slots(program, ty);
        self.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        (to, ty)
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
            let ret = self.reserve_slots(program, tuple_ty);
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
            Bc::CallDynamic { f, ret, arg } => {
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
