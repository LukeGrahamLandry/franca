//! Converts ASTs into... simpler asts.
//! Type checking, overload resolution, implicit function calls, inlining, monomorphization, etc.
//! Uses the interpreter for comptime evalutation (build scripts, generics, macros, etc).

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use codemap_diagnostic::Diagnostic;
use interp_derive::InterpSend;
use std::collections::HashMap;
use std::fmt::Write;
use std::hash::Hash;
use std::mem::{self, transmute};
use std::ops::DerefMut;
use std::sync::atomic::{AtomicIsize, Ordering};
use std::{ops::Deref, panic::Location};

use crate::ast::{
    garbage_loc, Annotation, Binding, FatStmt, Field, Flag, IntTypeInfo, Name, OverloadSet, Pattern, ScopeId, TargetArch, Var, VarType, WalkAst,
};

use crate::bc_to_asm::{emit_aarch64, Jitted};
use crate::emit_bc::emit_bc;
use crate::export_ffi::{__clear_cache, do_flat_call, do_flat_call_values};
use crate::ffi::InterpSend;
use crate::parse::ParseTasks;
use crate::scope::ResolveScope;
use crate::{
    ast::{Expr, FatExpr, FnType, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};
use crate::{bc::*, extend_options, ffi, impl_index, STATS};
use crate::{
    logging::{LogTag, PoolLog},
    outln,
};

use crate::logging::LogTag::ShowErr;
use crate::{assert, assert_eq, err, ice, logln, unwrap};

#[derive(Clone)]
pub struct CompileError<'p> {
    pub internal_loc: Option<&'static Location<'static>>,
    pub loc: Option<Span>,
    pub reason: CErr<'p>,
    pub trace: String,
    pub value_stack: Vec<Value>,
    pub call_stack: String,
}

#[derive(Clone, Debug)]
pub enum CErr<'p> {
    UndeclaredIdent(Ident<'p>),
    TypeError(&'static str, Values),
    TypeCheck(TypeId, TypeId, &'static str),
    AmbiguousCall,
    Diagnostic(Vec<Diagnostic>),
    Fatal(String),
}

pub type Res<'p, T> = Result<T, CompileError<'p>>;

#[repr(C)]
pub struct Compile<'a, 'p> {
    pub program: &'a mut Program<'p>, // SAFETY: this must be the first field (repr(C))
    pub pool: &'p StringPool<'p>,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    pub debug_trace: Vec<DebugState<'p>>,
    pub anon_fn_counter: usize,
    currently_inlining: Vec<FuncId>,
    currently_compiling: Vec<FuncId>, // TODO: use this to make recursion work
    pub last_loc: Option<Span>,
    pub save_bootstrap: Vec<FuncId>,
    pub tests: Vec<FuncId>,
    pub aarch64: Jitted,
    pub ready: BcReady<'p>,
    pub pending_ffi: Vec<Option<*mut FnWip<'p>>>,
    pub scopes: Vec<Scope<'p>>,
    pub parsing: ParseTasks<'p>,
}

pub struct Scope<'p> {
    pub parent: ScopeId,
    pub constants: HashMap<Var<'p>, (FatExpr<'p>, LazyType<'p>)>,
    pub vars: Vec<BlockScope<'p>>,
    pub depth: usize,
    pub funcs: Vec<FuncId>,
    pub name: Ident<'p>,
    pub block_in_parent: usize,
}

pub struct BlockScope<'p> {
    pub vars: Vec<Var<'p>>,
    pub parent: usize,
}

impl_index!(Compile<'_, 'p>, ScopeId, Scope<'p>, scopes);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DebugState<'p> {
    Compile(FuncId, Ident<'p>),
    EnsureCompiled(FuncId, Ident<'p>, ExecTime),
    RunInstLoop(FuncId, Ident<'p>),
    ComptimeCall(FuncId, Ident<'p>),
    ResolveFnType(FuncId, Ident<'p>),
    EvalConstants(FuncId, Ident<'p>),
    Msg(String),
    EmitBody(FuncId, Ident<'p>),
    EmitCapturingCall(FuncId, Ident<'p>),
    ResolveFnRef(Var<'p>),
    TypeOf,
}
#[repr(C)]
#[derive(Clone, Debug, InterpSend)]
pub struct FnWip<'p> {
    pub stack_slots: usize,
    pub vars: HashMap<Var<'p>, TypeId>, // TODO: use a vec
    pub when: ExecTime,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub callees: Vec<(FuncId, ExecTime)>,
}

pub static mut EXPECT_ERR_DEPTH: AtomicIsize = AtomicIsize::new(0);

impl<'a, 'p> Compile<'a, 'p> {
    pub fn new(pool: &'p StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        let parsing = ParseTasks::new(pool);
        let mut c = Self {
            pool,
            debug_trace: vec![],
            anon_fn_counter: 0,
            currently_inlining: vec![],
            last_loc: None,
            currently_compiling: vec![],
            program,
            aarch64: Jitted::new(1 << 26), // Its just virtual memory right? I really don't want to ever run out of space and need to change the address.
            ready: BcReady::default(),
            save_bootstrap: vec![],
            tests: vec![],
            pending_ffi: vec![],
            scopes: vec![],
            parsing,
        };
        c.new_scope(ScopeId::from_index(0), Flag::TopLevel.ident(), 0);
        c
    }
}
impl<'a, 'p> Compile<'a, 'p> {
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

    pub fn new_scope(&mut self, parent: ScopeId, name: Ident<'p>, block_in_parent: usize) -> ScopeId {
        let depth = if self.scopes.is_empty() { 0 } else { self[parent].depth + 1 }; // HACK
        self.scopes.push(Scope {
            parent,
            constants: Default::default(),
            vars: Default::default(),
            depth,
            funcs: vec![],
            name,
            block_in_parent,
        });
        ScopeId::from_index(self.scopes.len() - 1)
    }

    #[track_caller]
    pub(crate) fn push_state(&mut self, s: &DebugState<'p>) {
        self.debug_trace.push(s.clone());
    }

    #[track_caller]
    pub(crate) fn pop_state(&mut self, s: DebugState<'p>) {
        let found = self.debug_trace.pop().expect("state stack");
        debug_assert_eq!(found, s, "{}", self.log_trace()); // TODO: fix the way i deal with errors. i dont always short circuit so this doesnt work
    }

    pub fn compile_top_level(&mut self, ast: Func<'p>) -> Res<'p, FuncId> {
        let f = self.add_func(ast)?;

        if let Err(mut e) = self.ensure_compiled(f, ExecTime::Comptime) {
            e.loc = e.loc.or(self.last_loc);
            return Err(e);
        }
        Ok(f)
    }

    pub fn compile(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        if self.currently_compiling.contains(&f) {
            return Ok(());
        }
        let state = DebugState::Compile(f, self.program[f].name);
        self.push_state(&state);
        let before = self.debug_trace.len();
        debug_assert!(!self.program[f].evil_uninit);
        let mut result = self.ensure_compiled(f, when);
        if result.is_ok() {
            let func = &self.program[f];
            if let Some(wip) = func.wip.as_ref() {
                let callees = wip.callees.clone();
                for (id, when) in callees {
                    if id == f {
                        continue;
                    }
                    let res = self.compile(id, when);
                    self.tag_err(res)?;
                }
                result = emit_bc(self, f);
            } else {
                // TODO
                // result = self.compile(func.any_reg_template.unwrap(), ExecTime::Comptime);
            }
        }

        let result = self.tag_err(result);
        let after = self.debug_trace.len();
        if result.is_ok() {
            debug_assert_eq!(before, after);
            self.pop_state(state);
            // self.currently_compiling.retain(|check| *check != f);
        }
        result
    }

    pub fn run(&mut self, f: FuncId, arg: Values, when: ExecTime, result: Option<*mut FnWip<'p>>) -> Res<'p, Values> {
        let state2 = DebugState::RunInstLoop(f, self.program[f].name);
        self.push_state(&state2);
        self.pending_ffi.push(result);
        let arch = match when {
            ExecTime::Comptime => self.program.comptime_arch,
            ExecTime::Runtime => self.program.runtime_arch,
            ExecTime::Both => todo!(),
        };
        let result = match arch {
            TargetArch::Aarch64 => {
                emit_aarch64(self, f, when)?;
                let addr = if let Some(addr) = self.program[f].comptime_addr {
                    // we might be doing ffi at comptime, thats fine
                    addr as *const u8
                } else {
                    unwrap!(self.aarch64.get_fn(f), "not compiled {f:?}").as_ptr()
                };

                let ty = self.program[f].unwrap_ty();
                let comp_ctx = self.program[f].has_tag(Flag::Ct);
                let c_call = self.program[f].has_tag(Flag::C_Call);
                let flat_call = self.program[f].has_tag(Flag::Flat_Call);

                // symptom if you forget: bus error
                self.aarch64.make_exec();
                self.flush_cpu_instruction_cache();
                if flat_call {
                    assert!(comp_ctx && !c_call);
                    do_flat_call_values(self, unsafe { transmute(addr) }, arg, ty.ret)
                } else if c_call {
                    assert!(!flat_call);
                    assert!(addr as usize % 4 == 0);
                    let r = ffi::c::call(self, addr as usize, ty, arg, comp_ctx)?;
                    let mut out = vec![];
                    values_from_ints(self, ty.ret, &mut [r].into_iter(), &mut out)?;

                    Ok(out.into())
                } else {
                    todo!()
                }
            }
            TargetArch::Llvm => todo!(),
        };

        self.pending_ffi.pop().unwrap();
        let result = self.tag_err(result);
        if result.is_ok() {
            self.pop_state(state2);
        }
        result
    }

    // very similar to the above but nicer api without going though Values for flat_call.
    pub fn call_jitted<Arg: InterpSend<'p>, Ret: InterpSend<'p>>(
        &mut self,
        f: FuncId,
        when: ExecTime,
        result: Option<*mut FnWip<'p>>,
        arg: Arg,
    ) -> Res<'p, Ret> {
        let state = DebugState::RunInstLoop(f, self.program[f].name);
        self.push_state(&state);
        self.compile(f, when)?;
        emit_aarch64(self, f, when)?;
        self.pending_ffi.push(result);
        let addr = if let Some(addr) = self.program[f].comptime_addr {
            // it might be a builtin macro that's part of the compiler but is resolved like normal for consistancy (like @enum).
            addr as *const u8
        } else {
            unwrap!(self.aarch64.get_fn(f), "not compiled {f:?}").as_ptr()
        };

        let ty = self.program[f].unwrap_ty();
        let comp_ctx = self.program[f].has_tag(Flag::Ct);
        let c_call = self.program[f].has_tag(Flag::C_Call);
        let flat_call = self.program[f].has_tag(Flag::Flat_Call);

        let arg_ty = Arg::get_type(self.program);
        self.type_check_arg(arg_ty, ty.arg, "sanity ICE")?;
        let ret_ty = Ret::get_type(self.program);
        self.type_check_arg(ret_ty, ty.ret, "sanity ICE")?;

        // symptom if you forget: bus error
        self.aarch64.make_exec();
        self.flush_cpu_instruction_cache();
        let res = if flat_call {
            assert!(comp_ctx && !c_call);
            Ok(do_flat_call(self, unsafe { transmute(addr) }, arg))
        } else if c_call {
            assert!(!flat_call);
            assert!(addr as usize % 4 == 0);
            let arg = arg.serialize_one();
            let r = ffi::c::call(self, addr as usize, ty, arg, comp_ctx)?;
            let r = Ret::deserialize_from_ints(&mut [r].into_iter());
            Ok(unwrap!(r, ""))
        } else {
            todo!()
        };
        self.pending_ffi.pop().unwrap(); // TODO: should do this before short cirucuiting errpr
        if res.is_ok() {
            self.pop_state(state);
        }

        res
    }

    // This is much less painful than threading it through the macros
    fn tag_err<T>(&self, mut res: Res<'p, T>) -> Res<'p, T> {
        if let Err(err) = &mut res {
            err.trace = self.log_trace();
            err.loc = self.last_loc;
        }
        res
    }

    #[track_caller]
    fn empty_fn(&mut self, when: ExecTime, func: FuncId, loc: Span) -> FnWip<'p> {
        FnWip {
            stack_slots: 0,
            vars: Default::default(),
            when,
            func,
            why: self.log_trace(),
            last_loc: loc,
            callees: vec![],
        }
    }

    // Any environment constants must already be in the function.
    // It's fine to call this redundantly, the first one just takes the constants out of the list.
    fn eval_and_close_local_constants(&mut self, f: FuncId) -> Res<'p, ()> {
        debug_assert!(!self.program[f].evil_uninit);
        self.ensure_resolved_body(f)?;
        if self.program[f].local_constants.is_empty() {
            // Maybe no consts, or maybe we've already compiled them.
            return Ok(());
        }
        let state = DebugState::EvalConstants(f, self.program[f].name);
        self.push_state(&state);

        let consts = mem::take(&mut self.program[f].local_constants);
        for name in consts {
            let (val, ty) = mem::take(self[name.2].constants.get_mut(&name).unwrap());
            debug_assert!(!matches!(ty, LazyType::EvilUnit));
            if let Expr::AddToOverloadSet(funcs) = val.expr {
                self[name.2].constants.get_mut(&name).unwrap().1 = LazyType::Infer;
                for func in funcs {
                    self.decl_func(func)?;
                }
            } else if let Some(os) = val.expr.as_overload_set() {
                self[name.2].constants.get_mut(&name).unwrap().0.expr = Expr::Value {
                    ty: TypeId::overload_set(),
                    value: Value::OverloadSet(os).into(),
                };
                self[name.2].constants.get_mut(&name).unwrap().0.ty = TypeId::overload_set();
                self[name.2].constants.get_mut(&name).unwrap().1 = LazyType::Finished(TypeId::overload_set());
                for func in mem::take(&mut self.program.overload_sets[os].just_resolved) {
                    self.decl_func(func)?;
                }
            } else {
                self[name.2].constants.insert(name, (val, ty)); // TODO: dont take
            }
        }

        self.pop_state(state);
        Ok(())
    }

    // Don't pass constants in because the function declaration must have closed over anything it needs.
    fn ensure_compiled(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        let func = &self.program[f];
        if func.wip.is_some() {
            return Ok(());
        }
        if !add_unique(&mut self.currently_compiling, f) {
            // This makes recursion work.
            return Ok(());
        }
        debug_assert!(!func.evil_uninit);
        self.ensure_resolved_body(f)?;
        assert!(self.program[f].capture_vars.is_empty(), "closures need to be specialized");
        assert!(!self.program[f].any_const_args());
        let before = self.debug_trace.len();
        let state = DebugState::EnsureCompiled(f, self.program[f].name, when);
        self.push_state(&state);
        self.eval_and_close_local_constants(f)?;

        self.infer_types(f)?;
        let func = &self.program[f];
        if let Some(template) = func.any_reg_template {
            self.ensure_compiled(template, ExecTime::Comptime)?;
        } else {
            let mut result = self.empty_fn(when, f, func.loc);
            self.emit_body(&mut result, f)?;
            self.program[f].wip = Some(result);
        }
        self.pop_state(state);
        let after = self.debug_trace.len();
        debug_assert_eq!(before, after);

        // TODO: error safety ^
        self.currently_compiling.retain(|check| *check != f);
        Ok(())
    }

    /// IMPORTANT: this pulls a little sneaky on ya, so you can't access the body of the function inside the main emit handlers.
    fn emit_body(&mut self, result: &mut FnWip<'p>, f: FuncId) -> Res<'p, Structured> {
        let state = DebugState::EmitBody(f, self.program[f].name);
        self.push_state(&state);
        let has_body = self.program[f].body.is_some();
        debug_assert!(!self.program[f].evil_uninit);

        self.ensure_resolved_sign(f)?;
        self.ensure_resolved_body(f)?;
        mut_replace!(self.program[f], |func: Func<'p>| {
            debug_assert!(func.local_constants.is_empty());
            assert!(result.when == ExecTime::Comptime || !func.has_tag(Flag::Comptime));
            let arguments = func.arg.flatten();
            for (name, ty, kind) in arguments {
                // TODO: probably want to change this so you can do as much compiling as possible before expanding templates.
                debug_assert!(kind != VarType::Const, "Tried to emit before binding const args.");
                if let Some(name) = name {
                    debug_assert!(kind == name.3);
                    let prev = result.vars.insert(name, ty);
                    assert!(prev.is_none(), "overwrite arg?");
                }
            }
            Ok((func, ()))
        });
        if !has_body {
            let ret = mut_replace!(self.program[f], |mut func: Func<'p>| {
                // Functions without a body are always builtins.
                // It's convient to give them a FuncId so you can put them in a variable,
                if let Some(tag) = func.annotations.iter().find(|c| c.name == Flag::Comptime_Addr.ident()) {
                    let addr = unwrap!(unwrap!(tag.args.as_ref(), "").as_int(), "");
                    func.comptime_addr = Some(addr.to_bytes());
                    let _ = self.program.intern_type(TypeInfo::FnPtr(func.unwrap_ty()));
                    // make sure emit_ can get the type without mutating the Program.
                }
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

        let mut body_expr = self.program[f].body.clone().unwrap(); // TODO: no clone. make errors recoverable. no mut_replace -- Apr 24

        let ret_val = if let Some(arg) = body_expr.as_suffix_macro_mut(Flag::Asm) {
            self.inline_asm_body(f, arg)?;
            let fn_ty = self.program[f].unwrap_ty();
            let ret_ty = fn_ty.ret;
            let _ = self.program.intern_type(TypeInfo::FnPtr(fn_ty)); // make sure emit_ can get the type without mutating the Program.
            self.program[f].body = None;
            Structured::RuntimeOnly(ret_ty)
        } else {
            let hint = self.program[f].finished_ret;
            let res = self.compile_expr(result, &mut body_expr, hint)?;
            if self.program[f].finished_ret.is_none() {
                // If you got to the point of emitting the body, the only situation where you don't know the return type yet is if they didn't put a type anotation there.
                // This isn't true if you have an @generic function that isn't @comptime trying to use its args in its return type.
                // That case used to work becuase I'd always inline after doing const args so you've never even get to the point of compiling the function body on its own.
                // However, that still wasn't the same as @comptime because it didn't egarly evaluate unless already in a const context.
                // I do still want to get rid of @comptime and just use const args but I need to think about the semantics of when you inline more.
                //  -- Apr 6, 2024
                assert!(matches!(self.program[f].ret, LazyType::Infer));

                self.program[f].finished_ret = Some(res.ty());
                self.program[f].ret = LazyType::Finished(res.ty());
            }
            self.program[f].body = Some(body_expr);
            res
        };
        let func = &self.program[f];
        self.type_check_arg(ret_val.ty(), func.finished_ret.unwrap(), "bad return value")?;
        self.pop_state(state);

        Ok(ret_val)
    }

    // This is a normal function call. No comptime args, no runtime captures.
    fn emit_runtime_call(&mut self, result: &mut FnWip<'p>, f: FuncId, arg_expr: &mut FatExpr<'p>) -> Res<'p, Structured> {
        let arg_ty = unwrap!(self.program[f].finished_arg, "fn arg");
        self.compile_expr(result, arg_expr, Some(arg_ty))?;

        // self.last_loc = Some(expr.loc); // TODO: have a stack so i dont have to keep doing this.
        let func = &self.program[f];
        // TODO: some huristic based on how many times called and how big the body is.
        // TODO: pre-intern all these constants so its not a hash lookup everytime
        let force_inline = func.has_tag(Flag::Inline);
        assert!(func.capture_vars.is_empty());
        assert!(!force_inline);
        assert!(!func.any_const_args());
        self.add_callee(result, f, ExecTime::Both);
        self.ensure_compiled(f, result.when)?;
        let ret_ty = unwrap!(self.program[f].finished_ret, "fn ret");
        Ok(Structured::RuntimeOnly(ret_ty))
    }

    // TODO: i think the main reason <callees> is on the result not the func is because I used to use mut_replace for emit_body.  -- Apr 25
    fn add_callee(&mut self, result: &mut FnWip<'p>, f: FuncId, when: ExecTime) {
        // this fixes mutual recursion
        // TODO: now if other backends try to use callees they might not get everything they need.
        //       not failing tests rn because im egarly compiling on this backend and llvm doesn't support everything yet os not all tests run there anyway.  -- Apr 25
        if !self.currently_compiling.contains(&f) {
            add_unique(&mut result.callees, (f, when));
        }
    }

    // Replace a call expr with the body of the target function.
    // The idea is having zero cost (50 cycles) closures :)
    fn emit_capturing_call(&mut self, result: &mut FnWip<'p>, f: FuncId, expr_out: &mut FatExpr<'p>) -> Res<'p, Structured> {
        self.ensure_resolved_body(f)?; // it might not be a closure. it might be an inlined thing.
        let loc = expr_out.loc;
        debug_assert_ne!(f, result.func, "recusive inlining?");
        assert!(!self.program[f].evil_uninit);
        let state = DebugState::EmitCapturingCall(f, self.program[f].name);
        self.push_state(&state);
        let arg_expr = if let Expr::Call(_, arg) = expr_out.deref_mut() { arg } else { ice!("") };

        assert!(!self.currently_inlining.contains(&f), "Tried to inline recursive function.");
        self.currently_inlining.push(f);

        self.eval_and_close_local_constants(f)?;
        let func = &self.program.funcs[f.as_index()];
        assert!(!func.any_const_args());
        for capture in &func.capture_vars {
            assert!(capture.3 != VarType::Const);
            if result.vars.get(capture).is_none() {
                // :ChainedCaptures // TODO
                // now whatever function we're inlining _into_ needs to capture this variable.
                // I think this always happens for things declared in a macro becuase it doesn't recalculate the capture chain, but it works out in the end somehow.
                // but when it happens for a normal variable its a problem?
                // println!("- Missing closure capture {}. result fn: {:?}", capture.log(self.pool), result.func);
            }
        }

        let pattern = func.arg.clone();

        // TODO: can I mem::take func.body? I guess not because you're allowed to call multiple times, but that's sad for the common case of !if/!while.
        // TODO: dont bother if its just unit args (which most are because of !if and !while).
        // TODO: you want to be able to share work (across all the call-sites) compiling parts of the body that don't depend on the captured variables
        // TODO: need to move the const args to the top before eval_and_close_local_constants
        expr_out.expr = Expr::Block {
            resolved: None,
            body: vec![FatStmt {
                stmt: Stmt::DeclVarPattern {
                    binding: pattern,
                    value: Some(mem::take(arg_expr)),
                },
                annotations: vec![],
                loc,
            }],
            result: Box::new(func.body.as_ref().unwrap().clone()),
        };
        self.program.next_var = expr_out.renumber_vars(self.program.next_var); // Note: not renumbering on the function. didn't need to clone it.

        self.currently_inlining.retain(|check| *check != f);

        let hint = func.finished_ret;
        let res = self.compile_expr(result, expr_out, hint)?;
        if hint.is_none() {
            self.program[f].finished_ret = Some(res.ty());
        }
        self.pop_state(state);
        Ok(res)
    }

    // TODO: I should probably implement real runtime closures (even if my version is dumb and slow)
    //       and use those for comptime stuff cause it can't possibly be worse than cloning everything and recompiling.
    //       The cloning is only better for runtime functions where we're trying to output a simpler ast that an optimiser can specialize.
    // Curry a function from fn(a: A, @comptime b: B) to fn(a: A)
    // The argument type is evaluated in the function declaration's scope, the argument value is evaluated in the caller's scope.
    fn bind_const_arg(&mut self, o_f: FuncId, arg_name: Var<'p>, arg: Structured) -> Res<'p, ()> {
        // I don't want to renumber, so make sure to do the clone before resolving.
        // TODO: reslove captured constants anyway so dont haveto do the chain lookup redundantly on each speciailization. -- Apr 24
        debug_assert!(self.program[o_f].resolved_body && self.program[o_f].resolved_sign);

        let mut arg_x = self.program[o_f].arg.clone();
        let arg_ty = self.get_type_for_arg(&mut arg_x, arg_name)?;
        self.program[o_f].arg = arg_x;
        self.type_check_arg(arg.ty(), arg_ty, "bind arg")?;
        let arg_value = arg.get()?;
        outln!(LogTag::Generics, "bind_const_arg of {o_f:?}");

        // TODO: not sure if i actually need this but it seems like i should.
        if let Values::One(Value::GetFn(arg_func)) = &arg_value {
            // TODO: support fns nested in tuples.
            let arg_func_obj = &self.program[*arg_func];
            for capture in &arg_func_obj.capture_vars {
                debug_assert!(capture.3 != VarType::Const);
            }
            outln!(
                LogTag::Generics,
                "   {arg_func:?} with captures {:?}.",
                arg_func_obj.capture_vars.iter().map(|v| v.log(self.pool)).collect::<Vec<_>>()
            );
            for v in arg_func_obj.capture_vars.clone() {
                // its fine if same this is there multiple times but this makes it less messy to debug logs.
                add_unique(&mut self.program[o_f].capture_vars, v);
            }

            // :ChainedCaptures
            // TODO: HACK: captures aren't tracked properly.
            self.program[o_f].add_tag(Flag::Inline); // just this is enough to fix chained_captures
            self.program[*arg_func].add_tag(Flag::Inline); // but this is needed too for others (perhaps just when there's a longer chain than that simple example).
        }
        self.save_const_values(arg_name, arg_value, arg_ty)?;
        self.program[o_f].arg.remove_named(arg_name);

        let known_type = self.program[o_f].finished_arg.is_some();
        self.program[o_f].finished_arg = None;
        outln!(
            LogTag::Generics,
            "  => {o_f:?} with captures {:?}",
            self.program[o_f].capture_vars.iter().map(|v| v.log(self.pool)).collect::<Vec<_>>()
        );
        // If it was fully resolved before, we can't leave the wrong answer there.
        // But you might want to call bind_const_arg as part of a resolving a generic signeture so its fine if the type isn't fully known yet.
        if known_type {
            self.infer_types(o_f)?;
        }
        Ok(())
    }

    /// It's fine to call this if the type isn't fully resolved yet.
    /// We just need to be able to finish infering for the referenced argument.
    fn get_type_for_arg(&mut self, arg: &mut Pattern<'p>, arg_name: Var<'p>) -> Res<'p, TypeId> {
        for arg in &mut arg.bindings {
            match arg.name {
                Name::Ident(_) => unreachable!(),
                Name::Var(name) => {
                    if name == arg_name {
                        self.infer_types_progress(&mut arg.ty)?;
                        return Ok(arg.ty.unwrap());
                    }
                }
                Name::None => {}
            }
        }
        ice!("missing argument {}", arg_name.log(self.pool))
    }

    // TODO: you only need to call this for generic functions that operate on thier own types.
    //       If you're just doing comptime manipulations of a TypeInfo to be used elsewhere,
    //       it can be interpreted as a normal function so you don't have to clone the ast on every call.
    // TODO: fuse this with bind_const_arg. I have too much of a combinatoric explosion of calling styles going on.
    // TODO: !!! maybe call this @generic instead of @comptime? cause other things can be comptime
    // Return type is allowed to use args.
    fn emit_comptime_call(&mut self, result: &mut FnWip<'p>, template_f: FuncId, arg_expr: &mut FatExpr<'p>) -> Res<'p, (Values, TypeId)> {
        // println!("comptime_call {:?} {}", template_f, arg_expr.log(self.pool));
        // Don't want to renumber, so only resolve on the clone. // TODO: this does redundant work on closed constants.
        self.program[template_f].assert_body_not_resolved()?;

        debug_assert!(self.program[template_f].resolved_sign);

        let state = DebugState::ComptimeCall(template_f, self.program[template_f].name);
        self.push_state(&state);
        // We don't care about the constants in `result`, we care about the ones that existed when `f` was declared.
        // BUT... the *arguments* to the call need to be evaluated in the caller's scope.

        let no_memo = self.program[template_f].has_tag(Flag::No_Memo); // Currently this is only used by 'fn Unique' because that doesn't want to go in the generics_memo cache.

        let arg_ty = self.program[template_f].finished_arg.unwrap();
        self.compile_expr(result, arg_expr, Some(arg_ty))?;
        let arg_value = self.immediate_eval_expr(arg_expr.clone(), arg_ty)?;

        // Note: the key is the original function, not our clone of it. TODO: do this check before making the clone.
        let mut key = (template_f, arg_value); // TODO: no clone
        if !no_memo {
            let found = self.program.generics_memo.get(&key);
            if let Some(found) = found {
                let found = found.clone();
                self.pop_state(state);
                return Ok(found);
            }
        }
        let arg_value = key.1;

        // This one does need the be a clone because we're about to bake constant arguments into it.
        // If you try to do just the constants or chain them cleverly be careful about the ast rewriting.
        let mut func = self.program[template_f].clone();
        debug_assert!(func.local_constants.is_empty());
        ResolveScope::resolve_body(&mut func, self)?;
        func.annotations.retain(|a| a.name != Flag::Comptime.ident()); // this is our clone, just to be safe, remove the tag.

        // TODO: memo doesn't really work on most things you'd want it to (like pointers) because those functions aren't marked @comptime, so they dont get here, because types are just normal values now
        //       now only here for generic return type that depends on an arg type (which don't need memo for correctness but no reason why not),
        //       or when impl new functions so can only happen once so they dont make redundant overloads.  -- Apr 20
        debug_assert!(!func.evil_uninit);
        func.referencable_name = false;

        // TODO: wtf.someones calling it on a tuple whish shows up as a diferent type so you get multiple of inner functions because eval twice. but then cant resolve overlaods because they have the same type beause elsewhere handles single tuples correctly.
        // TODO: figure out what was causing and write a specific test for it. discovered in fmt @join
        let arg_value = arg_value.normalize();

        assert!(!arg_expr.ty.is_unknown());
        arg_expr.expr = Expr::Value {
            ty: arg_expr.ty,
            value: arg_value.clone(),
        };

        // Bind the arg into my new constants so the ret calculation can use it.
        // also the body constants for generics need this. much cleanup pls.
        // TODO: factor out from normal functions?

        let scope = func.scope.unwrap();
        let f = self.program.add_func(func);
        self[scope].funcs.push(f);

        let func = &self.program[f];
        let args = func.arg.flatten().into_iter();
        key.1 = arg_value.clone();
        let mut arg_values = arg_value.vec().into_iter();
        for (name, ty, kind) in args {
            let size = self.ready.sizes.slot_count(self.program, ty); // TODO: better pattern matching
            let mut values = vec![];
            for _ in 0..size {
                values.push(unwrap!(arg_values.next(), "ICE: missing arguments"));
            }

            if let Some(var) = name {
                assert_eq!(kind, VarType::Const, "@comptime arg not const in {}", self.pool.get(self.program[f].name)); // not outside the check because of implicit Unit.
                self.save_const_values(var, values.into(), ty)?; // this is fine cause we renumbered at the top.
            }
        }
        assert!(arg_values.next().is_none(), "ICE: unused arguments");

        // Now the args are stored in the new function's constants, so the normal infer thing should work on the return type.
        // Note: we haven't done local constants yet, so ret can't yse them.
        self.program[f].annotations.retain(|a| a.name != Flag::Generic.ident()); // HACK
        let fn_ty = unwrap!(self.infer_types(f)?, "not inferred");
        let ret = fn_ty.ret;

        // TODO: I think this is the only place that relies on it being a whole function.
        self.eval_and_close_local_constants(f)?;

        // Drop the instantiation of the function specialized to this call's arguments.
        // We cache the result and will never need to call it again.
        let mut func = mem::take(&mut self.program[f]);
        let name = func.name;
        let body = func.body.take();
        let result = if let Some(body) = body {
            self.immediate_eval_expr(body, ret)?
        } else {
            unreachable!("builtin")
        };

        let ty = self.program.type_of_raw(&result);
        self.type_check_arg(ty, ret, "generic result")?;

        outln!(LogTag::Generics, "{:?}={} of {:?} => {:?}", key.0, self.pool.get(name), key.1, result);

        if !no_memo {
            self.program.generics_memo.insert(key, (result.clone(), ret));
        }

        self.pop_state(state);
        Ok((result, ret))
    }

    fn compile_stmt(&mut self, result: &mut FnWip<'p>, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match &mut stmt.stmt {
            Stmt::ExpandParsedStmts(_) => unreachable!(),
            Stmt::DoneDeclFunc(_, _) => unreachable!("compiled twice?"),
            Stmt::Eval(expr) => {
                self.compile_expr(result, expr, None)?;
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclNamed { .. } => {
                ice!("Scope resolution failed {}", stmt.log(self.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                if let (Some(id), Some(os)) = self.decl_func(mem::take(func))? {
                    stmt.stmt = Stmt::DoneDeclFunc(id, os);
                } else {
                    stmt.stmt = Stmt::Noop;
                }
            }
            // TODO: make value not optonal and have you explicitly call uninitilized() if you want that for some reason.
            Stmt::DeclVar { name, ty, value, kind, .. } => self.decl_var(result, *name, ty, value, *kind, &stmt.annotations, stmt.loc)?,
            // TODO: don't make the backend deal with the pattern matching but it does it for args anyway rn.
            //       be able to expand this into multiple statements so backend never even sees a DeclVarPattern (and skip constants when doing so)
            // TODO: this is extremly similar to what bind_const_arg has to do. should be able to express args as this thing?
            // TODO: remove useless statements
            Stmt::DeclVarPattern { binding, value } => {
                if binding.bindings.is_empty() {
                    assert!(value.is_none() || value.as_ref().unwrap().expr.is_raw_unit());
                    return Ok(());
                }
                let arguments = binding.flatten();
                if arguments.len() == 1 {
                    let (name, ty, kind) = arguments.into_iter().next().unwrap();
                    if let Some(name) = name {
                        self.decl_var(result, name, &mut LazyType::Finished(ty), value, kind, &stmt.annotations, stmt.loc)?;
                        if kind == VarType::Const {
                            debug_assert_eq!(binding.bindings.len(), 1);
                            binding.bindings.clear();
                            *value = None;
                        }
                        return Ok(());
                    } else {
                        let e = value.as_mut().unwrap();
                        assert!(e.expr.is_raw_unit(), "var no name not unit: {}", e.log(self.pool));
                        stmt.stmt = Stmt::Noop; // not required. just want less stuff around when debugging
                        return Ok(());
                    }
                }

                let value = unwrap!(value.as_mut(), "currently DeclVarPattern is always added by compiler and has value.");
                let exprs = if let Expr::Tuple(exprs) = &mut value.expr {
                    exprs
                } else {
                    err!("TODO: more interesting pattern matching\n {}", value.log(self.pool))
                };
                assert_eq!(arguments.len(), exprs.len(), "TODO: non-trivial pattern");

                for ((name, ty, kind), expr) in arguments.iter().zip(exprs.iter_mut()) {
                    if let Some(name) = name {
                        let loc = expr.loc;
                        // TODO: HACK. make value not optional. cant just flip because when missing decl_var wants to put something there for imports. makes it hard. need to have a marker expr for missing value??
                        let mut value = Some(mem::take(expr));
                        self.decl_var(result, *name, &mut LazyType::Finished(*ty), &mut value, *kind, &stmt.annotations, loc)?;
                        *expr = value.unwrap();
                    } else {
                        assert!(expr.expr.is_raw_unit(), "var no name not unit");
                    }
                }

                // Remove constants so the backend doesn't have to deal with them.
                let mut removed = 0;
                for (i, (name, _, kind)) in arguments.iter().enumerate() {
                    if *kind == VarType::Const {
                        // TODO: do i have to put the expr in local constants somehow?
                        binding.remove_named(name.unwrap());
                        exprs.remove(i - removed);
                        removed += 1; // TODO: this sucks
                    }
                }
                // Fix trivial tuples
                binding.if_empty_add_unit();
                if exprs.is_empty() {
                    value.expr = Expr::unit();
                } else if exprs.len() == 1 {
                    *value = mem::take(exprs.iter_mut().next().unwrap());
                }
                // If we changed stuff, make sure not to give the backend false type info.
                if removed > 0 && !value.ty.is_unknown() {
                    value.ty = TypeId::unknown();
                    self.compile_expr(result, value, None)?;
                }
            }
        }
        Ok(())
    }

    fn decl_func(&mut self, mut func: Func<'p>) -> Res<'p, (Option<FuncId>, Option<usize>)> {
        debug_assert!(!func.evil_uninit);
        // TODO: allow language to declare @macro(.func) and do this there instead. -- Apr 27
        if let Some(when) = func.get_tag_mut(Flag::When) {
            let cond = unwrap!(when.args.as_mut(), "#when(cond: bool) requires argument");
            let cond: bool = self.immediate_eval_expr_known(cond.clone())?;
            if !cond {
                return Ok((None, None));
            }
        }

        let var = func.var_name;
        let for_bootstrap = func.has_tag(Flag::Bs);
        let any_reg_template = func.has_tag(Flag::Any_Reg);
        let is_struct = func.has_tag(Flag::Struct);
        let is_enum = func.has_tag(Flag::Enum);
        if func.has_tag(Flag::Macro) {
            assert!(!func.has_tag(Flag::Rt));
            func.add_tag(Flag::Ct);
            func.add_tag(Flag::Flat_Call);
        }

        assert!(!(is_struct && is_enum));
        if is_struct {
            ResolveScope::resolve_sign(&mut func, self)?;
            ResolveScope::resolve_body(&mut func, self)?;
            let init_overloadset = self.program.overload_sets.iter().position(|a| a.name == Flag::Init.ident()).unwrap();
            let ty = self.struct_type(&mut func.arg)?;
            let ty = self.program.intern_type(ty);
            assert!(matches!(func.ret, LazyType::Infer), "remove type annotation on struct initilizer");
            func.ret = LazyType::Finished(ty);
            func.finished_ret = Some(ty);
            if let Some(name) = func.var_name {
                self.save_const_values(name, Value::Type(ty).into(), TypeId::ty())?;
            }
            // TODO: do i need to set func.var_name? its hard because need to find an init? or can it be a new one?
            //       with new commitment to doing it without idents for modules, the answer is yes.
            func.name = Flag::Init.ident();
            if func.body.is_none() {
                let loc = func.loc;
                let mut init_pattern = func.arg.clone();
                for b in &mut init_pattern.bindings {
                    let name = if let Name::Var(name) = b.name { name } else { todo!() };
                    b.ty = LazyType::PendingEval(FatExpr::synthetic(Expr::GetVar(name), loc));
                }

                func.body = Some(FatExpr::synthetic(Expr::StructLiteralP(init_pattern), loc));
            }
            let id = self.add_func(func)?;
            self.program.overload_sets[init_overloadset].pending.push(id);
            return Ok((Some(id), Some(init_overloadset)));
        }
        if is_enum {
            ResolveScope::resolve_sign(&mut func, self)?;
            ResolveScope::resolve_body(&mut func, self)?;
            let init_overloadset = self.program.overload_sets.iter().position(|a| a.name == Flag::Init.ident()).unwrap();
            let ty = self.struct_type(&mut func.arg)?;
            let ty = self.program.to_enum(ty);
            assert!(matches!(func.ret, LazyType::Infer), "remove type annotation on struct initilizer");
            func.ret = LazyType::Finished(ty);
            func.finished_ret = Some(ty);
            if let Some(name) = func.var_name {
                self.save_const_values(name, Value::Type(ty).into(), TypeId::ty())?;
            }
            // TODO: do i need to set func.var_name? its hard because need to find an init? or can it be a new one?
            func.name = Flag::Init.ident();
            if func.body.is_none() {
                let loc = func.loc;
                let init_pattern = func.arg.clone();
                for mut b in init_pattern.bindings {
                    let mut new_func = func.clone();
                    debug_assert!(new_func.local_constants.is_empty());
                    new_func.arg.bindings = vec![b.clone()];
                    let name = if let Name::Var(name) = b.name { name } else { todo!() };
                    b.ty = LazyType::PendingEval(FatExpr::synthetic(Expr::GetVar(name), loc));
                    new_func.body = Some(FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![b], loc }), loc));
                    new_func.annotations.retain(|a| a.name != Flag::Enum.ident());
                    let id = self.add_func(new_func)?;
                    self.program.overload_sets[init_overloadset].pending.push(id);
                }
            } // TODO: new consts system -- apr 25
              // TODO: unfortunate that I have to short circuit, not handle extra annotations, and leave the garbage function there because i split them

            return Ok((None, None));
        }
        if for_bootstrap {
            func.add_tag(Flag::Aarch64); // TODO: could do for llvm too once i support eval body
            func.referencable_name = false;
        }

        let referencable_name = func.referencable_name;

        if any_reg_template {
            todo!("apr 25 const rework");
            // let mut body = func.body.take().unwrap();
            // self.compile_expr(result, &mut body, None)?;
            // func.any_reg_template = Some(body.as_fn().unwrap());
        }

        let public = func.has_tag(Flag::Pub);
        let id = self.add_func(func)?;

        if for_bootstrap {
            self.save_bootstrap.push(id);
        }

        let mut out = (None, None);
        // I thought i dont have to add to constants here because we'll find it on the first call when resolving overloads.
        // But it does need to have an empty entry in the overload pool because that allows it to be closed over so later stuff can find it and share if they compile it.
        if let Some(var) = var {
            if referencable_name && !is_enum && !is_struct {
                // TODO: allow function name to be any expression that resolves to an OverloadSet so you can overload something in a module with dot syntax.
                // TODO: distinguish between overload sets that you add to and those that you re-export
                debug_assert!(!self.program[id].resolved_sign);
                debug_assert!(!self.program[id].resolved_body);
                if let Some(overloads) = self.find_const(var)? {
                    let i = overloads.0.as_overload_set().unwrap();
                    let os = &mut self.program.overload_sets[i];
                    assert_eq!(os.public, public, "Overload visibility mismatch: {}", var.log(self.pool));

                    os.pending.push(id);
                    out = (Some(id), Some(i));
                } else {
                    let index = self.program.overload_sets.len();
                    self.program.overload_sets.push(OverloadSet {
                        ready: vec![],
                        name: var.0,
                        pending: vec![id],
                        public,
                        just_resolved: vec![],
                    });
                    self.save_const_values(var, Value::OverloadSet(index).into(), TypeId::overload_set())?;
                    out = (Some(id), Some(index));
                }
            }
        }

        let func = &self.program[id];

        if func.has_tag(Flag::Test) {
            // TODO: actually use this.
            // TODO: probably want referencable_name=false?
            self.tests.push(id);
        }
        Ok(out)
    }

    pub fn find_const(&mut self, name: Var<'p>) -> Res<'p, Option<(Values, TypeId)>> {
        if let Some(s) = self[name.2].constants.get(&name) {
            if let Some(known) = s.1.ty() {
                if let Expr::Value { ty, value } = &s.0.expr {
                    debug_assert!(!ty.is_unknown());
                    debug_assert_eq!(*ty, known);
                    return Ok(Some((value.clone(), *ty)));
                }
                ice!("constant with known type but unknown value.")
            }
            if matches!(s.0.expr, Expr::Poison) {
                // creating an overload set gets here I think -- Apr 27
                return Ok(None);
            }

            let (mut val, mut ty) = mem::take(self[name.2].constants.get_mut(&name).unwrap());
            // println!("- {} {} {}", name.log(self.pool), ty.log(self.pool), val.log(self.pool));
            self[name.2].constants.get_mut(&name).unwrap().1 = LazyType::Infer;
            self.infer_types_progress(&mut ty)?;
            let loc = val.loc;
            self.decl_const(name, &mut ty, &mut val, loc)?;
            return self.find_const(name);
        }
        Ok(None)
    }

    pub fn find_const_type(&mut self, var: Var<'p>) -> Res<'p, Option<TypeId>> {
        Ok(self.find_const(var)?.map(|(_, ty)| ty))
    }

    #[track_caller]
    pub fn ensure_resolved_sign(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.program[f].resolved_sign {
            return Ok(());
        }
        debug_assert!(!self.program[f].evil_uninit, "ensure_resolved_sign of evil_uninit {}", self.log_trace());
        self.program[f].why_resolved_sign = Some(Location::caller().to_string()); // self.log_trace()
        mut_replace!(self.program[f], |mut func: Func<'p>| {
            ResolveScope::resolve_sign(&mut func, self).unwrap(); // TODO
            Ok((func, ()))
        });
        Ok(())
    }

    #[track_caller]
    pub fn ensure_resolved_body(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.program[f].resolved_body {
            return Ok(());
        }
        debug_assert!(!self.program[f].evil_uninit, "ensure_resolved_body of evil_uninit {}", self.log_trace());
        self.program[f].why_resolved_body = Some(Location::caller().to_string()); // self.log_trace()
        self.ensure_resolved_sign(f)?;
        mut_replace!(self.program[f], |mut func: Func<'p>| {
            ResolveScope::resolve_body(&mut func, self)?;
            Ok((func, ()))
        });
        Ok(())
    }

    #[track_caller]
    fn func_expr(&mut self, id: FuncId) -> Expr<'p> {
        if self.program[id].finished_ret.is_some() {
            Expr::Value {
                ty: self.program.func_type(id),
                value: Value::GetFn(id).into(),
            }
        } else {
            Expr::WipFunc(id)
        }
    }

    // i just like that the debugger shows it's not an interesting frame
    // what the actual fuck. this makes it segfault
    // #[cfg_attr(debug_assertions, inline(always))]
    pub fn compile_expr(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, Structured> {
        assert!(expr.ty.as_index() < self.program.types.len());

        // TODO: it seems like i shouldn't compile something twice
        // debug_assert!(expr.ty.is_unknown(), "{}", expr.log(self.pool));
        let old = expr.ty;

        let mut res = self.compile_expr_inner(result, expr, requested)?;
        if !expr.ty.is_unknown() {
            self.last_loc = Some(expr.loc);
            //format!("sanity ICE {} {res:?}", expr.log(self.pool)).leak())?;
            self.type_check_arg(expr.ty, res.ty(), "sanity ICE")?;
        }
        if let Some(requested) = requested {
            // TODO: its possible to write code that gets here without being caught first.
            //       should fix so everywhere that relies on 'requested' does thier own typecheck because they can give a more contextual error message.
            //       but its annoying becuase this happens first so you cant just sanity check here and i dont want to trust that everyone remembered.
            //       so maybe its better to have more consistant use of the context stack so you always know what you're doing and forwarding typecheck responsibility doesnt mean poor error messages.
            //       but then you have to make sure not to mess up the stack when you hit recoverable errors. and that context has to not be formatted strings since thats slow.
            //       -- Apr 19
            // format!("sanity ICE {} {res:?}", expr.log(self.pool)).leak()
            self.type_check_arg(res.ty(), requested, "sanity ICE")?;
        }

        if let Structured::TupleDifferent(_, parts) = &res {
            if parts.iter().all(|p| matches!(p, Structured::Const(_, _))) {
                err!("sanity ICE",)
            }
        }

        if let Structured::Const(ty_res, _) = &res {
            if let &Expr::Value { ty, .. } = &expr.expr {
                self.type_check_arg(ty, *ty_res, "sanity ICE")?;
            }
            // TODO: why is that not always true and why do tests fail if i fix it here.
            //       like: 'else expr.expr = Expr::Value { ty: *ty_res, value: values.clone(), }'
            //       in const pattern match I destruct ::Const to ::TupleDifferent where it expects the expr to be ::Tuple not ::Value.
            //       should be fixable by iterating over sizes like emit_bc::bind_args does.     -- Apr 19
        } else if let Expr::Value { ty, value } = &expr.expr {
            // TODO: ideally this would either be handled only here (and nowhere else would bother) or never get here.
            res = Structured::Const(*ty, value.clone());
        }
        expr.ty = res.ty();
        if !old.is_unknown() {
            // TODO: cant just assert_eq because it does change for VoidPtr.
            self.type_check_arg(expr.ty, old, "sanity ICE")?;
        }
        Ok(res)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr_inner(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, Structured> {
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);
        let loc = expr.loc;

        Ok(match expr.deref_mut() {
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::Closure(_) => {
                let id = self.promote_closure(result, expr)?;
                Structured::Const(self.program.func_type(id), Value::GetFn(id).into())
            }
            &mut Expr::WipFunc(id) => {
                self.infer_types(id)?;
                expr.expr = self.func_expr(id);
                Structured::Const(self.program.func_type(id), Value::GetFn(id).into())
            }
            Expr::Call(f, arg) => {
                // self.compile_expr(result, arg, None)?; // TODO: infer arg type from fn??

                if let Some(ty) = self.type_of(result, f)? {
                    if let TypeInfo::FnPtr(f_ty) = self.program[ty] {
                        self.compile_expr(result, f, Some(ty))?;
                        self.compile_expr(result, arg, Some(f_ty.arg))?;
                        return Ok(Structured::RuntimeOnly(f_ty.ret));
                    }
                }

                if let Some(f_id) = self.maybe_direct_fn(result, f, arg, requested)? {
                    match f_id {
                        FuncRef::Exact(f_id) => return Ok(self.compile_call(result, expr, f_id, requested)?.0),
                        FuncRef::Split { ct, rt } => return self.compile_split_call(result, expr, ct, rt),
                    }
                }
                ice!("function not declared or \nTODO: dynamic call: {}\n\n{expr:?}", expr.log(self.pool))
            }
            Expr::Block { body, result: value, .. } => {
                body.retain(|s| !(matches!(s.stmt, Stmt::Noop) && s.annotations.is_empty())); // Not required but makes debugging easier cause there's less stuff.
                for stmt in body.iter_mut() {
                    self.compile_stmt(result, stmt)?;
                }
                // TODO: insert drops for locals
                let res = self.compile_expr(result, value, requested)?;
                if body.is_empty() {
                    *expr = mem::take(value); // Not required but makes debugging easier cause there's less stuff.
                }
                res
            }
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples");
                let values: Res<'p, Vec<_>> = values.iter_mut().map(|v| self.compile_expr(result, v, None)).collect();
                let values = values?;
                let types: Vec<_> = values.iter().map(|s| s.ty()).collect();
                let ty = self.program.tuple_of(types);
                self.produce_tuple(values, ty)?
            }
            Expr::GetVar(var) => {
                if let Some(ty) = result.vars.get(var).cloned() {
                    Structured::RuntimeOnly(ty)
                } else if let Some((value, ty)) = self.find_const(*var)? {
                    expr.expr = Expr::Value { ty, value: value.clone() };
                    expr.ty = ty;
                    Structured::Const(ty, value)
                } else {
                    outln!(ShowErr, "VARS: {:?}", result.vars);
                    ice!("Missing resolved variable {}", var.log(self.pool),)
                }
            }
            Expr::GetNamed(name) => err!("Undeclared Ident {}", self.pool.get(*name)), //err!(CErr::UndeclaredIdent(*name)),
            Expr::Value { ty, value } => Structured::Const(*ty, value.clone()),
            Expr::Raw { ty, value } => {
                // TODO: the whole point is to not always have to deserialize it. allow Structured::ConstRaw
                let mut out = vec![];
                values_from_ints(self, *ty, &mut value.iter().copied(), &mut out)?;
                Structured::Const(*ty, Values::from(out))
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = Flag::try_from(*macro_name)?;
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    Flag::If => self.emit_call_if(result, arg, requested)?,
                    Flag::While => self.emit_call_while(result, arg)?,
                    Flag::Addr => self.addr_macro(result, arg)?,
                    Flag::Quote => {
                        let mut arg: FatExpr<'p> = *arg.clone(); // Take the contents of the box, not the box itself!
                        let loc = arg.loc;

                        let mut walk = Unquote {
                            compiler: self,
                            placeholders: vec![],
                            result,
                        };
                        walk.expr(&mut arg);
                        let mut placeholders = walk.placeholders; // drop walk.

                        // TODO: want to do this but then my mutation fucks everything. you really do need to do the clone.
                        //       replace with that constant and a clone. need to impl clone. but deep clone that works on heap ptrs.
                        let mut value = arg.serialize_one();
                        value.make_heap_constant();
                        let ty = FatExpr::get_type(self.program);
                        expr.expr = Expr::Value { ty, value: value.clone() };
                        expr.ty = ty;
                        if placeholders.is_empty() {
                            Structured::Const(ty, value)
                        } else {
                            placeholders.push(Some(mem::take(expr)));
                            let arg = Box::new(FatExpr::synthetic(
                                Expr::Tuple(placeholders.into_iter().map(|p| p.unwrap()).collect()),
                                loc,
                            ));
                            let arg = FatExpr::synthetic(Expr::SuffixMacro(Flag::Slice.ident(), arg), loc);
                            let f = self.program.find_unique_func(Flag::Unquote_Macro_Apply_Placeholders.ident()).unwrap(); // TODO
                            let _ = self.infer_types(f)?.unwrap();
                            let f = FatExpr::synthetic(
                                Expr::Value {
                                    ty: self.program.func_type(f),
                                    value: Value::GetFn(f).into(),
                                },
                                loc,
                            );
                            *expr = FatExpr::synthetic(Expr::Call(Box::new(f), Box::new(arg)), loc);
                            // println!("{:?}", expr.log(self.pool));
                            self.compile_expr(result, expr, requested)?
                        }
                    }
                    Flag::Slice => {
                        // println!("{:?}", arg.log(self.pool));
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
                    Flag::C_Call => err!("!c_call has been removed. calling convention is part of the type now.",),
                    Flag::Deref => {
                        let requested = requested.map(|t| self.program.ptr_type(t));
                        let ptr = self.compile_expr(result, arg, requested)?;
                        let ty = unwrap!(self.program.unptr_ty(ptr.ty()), "deref not ptr: {}", self.program.log_type(ptr.ty()));

                        if let &Structured::Const(
                            _,
                            Values::One(Value::Heap {
                                value,
                                physical_first,
                                physical_count,
                            }),
                        ) = &ptr
                        {
                            // this check + the const eval in field access lets me do asm enum constants without doing heap values first.
                            if physical_count == 1 {
                                let value = Values::One(unsafe { (*value).values[physical_first] });
                                expr.expr = Expr::Value { ty, value: value.clone() };
                                Structured::Const(ty, value)
                            } else {
                                Structured::RuntimeOnly(ty)
                            }
                        } else {
                            Structured::RuntimeOnly(ty)
                        }
                    }
                    Flag::Type => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects. especially if it does const stuff.
                        let ty = self.compile_expr(result, arg, None)?.ty();
                        expr.expr = Expr::ty(ty);
                        self.program.load_value(Value::Type(ty))
                    }
                    Flag::Const_Eval => {
                        let res = self.compile_expr(result, arg, requested)?;
                        let ty = res.ty();
                        let value = if let Ok(val) = res.get() {
                            val
                        } else {
                            // TODO: its a bit silly that i have to specifiy the type since the first thing it does is compile it
                            self.immediate_eval_expr(mem::take(arg), ty)?
                        };
                        expr.expr = Expr::Value { ty, value: value.clone() };
                        expr.ty = ty;
                        Structured::Const(ty, value)
                    }
                    Flag::Size_Of => {
                        let ty: TypeId = self.immediate_eval_expr_known(mem::take(arg))?;
                        let size = self.ready.sizes.slot_count(self.program, ty);
                        expr.expr = Expr::int(size as i64);
                        self.program.load_value(Value::I64(size as i64))
                    }
                    Flag::Assert_Compile_Error => {
                        // TODO: this can still have side-effects on the compiler state tho :(
                        let saved_res = result.clone();
                        let before = self.debug_trace.len();
                        unsafe {
                            EXPECT_ERR_DEPTH.fetch_add(1, Ordering::SeqCst);
                        }
                        let res = self.compile_expr(result, arg, None);
                        unsafe {
                            EXPECT_ERR_DEPTH.fetch_sub(1, Ordering::SeqCst);
                        }
                        assert!(res.is_err());
                        *result = saved_res;
                        while self.debug_trace.len() > before {
                            self.debug_trace.pop();
                        }

                        expr.expr = Expr::unit();
                        self.program.load_value(Value::Unit)
                    }
                    Flag::Struct => {
                        if let Expr::StructLiteralP(pattern) = arg.deref_mut().deref_mut() {
                            let ty = self.struct_type(pattern)?;
                            let ty = self.program.intern_type(ty);
                            expr.expr = Expr::ty(ty);
                            self.program.load_value(Value::Type(ty))
                        } else {
                            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", arg);
                        }
                    }
                    Flag::Enum => {
                        if let Expr::StructLiteralP(pattern) = arg.deref_mut().deref_mut() {
                            let ty = self.struct_type(pattern)?;
                            let ty = self.program.to_enum(ty);
                            expr.expr = Expr::ty(ty);
                            self.program.load_value(Value::Type(ty))
                        } else {
                            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", arg);
                        }
                    }
                    Flag::Tag => {
                        // TODO: auto deref and typecheking
                        let container_ptr = self.compile_expr(result, arg, None)?;
                        let container_ptr_ty = self.program.raw_type(container_ptr.ty());
                        let depth = self.program.ptr_depth(container_ptr_ty);
                        assert_eq!(depth, 1, "!tag ptr must be one level of indirection. {:?}", container_ptr_ty);
                        let ty = self.program.intern_type(TypeInfo::Ptr(TypeId::i64()));
                        Structured::RuntimeOnly(ty)
                    }
                    Flag::Symbol => {
                        // TODO: use match
                        let value = if let Expr::GetNamed(i) = arg.deref_mut().deref_mut() {
                            Value::Symbol(i.0)
                        } else if let Expr::GetVar(v) = arg.deref_mut().deref_mut() {
                            Value::Symbol(v.0 .0)
                        } else if let Expr::String(i) = arg.deref_mut().deref_mut() {
                            Value::Symbol(i.0)
                        } else {
                            ice!("Expected identifier found {arg:?}")
                        };
                        expr.expr = Expr::Value {
                            ty: Ident::get_type(self.program),
                            value: value.into(),
                        };
                        self.program.load_value(value)
                    }
                    Flag::Fn_Ptr => {
                        // TODO: this should be immediate_eval_expr (which should be updated do the constant check at the beginning anyway).
                        //       currently !fn_ptr can't be an atrbitrary comptime expr which is silly.
                        let fn_val = self.compile_expr(result, arg, None)?.get()?;
                        match fn_val {
                            Values::One(Value::GetFn(id)) => {
                                assert!(!self.program[id].any_const_args());
                                // TODO: for now you just need to not make a mistake lol
                                //       you cant do a flat_call through the pointer but you can pass it to the compiler when it's expecting to do a flat_call.
                                // TODO: calling convention in function type so you don't have to keep remembering to check its not both flat_call and c_call.
                                // self.program[id].add_tag(Flag::C_Call);
                                // assert!(!self.program[id].has_tag(Flag::Flat_Call), "TODO: cc in ptr ty");
                                // assert!(!self.program[id].has_tag(Flag::Ct), "TODO: cc in ptr ty");
                                self.add_callee(result, id, ExecTime::Both);
                                self.compile(id, result.when)?;
                                // The backend still needs to do something with this, so just leave it
                                let ty = self.program.func_type(id);
                                let ty = self.program.fn_ty(ty).unwrap();
                                let ty = self.program.intern_type(TypeInfo::FnPtr(ty));
                                expr.expr = Expr::Value {
                                    ty,
                                    value: Values::One(Value::GetNativeFnPtr(id)),
                                };
                                expr.ty = ty;
                                Structured::RuntimeOnly(ty)
                            }
                            Values::One(Value::GetNativeFnPtr(_)) => err!("redundant use of !fn_ptr",),
                            _ => err!("!fn_ptr expected const fn not {fn_val:?}",),
                        }
                    }
                    Flag::From_Bit_Literal => {
                        let int = unwrap!(bit_literal(expr, self.pool), "not int");
                        let ty = self.program.intern_type(TypeInfo::Int(int.0));
                        let value = Value::I64(int.1);
                        expr.expr = Expr::Value { ty, value: value.into() };
                        Structured::Const(ty, value.into())
                    }
                    Flag::Uninitialized => {
                        assert!(arg.is_raw_unit());
                        assert!(requested.is_some(), "!uninitialized expr must have known type");
                        return Ok(Structured::RuntimeOnly(requested.unwrap()));
                    }
                    Flag::Unquote | Flag::Placeholder => ice!("Unhandled macro {}", self.pool.get(*macro_name)),
                    Flag::Builtin => {
                        let name = if let Some(v) = arg.as_ident() {
                            v
                        } else {
                            panic!("@builtin requires argument",);
                        };
                        let (value, ty) = unwrap!(self.builtin_constant(name), "unknown @builtin: {:?}", self.pool.get(name));
                        expr.expr = Expr::Value { ty, value: value.into() };
                        return Ok(Structured::Const(ty, value.into()));
                    }
                    Flag::Contextual_Field => {
                        let ty = unwrap!(requested, "!contextual_field (leading dot) requires name");
                        if let Some(name) = arg.as_ident() {
                            let fields = unwrap!(
                                self.program.contextual_fields[ty.as_index()].as_ref(),
                                "no contextual fields for type {ty:?}"
                            );
                            let (value, ty) = unwrap!(fields.get(&name), "contextual field not found {} for {ty:?}", self.pool.get(name));
                            expr.ty = *ty;
                            expr.expr = Expr::Value {
                                ty: *ty,
                                value: value.clone(),
                            };
                            return Ok(Structured::Const(*ty, value.clone()));
                        } else {
                            err!("!contextual_field (leading dot) requires name",)
                        }
                    }
                    _ => {
                        err!(CErr::UndeclaredIdent(*macro_name))
                    }
                }
            }
            Expr::FieldAccess(e, name) => {
                let container = self.compile_expr(result, e, None)?;

                if let Structured::Const(ty, val) = &container {
                    if let Values::One(Value::Type(ty)) = val {
                        if let Some(fields) = &self.program.contextual_fields[ty.as_index()] {
                            let (val, ty) = unwrap!(fields.get(name).cloned(), "missing contextual field {} for {ty:?}", self.pool.get(*name));
                            expr.expr = Expr::Value { ty, value: val.clone() };
                            expr.ty = ty;
                            return Ok(Structured::Const(ty, val));
                        }
                    }

                    if *ty == TypeId::scope() {
                        let (s, b) = val.as_int_pair()?;

                        if let Some(&var) = self[ScopeId::from_raw(s)].vars[b as usize].vars.iter().find(|v| v.0 == *name) {
                            debug_assert!(var.3 == VarType::Const);
                            if let Some((val, ty)) = self.find_const(var)? {
                                expr.expr = Expr::Value { ty, value: val.clone() };
                                return Ok(Structured::Const(ty, val));
                            } else {
                                err!("missing constant {}", var.log(self.pool))
                            }
                        } else {
                            err!(CErr::UndeclaredIdent(*name))
                        }
                    }
                }

                if container.ty() == TypeId::scope() {
                    err!("dot syntax on module must be const",)
                }

                let index = self.field_access_expr(result, e, *name)?;
                expr.expr = Expr::Index {
                    ptr: mem::take(e),
                    index: Box::new(FatExpr::synthetic(Expr::int(index as i64), loc)),
                };
                self.compile_expr(result, expr, requested)? // TODO: dont dispatch again
            }
            Expr::Index { ptr, index } => {
                let ptr = self.compile_expr(result, ptr, None)?;
                self.compile_expr(result, index, Some(TypeId::i64()))?;
                let i: usize = self.immediate_eval_expr_known(*index.clone())?;
                index.expr = Expr::Value {
                    ty: TypeId::i64(),
                    value: Values::One(Value::I64(i as i64)),
                };
                self.index_expr(ptr, i)?
            }
            // TODO: replace these with a more explicit node type?
            Expr::StructLiteralP(pattern) => self.construct_struct(result, requested, pattern)?,
            // err!("Raw struct literal. Maybe you meant to call 'init'?",),
            &mut Expr::String(i) => {
                let bytes = self.pool.get(i).to_string();
                let mut bytes = bytes.serialize_one();
                bytes.make_heap_constant();
                let ty = String::get_type(self.program);
                expr.expr = Expr::Value { ty, value: bytes.clone() };
                Structured::Const(ty, bytes)
            }
            Expr::PrefixMacro { handler, arg, target } => {
                outln!(
                    LogTag::Macros,
                    "PrefixMacro: {}\nARG: {}\nTARGET: {}",
                    handler.log(self.pool),
                    arg.log(self.pool),
                    target.log(self.pool)
                );
                let expr_ty = FatExpr::get_type(self.program);
                let mut arg = mem::take(arg.deref_mut());
                let mut target = mem::take(target.deref_mut());
                let want = FatExpr::get_type(self.program);
                let arg_ty = self.program.tuple_of(vec![expr_ty, expr_ty]);

                // TODO: this is dump copy-paste cause i cant easily resovle on type instead of expr
                // TODO: OverloadSet: InterpSend so I can use known. cant say usize even tho thats kinda what i want cause its unique and anyway would be dumb to give up the typechecking -- Apr 21
                // TODO: ask for a callable but its hard because i dont know if i want one or two argument version yet. actually i guess i do, just look an target sooner. but im not sure eval will resolve the overload for me yet -- Apr 21
                let os = self.immediate_eval_expr(*handler.clone(), TypeId::overload_set())?;
                let os = unwrap!(os.single()?.to_overloads(), "expected overload set. TODO: allow direct function");
                self.compute_new_overloads(os)?;

                let os = self.program.overload_sets[os]
                    .ready
                    .iter()
                    .filter(|o| (o.ret.is_none()) || o.ret.unwrap() == want);
                let mut os2 = os.clone().filter(|o| o.arg == arg_ty);

                // This allows @a E; instead of @a(E);
                if arg.is_raw_unit() && !target.is_raw_unit() {
                    mem::swap(&mut arg, &mut target);
                }

                // If they did '@m(e)' instead of '@m(e) s', prefer a handler that only expects one argument.
                // TODO: should probably distinguish '@m(e) unit' just incase
                if target.is_raw_unit() {
                    let mut os1 = os.clone().filter(|o| o.arg == want);
                    if let Some(f) = os1.next() {
                        assert!(os1.next().is_none(), "ambigous macro overload");
                        let f = f.func;
                        assert!(self.program[f].has_tag(Flag::Macro));
                        self.infer_types(f)?;
                        self.compile(f, ExecTime::Comptime)?;
                        self.typecheck_macro_outputs(f, requested)?;
                        let new_expr: FatExpr<'p> = self.call_jitted(f, ExecTime::Comptime, Some(result as *mut FnWip), arg)?;
                        *expr = new_expr;
                        return self.compile_expr(result, expr, requested);
                    } else {
                        err!(
                            "Missing macro overload (Expr) -> Expr. maybe you forgot a target expr on the invocation of {}",
                            handler.log(self.pool)
                        )
                    }
                }

                let f = unwrap!(os2.next(), "missing macro overload").func;
                assert!(os2.next().is_none(), "ambigous macro overload");
                assert!(self.program[f].has_tag(Flag::Macro));
                self.infer_types(f)?;
                self.compile(f, ExecTime::Comptime)?;

                self.typecheck_macro_outputs(f, requested)?;
                let new_expr: FatExpr<'p> = self.call_jitted(f, ExecTime::Comptime, Some(result as *mut FnWip), (arg, target))?;
                *expr = new_expr;
                outln!(LogTag::Macros, "OUTPUT: {}", expr.log(self.pool));
                outln!(LogTag::Macros, "================\n");
                // Now evaluate whatever the macro gave us.
                return self.compile_expr(result, expr, requested);
            }
        })
    }

    // This is a redundant check but it means you can move the error message to before the macro expansion which might make it more clear.
    // Only works for simple cases where its possible to give a static output type for all invocations of the macro.
    fn typecheck_macro_outputs(&mut self, macro_f: FuncId, requested: Option<TypeId>) -> Res<'p, ()> {
        if let Some(requested) = requested {
            if let Some(outputs) = &self.program[macro_f].annotations.iter().find(|a| a.name == Flag::Outputs.ident()) {
                let ty = unwrap!(outputs.args.clone(), "annotation #outputs(T) requires arg T");
                let ty: TypeId = self.immediate_eval_expr_known(ty)?;
                self.type_check_arg(requested, ty, "macro #outputs")?;
            }
        }
        Ok(())
    }

    // TODO: I think this can just be (arg, target) = '{ let v: <arg> = <target>; v }'
    //       but it still has special handling in type_of to stop early.
    //       would need better type inference to make that the same, but probably want that anyway.
    //       ideally would also allow macros to infer a type even if they can't run all the way?    -- Apr 21
    pub fn as_cast_macro(&mut self, result: &mut FnWip<'p>, mut arg: FatExpr<'p>, mut target: FatExpr<'p>) -> Res<'p, FatExpr<'p>> {
        self.compile_expr(result, &mut arg, Some(TypeId::ty()))?;
        let ty: TypeId = self.immediate_eval_expr_known(arg)?;
        assert!(!ty.is_unknown());
        target.ty = ty;
        // have to do this here because it doesn't pass requested in from saved infered type.  // TODO: maybe it should? -- Apr 21
        self.compile_expr(result, &mut target, Some(ty))?;
        Ok(target)
    }

    pub fn enum_constant_macro(&mut self, arg: FatExpr<'p>, mut target: FatExpr<'p>) -> Res<'p, FatExpr<'p>> {
        let loc = arg.loc;
        let ty: TypeId = self.immediate_eval_expr_known(arg.clone())?;
        if let Expr::StructLiteralP(pattern) = target.deref_mut().deref_mut().deref_mut() {
            let mut the_type = Pattern::empty(loc);
            let unique_ty = self.program.unique_ty(ty);
            let mut ctx_fields = HashMap::new();
            // Note: we expand target to an anonamus struct literal, not a type.
            for b in &mut pattern.bindings {
                assert!(b.default.is_some());
                assert!(matches!(b.lazy(), LazyType::Infer));

                let expr = unwrap!(b.default.take(), "");
                let val = self.immediate_eval_expr(expr, unique_ty)?;

                b.ty = LazyType::PendingEval(FatExpr::synthetic(
                    Expr::Value {
                        ty: unique_ty,
                        value: val.clone(),
                    },
                    loc,
                ));
                let name = unwrap!(b.name(), "@enum field missing name??");
                ctx_fields.insert(name, (val, unique_ty));
                the_type.bindings.push(Binding {
                    name: b.name,
                    // ty: LazyType::Finished(unique_ty),
                    ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(unique_ty), loc)),
                    default: None,
                    kind: VarType::Let,
                });
            }
            let i = unique_ty.as_index();
            extend_options(&mut self.program.contextual_fields, i);
            let old = self.program.contextual_fields[i].replace(ctx_fields);
            debug_assert!(old.is_none());

            let mut e = FatExpr::synthetic(
                Expr::Value {
                    ty: TypeId::ty(),
                    value: Values::One(Value::Type(unique_ty)),
                },
                loc,
            );
            e.ty = TypeId::ty();

            return Ok(e);
        }
        err!("Expected struct literal found {target:?}",);
    }
    fn addr_macro(&mut self, result: &mut FnWip<'p>, arg: &mut FatExpr<'p>) -> Res<'p, Structured> {
        match arg.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                if let Some(value_ty) = result.vars.get(var).cloned() {
                    assert!(!value_ty.is_any(), "took address of Any {}", var.log(self.pool));
                    if var.3 != VarType::Var {
                        err!(
                            "Can only take address of vars not {:?} {}. TODO: allow read field.",
                            var.3,
                            var.log(self.pool)
                        )
                    }
                    let ptr_ty = self.program.ptr_type(value_ty);
                    Ok(Structured::RuntimeOnly(ptr_ty))
                } else if let Some(value) = self.find_const(*var)? {
                    // HACK: this is wrong but it makes constant structs work bette
                    if let TypeInfo::Ptr(_) = self.program[value.1] {
                        return Ok(value.into());
                    }
                    err!(
                        "Took address of constant {}: {:?} ({}) {arg:?}",
                        var.log(self.pool),
                        value.1,
                        self.program.log_type(value.1)
                    )
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
            _ => err!("took address of r-value {}", arg.log(self.pool)),
        }
    }

    // HACK: type_of_inner can return None because of errors elsewhere in the compiler (which is a bad idea the way im currently doing it probably) so might leave junk on the context stack.
    pub(crate) fn type_of(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        let s = DebugState::TypeOf;
        self.push_state(&s);
        let before = self.debug_trace.len();
        let res = self.type_of_inner(result, expr)?;
        while self.debug_trace.len() > before {
            // TODO: HACK
            self.debug_trace.pop().unwrap();
        }
        self.pop_state(s);
        Ok(res)
    }
    // TODO: this is clunky. Err means invalid input, None means couldn't infer type (often just not implemented yet).
    // It's sad that this could mutate expr, but the easiest way to typecheck closures is to promote them to functions,
    // which you have to do anyway eventually so you might as well save that work.
    // But it means you have to be careful that if inference fails, you haven't lost the expr in the process.
    // TODO: maybe this should be more fused with the normal compiling process?
    //       then need to be more careful about what gets put in the result
    //       cause like for blocks too, it would be nice to infer a function's return type by just compiling
    //       the function and seeing what you get.
    // this is such a source of wierd nondeterminism bugs because of ^
    pub(crate) fn type_of_inner(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        if !expr.ty.is_unknown() {
            return Ok(Some(expr.ty));
        }

        // TODO: this is unfortunate
        if let Some((int, _)) = bit_literal(expr, self.pool) {
            return Ok(Some(self.program.intern_type(TypeInfo::Int(int))));
        }
        Ok(Some(match expr.deref_mut() {
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::WipFunc(_) => return Ok(None),
            Expr::Value { ty, .. } | Expr::Raw { ty, .. } => *ty,
            Expr::Call(f, arg) => {
                if let Some(id) = f.as_fn() {
                    return Ok(self.program.funcs[id.as_index()].finished_ret);
                }

                if let Some(i) = f.expr.as_overload_set() {
                    if let Ok(fid) = self.resolve_in_overload_set(result, arg, None, i) {
                        if let Ok(Some(f_ty)) = self.infer_types(fid.at_rt()) {
                            // Need to save this because resolving overloads eats named arguments
                            f.expr = Expr::Value {
                                ty: self.program.func_type(fid.at_rt()),
                                value: fid.as_value().into(),
                            };
                            return Ok(Some(f_ty.ret));
                        }
                    }
                }

                if let Expr::GetVar(i) = f.deref_mut().deref_mut() {
                    if i.3 == VarType::Const {
                        if let Ok(fid) = self.resolve_function(result, *i, arg, None) {
                            if let Ok(Some(f_ty)) = self.infer_types(fid.at_rt()) {
                                // Need to save this because resolving overloads eats named arguments
                                f.expr = Expr::Value {
                                    ty: self.program.func_type(fid.at_rt()),
                                    value: fid.as_value().into(),
                                };
                                return Ok(Some(f_ty.ret));
                            }
                        }
                    } else if let Some(ty) = result.vars.get(i) {
                        if let TypeInfo::FnPtr(f_ty) = self.program[*ty] {
                            return Ok(Some(f_ty.ret));
                        }
                    }
                } else if let Ok(Some(ty)) = self.type_of(result, f) {
                    if let Some(ty) = self.program.fn_ty(ty) {
                        return Ok(Some(ty.ret));
                    }
                }

                return Ok(None);
            }
            Expr::Block { result: e, .. } => return self.type_of(result, e),
            Expr::Tuple(values) => {
                let types: Res<'p, Vec<_>> = values.iter_mut().map(|v| self.type_of(result, v)).collect();
                let types = types?;
                let before = types.len();
                let types: Vec<_> = types.into_iter().flatten().collect();
                self.last_loc = Some(expr.loc);
                if before != types.len() {
                    return Ok(None);
                }
                self.program.tuple_of(types)
            }
            Expr::FieldAccess(container, _) => {
                if (self.type_of(result, container)?).is_some() {
                    self.compile_expr(result, expr, None)?.ty()
                } else {
                    return Ok(None);
                }
            }
            Expr::Index { ptr, index } => {
                if let Ok(Some(container_ptr_ty)) = self.type_of(result, ptr) {
                    self.get_index_type(container_ptr_ty, unwrap!(index.as_int(), "") as usize)?
                } else {
                    return Ok(None);
                }
            }
            Expr::Closure(_) => {
                if let Ok(id) = self.promote_closure(result, expr) {
                    if self.program[id].finished_ty().is_some() {
                        return Ok(Some(self.program.func_type(id)));
                    }
                    return Ok(None);
                } else {
                    ice!("TODO: closure inference failed. need to make promote_closure non-destructive")
                }
            }
            Expr::PrefixMacro { handler, arg, .. } => {
                // TODO: short-circuit on #outputs
                // Note: need to compile first so if something's not ready yet, you dont error in the asm where you just crash.
                if handler.as_ident() == Some(Flag::As.ident()) && self.compile_expr(result, arg, Some(TypeId::ty())).is_ok() {
                    // HACK: sad that 'as' is special
                    let ty: TypeId = self.immediate_eval_expr_known(*arg.clone())?;
                    return Ok(Some(ty));
                }

                // TODO: hack yuck. now that i rely on expr working so it can be passed into quoted things, this is extra ugly.
                match self.compile_expr(result, handler, Some(TypeId::overload_set())) {
                    Ok(res) => {
                        let os = res.get()?.as_overload_set()?;
                        // Note: need to compile first so if something's not ready yet, you dont error in the asm where you just crash.
                        if self.program.overload_sets[os].name == Flag::As.ident() && self.compile_expr(result, arg, Some(TypeId::ty())).is_ok() {
                            // HACK: sad that 'as' is special
                            let ty: TypeId = self.immediate_eval_expr_known(*arg.clone())?;
                            return Ok(Some(ty));
                        }
                    }
                    Err(e) => ice!("TODO: PrefixMacro handler inference failed. need to make it non-destructive?\n{e:?}",),
                }

                // TODO: if this fails you might have changed the state.
                match self.compile_expr(result, expr, None) {
                    Ok(res) => res.ty(),
                    Err(e) => ice!("TODO: PrefixMacro inference failed. need to make it non-destructive?\n{e:?}",),
                }
            }
            &mut Expr::GetNamed(_) => return Ok(None),
            Expr::StructLiteralP(_) => return Ok(None),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = Flag::try_from(*macro_name)?;
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    Flag::Addr => match arg.deref_mut().deref_mut() {
                        Expr::GetVar(var) => {
                            let value_ty = *result.vars.get(var).expect("Missing resolved var (TODO: addr of const?)");
                            self.program.intern_type(TypeInfo::Ptr(value_ty))
                        }
                        &mut Expr::GetNamed(i) => {
                            logln!("UNDECLARED IDENT: {} (in SuffixMacro::addr)", self.pool.get(i));
                            err!(CErr::UndeclaredIdent(i))
                        }
                        _ => err!("took address of r-value {}", arg.log(self.pool)),
                    },
                    Flag::Struct | Flag::Enum | Flag::Type => self.program.intern_type(TypeInfo::Type),
                    Flag::Deref => {
                        let ptr_ty = self.type_of(result, arg)?;
                        if let Some(ptr_ty) = ptr_ty {
                            return Ok(self.program.unptr_ty(ptr_ty));
                        }
                        return Ok(None);
                    }
                    Flag::Symbol => Ident::get_type(self.program),
                    Flag::Tag => self.program.ptr_type(TypeId::i64()),
                    Flag::Fn_Ptr => {
                        if let Some(f_ty) = self.type_of(result, arg)? {
                            if let Some(f_ty) = self.program.fn_ty(f_ty) {
                                return Ok(Some(self.program.intern_type(TypeInfo::FnPtr(f_ty))));
                            }
                        }
                        return Ok(None);
                    }
                    Flag::Quote => FatExpr::get_type(self.program),
                    Flag::While => TypeId::unit(),
                    // TODO: there's no reason this couldn't look at the types, but if logic is more complicated (so i dont want to reproduce it) and might fail often (so id be afraid of it getting to a broken state).
                    Flag::If => return Ok(None),
                    _ => match self.compile_expr(result, expr, None) {
                        Ok(res) => res.ty(),
                        Err(e) => ice!("TODO: SuffixMacro inference failed. need to make it non-destructive?\n{e:?}",),
                    },
                }
            }
            Expr::GetVar(var) => {
                let ty = if var.3 == VarType::Const {
                    self.find_const_type(*var)?
                } else {
                    result.vars.get(var).cloned()
                };
                unwrap!(ty, "type check missing var {:?}", var.log(self.pool))
                //TODO: else return Ok(None)?
            }
            Expr::String(_) => String::get_type(self.program),
        }))
    }

    // TODO: this kinda sucks. it should go in a builtin generated file like libc and use the normal name resolution rules
    pub fn builtin_constant(&mut self, name: Ident<'p>) -> Option<(Value, TypeId)> {
        let name = self.pool.get(name);
        use TypeInfo::*;
        let ty = match name {
            "i64" => Some(TypeInfo::Int(IntTypeInfo { bit_count: 64, signed: true })),
            "f64" => Some(F64),
            "Type" => Some(Type),
            "bool" => Some(Bool),
            "UnknownType" => Some(TypeInfo::Unknown),
            "Never" => Some(TypeInfo::Never),
            "VoidPtr" => Some(VoidPtr),
            "OverloadSet" => Some(TypeInfo::OverloadSet),
            "ScopedBlock" => Some(TypeInfo::Scope),
            _ => None,
        };
        if let Some(ty) = ty {
            let ty = self.program.intern_type(ty);
            let tyty = TypeId::ty();
            return Some((Value::Type(ty), tyty));
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
            "FatExpr" => ffi_type!(FatExpr),
            "Var" => ffi_type!(Var),
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

    #[track_caller]
    fn infer_types_progress(&mut self, ty: &mut LazyType<'p>) -> Res<'p, bool> {
        let done = match ty {
            LazyType::EvilUnit => panic!(),
            LazyType::Infer => false,
            LazyType::PendingEval(e) => {
                let value = self.immediate_eval_expr(e.clone(), TypeId::ty())?;
                let res = self.to_type(value)?;
                *ty = LazyType::Finished(res);
                true
            }
            LazyType::Finished(_) => true, // easy
            LazyType::Different(parts) => {
                let mut done = true;
                for p in parts.iter_mut() {
                    done &= self.infer_types_progress(p)?;
                }
                if done {
                    let types = parts.iter().map(|p| p.unwrap()).collect();
                    let types = self.program.tuple_of(types);
                    *ty = LazyType::Finished(types);
                } else {
                    *ty = LazyType::Different(mem::take(parts));
                }
                done
            }
        };
        Ok(done)
    }

    fn infer_binding_progress(&mut self, binding: &mut Binding<'p>) -> Res<'p, bool> {
        self.infer_types_progress(&mut binding.ty)
    }

    fn infer_pattern(&mut self, bindings: &mut [Binding<'p>]) -> Res<'p, Vec<TypeId>> {
        let mut types = vec![];
        for arg in bindings {
            if let Some(e) = arg.ty.expr_ref() {
                self.last_loc = Some(e.loc);
            }
            assert!(self.infer_binding_progress(arg)?, "{arg:?}");
            types.push(arg.unwrap());
        }
        Ok(types)
    }

    fn infer_arg(&mut self, func: FuncId) -> Res<'p, TypeId> {
        self.ensure_resolved_sign(func)?;
        // TODO: this looks redundant but if you just check the arg multiple times you don't want to bother attempting the return type multiple times.
        if let Some(ty) = self.program[func].finished_arg {
            Ok(ty)
        } else if let Some(ty) = self.infer_types(func)? {
            Ok(ty.arg)
        } else {
            Ok(unwrap!(self.program[func].finished_arg, "arg not inferred"))
        }
    }

    // Resolve the lazy types for Arg and Ret
    // Ok(None) means return type needs to be infered
    pub(crate) fn infer_types(&mut self, func: FuncId) -> Res<'p, Option<FnType>> {
        self.ensure_resolved_sign(func)?;
        let f = &self.program[func];
        // TODO: bad things are going on. it changes behavior if this is a debug_assert.
        //       oh fuck its because of the type_of where you can backtrack if you couldn't infer.
        //       so making it work in debug with debug_assert is probably the better outcome.
        assert!(!f.evil_uninit, "{}", self.pool.get(f.name));
        if let (Some(arg), Some(ret)) = (f.finished_arg, f.finished_ret) {
            return Ok(Some(FnType { arg, ret }));
        }
        let before = self.debug_trace.len();
        let state = DebugState::ResolveFnType(func, f.name);
        self.last_loc = Some(f.loc);
        self.push_state(&state);
        if self.program[func].finished_arg.is_none() {
            let mut arg = self.program[func].arg.bindings.clone();
            let types = self.infer_pattern(&mut arg)?;
            self.program[func].arg.bindings = arg;
            let arg = self.program.tuple_of(types);
            self.program[func].finished_arg = Some(arg);
        }
        let ty = if self.program[func].finished_ret.is_none() {
            let mut ret = self.program[func].ret.clone();
            if !self.program[func].has_tag(Flag::Generic) && self.infer_types_progress(&mut ret)? {
                self.program[func].ret = ret;
                self.program[func].finished_ret = Some(self.program[func].ret.unwrap());

                Some(self.program[func].unwrap_ty())
            } else {
                None
            }
        } else {
            Some(self.program[func].unwrap_ty())
        };
        self.pop_state(state);
        let after = self.debug_trace.len();
        debug_assert!(before == after);

        Ok(ty)
    }

    fn deserialize_values<Ret: InterpSend<'p>>(&mut self, values: Values) -> Res<'p, Ret> {
        let ints = self.aarch64.constants.store_to_ints(values.vec().iter());
        Ok(unwrap!(Ret::deserialize_from_ints(&mut ints.into_iter()), ""))
    }

    pub fn immediate_eval_expr_known<Ret: InterpSend<'p>>(&mut self, mut e: FatExpr<'p>) -> Res<'p, Ret> {
        let ret_ty = Ret::get_type(self.program);
        if let Some(val) = self.check_quick_eval(&mut e, ret_ty)? {
            return self.deserialize_values(val);
        }
        let func_id = self.make_lit_function(e, ret_ty)?;
        self.call_jitted(func_id, ExecTime::Comptime, None, ())
    }

    fn check_quick_eval(&mut self, e: &mut FatExpr<'p>, ret_ty: TypeId) -> Res<'p, Option<Values>> {
        match e.deref_mut() {
            Expr::Value { value, .. } => return Ok(Some(value.clone())),
            Expr::GetVar(var) => {
                if let Some((value, _)) = self.find_const(*var)? {
                    return Ok(Some(value));
                } else {
                    // TODO: -- Apr 24 I think this is always the problem. but what changed??
                    println!("comptime eval const not ready {}", var.log(self.pool));
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
                // TODO: this only helps if some can be quick-evaled by special cases above, otherwise makes it worse.
                let values: Res<'p, Vec<Values>> = elements
                    .iter()
                    .zip(types)
                    .map(|(e, ty)| self.immediate_eval_expr(e.clone(), ty))
                    .collect();
                let mut pls = vec![];
                for v in values? {
                    for v in v.vec() {
                        pls.push(v);
                    }
                }
                return Ok(Some(pls.into()));
            }
            Expr::Call(f, arg) => {
                // !slice and !addr can't be const evaled!
                if !matches!(arg.expr, Expr::SuffixMacro(_, _)) {
                    let f_id = if let Expr::GetVar(var) = f.expr {
                        if let Some((val, _)) = self.find_const(var)? {
                            match val {
                                Values::One(Value::GetFn(f)) => Some(f),
                                Values::One(Value::OverloadSet(f)) => {
                                    let ol = &self.program.overload_sets[f];
                                    if ol.pending.is_empty() && ol.ready.len() == 1 {
                                        Some(ol.ready[0].func)
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            }
                        } else {
                            None
                        }
                    } else {
                        f.as_fn()
                    };

                    if let Some(f) = f_id {
                        debug_assert!(!self.program[f].evil_uninit);
                        // TODO: try to compile it now.
                        // TODO: you do want to allow self.program[f].has_tag(Flag::Ct) but dont have the result here and cant tell which ones need it
                        let is_ready = self.program[f].comptime_addr.is_some() || (self.ready.ready.len() > f.as_index() && self.ready[f].is_some());
                        if is_ready && !self.program[f].any_const_args() {
                            // currently this mostly just helps with a bunch of SInt/Unique/UInt calls on easy constants at the beginning.
                            let arg_ty = self.program[f].finished_arg.unwrap();
                            let arg = self.immediate_eval_expr(mem::take(arg), arg_ty)?;
                            return Ok(Some(self.run(f, arg, ExecTime::Comptime, None)?));
                        }
                        // fallthrough
                    }
                    // fallthrough
                    // println!("{}", f.log(self.pool));
                }

                // fallthrough
            }
            // TODO: @enum field access
            _ => {} // fallthrough
        }
        Ok(None)
    }

    fn make_lit_function(&mut self, e: FatExpr<'p>, ret_ty: TypeId) -> Res<'p, FuncId> {
        debug_assert!(!(e.as_suffix_macro(Flag::Slice).is_some() || e.as_suffix_macro(Flag::Addr).is_some()));
        unsafe { STATS.make_lit_fn += 1 };
        let name = format!("$eval_{}${}$", self.anon_fn_counter, e.deref().log(self.pool));
        let (arg, ret) = Func::known_args(TypeId::unit(), ret_ty, e.loc);
        let mut fake_func = Func::new(self.pool.intern(&name), arg, ret, Some(e.clone()), e.loc, false, false);
        debug_assert!(fake_func.local_constants.is_empty());
        fake_func.resolved_body = true;
        fake_func.resolved_sign = true;
        fake_func.finished_arg = Some(TypeId::unit());
        fake_func.finished_ret = Some(ret_ty);
        fake_func.scope = Some(ScopeId::from_index(0));
        self.anon_fn_counter += 1;
        if self.ready.sizes.slot_count(self.program, ret_ty) > 1 && self.program.comptime_arch == TargetArch::Aarch64 {
            // println!("imm_eval as flat_call for ret {}", self.program.log_type(ret_ty));
            // TODO: my c_call can't handle aggragate returns
            fake_func.add_tag(Flag::Flat_Call);
            fake_func.add_tag(Flag::Ct); // not really needed but flat_call always does
        }
        let func_id = self.program.add_func(fake_func);
        logln!("Made anon: {func_id:?} = {}", self.program[func_id].log(self.pool));
        self.compile(func_id, ExecTime::Comptime)?;
        Ok(func_id)
    }

    // Here we're not in the context of a specific function so the caller has to pass in the constants in the environment.
    pub fn immediate_eval_expr(&mut self, mut e: FatExpr<'p>, ret_ty: TypeId) -> Res<'p, Values> {
        if let Some(values) = self.check_quick_eval(&mut e, ret_ty)? {
            return Ok(values);
        }
        let func_id = self.make_lit_function(e, ret_ty)?;
        self.run(func_id, Value::Unit.into(), ExecTime::Comptime, None)
    }

    #[track_caller]
    fn to_type(&mut self, value: Values) -> Res<'p, TypeId> {
        self.program.to_type(value)
    }

    pub(crate) fn add_func(&mut self, func: Func<'p>) -> Res<'p, FuncId> {
        // TODO: make this less trash. it fixes generics where it thinks a cpatured argument is var cause its arg but its actually in consts because generic.
        for capture in &func.capture_vars {
            assert!(capture.3 != VarType::Const);
        }
        let scope = func.scope.unwrap();
        let id = self.program.add_func(func);
        self[scope].funcs.push(id);
        Ok(id)
    }

    // TODO: calling this in infer is wrong because it might fail and lose the function
    pub fn promote_closure(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>) -> Res<'p, FuncId> {
        if let Expr::Closure(func) = expr.deref_mut() {
            let scope = func.scope.unwrap();
            let f = self.add_func(mem::take(func))?;
            self[scope].funcs.push(f);
            self.ensure_resolved_sign(f)?;

            if self.infer_types(f)?.is_none() {
                // TODO: i only do this for closures becuase its a pain to thread the &mut result through everything that calls infer_types().
                mut_replace!(self.program[f], |mut func: Func<'p>| {
                    if let Some(body) = &mut func.body {
                        // closures aren't lazy currently.
                        debug_assert!(func.resolved_body);
                        // TODO: this is very suspisious! what if it has captures
                        let res = self.type_of(result, body);
                        debug_assert!(res.is_ok()); // clearly its fine tho...
                        if let Some(ret_ty) = res? {
                            func.finished_ret = Some(ret_ty);
                        }
                    }
                    Ok((func, ()))
                });
            }
            expr.expr = self.func_expr(f);
            Ok(f)
        } else {
            ice!("want closure")
        }
    }

    fn finish_closure(&mut self, expr: &mut FatExpr<'p>) {
        if let Expr::WipFunc(id) = expr.expr {
            expr.expr = self.func_expr(id);
        }
    }

    // TODO: adding inline to the func is iffy now that i dont require it to be an inline closure. it will affect other callsites. only want that if this is the only one.
    // TODO: it still doesnt allow the funcs to be any expr because i dont compile them first.
    fn emit_call_if(
        &mut self,
        result: &mut FnWip<'p>,
        if_macro_arg: &mut FatExpr<'p>,
        requested: Option<TypeId>, // TODO: allow giving return type to infer
    ) -> Res<'p, Structured> {
        // if !arg.ty.is_unknown() {
        //     // We've been here before and already replaced closures with calls.
        //     return Ok(Structured::RuntimeOnly(arg.ty));
        // }
        let unit = TypeId::unit();
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        let mut unit_expr = FatExpr::synthetic(Expr::unit(), if_macro_arg.loc);
        if let Expr::Tuple(parts) = if_macro_arg.deref_mut() {
            let cond = self.compile_expr(result, &mut parts[0], Some(TypeId::bool()))?;
            self.type_check_arg(cond.ty(), TypeId::bool(), "bool cond")?;

            // If its constant, don't even bother emitting the other branch
            // TODO: option to toggle this off for testing.
            if let Structured::Const(_, val) = cond {
                let cond = if let Value::Bool(f) = val.single()? {
                    f
                } else {
                    err!("expected !if cond: bool",)
                };
                let cond_index = if cond { 1 } else { 2 };
                let other_index = if cond { 2 } else { 1 };
                if let Some(branch_body) = self.maybe_direct_fn(result, &mut parts[cond_index], &mut unit_expr, requested)? {
                    let branch_body = branch_body.single()?;
                    let branch_arg = self.infer_arg(branch_body)?;
                    self.type_check_arg(branch_arg, unit, sig)?;
                    self.program[branch_body].add_tag(Flag::Inline);
                    let res = self.emit_call_on_unit(result, branch_body, &mut parts[cond_index], requested)?;
                    assert!(self.program[branch_body].finished_ret.is_some());
                    // TODO: fully dont emit the branch
                    let unit = FatExpr::synthetic(Expr::unit(), parts[other_index].loc);
                    parts[other_index].expr = Expr::SuffixMacro(Flag::Unreachable.ident(), Box::new(unit));
                    parts[other_index].ty = TypeId::never();
                    return Ok(res);
                } else {
                    ice!("!if arg must be func not {:?}", parts[cond_index]);
                }
            }

            let (true_ty, expect_fn) = if let Some(if_true) = self.maybe_direct_fn(result, &mut parts[1], &mut unit_expr, requested)? {
                let if_true = if_true.single()?;
                self.program[if_true].add_tag(Flag::Inline);
                let true_arg = self.infer_arg(if_true)?;
                self.type_check_arg(true_arg, unit, sig)?;
                (self.emit_call_on_unit(result, if_true, &mut parts[1], requested)?.ty(), true)
            } else if parts[1].ty.is_unknown() {
                ice!("if second arg must be func not {}", parts[1].log(self.pool));
            } else {
                (parts[1].ty, false)
            };
            if expect_fn {
                if let Some(if_false) = self.maybe_direct_fn(result, &mut parts[2], &mut unit_expr, requested.or(Some(true_ty)))? {
                    let if_false = if_false.single()?;
                    self.program[if_false].add_tag(Flag::Inline);
                    let false_arg = self.infer_arg(if_false)?;
                    self.type_check_arg(false_arg, unit, sig)?;
                    let false_ty = self.emit_call_on_unit(result, if_false, &mut parts[2], requested)?.ty();
                    self.type_check_arg(true_ty, false_ty, sig)?;
                } else {
                    ice!("if third arg must be func not {:?}", parts[2]);
                }
                self.finish_closure(&mut parts[1]);
                self.finish_closure(&mut parts[2]);
            } else {
                assert!(!parts[2].ty.is_unknown());
                self.type_check_arg(true_ty, parts[2].ty, sig)?;
            }
            Ok(Structured::RuntimeOnly(true_ty))
        } else {
            ice!("if args must be tuple not {:?}", if_macro_arg);
        }
    }

    fn emit_call_while(&mut self, result: &mut FnWip<'p>, while_macro_arg: &mut FatExpr<'p>) -> Res<'p, Structured> {
        if !while_macro_arg.ty.is_unknown() {
            // We've been here before and already replaced closures with calls.
            return Ok(Structured::RuntimeOnly(TypeId::unit()));
        }

        let sig = "while(fn(Unit) bool, fn(Unit) Unit)";
        let mut unit_expr = FatExpr::synthetic(Expr::unit(), while_macro_arg.loc);
        if let Expr::Tuple(parts) = while_macro_arg.deref_mut() {
            if let Some(cond_fn) = self.maybe_direct_fn(result, &mut parts[0], &mut unit_expr, Some(TypeId::bool()))? {
                let cond_fn = cond_fn.single()?;
                self.program[cond_fn].add_tag(Flag::Inline);
                let cond_arg = self.infer_arg(cond_fn)?;
                self.type_check_arg(cond_arg, TypeId::unit(), sig)?;
                let cond_ret = self.emit_call_on_unit(result, cond_fn, &mut parts[0], None)?.ty();
                self.type_check_arg(cond_ret, TypeId::bool(), sig)?;
            } else {
                unwrap!(parts[0].as_fn(), "while first arg must be func not {:?}", parts[0]);
                todo!("shouldnt get here twice")
            }
            if let Some(body_fn) = self.maybe_direct_fn(result, &mut parts[1], &mut unit_expr, Some(TypeId::unit()))? {
                let body_fn = body_fn.single()?;
                self.program[body_fn].add_tag(Flag::Inline);
                let body_arg = self.infer_arg(body_fn)?;
                self.type_check_arg(body_arg, TypeId::unit(), sig)?;
                let body_ret = self.emit_call_on_unit(result, body_fn, &mut parts[1], None)?.ty();
                self.type_check_arg(body_ret, TypeId::unit(), sig)?;
            } else {
                unwrap!(parts[1].as_fn(), "while second arg must be func not {:?}", parts[1]);
                todo!("shouldnt get here twice")
            }
            self.finish_closure(&mut parts[0]);
            self.finish_closure(&mut parts[1]);
            while_macro_arg.ty = TypeId::unit();
        } else {
            ice!("if args must be tuple not {:?}", while_macro_arg);
        }

        Ok(Structured::RuntimeOnly(TypeId::unit()))
    }

    #[track_caller]
    pub fn type_check_arg(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        // TODO: dont do this. fix ffi types.
        let found = self.program.raw_type(found);
        let expected = self.program.raw_type(expected);

        debug_assert!(!found.is_any() && !expected.is_any(), "wip removing Any");

        if found == expected {
            Ok(())
        } else {
            match (&self.program[found], &self.program[expected]) {
                // :Coercion // TODO: only one direction makes sense
                (TypeInfo::Never, _) | (_, TypeInfo::Never) => return Ok(()),
                (TypeInfo::Int(a), TypeInfo::Int(b)) => {
                    // :Coercion
                    if a.bit_count == b.bit_count || a.bit_count == 64 || b.bit_count == 64 {
                        return Ok(()); // TODO: not this!
                    }
                }
                (TypeInfo::Tuple(f), TypeInfo::Tuple(e)) => {
                    if f.len() == e.len() {
                        let ok = f.iter().zip(e.iter()).all(|(f, e)| self.type_check_arg(*f, *e, msg).is_ok());
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
                (TypeInfo::Ptr(_), TypeInfo::VoidPtr) | (TypeInfo::VoidPtr, TypeInfo::Ptr(_)) => return Ok(()),
                (TypeInfo::FnPtr(_), TypeInfo::VoidPtr) | (TypeInfo::VoidPtr, TypeInfo::FnPtr(_)) => return Ok(()),
                (TypeInfo::Tuple(found_elements), TypeInfo::Type) => {
                    // :Coercion
                    if found_elements.iter().all(|e| self.type_check_arg(*e, TypeId::ty(), msg).is_ok()) {
                        return Ok(());
                    }
                }

                (TypeInfo::Type, TypeInfo::Tuple(_expected_elements)) => {
                    err!("TODO: expand out a tuple type?",)
                }

                (TypeInfo::Unit, TypeInfo::Type) => {
                    // :Coercion
                    // TODO: more consistant handling of empty tuple?
                    return Ok(());
                }
                // TODO: correct varience
                // TODO: calling convention for FnPtr
                (&TypeInfo::Fn(f), &TypeInfo::Fn(e)) | (&TypeInfo::FnPtr(f), &TypeInfo::FnPtr(e)) => {
                    if self.type_check_arg(f.arg, e.arg, msg).is_ok() && self.type_check_arg(f.ret, e.ret, msg).is_ok() {
                        return Ok(());
                    }
                }
                (&TypeInfo::OverloadSet, &TypeInfo::Fn(_)) => {
                    // :Coercion
                    // scary because relies on custom handling for constdecls?
                    return Ok(());
                }
                _ => {}
            }
            err!(CErr::TypeCheck(found, expected, msg))
        }
    }

    fn struct_type(&mut self, pattern: &mut Pattern<'p>) -> Res<'p, TypeInfo<'p>> {
        // TODO: maybe const keyword before name in func/struct lets you be generic.
        let types = self.infer_pattern(&mut pattern.bindings)?;
        let raw_fields = pattern.flatten();

        let as_tuple = self.program.tuple_of(types);
        let mut fields = vec![];
        for (name, ty, kind) in raw_fields {
            assert_ne!(kind, VarType::Const, "todo");
            fields.push(Field {
                name: unwrap!(name, "field name").0,
                ty,
                ffi_byte_offset: None,
            });
        }
        Ok(TypeInfo::simple_struct(fields, as_tuple))
    }

    fn set_deref(&mut self, result: &mut FnWip<'p>, place: &mut FatExpr<'p>, value: &mut FatExpr<'p>) -> Res<'p, ()> {
        match place.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                assert_eq!(var.3, VarType::Var, "Only 'var' can be reassigned (not let/const).");
                let oldty = result.vars.get(var);
                let oldty = *unwrap!(oldty, "SetVar: var must be declared: {}", var.log(self.pool));

                let value = self.compile_expr(result, value, Some(oldty))?;
                self.type_check_arg(value.ty(), oldty, "reassign var")?;
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: type checking
                // TODO: general place expressions.
                if let Ok(Flag::Deref) = Flag::try_from(*macro_name) {
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

    fn field_access_expr(&mut self, result: &mut FnWip<'p>, container_ptr: &mut FatExpr<'p>, name: Ident<'p>) -> Res<'p, usize> {
        let container_ptr = self.compile_expr(result, container_ptr, None)?;
        let container_ptr_ty = self.program.raw_type(container_ptr.ty());
        let depth = self.program.ptr_depth(container_ptr_ty);
        assert_eq!(
            depth,
            1,
            "index expr ptr must be one level of indirection. {:?} {:?}",
            self.program.log_type(container_ptr_ty),
            container_ptr
        );
        let container_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "",);

        let raw_container_ty = self.program.raw_type(container_ty);
        match &self.program[raw_container_ty] {
            TypeInfo::Struct { fields, .. } => {
                for (i, f) in fields.iter().enumerate() {
                    if f.name == name {
                        return Ok(i);
                    }
                }
                err!("unknown name {} on {:?}", self.pool.get(name), self.program.log_type(container_ty));
            }
            TypeInfo::Enum { cases, .. } => {
                for (i, (f_name, _)) in cases.iter().enumerate() {
                    if *f_name == name {
                        return Ok(i);
                    }
                }
                err!("unknown name {} on {:?}", self.pool.get(name), self.program.log_type(container_ty));
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.program.log_type(container_ty),
                self.program.log_type(raw_container_ty)
            ),
        }
    }

    fn index_expr(&mut self, container_ptr: Structured, index: usize) -> Res<'p, Structured> {
        let container_ptr_ty = self.program.raw_type(container_ptr.ty());
        let depth = self.program.ptr_depth(container_ptr_ty);
        assert_eq!(depth, 1, "index expr ptr must be one level of indirection");
        let container_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "");
        let mut raw_container_ty = self.program.raw_type(container_ty);

        if let TypeInfo::Struct { as_tuple, fields, .. } = &self.program[raw_container_ty] {
            // A struct with one field will have its as_tuple be another struct.
            if let TypeInfo::Struct { .. } = self.program[*as_tuple] {
                // There should only be one field
                assert_eq!(fields.len(), 1);
                assert_eq!(index, 0);
                // TODO: this is just a noop type cast, should update the ast.
                let ty = self.program.ptr_type(fields[0].ty);
                return Ok(match container_ptr {
                    Structured::Const(_, v) => Structured::Const(ty, v),
                    Structured::RuntimeOnly(_) => Structured::RuntimeOnly(ty),
                    _ => unreachable!(),
                });
            }
            raw_container_ty = *as_tuple;
        }

        if let TypeInfo::Enum { cases } = &self.program[raw_container_ty] {
            let ty = cases[index].1;
            let ty = self.program.ptr_type(ty);
            return Ok(Structured::RuntimeOnly(ty));
        }

        if let TypeInfo::Tuple(types) = &self.program[raw_container_ty] {
            let mut count = 0;
            for (i, f_ty) in types.clone().iter().enumerate() {
                if i == index {
                    let ty = self.program.ptr_type(*f_ty);
                    // const eval lets me do enum fields in asm without doign heap values first.
                    if let &Structured::Const(
                        _,
                        Values::One(Value::Heap {
                            value,
                            physical_first,
                            physical_count,
                        }),
                    ) = &container_ptr
                    {
                        let size = self.ready.sizes.slot_count(self.program, *f_ty);
                        assert!(physical_count >= size);
                        let value = Values::One(Value::Heap {
                            value,
                            physical_first: physical_first + count,
                            physical_count: size,
                        });
                        let s = Structured::Const(ty, value);
                        return Ok(s);
                    }
                    return Ok(Structured::RuntimeOnly(ty));
                }
                count += self.ready.sizes.slot_count(self.program, *f_ty);
            }
            err!("unknown index {index} on {:?}", self.program.log_type(container_ty));
        } else {
            err!("Only tuples support index expr, not {:?}", self.program[raw_container_ty])
        }
    }

    fn get_index_type(&mut self, container_ptr_ty: TypeId, index: usize) -> Res<'p, TypeId> {
        let container_ty = unwrap!(
            self.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.program.log_type(container_ptr_ty)
        );

        let raw_container_ty = self.program.raw_type(container_ty);
        match &self.program[raw_container_ty] {
            TypeInfo::Tuple(types) => Ok(self.program.ptr_type(types[index])),
            TypeInfo::Struct { fields, .. } => Ok(self.program.ptr_type(fields[index].ty)),
            TypeInfo::Enum { cases, .. } => Ok(self.program.ptr_type(cases[index].1)),
            TypeInfo::Unique(_, _) => unreachable!(),
            _ => err!(
                "only tuple/struct/enum/ support field access but found {}",
                self.program.log_type(container_ty)
            ),
        }
    }

    fn produce_tuple(&mut self, owned_values: Vec<Structured>, tuple_ty: TypeId) -> Res<'p, Structured> {
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

    // the bool return is did_inline which will be banned if its a Split FuncRef.
    fn compile_call(
        &mut self,
        result: &mut FnWip<'p>,
        expr: &mut FatExpr<'p>,
        mut fid: FuncId,
        requested: Option<TypeId>,
    ) -> Res<'p, (Structured, bool)> {
        let loc = expr.loc;
        let (f_expr, arg_expr) = if let Expr::Call(f, arg) = expr.deref_mut() { (f, arg) } else { ice!("") };
        let func = &self.program[fid];
        let is_comptime = func.has_tag(Flag::Comptime);
        if is_comptime {
            let (ret_val, ret_ty) = self.emit_comptime_call(result, fid, arg_expr)?;
            let ty = requested.unwrap_or(ret_ty); // TODO: make sure someone else already did the typecheck.
            assert!(!ty.is_unknown());
            expr.expr = Expr::Value { ty, value: ret_val.clone() };
            return Ok((Structured::Const(ty, ret_val), true));
        }

        self.ensure_resolved_sign(fid)?;
        let arg_ty = self.infer_arg(fid)?;
        let mut arg_val = self.compile_expr(result, arg_expr, Some(arg_ty))?;
        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
        if arg_val.is_empty() {
            arg_val = self.program.load_value(Value::Unit);
        }
        self.last_loc = Some(loc);
        self.type_check_arg(arg_val.ty(), arg_ty, "fn arg")?;

        // TODO: you really want to compile as much of the body as possible before you start baking things.
        let any_const_args = self.program[fid].any_const_args();

        // TODO: if its a pure function you might want to do the call at comptime
        // TODO: make sure I can handle this as well as Nim: https://news.ycombinator.com/item?id=31160234
        if any_const_args {
            fid = self.curry_const_args(fid, f_expr, arg_expr, arg_val)?;
        }

        let func = &self.program[fid];
        // TODO: some heuristic based on how many times called and how big the body is.
        let force_inline = func.has_tag(Flag::Inline);
        let deny_inline = func.has_tag(Flag::NoInline);
        assert!(!(force_inline && deny_inline), "{fid:?} is both @inline and @noinline");
        let will_inline = force_inline || !func.capture_vars.is_empty();
        assert!(!(will_inline && deny_inline), "{fid:?} has captures but is @noinline");

        let func = &self.program[fid];
        if will_inline && func.body.is_some() {
            // TODO: check that you're calling from the same place as the definition.
            Ok((self.emit_capturing_call(result, fid, expr)?, true))
        } else {
            let res = self.emit_runtime_call(result, fid, arg_expr)?;
            // Since we've called it, we must know the type by now.
            // TODO: cope with emit_runtime_call baking const args, needs to change the arg expr
            let ty = self.program.func_type(fid);
            f_expr.expr = Expr::Value {
                ty,
                value: Value::GetFn(fid).into(),
            };
            f_expr.ty = ty;
            Ok((res, false))
        }
    }

    fn curry_const_args(
        &mut self,
        original_f: FuncId,
        f_expr: &mut FatExpr<'p>,
        arg_expr: &mut FatExpr<'p>,
        mut arg_val: Structured,
    ) -> Res<'p, FuncId> {
        self.program[original_f].assert_body_not_resolved()?;
        let state = DebugState::Msg(format!("Bake CT Only {original_f:?}"));
        self.push_state(&state);
        // Some part of the argument must be known at comptime.
        // You better hope compile_expr noticed and didn't put it in a stack slot.
        let mut new_func = self.program[original_f].clone();
        debug_assert!(new_func.local_constants.is_empty());
        new_func.referencable_name = false;
        let scope = new_func.scope.unwrap();
        let new_fid = self.program.add_func(new_func);
        self[scope].funcs.push(new_fid);
        self.ensure_resolved_sign(new_fid)?;
        self.ensure_resolved_body(new_fid)?;
        let func = &self.program[new_fid];
        let pattern = func.arg.flatten();

        if pattern.len() == 1 {
            let (name, _, kind) = pattern.into_iter().next().unwrap();
            debug_assert_eq!(kind, VarType::Const);
            let name = unwrap!(name, "arg needs name (unreachable?)");
            self.bind_const_arg(new_fid, name, arg_val)?;
            arg_expr.expr = Expr::unit();
            arg_expr.ty = TypeId::unit();

            let ty = self.program.func_type(new_fid);
            f_expr.expr = Expr::Value {
                ty,
                value: Value::GetFn(new_fid).into(),
            };
            f_expr.ty = ty;
            self.pop_state(state);
            Ok(new_fid)
        } else {
            if let Structured::Const(ty, values) = arg_val {
                assert_eq!(
                    pattern.len(),
                    values.len(),
                    "TODO: non-trivial pattern matching\n{:?} <= {:?} for call to {:?}",
                    pattern,
                    values,
                    func.synth_name(self.pool)
                );
                arg_val = Structured::TupleDifferent(
                    ty,
                    values
                        .vec()
                        .into_iter()
                        .zip(pattern.iter())
                        .map(|(val, (_, ty, _))| Structured::Const(*ty, Values::One(val)))
                        .collect(),
                )
            }

            match arg_val {
                Structured::RuntimeOnly(_) | Structured::Emitted(_, _) => ice!("const arg but {:?} is only known at runtime.", arg_val),
                Structured::Const(_, _) => unreachable!(),
                Structured::TupleDifferent(_, arg_values) => {
                    // TODO: Really what you want is to cache the newly baked function so it can be reused if called multiple times but I don't do that yet.
                    assert_eq!(
                        pattern.len(),
                        arg_values.len(),
                        "TODO: non-trivial pattern matching\n{:?} <= {:?} for call to {:?}",
                        pattern,
                        arg_values,
                        func.synth_name(self.pool)
                    );
                    let arg_exprs = if let Expr::Tuple(v) = arg_expr.deref_mut() {
                        v
                    } else {
                        err!("TODO: pattern match on non-tuple",)
                    };
                    assert_eq!(arg_exprs.len(), pattern.len(), "TODO: non-tuple baked args");
                    let mut skipped_args = vec![];
                    let mut skipped_types = vec![];
                    let mut removed = 0;
                    for (i, ((name, ty, kind), arg_value)) in pattern.into_iter().zip(arg_values).enumerate() {
                        if kind == VarType::Const {
                            let name = unwrap!(name, "arg needs name (unreachable?)");
                            // bind_const_arg handles adding closure captures.
                            // since it needs to do a remap, it gives back the new argument names so we can adjust our bindings acordingly. dont have to deal with it above since there's only one.
                            self.bind_const_arg(new_fid, name, arg_value)?;
                            // TODO: this would be better if i was iterating backwards
                            arg_exprs.remove(i - removed);
                            removed += 1; // TODO: this sucks
                        } else {
                            skipped_args.push(arg_value);
                            skipped_types.push(ty);
                        }
                    }
                    let arg_ty = self.program.tuple_of(skipped_types);
                    debug_assert_ne!(new_fid, original_f);
                    if let Expr::Tuple(v) = arg_expr.deref_mut().deref_mut() {
                        if v.is_empty() {
                            // Note: this started being required when I added fn while.
                            arg_expr.expr = Expr::unit();
                        } else if v.len() == 1 {
                            *arg_expr = mem::take(v.iter_mut().next().unwrap());
                        }
                    }
                    // We might have compiled the arg when resolving the call so we'd save the type but it just changed because some were baked.
                    // Symptom of forgetting this was emit_bc passing extra uninit args.
                    arg_expr.ty = arg_ty;

                    let ty = self.program.func_type(new_fid);
                    f_expr.expr = Expr::Value {
                        ty,
                        value: Value::GetFn(new_fid).into(),
                    };
                    f_expr.ty = ty;
                    let f_ty = self.program.fn_ty(ty).unwrap();
                    self.type_check_arg(arg_ty, f_ty.arg, "sanity: post bake arg")?;
                    self.pop_state(state);
                    // Don't need to explicitly force capturing because bind_const_arg added them if any args were closures.
                    Ok(new_fid)
                }
            }
        }
    }

    fn emit_call_on_unit(
        &mut self,
        result: &mut FnWip<'p>,
        cond_fn: FuncId,
        expr_out: &mut FatExpr<'p>,
        requested: Option<TypeId>,
    ) -> Res<'p, Structured> {
        let get_fn = FatExpr::synthetic(self.func_expr(cond_fn), expr_out.loc);
        let unit = FatExpr::synthetic(Expr::unit(), expr_out.loc);
        expr_out.expr = Expr::Call(Box::new(get_fn), Box::new(unit));
        self.compile_expr(result, expr_out, requested)
    }

    // TODO: debug warning for misuse of #inline and #one_ret_pic
    /// The first two can be used for early bootstrapping since they just look at the ast without comptime eval.
    /// - tuple of string literals -> llvm-ir
    /// - tuple of 32-bit int literals -> aarch64 asm ops
    /// - anything else, comptime eval expecting Slice(u32) -> aarch64 asm ops
    fn inline_asm_body(&mut self, f: FuncId, asm: &mut FatExpr<'p>) -> Res<'p, ()> {
        self.ensure_resolved_body(f)?;

        // TODO: assert f has an arch and a calling convention annotation (I'd rather make people be explicit just guess, even if you can always tell arch).

        let ops = if let Expr::Tuple(parts) = asm.deref_mut().deref_mut() {
            // TODO: allow quick const eval for single expression of correct type instead of just tuples.
            // TODO: annotations to say which target you're expecting.
            let llvm_ir: Option<Vec<Ident<'p>>> = parts
                .iter()
                .map(|op| if let Expr::String(op) = op.expr { Some(op) } else { None })
                .collect();
            if llvm_ir.is_some() {
                self.program[f].add_tag(Flag::Llvm);
                self.program[f].llvm_ir = llvm_ir;
                self.program.inline_llvm_ir.push(f);
                return Ok(());
            }
            let asm_bytes: Option<Vec<u32>> = parts
                .iter()
                .map(|op| bit_literal(op, self.pool).and_then(|(ty, val)| if ty.bit_count == 32 { Some(val as u32) } else { None }))
                .collect();

            if let Some(ops) = asm_bytes {
                ops
            } else {
                // TODO: support dynamic eval to string for llvm ir.
                self.immediate_eval_expr_known(asm.clone())?
            }
        } else {
            let ops: Vec<u32> = self.immediate_eval_expr_known(asm.clone())?;
            ops
        };
        self.program[f].add_tag(Flag::Aarch64);
        // TODO: emit into the Jitted thing instead of this.
        //       maybe just keep the vec, defer dealing with it and have bc_to_asm do it?
        self.program[f].jitted_code = Some(ops.clone());

        Ok(())
    }

    fn construct_struct(&mut self, result: &mut FnWip<'p>, requested: Option<TypeId>, pattern: &mut Pattern<'p>) -> Res<'p, Structured> {
        let requested = unwrap!(requested, "struct literal needs type hint");
        let names: Vec<_> = pattern.flatten_names();
        Ok(mut_replace!(*pattern, |mut pattern: Pattern<'p>| {
            // TODO: why must this suck so bad
            let values: Option<_> = pattern.flatten_exprs_mut();
            let mut values: Vec<_> = values.unwrap();
            assert_eq!(names.len(), values.len());
            let raw_container_ty = self.program.raw_type(requested);

            let res = match self.program[raw_container_ty].clone() {
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
                        assert_eq!(name, field.name, "{} vs {}", self.pool.get(name), self.pool.get(field.name));
                        let value = self.compile_expr(result, value, Some(field.ty))?;
                        self.type_check_arg(value.ty(), field.ty, "struct field")?;
                        values.push(value.unchecked_cast(field.ty));
                    }

                    Structured::RuntimeOnly(requested)
                }
                TypeInfo::Enum { cases, .. } => {
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
                _ => err!(
                    "struct literal {pattern:?} but expected {:?} = {}",
                    requested,
                    self.program.log_type(requested)
                ),
            };
            Ok((pattern, res))
        }))
    }

    #[allow(clippy::too_many_arguments)]
    fn decl_var(
        &mut self,
        result: &mut FnWip<'p>,
        name: Var<'p>,
        ty: &mut LazyType<'p>,
        value: &mut Option<FatExpr<'p>>,
        kind: VarType,
        _annotations: &[Annotation<'p>],
        loc: Span,
    ) -> Res<'p, ()> {
        let no_type = matches!(ty, LazyType::Infer);
        self.infer_types_progress(ty)?;

        let value = match value {
            Some(value) => value,
            None => {
                err!(
                    "binding {} requires a value (use unsafe '()!uninitilized' if thats what you really want)",
                    name.log(self.pool)
                );
            }
        };

        match kind {
            VarType::Const => self.decl_const(name, ty, value, loc)?,
            VarType::Let | VarType::Var => {
                if kind == VarType::Let && value.expr.as_suffix_macro(Flag::Uninitialized).is_some() {
                    let name = self.pool.get(name.0);
                    err!("let bindings cannot be reassigned so '{name}' cannot be !uninitialized",)
                }
                let value = self.compile_expr(result, value, ty.ty())?;
                let final_ty = if no_type {
                    *ty = LazyType::Finished(value.ty());
                    value.ty()
                } else {
                    self.type_check_arg(value.ty(), ty.unwrap(), "var decl")?;
                    ty.unwrap()
                };

                // TODO: this is so emit_ir which can't mutate program can find it.
                self.program.intern_type(TypeInfo::Ptr(final_ty));

                let prev = result.vars.insert(name, final_ty);
                assert!(prev.is_none() || prev.unwrap() == final_ty);
                // TODO: should always be none?? but its not a constant and seems to always be the same so its probablby not a super huge deal? -- Apr 23
                //       maybe its just cause im not zeroing the stmt and end up compiling multiple times. -- Apr 25
            }
        }
        Ok(())
    }

    fn decl_const(&mut self, name: Var<'p>, ty: &mut LazyType<'p>, value: &mut FatExpr<'p>, loc: Span) -> Res<'p, ()> {
        // TODO: doing the check here every time is sad.
        if value.expr.as_suffix_macro(Flag::Uninitialized).is_some() {
            let name = self.pool.get(name.0);
            err!("const bindings cannot be reassigned so '{name}' cannot be '()!uninitialized'",)
        }
        // You don't need to precompile, immediate_eval_expr will do it for you.
        // However, we want to update value.ty on our copy to use below to give constant pointers better type inference.
        // This makes addr_of const for @enum work
        //
        let mut r = self.empty_fn(ExecTime::Comptime, FuncId::from_index(99999999), loc);
        let res = self.compile_expr(&mut r, value, ty.ty())?;
        let mut val = self.immediate_eval_expr(value.clone(), res.ty())?;
        // TODO: clean this up. all the vardecl stuff is kinda messy and I need to be able to reuse for the pattern matching version.
        // TODO: more efficient way of handling overload sets
        // TODO: better syntax for inserting a function into an imported overload set.
        if let Values::One(Value::OverloadSet(i)) = val {
            if let Some(ty) = ty.ty() {
                if ty != TypeId::overload_set() {
                    // TODO: fn name instead of var name in messages?
                    let f_ty = unwrap!(
                        self.program.fn_ty(ty),
                        "const {} OverloadSet must have function type not {:?}",
                        name.log(self.pool),
                        ty
                    );

                    self.compute_new_overloads(i)?;
                    // TODO: just filter the iterator.
                    let mut overloads = self.program.overload_sets[i].clone(); // sad

                    overloads
                        .ready
                        .retain(|f| f.arg == f_ty.arg && (f.ret.is_none() || f.ret.unwrap() == f_ty.ret));
                    // TODO: You can't just filter here anymore because what if its a Split FuncRef.
                    let found = match overloads.ready.len() {
                        0 => err!("Missing overload",),
                        1 => overloads.ready[0].func,
                        _ => err!("Ambigous overload \n{:?}", overloads.ready),
                    };
                    val = Values::One(Value::GetFn(found));
                }
            } else {
                // TODO: make sure its not a problem that usage in an expression can end up as different types each time
                *ty = LazyType::Finished(TypeId::overload_set())
            }
        }

        // let found_ty = self.program.type_of_raw(&val);
        let final_ty = if let Some(expected_ty) = ty.ty() {
            if self.program[expected_ty] == TypeInfo::Type {
                // HACK. todo: general overloads for cast()
                val = Value::Type(self.to_type(val)?).into()
            } else {
                // TODO: you want the type check but doing it against type_of_raw is kinda worthless
                // like const a: *i64 = @as(VoidPtr) malloc(8); should be fine? but i have to serialize that as an int and then type_of_raw is wrong.
                // but i do need to think about how memory makes its way into your exe.
                // self.type_check_arg(found_ty, expected_ty, "const decl")?;
            }

            expected_ty
        } else {
            assert!(!(value.ty.is_unknown() || value.ty.is_any()));
            let found = value.ty;
            *ty = LazyType::Finished(found);
            found
        };

        let val_expr = Expr::Value {
            ty: final_ty,
            value: val.clone(),
        };
        self.save_const(name, val_expr, final_ty, loc)?;
        Ok(())
    }

    // Note: don't care about the requested type because functions are already resolved.
    fn compile_split_call(
        &mut self,
        result: &mut FnWip<'p>,
        expr: &mut FatExpr<'p>,
        mut ct: FuncId,
        mut rt: FuncId,
    ) -> Result<Structured, CompileError<'p>> {
        debug_assert_ne!(ct, rt);
        let (f_expr, arg_expr) = if let Expr::Call(f, arg) = expr.deref_mut() { (f, arg) } else { ice!("") };

        let arg_ty_ct = unwrap!(self.program[ct].finished_arg, "ct fn arg");
        let arg_ty = unwrap!(self.program[rt].finished_arg, "rt fn arg");
        self.type_check_arg(arg_ty_ct, arg_ty, "ct vs rt")?;
        let arg_val = self.compile_expr(result, arg_expr, Some(arg_ty))?;
        let ret_ty_ct = unwrap!(self.program[ct].finished_ret, "ct fn ret");
        let ret_ty = unwrap!(self.program[rt].finished_ret, "rt fn ret");
        self.type_check_arg(ret_ty_ct, ret_ty, "ct vs rt")?;
        let mut f_ty = self.program.func_type(rt);
        // At this point we know they have matching arg and ret types.

        assert!(
            !self.program[ct].has_tag(Flag::Comptime) && !self.program[rt].has_tag(Flag::Comptime),
            "@comptime fn must support all architectures"
        );
        assert!(!self.program[ct].has_tag(Flag::Inline) && !self.program[rt].has_tag(Flag::Inline));

        let ct_consts: Vec<_> = self.program[ct].arg.bindings.iter().map(|b| b.kind == VarType::Const).collect();
        let rt_consts: Vec<_> = self.program[rt].arg.bindings.iter().map(|b| b.kind == VarType::Const).collect();
        assert_eq!(
            ct_consts,
            rt_consts,
            "Split FuncRef arg bindings must have same const-ness. {}",
            self.pool.get(self.program[rt].name)
        );

        if rt_consts.iter().any(|&b| b) {
            let mut arg_expr2 = arg_expr.clone();
            // Pretend we're just going to do one and bake the args so no more const.
            rt = self.curry_const_args(rt, f_expr, arg_expr, arg_val.clone())?;
            // Now do the same for the other, so now we have two functions that each go through a chain of calls baking the same const args.
            ct = self.curry_const_args(ct, f_expr, &mut arg_expr2, arg_val)?;
            // TODO: assert no captures and that arg_expr===arg_expr2

            // This will have changed from baking the args.
            f_ty = self.program.func_type(rt);
            let ct_ty = self.program.func_type(ct);
            assert_eq!(f_ty, ct_ty);
        }

        // Since we just baked args, the ids might have changed so we have to update them.
        f_expr.ty = f_ty;
        f_expr.expr = Expr::Value {
            ty: f_ty,
            value: Value::SplitFunc { ct, rt }.into(),
        };

        self.add_callee(result, ct, ExecTime::Comptime);
        self.add_callee(result, rt, ExecTime::Runtime);
        self.compile(ct, ExecTime::Comptime)?;
        self.compile(rt, ExecTime::Runtime)?;

        Ok(Structured::RuntimeOnly(ret_ty))
    }

    pub fn flush_cpu_instruction_cache(&mut self) {
        // This fixes 'illegal hardware instruction'.
        // sleep(Duration::from_millis(1)) also works (in debug mode). That's really cool, it gives it time to update the other cache because instructions and data are seperate?!
        // Especially fun becuase if you run it in lldb so you break on the error and disassemble... you get perfectly valid instructions because it reads the data cache!
        // https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/caches-and-self-modifying-code
        // https://stackoverflow.com/questions/35741814/how-does-builtin-clear-cache-work
        // https://stackoverflow.com/questions/10522043/arm-clear-cache-equivalent-for-ios-devices
        // https://github.com/llvm/llvm-project/blob/main/compiler-rt/lib/builtins/clear_cache.c
        // https://github.com/apple/darwin-libplatform/blob/main/src/cachecontrol/arm64/cache.s
        // https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/sys_icache_invalidate.3.htmls

        let (beg, end) = self.aarch64.bump_dirty();
        if beg != end {
            unsafe { __clear_cache(beg as *mut libc::c_char, end as *mut libc::c_char) }
        }
    }

    fn save_const(&mut self, name: Var<'p>, val_expr: Expr<'p>, final_ty: TypeId, loc: Span) -> Res<'p, ()> {
        if let Some((val, ty)) = self[name.2].constants.get_mut(&name) {
            if matches!(ty, LazyType::Finished(_)) {
                ice!("tried to re-save constant {}", name.log(self.pool));
            }
            if !matches!(val.expr, Expr::Poison) {
                ice!("tried to stomp constant {}", name.log(self.pool));
            }
            val.expr = val_expr;
            val.ty = final_ty;
            *ty = LazyType::Finished(final_ty);
        } else {
            // I think this just means we renumbered for a specialization.
            let e = FatExpr {
                loc,
                expr: val_expr,
                ty: final_ty,
            };
            let val = (e, LazyType::Finished(final_ty));
            self[name.2].constants.insert(name, val);
        }
        Ok(())
    }

    fn save_const_values(&mut self, name: Var<'p>, value: Values, final_ty: TypeId) -> Res<'p, ()> {
        self.save_const(name, Expr::Value { ty: final_ty, value }, final_ty, garbage_loc())
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
    Both,
}

pub fn add_unique<T: PartialEq>(vec: &mut Vec<T>, new: T) -> bool {
    if !vec.contains(&new) {
        vec.push(new);
        return true;
    }
    false
}

pub fn bit_literal<'p>(expr: &FatExpr<'p>, _pool: &StringPool<'p>) -> Option<(IntTypeInfo, i64)> {
    if let Expr::SuffixMacro(name, arg) = &expr.expr {
        if *name == Flag::From_Bit_Literal.ident() {
            if let Expr::Tuple(parts) = arg.deref().deref() {
                if let &Expr::Value {
                    value: Values::One(Value::I64(bit_count)),
                    ..
                } = parts[0].deref()
                {
                    if let &Expr::Value {
                        value: Values::One(Value::I64(val)),
                        ..
                    } = parts[1].deref()
                    {
                        return Some((IntTypeInfo { bit_count, signed: false }, val));
                    }
                }
            }
        }
    }
    None
}

// i like when my code is rocks not rice
// its a lot more challenging to eat but at least you can Find it

pub(crate) trait ToBytes {
    fn to_bytes(self) -> u64;
}

impl ToBytes for i64 {
    fn to_bytes(self) -> u64 {
        u64::from_le_bytes(self.to_le_bytes())
    }
}

pub struct Unquote<'z, 'a, 'p> {
    pub compiler: &'z mut Compile<'a, 'p>,
    pub placeholders: Vec<Option<FatExpr<'p>>>,
    pub result: &'z mut FnWip<'p>,
}

impl<'z, 'a, 'p> WalkAst<'p> for Unquote<'z, 'a, 'p> {
    // TODO: track if we're in unquote mode or placeholder mode.
    fn pre_walk_expr(&mut self, expr: &mut FatExpr<'p>) -> bool {
        if let Expr::SuffixMacro(name, arg) = &mut expr.expr {
            if *name == Flag::Unquote.ident() {
                let expr_ty = FatExpr::get_type(self.compiler.program);
                // self.compiler
                //     .compile_expr(self.result, arg, Some(expr_ty))
                //     .unwrap_or_else(|e| panic!("Expected comple ast but \n{e:?}\n{:?}", arg.log(self.compiler.pool))); // TODO
                let loc = arg.loc;
                let placeholder = Expr::Value {
                    ty: TypeId::i64(),
                    value: Value::I64(self.placeholders.len() as i64).into(),
                };
                let placeholder = FatExpr::synthetic(placeholder, loc);
                let mut placeholder = FatExpr::synthetic(Expr::SuffixMacro(Flag::Placeholder.ident(), Box::new(placeholder)), loc);
                placeholder.ty = expr_ty;
                // Note: take <arg> but replace the whole <expr>
                self.placeholders.push(Some(mem::take(arg.deref_mut())));
                *expr = placeholder;
            } else if *name == Flag::Placeholder.ident() {
                let index = arg.as_int().expect("!placeholder expected int") as usize;
                let value = self.placeholders[index].take(); // TODO: make it more obvious that its only one use and the slot is empty.
                *expr = value.unwrap();
            } else if *name == Flag::Quote.ident() {
                // TODO: add a simpler test case than the derive thing (which is what discovered this problem).
                // Don't go into nested !quote. This allows having macros expand to other macro calls without stomping eachother.
                // TODO: feels like you might still end up with two going on at once so need to have a monotonic id number for each expansion stored in the !placeholder.
                //       but so far this is good enough.
                return false;
            };
        }
        true
    }
}
