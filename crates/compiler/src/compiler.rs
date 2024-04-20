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
use std::{ops::Deref, panic::Location};

use crate::ast::{
    garbage_loc, Annotation, Binding, FatStmt, Field, Flag, IntTypeInfo, Module, ModuleBody, ModuleId, Name, OverloadSet, Pattern, TargetArch, Var,
    VarInfo, VarType, WalkAst,
};

use crate::bc_to_asm::{emit_aarch64, Jitted};
use crate::emit_bc::emit_bc;
use crate::export_ffi::do_flat_call_values;
use crate::ffi::InterpSend;
use crate::scope::ResolveScope;
use crate::{
    ast::{Expr, FatExpr, FnType, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};
use crate::{bc::*, ffi};
use crate::{
    logging::{LogTag, PoolLog},
    outln,
};

use crate::logging::LogTag::{ShowErr, ShowPrint};
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
    Diagnostic(Vec<Diagnostic>),
}

pub type Res<'p, T> = Result<T, CompileError<'p>>;

#[repr(C)]
pub struct Compile<'a, 'p: 'a> {
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
}

#[derive(Clone, Debug)]
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
    pub callees: Vec<(FuncId, ExecTime)>,
    pub module: Option<ModuleId>,
}

impl<'a, 'p> Compile<'a, 'p> {
    pub fn new(pool: &'p StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        Self {
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
    pub(crate) fn push_state(&mut self, s: &DebugState<'p>) {
        self.debug_trace.push(s.clone());
    }

    pub(crate) fn pop_state(&mut self, _s: DebugState<'p>) {
        let _found = self.debug_trace.pop(); //.expect("state stack");
                                             // debug_assert_eq!(found, s);  // TODO: fix the way i deal with errors. i dont always short circuit so this doesnt work
    }

    pub fn add_module(&mut self, name: Ident<'p>, parent: Option<ModuleId>) -> Res<'p, ModuleId> {
        let module = ModuleId(self.program.modules.len());
        self.program.modules.push(Module {
            name,
            id: module,
            parent,
            toplevel: ModuleBody::Resolving,
            exports: Default::default(),
            children: Default::default(),
            i_depend_on: Default::default(),
            depend_on_me: Default::default(),
        });
        if let Some(parent) = parent {
            let prev = self.program.modules[parent.0].children.insert(name, module);
            assert!(prev.is_none(), "Shadowed module {}", self.pool.get(name));
        }
        Ok(module)
    }

    pub fn compile_module(&mut self, ast: Func<'p>) -> Res<'p, FuncId> {
        let module = unwrap!(ast.module, "You must ast.module = Some(compiler.add_module(name, parent))");
        assert!(matches!(self.program[module].toplevel, ModuleBody::Resolving));
        let f = self.add_func(ast, &Constants::empty())?;
        self.program[module].toplevel = ModuleBody::Compiling(f);
        self.ensure_compiled(f, ExecTime::Comptime)?;
        self.program[module].toplevel = ModuleBody::Ready(f);
        // let exports = self.program[f].closed_constants.clone();
        // for var in exports.local.keys() {
        //     let _prev = self.program.modules[module.0].exports.insert(var.0, *var);
        //     // TODO
        //     // assert!(prev.is_none(), "Shadowed export {} from {}", self.pool.get(var.0), self.pool.get(name));
        // }
        Ok(f)
    }

    pub fn compile(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        if !add_unique(&mut self.currently_compiling, f) {
            // This makes recursion work.
            return Ok(());
        }
        let state = DebugState::Compile(f);
        self.push_state(&state);
        debug_assert!(!self.program[f].evil_uninit);
        let mut result = self.ensure_compiled(f, when);
        if result.is_ok() {
            let func = &self.program[f];
            if let Some(wip) = func.wip.as_ref() {
                let callees = wip.callees.clone();
                for (id, when) in callees {
                    self.compile(id, when)?;
                }
                result = emit_bc(self, f);
            } else {
                self.compile(func.any_reg_template.unwrap(), ExecTime::Comptime)?;
            }
        }

        let result = self.tag_err(result);
        self.pop_state(state);
        self.currently_compiling.retain(|check| *check != f);
        result
    }

    pub fn run(&mut self, f: FuncId, arg: Values, when: ExecTime, result: Option<*mut FnWip<'p>>) -> Res<'p, Values> {
        let state2 = DebugState::RunInstLoop(f);
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
                let addr = unwrap!(self.aarch64.get_fn(f), "not compiled {f:?}").as_ptr();
                let ty = self.program[f].unwrap_ty();
                let comp_ctx = self.program[f].has_tag(Flag::Ct);
                let c_call = self.program[f].has_tag(Flag::C_Call);
                let flat_call = self.program[f].has_tag(Flag::Flat_Call);

                self.aarch64.make_exec(); // otherwise: bus error
                if flat_call {
                    assert!(comp_ctx && !c_call);
                    // println!("flat_call {f:?}");
                    do_flat_call_values(self, unsafe { transmute(addr) }, arg, ty.ret)
                } else if c_call {
                    assert!(!flat_call);
                    assert!(addr as usize % 4 == 0);
                    // println!("c_call {f:?} {addr:?} {arg:?} -> {}", self.program.log_type(ty.ret));
                    // TODO: !!! removin this makes it illeegal hardware insturciton.
                    //       would be really cool if that gives it time to update the other cache cause instructions and data are seperate ????!!!!
                    // sleep(Duration::from_millis(1));
                    // https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/caches-and-self-modifying-code
                    // https://stackoverflow.com/questions/35741814/how-does-builtin-clear-cache-work
                    // https://stackoverflow.com/questions/10522043/arm-clear-cache-equivalent-for-ios-devices
                    extern "C" {
                        pub fn __clear_cache(beg: *mut libc::c_char, end: *mut libc::c_char);
                    }

                    let (beg, end) = self.aarch64.get_dirty();
                    if beg != end {
                        unsafe { __clear_cache(beg as *mut libc::c_char, end as *mut libc::c_char) }
                    }
                    let r = ffi::c::call(self, addr as usize, ty, arg, comp_ctx);
                    r
                } else {
                    todo!()
                }
            }
            TargetArch::Llvm => todo!(),
        };

        self.pending_ffi.pop().unwrap();
        let result = self.tag_err(result);
        self.pop_state(state2);
        result
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
    fn empty_fn(&mut self, when: ExecTime, func: FuncId, loc: Span, parent: Option<Constants<'p>>, module: Option<ModuleId>) -> FnWip<'p> {
        FnWip {
            stack_slots: 0,
            vars: Default::default(),
            when,
            func,
            why: self.log_trace(),
            last_loc: loc,
            constants: parent.unwrap_or_else(|| self.program[func].closed_constants.clone()),
            callees: vec![],
            module,
        }
    }

    // Any environment constants must already be in the function.
    fn eval_and_close_local_constants(&mut self, f: FuncId) -> Res<'p, ()> {
        debug_assert!(!self.program[f].evil_uninit);
        debug_assert!(!self.program[f].evil_uninit);
        if self.program[f].local_constants.is_empty() {
            // Maybe no consts, or maybe we've already compiled them.
            return Ok(());
        }
        let state = DebugState::EvalConstants(f);
        self.push_state(&state);
        let loc = self.program[f].loc;
        let module = self.program[f].module;

        mut_replace!(self.program[f].closed_constants, |constants| {
            let mut result = self.empty_fn(
                ExecTime::Comptime,
                FuncId(f.0 + 10000000), // TODO: do i even need to pass an index? probably just for debugging
                loc,
                Some(constants),
                module,
            );

            mut_replace!(self.program[f].local_constants, |mut local_constants| {
                for stmt in &mut local_constants {
                    self.compile_stmt(&mut result, stmt)?;
                }
                // Note: not putting local_constants back. We only need to evaluate them once (even if capturing_call runs many times)
                //      and they get moved into result.constants, which becomes the closed_constants of 'f' (but as values instead of statements)
                Ok((vec![], ()))
            });

            self.pop_state(state);

            // Now this includes stuff inherited from the parent, plus any constants pulled up from the function body.
            Ok((result.constants, ()))
        });

        Ok(())
    }

    // Don't pass constants in because the function declaration must have closed over anything it needs.
    fn ensure_compiled(&mut self, f: FuncId, when: ExecTime) -> Res<'p, ()> {
        let func = &self.program[f];
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

        self.infer_types(f)?;
        let func = &self.program[f];
        if let Some(template) = func.any_reg_template {
            self.ensure_compiled(template, ExecTime::Comptime)?;
        } else {
            let mut result = self.empty_fn(when, f, func.loc, None, func.module);
            self.emit_body(&mut result, f)?;
            self.program[f].wip = Some(result);
        }
        self.pop_state(state);
        Ok(())
    }

    /// IMPORTANT: this pulls a little sneaky on ya, so you can't access the body of the function inside the main emit handlers.
    fn emit_body(&mut self, result: &mut FnWip<'p>, f: FuncId) -> Res<'p, Structured> {
        let state = DebugState::EmitBody(f);
        self.push_state(&state);
        let has_body = self.program[f].body.is_some();
        debug_assert!(!self.program[f].evil_uninit);

        mut_replace!(self.program[f], |func: Func<'p>| {
            assert!(result.when == ExecTime::Comptime || !func.has_tag(Flag::Comptime));
            let arguments = func.arg.flatten();
            for (name, ty, kind) in arguments {
                // TODO: probably want to change this so you can do as much compiling as possible before expanding templates.
                assert!(kind != VarType::Const, "Tried to emit before binding const args.");
                if let Some(name) = name {
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

        let ret_val = mut_replace!(self.program[f].body, |mut body: Option<FatExpr<'p>>| {
            let body_expr = body.as_mut().unwrap();

            if let Expr::SuffixMacro(name, arg) = body_expr.deref_mut() {
                if *name == Flag::Asm.ident() {
                    self.inline_asm_body(result, f, arg)?;
                    let fn_ty = self.program[f].unwrap_ty();
                    let ret_ty = fn_ty.ret;
                    let _ = self.program.intern_type(TypeInfo::FnPtr(fn_ty)); // make sure emit_ can get the type without mutating the Program.
                    return Ok((None, Structured::RuntimeOnly(ret_ty)));
                }
            }

            let hint = self.program[f].finished_ret;
            let res = self.compile_expr(result, body_expr, hint)?;
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
            Ok((body, res))
        });

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
        add_unique(&mut result.callees, (f, ExecTime::Both));
        self.ensure_compiled(f, result.when)?;
        let ret_ty = unwrap!(self.program[f].finished_ret, "fn ret");
        Ok(Structured::RuntimeOnly(ret_ty))
    }

    // Replace a call expr with the body of the target function.
    // The idea is having zero cost (50 cycles) closures :)
    fn emit_capturing_call(&mut self, result: &mut FnWip<'p>, f: FuncId, expr_out: &mut FatExpr<'p>) -> Res<'p, Structured> {
        let loc = expr_out.loc;
        debug_assert_ne!(f, result.func, "recusive inlining?");
        let state = DebugState::EmitCapturingCall(f);
        self.push_state(&state);
        let arg_expr = if let Expr::Call(_, arg) = expr_out.deref_mut() { arg } else { ice!("") };

        assert!(!self.currently_inlining.contains(&f), "Tried to inline recursive function.");
        self.currently_inlining.push(f);

        // We close them into f's Func, but we need to emit
        // into the caller's body.
        self.eval_and_close_local_constants(f)?; // TODO: only if this is the first time calling 'f'
        let func = &self.program.funcs[f.0];
        let my_consts = &func.closed_constants;
        result.constants.add_all(my_consts);

        let pattern = func.arg.clone();
        let locals = func.arg.collect_vars();

        // TODO: can I mem::take func.body? I guess not because you're allowed to call multiple times, but that's sad for the common case of !if/!while.
        // TODO: dont bother if its just unit args (which most are because of !if and !while).
        // TODO: you want to be able to share work (across all the call-sites) compiling parts of the body that don't depend on the captured variables
        // TODO: need to move the const args to the top before eval_and_close_local_constants
        expr_out.expr = Expr::Block {
            body: vec![FatStmt {
                stmt: Stmt::DeclVarPattern {
                    binding: pattern,
                    value: Some(mem::take(arg_expr)),
                },
                annotations: vec![],
                loc,
            }],
            result: Box::new(func.body.as_ref().unwrap().clone()),
            locals: Some(locals), // Note: just the declarations in this block, not recursivly bubbled up.
        };
        expr_out.renumber_vars(&mut self.program.vars);

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
    fn bind_const_arg(&mut self, o_f: FuncId, arg_name: Var<'p>, arg: Structured) -> Res<'p, FuncId> {
        let mut new_func = self.program[o_f].clone();
        new_func.referencable_name = false;
        let arg_ty = self.get_type_for_arg(&new_func.closed_constants, &mut new_func.arg, arg_name)?;
        self.type_check_arg(arg.ty(), arg_ty, "bind arg")?;
        let arg_value = arg.get()?;

        // TODO: not sure if i actually need this but it seems like i should.
        if let Values::One(Value::GetFn(arg_func)) = &arg_value {
            // TODO: support fns nested in tuples.
            let extra_captures = &self.program[*arg_func].capture_vars;
            new_func.capture_vars.extend(extra_captures);
            // TODO: do I need to take its closed closed constants too?
        }
        new_func.closed_constants.insert(arg_name, (arg_value, arg_ty));
        new_func.arg.remove_named(arg_name);

        let known_type = new_func.finished_arg.is_some();
        new_func.finished_arg = None;
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
    fn get_type_for_arg(&mut self, constants: &Constants<'p>, arg: &mut Pattern<'p>, arg_name: Var<'p>) -> Res<'p, TypeId> {
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
    fn emit_comptime_call(&mut self, result: &mut FnWip<'p>, template_f: FuncId, arg_expr: &mut FatExpr<'p>) -> Res<'p, (Values, TypeId)> {
        // We don't care about the constants in `result`, we care about the ones that existed when `f` was declared.
        // BUT... the *arguments* to the call need to be evaluated in the caller's scope.

        // This one does need the be a clone because we're about to bake constant arguments into it.
        // If you try to do just the constants or chain them cleverly be careful about the ast rewriting.
        let mut func = self.program[template_f].clone();
        debug_assert!(!func.evil_uninit);
        debug_assert!(func.closed_constants.is_valid);
        func.referencable_name = false;
        func.closed_constants.add_all(&result.constants);

        // I used to use func.closed_constants instead of result.constants. But it seems fine this way. --Apr 15.
        let types = self.infer_pattern(&result.constants, &mut func.arg.bindings)?;
        // TODO: update self.program[f]
        let arg_ty = self.program.tuple_of(types);
        self.compile_expr(result, arg_expr, Some(arg_ty))?;
        let arg_value = self.immediate_eval_expr_in(result, arg_expr.clone(), arg_ty)?;

        // TODO: wtf.someones calling it on a tuple whish shows up as a diferent type so you get multiple of inner functions because eval twice. but then cant resolve overlaods because they have the same type beause elsewhere handles single tuples correctly.
        // TODO: figure out what was causing and write a specific test for it. discovered in fmt @join
        let arg_value = arg_value.normalize();

        assert!(!arg_expr.ty.is_unknown());
        arg_expr.expr = Expr::Value {
            ty: arg_expr.ty,
            value: arg_value.clone(),
        };

        // TODO: !!!! now Unique doesnt work maybe?
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

        // Bind the arg into my new constants so the ret calculation can use it.
        // also the body constants for generics need this. much cleanup pls.
        // TODO: factor out from normal functions?

        let f = self.program.add_func(func);
        let func = &self.program[f];
        let args = func.arg.flatten().into_iter();
        let mut arg_values = arg_value.vec().into_iter();
        for (name, ty, _) in args {
            let size = self.ready.sizes.slot_count(self.program, ty); // TODO: better pattern matching
            let mut values = vec![];
            for _ in 0..size {
                values.push(unwrap!(arg_values.next(), "ICE: missing arguments"));
            }

            if let Some(var) = name {
                let prev = self.program[f].closed_constants.insert(var, (values.into(), ty));
                assert!(prev.is_none(), "overwrite arg?");
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
            self.immediate_eval_expr(&func.closed_constants, body, ret)?
        } else {
            unreachable!("builtin")
        };

        let ty = self.program.type_of_raw(&result);
        self.type_check_arg(ty, ret, "generic result")?;

        outln!(LogTag::Generics, "{:?}={} of {:?} => {:?}", key.0, self.pool.get(name), key.1, result);

        self.program.generics_memo.insert(key, (result.clone(), ret));

        Ok((result, ret))
    }

    fn compile_stmt(&mut self, result: &mut FnWip<'p>, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        debug_assert!(result.constants.is_valid);
        self.last_loc = Some(stmt.loc);
        match &mut stmt.stmt {
            Stmt::DoneDeclFunc(_) => unreachable!("compiled twice?"),
            Stmt::Eval(expr) => {
                if let Some(_module_name) = stmt.annotations.iter().find(|a| a.name == Flag::Module.ident()) {
                    // TODO: need to not pull out constants somehow.
                    todo!()
                }
                self.compile_expr(result, expr, None)?;
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclNamed { .. } => {
                ice!("Scope resolution failed {}", stmt.log(self.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                let mut func = mem::take(func);
                func.module = result.module;
                let var = func.var_name;
                let for_bootstrap = func.has_tag(Flag::Bs);
                let any_reg_template = func.has_tag(Flag::Any_Reg);
                let is_struct = func.has_tag(Flag::Struct);
                let is_enum = func.has_tag(Flag::Enum);
                if func.has_tag(Flag::Annotation) {
                    assert!(!func.has_tag(Flag::Rt));
                    func.add_tag(Flag::Ct);
                    func.add_tag(Flag::Flat_Call);
                }
                assert!(!(is_struct && is_enum));
                if is_struct {
                    let init_overloadset = self.program.overload_sets.iter().position(|a| a.name == Flag::Init.ident()).unwrap();
                    let ty = self.struct_type(result, &mut func.arg)?;
                    let ty = self.program.intern_type(ty);
                    assert!(matches!(func.ret, LazyType::Infer), "remove type annotation on struct initilizer");
                    func.ret = LazyType::Finished(ty);
                    func.finished_ret = Some(ty);
                    if let Some(name) = func.var_name {
                        result.constants.local.insert(name, (Value::Type(ty).into(), TypeId::ty()));
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

                        let init_expr = Box::new(FatExpr::synthetic(Expr::StructLiteralP(init_pattern), loc));
                        let construct = Flag::Construct.ident();
                        func.body = Some(FatExpr::synthetic(Expr::SuffixMacro(construct, init_expr), loc));
                    }
                    let id = self.add_func(func, &result.constants)?;
                    *stmt.deref_mut() = Stmt::DoneDeclFunc(id);
                    self.program.overload_sets[init_overloadset].pending.push(id);
                    return Ok(());
                }
                if is_enum {
                    let init_overloadset = self.program.overload_sets.iter().position(|a| a.name == Flag::Init.ident()).unwrap();
                    let ty = self.struct_type(result, &mut func.arg)?;
                    let ty = self.program.to_enum(ty);
                    assert!(matches!(func.ret, LazyType::Infer), "remove type annotation on struct initilizer");
                    func.ret = LazyType::Finished(ty);
                    func.finished_ret = Some(ty);
                    if let Some(name) = func.var_name {
                        result.constants.local.insert(name, (Value::Type(ty).into(), TypeId::ty()));
                    }
                    // TODO: do i need to set func.var_name? its hard because need to find an init? or can it be a new one?
                    func.name = Flag::Init.ident();
                    if func.body.is_none() {
                        let loc = func.loc;
                        let init_pattern = func.arg.clone();
                        for mut b in init_pattern.bindings {
                            let mut new_func = func.clone();
                            new_func.arg.bindings = vec![b.clone()];
                            let name = if let Name::Var(name) = b.name { name } else { todo!() };
                            b.ty = LazyType::PendingEval(FatExpr::synthetic(Expr::GetVar(name), loc));
                            let init_expr = Box::new(FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![b], loc }), loc));
                            new_func.body = Some(FatExpr::synthetic(Expr::SuffixMacro(Flag::Construct.ident(), init_expr), loc));
                            new_func.annotations.retain(|a| a.name != Flag::Enum.ident());
                            let id = self.add_func(new_func, &result.constants)?;
                            self.program.overload_sets[init_overloadset].pending.push(id);
                        }
                    }
                    *stmt.deref_mut() = Stmt::Noop;
                    // TODO: unfortunate that I have to short circuit, not handle extra annotations, and leave the garbage function there because i split them
                    return Ok(());
                }
                if for_bootstrap {
                    func.add_tag(Flag::Aarch64); // TODO: could do for llvm too once i support eval body
                    func.referencable_name = false;
                }

                let referencable_name = func.referencable_name;

                if any_reg_template {
                    let mut body = func.body.take().unwrap();
                    self.compile_expr(result, &mut body, None)?;
                    func.any_reg_template = Some(body.as_fn().unwrap());
                }

                let public = func.has_tag(Flag::Pub);
                let id = self.add_func(func, &result.constants)?;
                *stmt.deref_mut() = Stmt::DoneDeclFunc(id);

                if for_bootstrap {
                    self.save_bootstrap.push(id);
                }

                // I thought i dont have to add to constants here because we'll find it on the first call when resolving overloads.
                // But it does need to have an empty entry in the overload pool because that allows it to be closed over so later stuff can find it and share if they compile it.
                if let Some(var) = var {
                    if referencable_name && !is_enum && !is_struct {
                        // TODO: allow function name to be any expression that resolves to an OverloadSet so you can overload something in a module with dot syntax.
                        // TODO: distinguish between overload sets that you add to and those that you re-export
                        if let Some(overloads) = result.constants.get(var) {
                            let i = overloads.0.as_overload_set()?;
                            let os = &mut self.program.overload_sets[i];
                            assert_eq!(os.public, public, "Overload visibility mismatch: {}", var.log(self.pool));
                            os.pending.push(id);
                        } else {
                            let index = self.program.overload_sets.len();
                            self.program.overload_sets.push(OverloadSet {
                                ready: vec![],
                                name: var.0,
                                pending: vec![id],
                                public,
                            });
                            result.constants.insert(var, (Value::OverloadSet(index).into(), TypeId::overload_set()));
                        }
                    }
                }

                let func = &self.program.funcs[id.0];
                if func.has_tag(Flag::Impl) {
                    for stmt in &func.local_constants {
                        if let Stmt::DeclFunc(new) = &stmt.stmt {
                            if new.referencable_name {
                                // TODO: put 'id' in an impls list in the overload set of 'new' somehow
                            }
                        }
                    }
                }

                // TODO: allow macros do add to a HashMap<TypeId, HashMap<Ident, Values>>,
                //       to give generic support for 'let x: E.T[] = T.Value[] === let x: T.T[] = .Value' like Zig/Swift.
                //       then have @test(.aarch64, .llvm) instead of current special handling.
                //       also add a new annotation for declaring macros with some other type as the argument where it const evals the expr before calling the macro.
                //       that should be doable in the language once I allow user macros to modify functions like the builtin ones do.
                //       -- Apr 19
                if func.has_tag(Flag::Test) {
                    // TODO: actually use this.
                    // TODO: probably want referencable_name=false?
                    self.tests.push(id);
                }
            }
            // TODO: make value not optonal and have you explicitly call uninitilized() if you want that for some reason.
            Stmt::DeclVar { name, ty, value, kind, .. } => self.decl_var(result, *name, ty, value, *kind, &stmt.annotations)?,
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
                        self.decl_var(result, name, &mut LazyType::Finished(ty), value, kind, &stmt.annotations)?;
                        if kind == VarType::Const {
                            debug_assert_eq!(binding.bindings.len(), 1);
                            binding.bindings.clear();
                            *value = None;
                        }
                        return Ok(());
                    } else {
                        let e = value.as_mut().unwrap();
                        assert!(e.expr.is_raw_unit(), "var no name not unit: {}", e.log(self.pool));
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
                        // TODO: HACK. make value not optional. cant just flip because when missing decl_var wants to put something there for imports. makes it hard. need to have a marker expr for missing value??
                        let mut value = Some(mem::take(expr));
                        self.decl_var(result, *name, &mut LazyType::Finished(*ty), &mut value, *kind, &stmt.annotations)?;
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

    pub fn compile_expr(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, Structured> {
        assert!(expr.ty.0 < self.program.types.len() as u64);

        // TODO: it seems like i shouldn't compile something twice
        // debug_assert!(expr.ty.is_unknown(), "{}", expr.log(self.pool));
        let old = expr.ty;

        let mut res = self.compile_expr_inner(result, expr, requested)?;
        if !expr.ty.is_unknown() {
            self.type_check_arg(expr.ty, res.ty(), format!("sanity ICE {} {res:?}", expr.log(self.pool)).leak())?;
            //"sanity ICE")?;
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
                        FuncRef::Split { ct, rt } => return self.compile_split_call(result, expr, ct, rt, requested),
                    }
                }
                ice!("function not declared or \nTODO: dynamic call: {}\n\n{expr:?}", expr.log(self.pool))
            }
            Expr::Block { body, result: value, .. } => {
                for stmt in body {
                    self.compile_stmt(result, stmt)?;
                }
                // TODO: insert drops for locals
                self.compile_expr(result, value, requested)?
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
                } else if let Some((value, ty)) = result.constants.get(*var) {
                    Structured::Const(ty, value)
                } else {
                    outln!(ShowErr, "VARS: {:?}", result.vars);
                    outln!(ShowErr, "CONSTANTS: {:?}", self.program.log_consts(&result.constants));
                    // let current_func = &self.program.funcs[result.func.0];
                    // outln!(ShowErr, "{}", current_func.log_captures(self.pool));
                    ice!("Missing resolved variable {:?} '{}'", var, self.pool.get(var.0),)
                }
            }
            Expr::GetNamed(name) => {
                let name = self.pool.get(*name);
                if let Some((val, ty)) = self.builtin_constant(name) {
                    expr.expr = Expr::Value { ty, value: val.into() };
                    Structured::Const(ty, val.into())
                } else {
                    ice!("Scope resolution failed {} (in Expr::GetNamed)", expr.log(self.pool));
                }
            }
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
                    "slice" => {
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
                    "c_call" => err!("!c_call has been removed. calling convention is part of the type now.",),
                    "deref" => {
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
                    "reflect_print" => {
                        self.compile_expr(result, arg, None)?;
                        // TODO: replace expr with fn call
                        Structured::RuntimeOnly(TypeId::unit())
                    }
                    "type" => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects. especially if it does const stuff.
                        let ty = self.compile_expr(result, arg, None)?.ty();
                        expr.expr = Expr::ty(ty);
                        self.program.load_value(Value::Type(ty))
                    }
                    "size_of" => {
                        let ty = self.immediate_eval_expr_in(result, mem::take(arg), TypeId::ty())?;
                        let ty = self.to_type(ty)?;
                        let size = self.ready.sizes.slot_count(self.program, ty);
                        expr.expr = Expr::int(size as i64);
                        self.program.load_value(Value::I64(size as i64))
                    }
                    "assert_compile_error" => {
                        // TODO: this can still have side-effects on the compiler state tho :(
                        let saved_res = result.clone();
                        let res = self.compile_expr(result, arg, None);
                        assert!(res.is_err());
                        *result = saved_res;

                        expr.expr = Expr::unit();
                        self.program.load_value(Value::Unit)
                    }
                    "comptime_print" => {
                        outln!(ShowPrint, "EXPR : {}", arg.log(self.pool));
                        let value = self.immediate_eval_expr_in(result, *arg.clone(), TypeId::unknown());
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
                            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", arg);
                        }
                    }
                    "enum" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref_mut().deref_mut() {
                            let ty = self.struct_type(result, pattern)?;
                            let ty = self.program.to_enum(ty);
                            expr.expr = Expr::ty(ty);
                            self.program.load_value(Value::Type(ty))
                        } else {
                            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", arg);
                        }
                    }
                    "tag" => {
                        // TODO: auto deref and typecheking
                        let container_ptr = self.compile_expr(result, arg, None)?;
                        let container_ptr_ty = self.program.raw_type(container_ptr.ty());
                        let depth = self.program.ptr_depth(container_ptr_ty);
                        assert_eq!(depth, 1, "!tag ptr must be one level of indirection. {:?}", container_ptr_ty);
                        let ty = self.program.intern_type(TypeInfo::Ptr(TypeId::i64()));
                        Structured::RuntimeOnly(ty)
                    }
                    "symbol" => {
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
                    "fn_ptr" => {
                        // TODO: this should be immediate_eval_expr (which should be updated do the constant check at the beginning anyway).
                        //       currently !fn_ptr can't be an atrbitrary comptime expr which is silly.
                        let fn_val = self.compile_expr(result, arg, None)?.get()?;
                        match fn_val {
                            Values::One(Value::GetFn(id)) => {
                                assert!(!self.program[id].any_const_args());
                                add_unique(&mut result.callees, (id, ExecTime::Both));
                                self.ensure_compiled(id, result.when)?;
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
                    "construct" => {
                        if let Expr::StructLiteralP(pattern) = &mut arg.expr {
                            let out = self.construct_struct(result, requested, pattern)?;
                            arg.ty = requested.unwrap();
                            out
                        } else {
                            err!("!construct expected map literal.",)
                        }
                    }
                    "from_bit_literal" => {
                        let int = bit_literal(expr, self.pool)?;
                        let ty = self.program.intern_type(TypeInfo::Int(int.0));
                        let value = Value::I64(int.1);
                        expr.expr = Expr::Value { ty, value: value.into() };
                        Structured::Const(ty, value.into())
                    }
                    "unquote" | "placeholder" => err!("ICE: Unhandled macro {}", self.pool.get(*macro_name)),
                    _ => {
                        err!(CErr::UndeclaredIdent(*macro_name))
                    }
                }
            }
            Expr::FieldAccess(e, name) => {
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
                let value = self.immediate_eval_expr_in(result, *index.clone(), TypeId::i64())?;
                let i = value.clone().single()?.to_int()? as usize;
                index.expr = Expr::Value { ty: TypeId::i64(), value };
                self.index_expr(result, ptr, i)?
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
            Expr::PrefixMacro { name, arg, target } => {
                let name_str = self.pool.get(name.0);
                // TODO: dont do the builtin ones this way. put them in thier own function and expose them as @ct @comptime_addr(_) @call_conv(RustPrefixMacro) === fn name(&mut self, arg: FatExpr, target: FatExpr) -> FatExpr;
                //       then you don't have to eat the these checks every time, you just get there when you get there.
                //       plus it makes the transition to writing them in the language smoother. until then, the only cost is that it becomes a virtual call.
                // TODO: this doesn't work in general because it doesnt recalculate closure captures.
                if name_str == "with_var" {
                    if let Expr::Tuple(exprs) = &mut arg.expr {
                        assert_eq!(exprs.len(), 2);
                        if let Expr::GetNamed(name) = exprs[0].expr {
                            let name = Var(name, self.program.vars.len());
                            self.program.vars.push(VarInfo { kind: VarType::Var, loc });

                            let mut res = mem::take(target);
                            let value = mem::take(&mut exprs[1]);
                            // TODO: add to closure captures? should really just factor out the scope visitor.
                            let mut f = |expr: &mut Expr<'p>| {
                                if let Expr::GetNamed(n) = expr {
                                    if *n == name.0 {
                                        *expr = Expr::GetVar(name);
                                    }
                                }
                            };
                            f.expr(&mut res);
                            expr.expr = Expr::Block {
                                body: vec![FatStmt {
                                    stmt: Stmt::DeclVar {
                                        name,
                                        ty: LazyType::Infer,
                                        value: Some(value),
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
                    let ty = self.immediate_eval_expr_in(result, *arg.clone(), TypeId::ty())?;
                    let ty = self.to_type(ty)?;
                    if let Expr::StructLiteralP(pattern) = target.deref_mut().deref_mut().deref_mut() {
                        let mut the_type = Pattern::empty(loc);
                        let unique_ty = self.program.unique_ty(ty);
                        // Note: we expand target to an anonamus struct literal, not a type.
                        for b in &mut pattern.bindings {
                            assert!(b.default.is_some());
                            assert!(matches!(b.lazy(), LazyType::Infer));
                            b.ty = LazyType::PendingEval(unwrap!(b.default.take(), ""));
                            the_type.bindings.push(Binding {
                                name: b.name,
                                // ty: LazyType::Finished(unique_ty),
                                ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(unique_ty), loc)),
                                default: None,
                                kind: VarType::Let,
                            });
                        }
                        let var = Var(self.pool.intern("T"), self.program.vars.len());
                        self.program.vars.push(VarInfo { kind: VarType::Const, loc });
                        pattern.bindings.push(Binding {
                            name: Name::Var(var),
                            ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(unique_ty), loc)),
                            default: None,
                            kind: VarType::Let,
                        });
                        the_type.bindings.push(Binding {
                            name: Name::Var(var),
                            ty: LazyType::PendingEval(FatExpr::synthetic(Expr::ty(TypeId::ty()), loc)),
                            default: None,
                            kind: VarType::Let,
                        });
                        let the_type = Box::new(FatExpr::synthetic(Expr::StructLiteralP(the_type), loc));
                        let the_type = FatExpr::synthetic(Expr::SuffixMacro(Flag::Struct.ident(), the_type), loc);
                        let the_type = self.immediate_eval_expr_in(result, the_type, TypeId::ty())?;
                        let the_type = self.to_type(the_type)?;
                        // *expr = FatExpr::synthetic(Expr::PrefixMacro { name: var, arg, target: mem::take(target) }, loc);

                        let construct_expr = FatExpr::synthetic(Expr::SuffixMacro(Flag::Construct.ident(), mem::take(target)), loc);
                        let value = self.immediate_eval_expr_in(result, construct_expr, the_type)?;
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
                    target.ty = ty;
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
                    value: Values::Many(values.clone()), // TODO: sad
                };
                let mut full_arg = FatExpr::synthetic(full_arg, loc);
                let f = self.resolve_function(result, *name, &mut full_arg, Some(want))?.single()?;
                assert!(self.program[f].has_tag(Flag::Annotation));
                self.infer_types(f)?;
                // let get_func = FatExpr::synthetic(self.func_expr(f), loc);
                // let full_call = FatExpr::synthetic(Expr::Call(Box::new(get_func), Box::new(full_arg)), loc);

                // let new_expr = self.immediate_eval_expr(&result.constants, full_call, want)?;
                self.compile(f, ExecTime::Comptime)?;

                let new_expr = self.run(f, Values::Many(values), ExecTime::Comptime, Some(result as *mut FnWip))?;
                // TODO: deserialize should return a meaningful error message
                *expr = unwrap!(FatExpr::deserialize_one(new_expr.clone()), "macro failed. returned {new_expr:?}");
                outln!(LogTag::Macros, "OUTPUT: {}", expr.log(self.pool));
                outln!(LogTag::Macros, "================\n");
                // Now evaluate whatever the macro gave us.
                self.compile_expr(result, expr, requested)?
            }
        })
    }

    fn addr_macro(&mut self, result: &mut FnWip<'p>, arg: &mut FatExpr<'p>) -> Res<'p, Structured> {
        match arg.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                if let Some(value_ty) = result.vars.get(var).cloned() {
                    assert!(!value_ty.is_any(), "took address of Any {}", var.log(self.pool));
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
            _ => err!(CErr::AddrRvalue(arg.clone())),
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
    // this is such a source of wierd nondeterminism bugs because of ^
    pub(crate) fn type_of(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        if !expr.ty.is_unknown() {
            return Ok(Some(expr.ty));
        }

        // TODO: this is unfortunate
        if let Ok((int, _)) = bit_literal(expr, self.pool) {
            // return Ok(Some(TypeId::i64()));
            return Ok(Some(self.program.intern_type(TypeInfo::Int(int)))); // but that breaks assert_Eq
        }
        Ok(Some(match expr.deref_mut() {
            Expr::WipFunc(_) => return Ok(None),
            Expr::Value { ty, value } => {
                if value.as_overload_set().is_ok() {
                    return Ok(None);
                } else {
                    *ty
                }
            }
            Expr::Call(f, arg) => {
                if let Some(id) = f.as_fn() {
                    return Ok(self.program.funcs[id.0].finished_ret);
                }

                if let Expr::Value { value, .. } = f.deref_mut().deref_mut() {
                    if let Ok(i) = value.as_overload_set() {
                        // println!("type_of overload {i}");
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
                }

                if let Expr::GetVar(i) = f.deref_mut().deref_mut() {
                    if let Some(ty) = result.vars.get(i) {
                        if let TypeInfo::FnPtr(f_ty) = self.program[*ty] {
                            return Ok(Some(f_ty.ret));
                        }
                    }
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
                assert_eq!(before, types.len(), "some of tuple not infered");
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
            Expr::PrefixMacro { name, arg, .. } => {
                if name.0 == Flag::As.ident() {
                    let ty = self.immediate_eval_expr(&result.constants, *arg.clone(), TypeId::ty())?;
                    self.program.to_type(ty)?
                }
                // TODO: if this fails you might have changed the state.
                else {
                    match self.compile_expr(result, expr, None) {
                        Ok(res) => res.ty(),
                        Err(e) => ice!("TODO: PrefixMacro inference failed. need to make it non-destructive?\n{e:?}",),
                    }
                }
            }
            &mut Expr::GetNamed(name) => {
                let name = self.pool.get(name);
                if let Some((val, ty)) = self.builtin_constant(name) {
                    expr.expr = Expr::Value { ty, value: val.into() };
                    ty
                } else {
                    return Ok(None);
                }
            }
            Expr::StructLiteralP(_) => return Ok(None),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    "addr" => match arg.deref_mut().deref_mut() {
                        Expr::GetVar(var) => {
                            let value_ty = *result.vars.get(var).expect("Missing resolved var (TODO: addr of const?)");
                            self.program.intern_type(TypeInfo::Ptr(value_ty))
                        }
                        &mut Expr::GetNamed(i) => {
                            logln!("UNDECLARED IDENT: {} (in SuffixMacro::addr)", self.pool.get(i));
                            err!(CErr::UndeclaredIdent(i))
                        }
                        _ => err!(CErr::AddrRvalue(*arg.clone())),
                    },
                    "struct" | "enum" | "type" => self.program.intern_type(TypeInfo::Type),
                    "deref" => {
                        let ptr_ty = self.type_of(result, arg)?;
                        if let Some(ptr_ty) = ptr_ty {
                            return Ok(self.program.unptr_ty(ptr_ty));
                        }
                        return Ok(None);
                    }
                    "symbol" => Ident::get_type(self.program),
                    "tag" => self.program.ptr_type(TypeId::i64()),
                    "fn_ptr" => {
                        if let Some(f_ty) = self.type_of(result, arg)? {
                            if let Some(f_ty) = self.program.fn_ty(f_ty) {
                                return Ok(Some(self.program.intern_type(TypeInfo::FnPtr(f_ty))));
                            }
                        }
                        return Ok(None);
                    }
                    "quote" => FatExpr::get_type(self.program),
                    "while" => TypeId::unit(),
                    // TODO: there's no reason this couldn't look at the types, but if logic is more complicated (so i dont want to reproduce it) and might fail often (so id be afraid of it getting to a broken state).
                    "if" => return Ok(None),
                    _ => match self.compile_expr(result, expr, None) {
                        Ok(res) => res.ty(),
                        Err(e) => ice!("TODO: SuffixMacro inference failed. need to make it non-destructive?\n{e:?}",),
                    },
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

    // TODO: this kinda sucks. it should go in a builtin generated file like libc and use the normal name resolution rules
    fn builtin_constant(&mut self, name: &str) -> Option<(Value, TypeId)> {
        use TypeInfo::*;
        let ty = match name {
            "i64" => Some(TypeInfo::Int(IntTypeInfo { bit_count: 64, signed: true })),
            "f64" => Some(F64),
            "Type" => Some(Type),
            "bool" => Some(Bool),
            "Never" => Some(TypeInfo::Never),
            "VoidPtr" => Some(VoidPtr),
            "OverloadSet" => Some(TypeInfo::OverloadSet),
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

    fn infer_types_progress(&mut self, constants: &Constants<'p>, ty: &mut LazyType<'p>) -> Res<'p, bool> {
        Ok(mut_replace!(*ty, |mut ty| {
            let done = match ty {
                LazyType::EvilUnit => panic!(),
                LazyType::Infer => false,
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

    fn infer_binding_progress(&mut self, constants: &Constants<'p>, binding: &mut Binding<'p>) -> Res<'p, bool> {
        self.infer_types_progress(constants, &mut binding.ty)
    }

    fn infer_pattern(&mut self, constants: &Constants<'p>, bindings: &mut [Binding<'p>]) -> Res<'p, Vec<TypeId>> {
        let mut types = vec![];
        for arg in bindings {
            if let Some(e) = arg.ty.expr_ref() {
                self.last_loc = Some(e.loc);
            }
            assert!(self.infer_binding_progress(constants, arg)?, "{arg:?}");
            types.push(arg.unwrap());
        }
        Ok(types)
    }

    fn infer_arg(&mut self, func: FuncId) -> Res<'p, TypeId> {
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
        let f = &self.program[func];
        // TODO: bad things are going on. it changes behavior if this is a debug_assert.
        //       oh fuck its because of the type_of where you can backtrack if you couldn't infer.
        //       so making it work in debug with debug_assert is probably the better outcome.
        assert!(!f.evil_uninit, "{}", self.pool.get(f.name));
        if let (Some(arg), Some(ret)) = (f.finished_arg, f.finished_ret) {
            return Ok(Some(FnType { arg, ret }));
        }

        Ok(mut_replace!(self.program[func], |mut f: Func<'p>| {
            let state = DebugState::ResolveFnType(func);
            self.last_loc = Some(f.loc);
            self.push_state(&state);
            if f.finished_arg.is_none() {
                let types = self.infer_pattern(&f.closed_constants, &mut f.arg.bindings)?;
                let arg = self.program.tuple_of(types);
                f.finished_arg = Some(arg);
            }
            if f.finished_ret.is_none() {
                if !f.has_tag(Flag::Generic) && self.infer_types_progress(&f.closed_constants, &mut f.ret)? {
                    f.finished_ret = Some(f.ret.unwrap());

                    self.pop_state(state);
                    let ty = f.unwrap_ty();
                    Ok((f, Some(ty)))
                } else {
                    self.pop_state(state);
                    Ok((f, None))
                }
            } else {
                self.pop_state(state);
                let ty = f.unwrap_ty();
                Ok((f, Some(ty)))
            }
        }))
    }

    // If we have access to a 'result' context, might as well try compiling the expression first and see if its something trivial that doesn't need to spin up a whole new function.
    // It shouldn't really waste any time because most of the work compiling will be saved and you'd have to do anyway for the shim function body.
    // There's some subtle order of execution difference that means you can't use this for the type in @as which is a bit scary.
    // TODO: maybe i need to renumber declared variables?
    fn immediate_eval_expr_in(&mut self, result: &mut FnWip<'p>, mut e: FatExpr<'p>, ret_ty: TypeId) -> Res<'p, Values> {
        let res = self.compile_expr(result, &mut e, Some(ret_ty))?;
        match res {
            Structured::Const(ty, val) => {
                self.type_check_arg(ty, ret_ty, "immediate_eval_expr_in")?;
                Ok(val)
            }
            _ => self.immediate_eval_expr(&result.constants, e, ret_ty),
        }
    }

    // Here we're not in the context of a specific function so the caller has to pass in the constants in the environment.
    fn immediate_eval_expr(&mut self, constants: &Constants<'p>, mut e: FatExpr<'p>, ret_ty: TypeId) -> Res<'p, Values> {
        // println!("- Eval {} as {:?}", e.log(self.pool), ret_ty);
        match e.deref_mut() {
            Expr::Value { value, .. } => return Ok(value.clone()),
            Expr::GetVar(var) => {
                if let Some((value, _)) = constants.get(*var) {
                    debug_assert_ne!(value, Values::One(Value::Poison));
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
                // TODO: this only helps if some can be quick-evaled by special cases above, otherwise makes it worse.
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
        let name = format!("$eval_{}${}$", self.anon_fn_counter, e.deref().log(self.pool));
        let (arg, ret) = Func::known_args(TypeId::unit(), ret_ty, e.loc);
        let mut fake_func = Func::new(self.pool.intern(&name), arg, ret, Some(e.clone()), e.loc, false);
        fake_func.closed_constants = constants.clone();
        fake_func.finished_arg = Some(TypeId::unit());
        fake_func.finished_ret = Some(ret_ty);
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
        let result = self.run(func_id, Value::Unit.into(), ExecTime::Comptime, None)?;
        logln!(
            "COMPUTED: {} -> {:?} under {}",
            e.log(self.pool),
            result,
            self.program[func_id].log(self.pool)
        );
        self.pop_state(state);
        Ok(result)
    }

    #[track_caller]
    fn to_type(&mut self, value: Values) -> Res<'p, TypeId> {
        match value {
            Values::One(Value::Unit) => Ok(TypeId::unit()),
            Values::One(Value::Type(id)) => Ok(id),
            Values::Many(values) => {
                let values: Res<'_, Vec<_>> = values.into_iter().map(|v| self.to_type(v.into())).collect();
                Ok(self.program.tuple_of(values?))
            }
            _ => {
                err!(CErr::TypeError("Type", value))
            }
        }
    }

    pub(crate) fn add_func(&mut self, mut func: Func<'p>, constants: &Constants<'p>) -> Res<'p, FuncId> {
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
    pub fn promote_closure(&mut self, result: &mut FnWip<'p>, expr: &mut FatExpr<'p>) -> Res<'p, FuncId> {
        if let Expr::Closure(func) = expr.deref_mut() {
            func.module = result.module;
            let f = self.add_func(mem::take(func), &result.constants)?;
            if self.infer_types(f)?.is_none() {
                // TODO: i only do this for closures becuase its a pain to thread the &mut result through everything that calls infer_types().
                mut_replace!(self.program[f], |mut func: Func<'p>| {
                    if let Some(body) = &mut func.body {
                        if let Some(ret_ty) = self.type_of(result, body)? {
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
        arg: &mut FatExpr<'p>,
        requested: Option<TypeId>, // TODO: allow giving return type to infer
    ) -> Res<'p, Structured> {
        if !arg.ty.is_unknown() {
            // We've been here before and already replaced closures with calls.
            return Ok(Structured::RuntimeOnly(TypeId::unit()));
        }
        let unit = TypeId::unit();
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        let mut unit_expr = FatExpr::synthetic(Expr::unit(), arg.loc);
        if let Expr::Tuple(parts) = arg.deref_mut() {
            let cond = self.compile_expr(result, &mut parts[0], Some(TypeId::bool()))?;
            self.type_check_arg(cond.ty(), TypeId::bool(), "bool cond")?;

            // If its constant, don't even bother emitting the other branch
            // TODO: option to toggle this off for testing.
            if let Structured::Const(_, val) = cond {
                let cond = val.single()?.to_bool().unwrap();
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

            let true_ty = if let Some(if_true) = self.maybe_direct_fn(result, &mut parts[1], &mut unit_expr, requested)? {
                let if_true = if_true.single()?;
                self.program[if_true].add_tag(Flag::Inline);
                let true_arg = self.infer_arg(if_true)?;
                self.type_check_arg(true_arg, unit, sig)?;
                self.emit_call_on_unit(result, if_true, &mut parts[1], requested)?.ty()
            } else {
                ice!("if second arg must be func not {}", parts[1].log(self.pool));
            };
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
            arg.ty = TypeId::unit();
            Ok(Structured::RuntimeOnly(true_ty))
        } else {
            ice!("if args must be tuple not {:?}", arg);
        }
    }

    fn emit_call_while(&mut self, result: &mut FnWip<'p>, arg: &mut FatExpr<'p>) -> Res<'p, Structured> {
        if !arg.ty.is_unknown() {
            // We've been here before and already replaced closures with calls.
            return Ok(Structured::RuntimeOnly(TypeId::unit()));
        }

        let sig = "while(fn(Unit) bool, fn(Unit) Unit)";
        let mut unit_expr = FatExpr::synthetic(Expr::unit(), arg.loc);
        if let Expr::Tuple(parts) = arg.deref_mut() {
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
            arg.ty = TypeId::unit();
        } else {
            ice!("if args must be tuple not {:?}", arg);
        }

        Ok(Structured::RuntimeOnly(TypeId::unit()))
    }

    #[track_caller]
    fn type_check_arg(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
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

    fn struct_type(&mut self, result: &FnWip<'p>, pattern: &mut Pattern<'p>) -> Res<'p, TypeInfo<'p>> {
        // TODO: maybe const keyword before name in func/struct lets you be generic.
        let types = self.infer_pattern(&result.constants, &mut pattern.bindings)?;
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
                let var_info = self.program.vars[var.1]; // TODO: the type here isn't set.
                assert_eq!(var_info.kind, VarType::Var, "Only 'var' can be reassigned (not let/const). {:?}", place);
                let oldty = result.vars.get(var);
                let oldty = *unwrap!(oldty, "SetVar: var must be declared: {}", var.log(self.pool));

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

    // TODO: desugar field access into this.
    fn index_expr(&mut self, _result: &mut FnWip<'p>, container_ptr: Structured, index: usize) -> Res<'p, Structured> {
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

        let arg_ty = self.infer_arg(fid)?;
        let mut arg_val = self.compile_expr(result, arg_expr, Some(arg_ty))?;
        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
        if arg_val.is_empty() {
            arg_val = self.program.load_value(Value::Unit);
        }
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
        let state = DebugState::Msg(format!("Bake CT Only {original_f:?}"));
        self.push_state(&state);
        // Some part of the argument must be known at comptime.
        // You better hope compile_expr noticed and didn't put it in a stack slot.
        let func = &self.program[original_f];
        let pattern = func.arg.flatten();

        if pattern.len() == 1 {
            let (name, _, kind) = pattern.into_iter().next().unwrap();
            debug_assert_eq!(kind, VarType::Const);
            let name = unwrap!(name, "arg needs name (unreachable?)");
            let current_fn = self.bind_const_arg(original_f, name, arg_val)?;
            arg_expr.expr = Expr::unit();
            arg_expr.ty = TypeId::unit();

            let ty = self.program.func_type(current_fn);
            f_expr.expr = Expr::Value {
                ty,
                value: Value::GetFn(current_fn).into(),
            };
            f_expr.ty = ty;
            Ok(current_fn)
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
                    let mut current_fn = original_f;
                    let mut skipped_args = vec![];
                    let mut skipped_types = vec![];
                    let mut removed = 0;
                    for (i, ((name, ty, kind), arg_value)) in pattern.into_iter().zip(arg_values).enumerate() {
                        if kind == VarType::Const {
                            let name = unwrap!(name, "arg needs name (unreachable?)");
                            // bind_const_arg handles adding closure captures.
                            current_fn = self.bind_const_arg(current_fn, name, arg_value)?;
                            // TODO: this would be better if i was iterating backwards
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

                    let ty = self.program.func_type(current_fn);
                    f_expr.expr = Expr::Value {
                        ty,
                        value: Value::GetFn(current_fn).into(),
                    };
                    f_expr.ty = ty;
                    let f_ty = self.program.fn_ty(ty).unwrap();
                    self.type_check_arg(arg_ty, f_ty.arg, "sanity: post bake arg")?;
                    self.pop_state(state);
                    // Don't need to explicitly force capturing because bind_const_arg added them if any args were closures.
                    Ok(current_fn)
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

    /// The first two can be used for early bootstrapping since they just look at the ast without comptime eval.
    /// - tuple of string literals -> llvm-ir
    /// - tuple of 32-bit int literals -> aarch64 asm ops
    /// - anything else, comptime eval expecting Slice(u32) -> aarch64 asm ops
    fn inline_asm_body(&mut self, result: &FnWip<'p>, f: FuncId, asm: &mut FatExpr<'p>) -> Res<'p, ()> {
        let src = asm.log(self.pool);
        let asm_ty = Vec::<u32>::get_type(self.program);
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
            let asm_bytes: Res<'p, Vec<u32>> = parts
                .iter()
                .map(|op| {
                    if let Ok((ty, val)) = bit_literal(op, self.pool) {
                        assert_eq!(ty.bit_count, 32);
                        Ok(val as u32)
                    } else {
                        err!("not int",)
                    }
                })
                .collect();

            if let Ok(ops) = asm_bytes {
                ops
            } else {
                // TODO: support dynamic eval to string for llvm ir.
                let ops = self.immediate_eval_expr(&result.constants, asm.clone(), asm_ty)?;
                unwrap!(Vec::<u32>::deserialize(&mut ops.vec().into_iter()), "")
            }
        } else {
            let ops = self.immediate_eval_expr(&result.constants, asm.clone(), asm_ty)?;
            unwrap!(Vec::<u32>::deserialize(&mut ops.vec().into_iter()), "")
        };
        self.program[f].add_tag(Flag::Aarch64);
        outln!(LogTag::Jitted, "=======\ninline asm\n~~~{src}~~~");
        for op in &ops {
            outln!(LogTag::Jitted, "{op:#05x}");
        }
        outln!(LogTag::Jitted, "\n=======");
        // TODO: emit into the Jitted thing instead of this.
        //       maybe just keep the vec, defer dealing with it and have bc_to_asm do it?
        //       but then interp can't use it.
        self.program[f].jitted_code = Some(ops.clone());
        let map = crate::export_ffi::copy_to_mmap_exec(ops);
        // Need to set comptime_addr because interp might try to call it and it doesn't know it needs to go through bc_to_asm first.
        self.program[f].comptime_addr = Some(map.1 as u64);
        let _ = Box::leak(map.0); // TODO: dont leak
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

    fn resolve_import(&mut self, module: ModuleId, name: Ident<'_>) -> Res<'p, (Values, TypeId)> {
        let module_f = self.get_module(module)?;
        if let Some(var) = self.program[module].exports.get(&name) {
            let value = unwrap!(
                self.program[module_f].closed_constants.get(*var),
                "missing export: {}",
                var.log(self.pool)
            );
            Ok(value) // TODO: include type
        } else {
            err!("Module {} does not export {}", module.0, self.pool.get(name))
        }
    }

    pub fn get_module(&mut self, module: ModuleId) -> Res<'p, FuncId> {
        match self.program[module].toplevel {
            ModuleBody::Ready(f) => Ok(f),
            ModuleBody::Compiling(_) | ModuleBody::Resolving => err!("Module {} is not ready yet. Circular dependency?", module.0),
            ModuleBody::Parsed(_) => {
                let body = mem::replace(&mut self.program[module].toplevel, ModuleBody::Resolving);
                let mut body = if let ModuleBody::Parsed(f) = body { f } else { unreachable!() };
                ResolveScope::of(&mut body, self, vec![])?; // TODO: nested directives
                let f = self.compile_module(body)?;
                Ok(f)
            }
            ModuleBody::Src(_) => unreachable!(),
        }
    }

    pub fn resolve_module(&mut self, current: ModuleId, mut import_path: &[Ident<'p>]) -> Res<'p, ModuleId> {
        let mut module = ModuleId(0);
        if import_path[0] == Flag::This.ident() {
            module = current;
            import_path = &import_path[1..];
        } else if import_path[0] == Flag::Super.ident() {
            module = unwrap!(self.program[current].parent, "tried to refer to parent of root module");
            import_path = &import_path[1..];
        };

        for path in import_path {
            if let Some(found) = self.program[module].children.get(path) {
                module = *found
            } else {
                err!("Module not found {}", self.pool.get(*path))
            }
        }

        Ok(module)
    }

    fn decl_var(
        &mut self,
        result: &mut FnWip<'p>,
        name: Var<'p>,
        ty: &mut LazyType<'p>,
        value: &mut Option<FatExpr<'p>>,
        kind: VarType,
        annotations: &[Annotation<'p>],
    ) -> Res<'p, ()> {
        let no_type = matches!(ty, LazyType::Infer);
        self.infer_types_progress(&result.constants, ty)?;

        match kind {
            VarType::Const => {
                let mut val = match value {
                    Some(value) => {
                        // TODO: just treat @builtin as a normal expression instead of a magic thing that const looks for so you can do 'const Type: @builtin("Type") = @builtin("Type");'
                        //       then you dont have to eat this check for every constant, you just get there when you get there.
                        if let Some((arg, _)) = value.expr.as_prefix_macro(Flag::Builtin) {
                            let name_str = self.pool.get(name.0);
                            let expected = unwrap!(arg.as_string(), "@builtin requires string argument: {name_str}");
                            assert_eq!(name.0, expected, "builtin name mismatch: {:?}", name_str);
                            if no_type {
                                if name_str == "Type" {
                                    *ty = LazyType::Finished(TypeId::ty());
                                } else {
                                    err!("@builtin const requires type hint {}", name.log(self.pool));
                                }
                            }
                            unwrap!(self.builtin_constant(name_str), "non-blessed const marked @builtin: {:?}", name_str)
                                .0
                                .into()
                        } else {
                            // You don't need to precompile, immediate_eval_expr will do it for you.
                            // However, we want to update value.ty on our copy to use below to give constant pointers better type inference.
                            // This makes addr_of const for @enum work
                            let res = self.compile_expr(result, value, ty.ty())?;
                            self.immediate_eval_expr_in(result, value.clone(), res.ty())?
                        }
                    }
                    None => {
                        if let Some(import_path) = annotations.iter().find(|a| a.name == Flag::Import.ident()) {
                            let import_path = unwrap!(import_path.args.as_ref(), "@import requires argument");
                            let module = if let Some(module) = import_path.as_int() {
                                let m = ModuleId(module as usize);
                                assert!(self.program.modules.len() > m.0);
                                m
                            } else {
                                let import_path = import_path.parse_dot_chain()?;
                                self.resolve_module(result.module.unwrap(), &import_path)?
                            };
                            let (val, found_ty) = self.resolve_import(module, name.0)?;
                            // This fixes importing const @enum(T) being seen as VoidPtr
                            if let LazyType::Infer = ty {
                                *ty = LazyType::Finished(found_ty);
                            }
                            val
                        } else {
                            let name = self.pool.get(name.0);
                            err!("const binding '{name}' must have a value",)
                        }
                    }
                };

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

                let found_ty = self.program.type_of_raw(&val);
                let final_ty = if let Some(expected_ty) = ty.ty() {
                    if self.program[expected_ty] == TypeInfo::Type {
                        // HACK. todo: general overloads for cast()
                        val = Value::Type(self.to_type(val)?).into()
                    } else {
                        // TODO: you want the type check but doing it against type_of_raw is kinda worthless
                        self.type_check_arg(found_ty, expected_ty, "const decl")?;
                    }

                    expected_ty
                } else {
                    let found = if let Some(value) = value {
                        assert!(!(value.ty.is_unknown() || value.ty.is_any()));
                        // HACK This makes addr_of const for @enum work
                        value.ty
                    } else {
                        found_ty
                    };
                    *ty = LazyType::Finished(found);
                    found
                };

                let val_expr = Expr::Value {
                    ty: final_ty,
                    value: val.clone(),
                };
                if let Some(e) = value {
                    e.expr = val_expr;
                } else {
                    // TODO: use stmt.loc
                    *value = Some(FatExpr::synthetic(val_expr, garbage_loc()));
                }
                let prev = result.constants.insert(name, (val, final_ty));
                assert!(prev.is_none());
                // println!("{}", self.program.log_consts(&result.constants));
            }
            VarType::Let | VarType::Var => {
                let final_ty = match value {
                    None => {
                        if no_type {
                            err!("uninit vars require type hint {}", name.log(self.pool));
                        }
                        ty.unwrap()
                    }
                    Some(value) => {
                        let value = self.compile_expr(result, value, ty.ty())?;
                        if no_type {
                            *ty = LazyType::Finished(value.ty());
                            value.ty()
                        } else {
                            self.type_check_arg(value.ty(), ty.unwrap(), "var decl")?;
                            ty.unwrap()
                        }
                    }
                };
                // TODO: this is so emit_ir which can't mutate program can find it.
                self.program.intern_type(TypeInfo::Ptr(final_ty));

                let _prev = result.vars.insert(name, final_ty);
                // TODO: closures break this
                // assert!(prev.is_none(), "shadow is still new var");
            }
        }
        Ok(())
    }

    fn compile_split_call(
        &mut self,
        result: &mut FnWip<'p>,
        expr: &mut FatExpr<'p>,
        mut ct: FuncId,
        mut rt: FuncId,
        _requested: Option<TypeId>,
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

        add_unique(&mut result.callees, (ct, ExecTime::Comptime));
        add_unique(&mut result.callees, (rt, ExecTime::Runtime));
        self.ensure_compiled(ct, ExecTime::Comptime)?;
        self.ensure_compiled(rt, ExecTime::Runtime)?;

        Ok(Structured::RuntimeOnly(ret_ty))
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

fn add_unique<T: PartialEq>(vec: &mut Vec<T>, new: T) -> bool {
    if !vec.contains(&new) {
        vec.push(new);
        return true;
    }
    false
}

pub fn bit_literal<'p>(expr: &FatExpr<'p>, _pool: &StringPool<'p>) -> Res<'p, (IntTypeInfo, i64)> {
    if let Expr::SuffixMacro(name, arg) = &expr.expr {
        if *name == Flag::From_Bit_Literal.ident() {
            if let Expr::Tuple(parts) = arg.deref().deref() {
                if let Expr::Value { value, .. } = parts[0].deref() {
                    let bit_count = value.clone().single()?.to_int()?;
                    if let Expr::Value { value, .. } = parts[1].deref() {
                        let val = value.clone().single()?.to_int()?;
                        return Ok((IntTypeInfo { bit_count, signed: false }, val));
                    }
                }
            }
        }
    }
    err!("not int",)
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
                self.compiler
                    .compile_expr(self.result, arg, Some(expr_ty))
                    .unwrap_or_else(|e| panic!("Expected comple ast but \n{e:?}\n{:?}", arg.log(self.compiler.pool))); // TODO
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
