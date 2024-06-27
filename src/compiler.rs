//! Converts ASTs into... simpler asts.
//! Type checking, overload resolution, implicit function calls, inlining, monomorphization, etc.
//! The rest of the compiler can be organised as nicely as it wants but this is where we draw the rest of the fucking owl.

#![allow(clippy::wrong_self_convention)]

use crate::self_hosted::{show_error_line, Span};
use core::slice;
use std::collections::HashSet;
use std::ffi::CString;
use std::fmt::Write;
use std::hash::Hash;
use std::mem::{self, transmute};
use std::ops::DerefMut;
use std::ptr::{self, addr_of};
use std::sync::atomic::AtomicIsize;
use std::{ops::Deref, panic::Location};

use crate::ast::{
    Annotation, Binding, CallConv, FatStmt, Flag,
    FnFlag::{self, *},
    FuncImpl, IntTypeInfo, LabelId, Name, OverloadSet, OverloadSetId, Pattern, RenumberVars, ScopeId, TargetArch, Var, VarType, WalkAst,
};

use crate::bc_to_asm::{emit_aarch64, Jitted};
use crate::emit_bc::emit_bc;
use crate::export_ffi::{struct_macro, tagged_macro, type_macro, BigOption, BigResult, ExportVTable, ImportVTable, IMPORT_VTABLE};
use crate::ffi::InterpSend;
use crate::logging::PoolLog;
use crate::overloading::where_the_fuck_am_i;
use crate::scope::ResolveScope;
use crate::{
    ast::{Expr, FatExpr, FnType, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo},
    self_hosted::Ident,
};
use crate::{bc::*, extend_options, ffi, unwrap2, Map, STACK_MIN, STATS};

use crate::{assert, assert_eq, err, ice, unwrap};
use BigResult::*;
#[repr(C)]
#[derive(Clone)]
pub struct CompileError<'p> {
    pub internal_loc: Option<&'static Location<'static>>,
    pub loc: Option<Span>,
    pub reason: CErr<'p>,
    pub trace: String,
}

#[repr(C, i64)]
#[derive(Clone, Debug)]
pub enum CErr<'p> {
    UndeclaredIdent(Ident<'p>),
    TypeError(&'static str, Values),
    TypeCheck(TypeId, TypeId, &'static str),
    AmbiguousCall,
    NeedsTypeHint(&'static str),
    Fatal(String),
}

pub type Res<'p, T> = BigResult<T, Box<CompileError<'p>>>;

#[repr(C)]
pub struct Compile<'a, 'p> {
    pub program: &'a mut Program<'p>, // SAFETY: this must be the first field (repr(C))
    temp_vtable: *const ImportVTable,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    pub debug_trace: Vec<DebugState<'p>>,
    pub anon_fn_counter: usize,
    currently_inlining: Vec<FuncId>,
    currently_compiling: Vec<FuncId>, // TODO: use this to make recursion work
    pub last_loc: Option<Span>,
    pub tests: Vec<FuncId>,
    pub tests_broken: Vec<FuncId>,
    pub aarch64: Jitted,
    pub next_label: usize,
    pub wip_stack: Vec<(Option<FuncId>, ExecStyle)>,
    #[cfg(feature = "cranelift")]
    pub cranelift: crate::cranelift::JittedCl<cranelift_jit::JITModule>,
    pub make_slice_t: Option<FuncId>,
    pending_redirects: Vec<(FuncId, FuncId)>,
    pub export: Vec<FuncId>,
    pub driver_vtable: (Box<ExportVTable>, *mut ()),
    
    pub already_loaded: HashSet<Ident<'p>>,
}
#[repr(C)]
#[derive(Debug, Clone)]
pub struct Scope<'p> {
    pub parent: ScopeId,
    pub constants: Map<Var<'p>, (FatExpr<'p>, LazyType<'p>)>,
    pub rt_types: Map<Var<'p>, TypeId>,
    pub vars: Vec<BlockScope<'p>>,
    pub depth: usize,
    pub name: Ident<'p>,
    pub block_in_parent: usize,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct BlockScope<'p> {
    pub vars: Vec<Var<'p>>,
    pub parent: usize,
}

#[repr(C, i64)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DebugState<'p> {
    Compile(FuncId, Ident<'p>),
    EnsureCompiled(FuncId, Ident<'p>, ExecStyle),
    RunInstLoop(FuncId, Ident<'p>),
    ComptimeCall(FuncId, Ident<'p>),
    ResolveFnType(FuncId, Ident<'p>),
    EvalConstants(FuncId, Ident<'p>),
    Msg(String),
    EmitBody(FuncId, Ident<'p>),
    EmitCapturingCall(FuncId, Ident<'p>),
    ResolveFnRef(Var<'p>),
    TypeOf,
    ResolveConstant(Var<'p>),
}

pub static mut EXPECT_ERR_DEPTH: AtomicIsize = AtomicIsize::new(0);

impl<'a, 'p> Compile<'a, 'p> {
    pub fn new(program: &'a mut Program<'p>) -> Self {
        let c = Self {
            temp_vtable: addr_of!(IMPORT_VTABLE),
            already_loaded: Default::default(),
            driver_vtable: (Default::default(), ptr::null_mut()),
            export: vec![],
            pending_redirects: vec![],
            make_slice_t: None,
            wip_stack: vec![],
            debug_trace: vec![],
            anon_fn_counter: 0,
            currently_inlining: vec![],
            last_loc: None,
            currently_compiling: vec![],
            program,
            aarch64: Jitted::new(1 << 26), // Its just virtual memory right? I really don't want to ever run out of space and need to change the address.

            tests: vec![],
            tests_broken: vec![],
            next_label: 0,

            #[cfg(feature = "cranelift")]
            cranelift: crate::cranelift::JittedCl::default(),
        };
        c.program.pool.new_scope(ScopeId::from_index(0), Flag::TopLevel.ident(), 0);
        // TODO: HACK: for emit_relocatable_constant
        let ty = <(i64, i64)>::get_or_create_type(c.program);
        c.program.finish_layout(ty).unwrap();
        c
    }

    fn create_slice_type(&mut self, expect: TypeId, loc: Span) -> Res<'p, TypeId> {
        let value = to_values(self.program, expect)?;
        let f = self.as_literal(unwrap!(self.make_slice_t, "slice type not ready!"), loc)?;
        // f.ty = ty; // TODO: it doesn't compile the function if the type here is FuncId?
        let a = FatExpr::synthetic_ty(Expr::Value { value, coerced: false }, loc, TypeId::ty);
        let s_ty = FatExpr::synthetic(Expr::Call(Box::new(f), Box::new(a)), loc);
        self.immediate_eval_expr_known(s_ty)
    }

    #[track_caller]
    fn as_values<T: InterpSend<'p>>(&mut self, ret: T) -> Res<'p, Values> {
        to_values(self.program, ret)
    }

    fn contextual_field(&mut self, name: Ident<'p>, expr: &mut FatExpr<'p>, ty: TypeId) -> Res<'p, TypeId> {
        match &self.program[ty] {
            TypeInfo::Enum { fields, .. } => {
                let field = fields.iter().find(|f| f.0 == name);
                let (_, value) = unwrap!(
                    field,
                    "contextual field not found {} for {ty:?}: {}\nexpected: {:?}",
                    self.program.pool.get(name),
                    self.program.log_type(ty),
                    fields.iter().map(|f| self.program.pool.get(f.0)).collect::<Vec<_>>()
                );
                expr.set(value.clone(), ty);
                Ok(ty)
            }
            TypeInfo::Tagged { cases } => {
                let field = cases.iter().find(|f| f.0 == name);
                // TODO: sad copy paste.
                let (_, field) = unwrap!(
                    field,
                    "contextual field not found {} for {ty:?}: {}\nexpected: {:?}",
                    self.program.pool.get(name),
                    self.program.log_type(ty),
                    cases.iter().map(|f| self.program.pool.get(f.0)).collect::<Vec<_>>()
                );
                let field = self.program.raw_type(*field); // just incase its a named ffi thing.
                assert!(
                    field.is_unit(),
                    "contextual field {} of tagged union must be unit found {}",
                    self.program.pool.get(name),
                    self.program.log_type(field)
                );

                let unit = self.as_literal((), expr.loc)?;

                expr.expr = Expr::StructLiteralP(Pattern {
                    bindings: vec![Binding {
                        name: Name::Ident(name),
                        ty: LazyType::Infer,
                        default: BigOption::Some(unit),
                        kind: VarType::Let,
                    }],
                    loc: expr.loc,
                });
                expr.ty = ty;
                self.compile_expr(expr, Some(ty))
            }
            TypeInfo::Struct { fields, .. } => {
                // TODO: deny leading dot with inferred type because that only makes sense for enums so we can give a better error message here.

                let field = fields.iter().find(|f| f.name == name);
                let field = unwrap!(
                    field,
                    "{} is not a field of struct {ty:?}: {}\nexpected: {:?}",
                    self.program.pool.get(name),
                    self.program.log_type(ty),
                    fields.iter().map(|f| self.program.pool.get(f.name)).collect::<Vec<_>>()
                );
                assert_eq!(field.kind, VarType::Const, 
                    "Field {} is not constant on struct {ty:?} {:?}. \nDid you mean to access a field on an instance of the type instead of the type itself?",
                    self.program.pool.get(name),
                    self.program.log_type(ty)
                );
                let value = unwrap2!(field.default.clone(), "unreachable. constant fields have value");
                expr.set(value, field.ty);
                Ok(field.ty)
            }
            _ => err!("no contextual fields for type {ty:?} {}", self.program.log_type(ty)),
        }
    }

    fn builtin_prefix_macro(&mut self, handler: &mut FatExpr<'p>, arg: &mut FatExpr<'p>, target: &mut FatExpr<'p>) -> Res<'p, FatExpr<'p>> {
        let Expr::GetVar(name) = handler.expr else {
            err!("macro invocations must be GetVar while bootstrapping. not: ({})", handler.log(self.program.pool))
        };
        let name = Flag::try_from(name.name)?;
        match name {
            Flag::Type => {
                assert!(target.is_raw_unit());
                return Ok(type_macro(self, mem::take(arg)));
            }
            Flag::Struct => {
                assert!(target.is_raw_unit());
                return Ok(struct_macro(self, mem::take(arg)));
            }
            Flag::Tagged => {
                assert!(target.is_raw_unit());
                return Ok(tagged_macro(self, mem::take(arg)));
            }
            Flag::Enum => {
                assert!(!target.is_raw_unit(), "@enum type must be specified while bootstrapping.");
                return self.enum_constant_macro(mem::take(arg), mem::take(target));
            }
            Flag::Builtin => {
                assert!(target.is_raw_unit());
               return Ok(self.get_builtin_macro(mem::take(arg)));
            }
            _ => err!("unknown macro invocation {:?} while bootstrapping", name),
        }
    }
    
    pub extern "C" fn get_builtin_macro(&mut self, arg: FatExpr<'p>) -> FatExpr<'p> {
        let Some(name) = arg.as_ident() else { panic!("@builtin requires argument",) };
        let Some((value, ty)) = self.builtin_constant(name) else {
            panic!("unknown @builtin: {:?}", self.program.pool.get(name))
        } ;
        FatExpr::synthetic_ty(Expr::Value { value, coerced: false }, arg.loc, ty)
    }

    // :bake_relocatable_value
    pub(crate) fn check_for_new_aot_bake_overloads(&mut self) -> Res<'p, ()> {
        let os = self.program.bake_os.expect("cannot aot during bootstrapping.");
        let prev = self.program[os].ready.len();
        self.compute_new_overloads(os, Some(1));
        // TODO: this will miss them if someone caused new things to resolve outside this function.
        //       so if you ever call bake_relocatable_value manually. :FUCKED -- Jun 19
        let current = self.program[os].ready.len();
        let new = self.program[os].ready[prev..current].to_vec();
        for f in new {
            let args = &self.program[f.func].arg.bindings;
            if args.len() != 1 {
                continue;
            }
            let ret = self.program[f.func].finished_ret.unwrap();
            let TypeInfo::Struct { fields, .. } = &self.program[ret] else {
                continue;
            };
            // TODO: make sure its actually a Slice(BakedEntry)
            if fields.len() != 2 {
                continue;
            }

            let first = args[0].ty.unwrap();
            let TypeInfo::Ptr(ty) = self.program[first] else { continue };
            // TODO: you really want to do the compile lazily, so you only do it when you actually try to emit a type that needs it. 
            //       but EmitBc doesn't have mutable access to the compiler so its painful.
            //       but this is cripplingly stupid with the current model of eh fuck it just load all the code in the universe every time. 
            //       especially once i overload it for things in generics, currently slices are a magic special case.    -- Jun 19  :SLOW
            self.compile(f.func, ExecStyle::Jit)?;
            let f = unwrap!(self.aarch64.get_fn(f.func), "failed to compile function");
            let prev = self.program.custom_bake_constant.insert(ty, unsafe { transmute(f) });
            assert!(prev.is_none(), "conflicting overload for bake AOT constant");
        }
        Ok(())
    }

    #[track_caller]
    pub fn log_trace(&self) -> String {
        let mut out = String::new();
        writeln!(out, "=== TRACE ===").unwrap();
        // writeln!(out, "{}", Location::caller()).unwrap();  // Always called from the same place now so this is useless

        for (i, s) in self.debug_trace.iter().enumerate() {
            writeln!(out, "{i} {};", s.log(self.program.pool, self.program)).unwrap();
        }
        writeln!(out, "=============").unwrap();
        out
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

        if let Err(mut e) = self.ensure_compiled(f, ExecStyle::Jit) {
            e.loc = e.loc.or(self.last_loc);
            return Err(e);
        }
        Ok(f)
    }

    // goal is to unify all the places you have to check the stupid tags.
    // this is safe to call even if you don't fully know the types yet, and you probably have to before trying to call it to check if it needs to be inlined.
    fn update_cc(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.program[f].has_tag(Flag::Inline) {
            self.program[f].set_cc(CallConv::Inline)?;
        }
        if self.program[f].has_tag(Flag::One_Ret_Pic) {
            self.program[f].set_cc(CallConv::OneRetPic)?;
        }
        if self.program[f].cc == BigOption::Some(CallConv::Inline) {
            // TODO: err on other cc tags
            // skip cause we dont care about big arg checks.
            return Ok(());
        }

        if let Some(ty) = self.program[f].finished_ty() {
            let comp_ctx = self.program[f].has_tag(Flag::Ct);
            self.program.finish_layout(ty.arg)?;
            self.program.finish_layout(ty.ret)?;
            if comp_ctx {
                debug_assert!(
                    self.program[f].body.comptime_addr().is_some() || self.program[f].has_tag(Flag::Comptime_Addr),
                    "#ct is only for compiler builtins"
                );
                self.program[f].set_cc(CallConv::CCallRegCt)?;
            } else {
                self.program[f].set_cc(CallConv::CCallReg)?;
            }
        }

        Ok(())
    }

    pub fn compile(&mut self, f: FuncId, when: ExecStyle) -> Res<'p, ()> {
        let r = self.compile_inner(f, when);
        self.tag_err(r)
    }

    pub fn compile_inner(&mut self, f: FuncId, when: ExecStyle) -> Res<'p, ()> {
        // TODO: this is a little sketchy. can one merged arch cause asm_done to be set and then other arches don't get processed?
        //       i dont think so cause overloading short circuits, and what ever arch did happen will be in the dispatch table.
        //       need to revisit when not jitting tho.   -- May 16
        if self.currently_compiling.contains(&f) {
            return Ok(());
        }
        if self.program[f].get_flag(AsmDone) {
            return Ok(());
        }
        let state = DebugState::Compile(f, self.program[f].name);
        self.push_state(&state);
        let before = self.debug_trace.len();
        debug_assert!(self.program[f].get_flag(NotEvilUninit));
        self.ensure_compiled(f, when)?;

        if let FuncImpl::Redirect(target) = self.program[f].body {
            debug_assert_ne!(f, target);
            self.compile(target, when)?;
            self.pending_redirects.push((f, target));
        }
        let mut i = 0;
        while let Some(&id) = &self.program[f].callees.get(i) {
            if id == f {
                continue;
            }
            let res = self.compile(id, when);
            self.tag_err(res)?;
            i += 1;
        }
        self.compile_asm_no_rec(f, when)?;
        let after = self.debug_trace.len();
        debug_assert_eq!(before, after);
        self.pop_state(state);
        // self.currently_compiling.retain(|check| *check != f);

        assert!(!matches!(self.program[f].body, FuncImpl::Empty));
        Ok(())
    }

    fn compile_asm_no_rec(&mut self, f: FuncId, when: ExecStyle) -> Res<'p, ()> {
        self.last_loc = Some(self.program[f].loc);

        let use_cl: bool =
            cfg!(feature = "cranelift") && (self.program[f].has_tag(Flag::Force_Cranelift) || self.program.comptime_arch == TargetArch::Cranelift);

        if let Some(code) = &self.program[f].body.jitted_aarch64() {
            self.aarch64.copy_inline_asm(f, code);
        }
        #[cfg(feature = "cranelift")]
        if use_cl && self.program[f].body.cranelift_emit().is_some() {
            crate::cranelift::emit_cl_intrinsic(self.program, &mut self.cranelift, f)?
        }

        if self.program[f].cc.unwrap() != CallConv::Inline {
            if let FuncImpl::Normal(_) = self.program[f].body {
                let body = emit_bc(self, f, when)?;
                // TODO: they can't try to do tailcalls between eachother because they disagress about what that means.

                #[cfg(feature = "cranelift")]
                let aarch = {
                    if use_cl {
                        let res = crate::cranelift::emit_cl(self, &body, f);
                        self.tag_err(res)?;
                    }
                    !use_cl
                };
                #[cfg(not(feature = "cranelift"))]
                let aarch = true;

                // && when == ExecStyle::Jit // TODO!!! this breaks llvm... which is odd. 
                if aarch {
                    let res = emit_aarch64(self, f, when, &body);
                    self.tag_err(res)?;
                }
            }
        }
        self.program[f].set_flag(AsmDone, true);
        self.last_loc = Some(self.program[f].loc);
        let cd = self.callees_done(f);
        self.program[f].set_flag(CalleesAsmDone, cd);
        // TODO: assert that if body is ::merged, none are Redirect/Normal/ComptimeAddr cause that doesn't really make sense.
        Ok(())
    }

    fn callees_done(&self, f: FuncId) -> bool {
        if self.program[f].get_flag(CalleesAsmDone) {
            return true;
        }
        let mut callees_done = true;
        for c in &self.program[f].callees {
            callees_done &= self.program[*c].get_flag(CalleesAsmDone);
        }
        for c in &self.program[f].mutual_callees {
            callees_done &= self.program[*c].get_flag(CalleesAsmDone);
        }
        callees_done
    }

    pub fn flush_callees(&mut self, f: FuncId) -> Res<'p, ()> {
        #[cfg(feature = "cranelift")]
        self.cranelift.flush_pending_defs(&mut self.aarch64)?;

        for (from, to) in self.pending_redirects.drain(0..) {
            self.last_loc = Some(self.program[to].loc);
            let ptr = unwrap!(
                self.aarch64.get_fn(to),
                "redirect target not ready {from:?} -> {to:?} {:#?}",
                self.program[to]
            );
            debug_assert!(self.aarch64.get_fn(from).is_none());
            self.aarch64.dispatch[from.as_index()] = ptr;
        }

        // TODO: need to recurse on the mutual_callees somewhere. this is just a hack.
        //       need ensure_compiled, compile, and like really_super_compile.
        for c in self.program[f].mutual_callees.clone() {
            let wip = (Some(c), ExecStyle::Jit);
            self.wip_stack.push(wip);
            self.ensure_compiled_force(f, ExecStyle::Jit)?;
            self.compile_asm_no_rec(c, ExecStyle::Jit)?;
            let cc = self.wip_stack.pop();
            debug_assert_eq!(Some(wip), cc);
        }

        #[cfg(feature = "cranelift")]
        self.cranelift.flush_pending_defs(&mut self.aarch64)?;
        // assert!(self.callees_done(f));
        for c in &self.program[f].callees {
            debug_assert!(
                self.aarch64.get_fn(*c).is_some(),
                "missing callee {}",
                self.program.pool.get(self.program[*c].name)
            )
        }
        for c in &self.program[f].mutual_callees {
            debug_assert!(
                self.aarch64.get_fn(*c).is_some(),
                "missing callee {}",
                self.program.pool.get(self.program[*c].name)
            )
        }

        self.flush_cpu_instruction_cache();
        Ok(())
    }

    pub fn run(&mut self, f: FuncId, arg: Values) -> Res<'p, Values> {
        let loc = self.last_loc;
        unsafe { STATS.jit_call += 1 };
        let state2 = DebugState::RunInstLoop(f, self.program[f].name);
        self.push_state(&state2);
        self.compile(f, ExecStyle::Jit)?;
        assert!(!matches!(self.program[f].body, FuncImpl::Empty));

        let ty = self.program[f].unwrap_ty();
        self.program.finish_layout_deep(ty.arg)?;
        self.program.finish_layout_deep(ty.ret)?;

        self.flush_callees(f)?;
        self.last_loc = loc;
        // cranelift puts stuff here too and so does comptime_addr
        let addr = unwrap!(self.aarch64.get_fn(f), "not compiled {f:?} {}", self.program.pool.get(self.program[f].name));

        let cc = self.program[f].cc.unwrap();

        #[cfg(target_arch = "aarch64")]
        debug_assert_eq!(addr as usize % 4, 0);

        debugln_call!(
            "Call {f:?} {} {};      callees={:?}",
            self.program.pool.get(self.program[f].name),
            addr as usize,
            self.program[f].callees
        );
        self.flush_cpu_instruction_cache();
        let expected_ret_bytes = self.program.get_info(ty.ret).stride_bytes;
        let mut ints = vec![];
        deconstruct_values(self.program, ty.arg, &mut ReadBytes { bytes: arg.bytes(), i: 0 }, &mut ints, &mut None)?;
        debugln_call!("IN: {ints:?}");
        let result = ffi::c::call(self, addr as usize, ty, ints, cc == CallConv::CCallRegCt);
        debugln_call!("OUT: {result:?}");

        let result = self.tag_err(result);
        if let Ok(result) = &result {
            assert_eq!(result.bytes().len(), expected_ret_bytes as usize, "{}\n{}", self.program.log_type(ty.ret), self.program[f].body.log(self.program.pool));
            self.pop_state(state2);
        }
        // println!("Done {f:?} {}", self.program.pool.get(self.program[f].name),);
        result
    }

    pub fn call_jitted<Arg: InterpSend<'p>, Ret: InterpSend<'p>>(&mut self, f: FuncId, arg: Arg) -> Res<'p, Ret> {
        self.ensure_compiled(f, ExecStyle::Jit)?;
        let ty = self.program[f].unwrap_ty();
        let arg_ty = Arg::get_or_create_type(self.program);
        self.type_check_arg(arg_ty, ty.arg, "sanity ICE jit_arg")?;
        let ret_ty = Ret::get_or_create_type(self.program);
        self.type_check_arg(ret_ty, ty.ret, "sanity ICE jit_ret")?;
        self.program.finish_layout_deep(arg_ty)?;
        self.program.finish_layout_deep(ret_ty)?;
        let arg = to_values(self.program, arg)?;
        // let again = Arg::deserialize_from_ints(self.program, &mut ReadBytes { bytes: &bytes, i: 0 }); // TOOD: remove!
        // debug_assert!(again.is_some());
        let ret = self.run(f, arg)?;
        from_values(self.program, ret)
    }

    // This is much less painful than threading it through the macros
    pub fn tag_err<T>(&self, res: Res<'p, T>) -> Res<'p, T> {
        res.map_err(|mut err| {
            err.trace = self.log_trace();
            err.loc = err.loc.or(self.last_loc);
            println!("{}", err.reason.log(self.program, self.program.pool));
            err
        })
    }

    pub fn hoist_constants(&mut self, body: &mut [FatStmt<'p>]) -> Res<'p, ()> {
        // Function tags (#when) are evaluated during decl_func but may refer to constants.
        // TODO: they should be allowed to refer to functiobns too so need to delay them.
        //       plus the double loop looks really dumb.
        for stmt in body.iter_mut() {
            if let Stmt::DeclVar { name, ty, value } = &mut stmt.stmt {
                if name.kind == VarType::Const {
                    self.program.pool.put_constant(*name, mem::take(value), mem::take(ty));
                    stmt.stmt = Stmt::Noop;
                }
            }
        }
        for stmt in body.iter_mut() {
            if let Stmt::DeclFunc(func) = &mut stmt.stmt {
                if let Err(e) = self.decl_func(mem::take(func)) {
                    ice!("hoist_constants mem::take so must not fail {:?}", e);
                }
                stmt.stmt = Stmt::Noop;
            }
        }
        Ok(())
    }

    pub fn ensure_compiled(&mut self, f: FuncId, when: ExecStyle) -> Res<'p, ()> {
        if !add_unique(&mut self.currently_compiling, f) {
            // This makes recursion work.
            return Ok(());
        }
        self.ensure_compiled_force(f, when)?;
        self.currently_compiling.retain(|check| *check != f);
        Ok(())
    }
    
    pub extern "C" fn save_function_header(&mut self, push: FuncId, pop: FuncId) {
        self.program.inject_function_header = Some((push, pop));
    }

    // Don't pass constants in because the function declaration must have closed over anything it needs.
    pub fn ensure_compiled_force(&mut self, f: FuncId, when: ExecStyle) -> Res<'p, ()> {
        if self.program[f].get_flag(EnsuredCompiled) {
            return Ok(());
        }
        self.program[f].set_flag(EnsuredCompiled, true);
        let func = &self.program[f];
        let wip = (Some(f), when);
        self.wip_stack.push(wip);
        debug_assert!(func.get_flag(NotEvilUninit));
        self.ensure_resolved_body(f)?;
        assert!(!self.program[f].get_flag(MayHaveAquiredCaptures), "closures need to be specialized");
        assert!(!self.program[f].any_const_args(), "{:?}", self.program[f].log(self.program.pool));
        let before = self.debug_trace.len();
        let state = DebugState::EnsureCompiled(f, self.program[f].name, when);
        self.push_state(&state);
        self.infer_types(f)?;
        self.emit_body(f)?;
        self.pop_state(state);
        let after = self.debug_trace.len();
        debug_assert_eq!(before, after);
        self.update_cc(f)?;

        // TODO: error safety ^
        let end = self.wip_stack.pop();
        assert_eq!(end, Some(wip), "ICE: fucked up the stack. {:?}", self.wip_stack);
        Ok(())
    }

    pub fn emit_special_body(&mut self, f: FuncId, no_redirect: bool) -> Res<'p, bool> {
        debug_assert!(self.program[f].get_flag(NotEvilUninit));
        self.ensure_resolved_sign(f)?;
        self.ensure_resolved_body(f)?;
        assert!(!self.program[f].get_flag(Generic));
        self.last_loc = Some(self.program[f].loc);

        let mut special = false;
        if let FuncImpl::Normal(body) = &mut self.program[f].body {
            if let Some(arg) = body.as_suffix_macro_mut(Flag::Asm) {
                let mut a = arg.clone();
                self.program[f].body = self.inline_asm_body(f, &mut a)?;
                special = true;
            }
        } else {
            assert!(self.program[f].finished_ty().is_some(), "fn without body needs type annotations.");

        }

        if let Some(types) = self.program[f].get_tag_mut(Flag::Redirect).cloned() {
            assert!(!no_redirect);

            let (arg, ret, os): (TypeId, TypeId, OverloadSetId) = self.immediate_eval_expr_known(types.args.unwrap())?;
            // TODO: this wont work cause you can get here while working on the overload set so its order dependednt.
            self.compute_new_overloads(os, None)?;
            let mut found: Vec<_> = self.program[os]
                .ready
                .iter()
                .filter(|o| o.arg == arg && o.ret == Some(ret))
                .cloned()
                .collect();
            if found.len() > 1 {
                self.do_merges(&mut found, os)?;
            }
            assert!(found.len() == 1, "{found:?}");
            assert!(matches!(self.program[f].body, FuncImpl::Empty));
            debug_assert_ne!(f, found[0].func);
            self.program[f].body = FuncImpl::Redirect(found[0].func);
            self.program[f].cc = self.program[found[0].func].cc;
            special = true;
        }

        if let Some(tag) = self.program[f].get_tag(Flag::Comptime_Addr) {
            let addr: i64 = self.immediate_eval_expr_known(unwrap2!(tag.args.clone(), ""))?;
            assert!(matches!(self.program[f].body, FuncImpl::Empty));
            self.program[f].body = FuncImpl::ComptimeAddr(addr as usize);
            self.aarch64.extend_blanks(f);
            self.aarch64.dispatch[f.as_index()] = addr as *const u8;
            special = true;
        }
        // TODO: put this in the driver program? put all the special fns in the driver program?
        if self.program[f].has_tag(Flag::Libc) {
            assert!(matches!(self.program[f].body, FuncImpl::Empty));
            // TODO: figure out the right path for different oses.
            // TODO: don't re-dlopen every time. tho i think its cached.
            let mut s = "/usr/lib/libc.dylib".as_bytes().to_vec();
            s.push(0);
            let s = CString::from_vec_with_nul(s).unwrap();

            let libc = unsafe { libc::dlopen(s.as_ptr(), libc::RTLD_LAZY) };
            let s = self.program.pool.get_c_str(self.program[f].name);
            let addr = unsafe { libc::dlsym(libc, s as *const i8) };
            if addr.is_null() {
                // TODO: warn? have different #import that means we expect not at comptime?
                self.program[f].body = FuncImpl::DynamicImport(self.program[f].name);
            } else {
                self.program[f].body = FuncImpl::Merged(vec![FuncImpl::ComptimeAddr(addr as usize), FuncImpl::DynamicImport(self.program[f].name)]);
                self.aarch64.dispatch[f.as_index()] = addr as *const u8;
            }
            special = true;
        }

        // TODO: use this for libc as well.
        if let Some(lib_name) = self.program[f].get_tag(Flag::Import) {
            let lib_name = if let Some(lib_name) = lib_name.args.as_ref() {
                self.eval_str(&mut lib_name.clone())?
            } else {
                self.program.pool.intern("")
            };

            let name = self.program[f].name;
            let (vtable, data) = &self.driver_vtable;
            if let BigOption::Some(callback) = vtable.resolve_comptime_import {
                if let BigOption::Some(addr) = unsafe { callback(*data, self, f, lib_name, name) } {
                    self.program[f].body = FuncImpl::Merged(vec![FuncImpl::ComptimeAddr(addr as usize), FuncImpl::DynamicImport(name)]);
                    self.aarch64.dispatch[f.as_index()] = addr;
                    special = true;
                }
            }
        }

        #[cfg(feature = "cranelift")]
        if let Some(addr) = self.program[f].get_tag(Flag::Cranelift_Emit) {
            let arg = unwrap2!(addr.args.clone(), "Cranelift_Emit needs arg");
            let addr: i64 = self.immediate_eval_expr_known(arg)?;
            assert!(matches!(self.program[f].body, FuncImpl::Empty));
            self.program[f].body = FuncImpl::EmitCranelift(addr as usize);
            special = true;
        }

        if special {
            assert!(self.program[f].finished_ty().is_some(), "special def needs known type");
            self.update_cc(f)?;
        }

        Ok(special)
    }

    fn emit_body(&mut self, f: FuncId) -> Res<'p, TypeId> {
        let state = DebugState::EmitBody(f, self.program[f].name);
        self.push_state(&state);

        if self.emit_special_body(f, false)? {
            self.pop_state(state);
            return Ok(self.program[f].finished_ret.unwrap());
        }

        assert!(!self.program[f].get_flag(Generic));
        let arguments = self.program[f].arg.flatten();
        for (name, ty, kind) in arguments {
            // TODO: probably want to change this so you can do as much compiling as possible before expanding templates.
            debug_assert!(kind != VarType::Const, "Tried to emit before binding const args.");
            if let Some(name) = name {
                debug_assert!(kind == name.kind);
                let prev = self.program.pool.get_var_type(name); // TODO: dumb double lookup
                self.program.pool.put_var_type(name, ty);
                assert!(prev.is_none(), "overwrite arg?");
            }
        }

        // TODO: no clone. make errors recoverable. no mut_replace -- Apr 24
        let FuncImpl::Normal(mut body_expr) = self.program[f].body.clone() else {
            err!("expected normal body",)
        };

        debug_assert!(body_expr.as_suffix_macro_mut(Flag::Asm).is_none(), "special should handle");

        if !self.program[f].get_flag(Generic) && !matches!(self.program[f].ret, LazyType::Infer) {
            self.infer_types(f)?;
        }

        let hint = self.program[f].finished_ret;
        if let BigOption::Some(return_var) = self.program[f].return_var {
            if let BigOption::Some(ret_ty) = hint {
                if let Expr::Block { ret_label, .. } = &mut body_expr.expr {
                    let ret = LabelId::from_index(self.next_label);
                    self.next_label += 1;
                    let label_ty = self.program.intern_type(TypeInfo::Label(ret_ty));

                    let val = self.as_values(ret)?;
                    self.save_const_values(return_var, val, label_ty, self.program[f].loc)?;
                    *ret_label = BigOption::Some(ret);
                }
            }
        }

        let ret_ty = self.compile_expr(&mut body_expr, hint.into())?;
        if self.program[f].finished_ret.is_none() {
            // If you got to the point of emitting the body, the only situation where you don't know the return type yet is if they didn't put a type anotation there.
            // This isn't true if you have an @generic function that isn't @comptime trying to use its args in its return type.
            // That case used to work becuase I'd always inline after doing const args so you've never even get to the point of compiling the function body on its own.
            // However, that still wasn't the same as @comptime because it didn't egarly evaluate unless already in a const context.
            // I do still want to get rid of @comptime and just use const args but I need to think about the semantics of when you inline more.
            //  -- Apr 6, 2024
            assert!(matches!(self.program[f].ret, LazyType::Infer));

            self.program[f].finished_ret = BigOption::Some(ret_ty);
            self.program[f].ret = LazyType::Finished(ret_ty);
        }
        
        // UNSURE. It offended me to see recursing ptr::drop_in_place in the profiler if im leaking all the memory anyway.
        // the right answer is not clone in the first place but maybe you want to allow recovering from errors
        // im not really sure.  -- Jun 12
        let mut temp = FuncImpl::Normal(body_expr);
        mem::swap(&mut self.program[f].body, &mut temp);
        mem::forget(temp); //

        let func = &self.program[f];
        self.type_check_arg(ret_ty, func.finished_ret.unwrap(), "bad return value")?;
        self.pop_state(state);
        
        

        Ok(ret_ty)
    }

    // This is a normal function call. No comptime args, no runtime captures.
    fn emit_runtime_call(&mut self, f: FuncId, arg_expr: &mut FatExpr<'p>) -> Res<'p, TypeId> {
        let arg_ty = unwrap2!(self.program[f].finished_arg, "fn arg");
        self.compile_expr(arg_expr, Some(arg_ty))?;

        // self.last_loc = Some(expr.loc); // TODO: have a stack so i dont have to keep doing this.
        let func = &self.program[f];
        // TODO: some huristic based on how many times called and how big the body is.
        // TODO: pre-intern all these constants so its not a hash lookup everytime
        let force_inline = func.cc == BigOption::Some(CallConv::Inline);
        assert!(!func.get_flag(MayHaveAquiredCaptures));
        assert!(!force_inline);
        assert!(!func.any_const_args());
        self.add_callee(f);
        self.compile(f, self.exec_style())?;
        let ret_ty = unwrap2!(self.program[f].finished_ret, "fn ret");
        Ok(ret_ty)
    }

    // TODO: i think the main reason <callees> is on the result not the func is because I used to use mut_replace for emit_body.  -- Apr 25
    fn add_callee(&mut self, f: FuncId) {
        // this fixes mutual recursion
        // TODO: now if other backends try to use callees they might not get everything they need.
        //       not failing tests rn because im egarly compiling on this backend and llvm doesn't support everything yet os not all tests run there anyway.  -- Apr 25

        // println!(
        //     "{wip:?}={} calls {f:?}={}",
        //     self.program.pool.get(self.program[wip].name),
        //     self.program.pool.get(self.program[f].name)
        // )
        let wip = self.wip_stack.last().unwrap().0;
        if let Some(wip) = wip {
            let callees = if !self.currently_compiling.contains(&f) {
                &mut self.program[wip].callees
            } else {
                &mut self.program[wip].mutual_callees
            };
            add_unique(callees, f);
        }
        //else {
        //     add_unique(&mut self.aarch64.pending_indirect, f); // HACK
        //                                                        // println!("not adding callee currently_compiling {f:?} {}", self.program.pool.get(self.program[f].name));
        // }
    }

    // Replace a call expr with the body of the target function.
    fn emit_capturing_call(&mut self, f: FuncId, expr_out: &mut FatExpr<'p>) -> Res<'p, TypeId> {
        self.ensure_resolved_body(f)?; // it might not be a closure. it might be an inlined thing.
        let loc = expr_out.loc;
        assert!(self.program[f].get_flag(NotEvilUninit));
        let state = DebugState::EmitCapturingCall(f, self.program[f].name);
        self.push_state(&state);
        let Expr::Call(_, arg_expr) = expr_out.deref_mut() else { ice!("") };

        debug_assert!(!self.program[f].has_tag(Flag::NoInline));

        assert!(!self.currently_inlining.contains(&f), "Tried to inline recursive function.");
        self.currently_inlining.push(f);

        let func = &self.program.funcs[f.as_index()];
        // TODO: if let this? its for return_var
        let BigOption::Some(ret_ty) = func.finished_ret else {
            err!("Unknown ret type for {f:?} {}", self.program.pool.get(self.program[f].name))
        };
        let hint = func.finished_ret;
        let label_ty = self.program.intern_type(TypeInfo::Label(ret_ty));
        // TODO: mutual callees as well.
        // for inlined or things with const args, the body will already have been compiled, but the backend needs to see the whole callgraph.
        for callee in self.program[f].callees.clone() {
            self.add_callee(callee);
        }
        let func = &self.program.funcs[f.as_index()];

        assert!(!func.any_const_args());
        let pattern = func.arg.clone();

        // TODO: can I mem::take func.body? I guess not because you're allowed to call multiple times, but that's sad for the common case of !if/!while.
        // TODO: dont bother if its just unit args (which most are because of !if and !while).
        // TODO: you want to be able to share work (across all the call-sites) compiling parts of the body that don't depend on the captured variables
        // TODO: need to move the const args to the top before eval_and_close_local_constants
        let old_ret_var = func.return_var.unwrap();
        let mut new_ret_var = old_ret_var;
        new_ret_var.id = self.program.next_var;
        self.program.next_var += 1;

        let ret_label = LabelId::from_index(self.next_label);
        self.next_label += 1;
        let FuncImpl::Normal(body) = &func.body else { unreachable!() };
        let may_have_early_return = !matches!(body.expr, Expr::Value { .. });
        // the second case would be sufficient for correctness but this is so common (if(_,!,!), loop(!), etc) that it makes me less sad. 
        if arg_expr.is_raw_unit() {
            // TODO: if !may_have_early_return, should be able to just inline the value but it doesnt work!
            //       similarly i cant have ret_label:None so im clearly wrong. its `not callable` in overloading.rs. trying to call the cond of an if??
            //       -- Jun 16
            expr_out.expr = Expr::Block {
                body: vec![],
                result: Box::new(body.clone()),
                ret_label: BigOption::Some(ret_label),
                hoisted_constants: false, // TODO
            };
        } else {
            expr_out.expr = Expr::Block {
                body: vec![FatStmt {
                    stmt: Stmt::DeclVarPattern {
                        binding: pattern,
                        value: mem::take(arg_expr),
                    },
                    annotations: vec![],
                    loc,
                }],
                result: Box::new(body.clone()),
                ret_label: BigOption::Some(ret_label),
                hoisted_constants: false, // TODO
            };
        }
        self.currently_inlining.retain(|check| *check != f);
        if may_have_early_return {
            let mut mapping = Map::<Var, Var>::default();
            mapping.insert(old_ret_var, new_ret_var);
            // println!("renumber ret: {} to {}", old_ret_var.log(self.program.pool), new_ret_var.log(self.program.pool));
            expr_out.renumber_vars(&mut mapping, self); // Note: not renumbering on the function. didn't need to clone it.
            let value = to_values(self.program, ret_label)?;
            self.save_const(new_ret_var, Expr::Value { value, coerced: false }, label_ty, loc)?;
        }
        
        let res = self.compile_expr(expr_out, hint.into())?;
        if hint.is_none() {
            self.program[f].finished_ret = BigOption::Some(res);
        }
        self.pop_state(state);
        Ok(res)
    }

    // TODO: I should probably implement real runtime closures (even if my version is dumb and slow)
    //       and use those for comptime stuff cause it can't possibly be worse than cloning everything and recompiling.
    //       The cloning is only better for runtime functions where we're trying to output a simpler ast that an optimiser can specialize.
    // Curry a function from fn(a: A, @comptime b: B) to fn(a: A)
    // The argument type is evaluated in the function declaration's scope, the argument value is evaluated in the caller's scope.
    fn bind_const_arg(&mut self, o_f: FuncId, arg_name: Var<'p>, arg_value: Values, arg_ty_found: TypeId, loc: Span) -> Res<'p, ()> {
        // I don't want to renumber, so make sure to do the clone before resolving.
        // TODO: reslove captured constants anyway so dont haveto do the chain lookup redundantly on each speciailization. -- Apr 24
        debug_assert!(self.program[o_f].get_flag(ResolvedBody) && self.program[o_f].get_flag(ResolvedSign));
        // println!("bind {}", arg_name.log(self.program.pool));
        let mut arg_x = self.program[o_f].arg.clone();
        let arg_ty = self.get_type_for_arg(&mut arg_x, arg_name)?;
        self.program[o_f].arg = arg_x;
        self.type_check_arg(arg_ty_found, arg_ty, "bind arg")?;

        // TODO: not sure if i actually need this but it seems like i should.
        if matches!(self.program[arg_ty], TypeInfo::Fn(_)) {
            assert!(
                !self.program[o_f].has_tag(Flag::NoInline),
                "functions with constant lambda arguments are always inlined"
            );
            let arg_func = arg_value.unwrap_func_id();
            // :ChainedCaptures
            // TODO: HACK: captures aren't tracked properly.
            self.program[o_f].set_flag(FnFlag::MayHaveAquiredCaptures, true);
            self.program[o_f].set_cc(CallConv::Inline)?; // just this is enough to fix chained_captures
            self.program[arg_func].set_cc(CallConv::Inline)?; // but this is needed too for others (perhaps just when there's a longer chain than that simple example).
        }
        self.save_const_values(arg_name, arg_value, arg_ty, loc)?;
        self.program[o_f].arg.remove_named(arg_name);

        let known_type = self.program[o_f].finished_arg.is_some();
        self.program[o_f].finished_arg = BigOption::None;
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
        ice!("missing argument {}", arg_name.log(self.program.pool))
    }

    fn compile_stmt(&mut self, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match &mut stmt.stmt {
            Stmt::ExpandParsedStmts(_) => unreachable!(),
            Stmt::Eval(expr) => {
                // Note: can't do this check in parser because you want to allow @match/@switch that moves around the stmts.
                if matches!(expr.expr, Expr::Closure(_) | Expr::WipFunc(_)) {
                    err!("you probably don't want to discard an anonymous function.",);
                }
                self.compile_expr(expr, None)?;

                // Don't match on res being const because the function might still have side effects
                if expr.is_raw_unit() {
                    stmt.stmt = Stmt::Noop; // TODO: might need to remove this for janky @namespace but should find better fix. having less stuff makes it nicer to debug -- May 1
                }
            }
            Stmt::Set { .. } => self.set_deref(stmt)?,
            Stmt::DeclNamed { .. } => {
                ice!("Scope resolution failed {}", stmt.log(self.program.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(_) => unreachable!("DeclFunc gets hoisted"),
            // TODO: make value not optonal and have you explicitly call uninitilized() if you want that for some reason.
            Stmt::DeclVar { name, ty, value, .. } => {
                debug_assert_ne!(name.kind, VarType::Const);
                self.decl_var(*name, ty, value, name.kind, &stmt.annotations)?;
            }
            // TODO: don't make the backend deal with the pattern matching but it does it for args anyway rn.
            //       be able to expand this into multiple statements so backend never even sees a DeclVarPattern (and skip constants when doing so)
            // TODO: this is extremly similar to what bind_const_arg has to do. should be able to express args as this thing?
            // TODO: remove useless statements
            Stmt::DeclVarPattern { binding, value } => {
                if value.is_raw_unit() {
                    stmt.stmt = Stmt::Noop;
                    return Ok(());
                }
                let arguments = binding.flatten();
                if arguments.len() == 1 {
                    let (name, ty, kind) = arguments.into_iter().next().unwrap();
                    if let Some(name) = name {
                        self.decl_var(name, &mut LazyType::Finished(ty), value, kind, &stmt.annotations)?;
                        if kind == VarType::Const {
                            debug_assert_eq!(binding.bindings.len(), 1);
                            binding.bindings.clear();
                            value.set(Values::unit(), TypeId::unit);
                        }
                        return Ok(());
                    } else {
                        if value.is_raw_unit() {
                            stmt.stmt = Stmt::Noop; // not required. just want less stuff around when debugging
                        } else {
                            self.compile_expr(value, Some(ty))?;
                        }

                        return Ok(());
                    }
                }

                let Expr::Tuple(exprs) = &mut value.expr else {
                    err!("TODO: more interesting pattern matching\n {}", value.log(self.program.pool))
                };
                assert_eq!(arguments.len(), exprs.len(), "TODO: non-trivial pattern");

                for ((name, ty, kind), expr) in arguments.iter().zip(exprs.iter_mut()) {
                    if let Some(name) = name {
                        self.decl_var(*name, &mut LazyType::Finished(*ty), expr, *kind, &stmt.annotations)?;
                    } else {
                        assert!(expr.is_raw_unit(), "var no name not unit");
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
                    self.set_literal(value, ())?; // redundant now
                    stmt.stmt = Stmt::Noop;

                    return Ok(());
                } else if exprs.len() == 1 {
                    *value = mem::take(exprs.iter_mut().next().unwrap());
                }
                // If we changed stuff, make sure not to give the backend false type info.
                if removed > 0 && !value.ty.is_unknown() {
                    value.ty = TypeId::unknown;
                    self.compile_expr(value, None)?;
                }
            }
        }
        Ok(())
    }

    fn decl_func(&mut self, mut func: Func<'p>) -> Res<'p, (Option<FuncId>, Option<OverloadSetId>)> {
        // println!("{}", self.program.pool.get(func.name));
        // TODO: cross compiling.
        #[cfg(not(target_arch = "aarch64"))]
        if func.has_tag(Flag::Aarch64) {
            return Ok((None, None));
        }

        debug_assert!(func.get_flag(NotEvilUninit));
        let loc = func.loc;
        // TODO: allow language to declare @macro(.func) and do this there instead. -- Apr 27
        if let Some(when) = func.get_tag_mut(Flag::When) {
            let cond = unwrap!(when.args.as_mut(), "#when(cond: bool) requires argument");
            let cond: bool = self.immediate_eval_expr_known(cond.clone())?;
            if !cond {
                return Ok((None, None));
            }
        }

        let var = func.var_name;
        if func.has_tag(Flag::Macro) {
            assert!(!func.has_tag(Flag::Rt));
            // func.add_tag(Flag::Ct);
        }

        let id = self.add_func(func)?;

        let mut out = (None, None);
        // I thought i dont have to add to constants here because we'll find it on the first call when resolving overloads.
        // But it does need to have an empty entry in the overload pool because that allows it to be closed over so later stuff can find it and share if they compile it.
        if let BigOption::Some(var) = var {
            // TODO: allow function name to be any expression that resolves to an OverloadSet so you can overload something in a module with dot syntax.
            // TODO: distinguish between overload sets that you add to and those that you re-export
            debug_assert!(!self.program[id].get_flag(ResolvedSign));
            debug_assert!(!self.program[id].get_flag(ResolvedBody));
            if let Some(overloads) = self.find_const(var)? {
                assert_eq!(overloads.1, TypeId::overload_set);
                let i: OverloadSetId = from_values(self.program, overloads.0)?;
                let os = &mut self.program[i];

                os.pending.push(id);
                out = (Some(id), Some(i));
            } else {
                let index = OverloadSetId::from_index(self.program.overload_sets.len());
                self.program.overload_sets.push(OverloadSet {
                    ready: vec![],
                    name: var.name,
                    pending: vec![id],
                    just_resolved: vec![],
                });
                self.save_const_values(var, (index.as_raw()).into(), TypeId::overload_set, loc)?;
                out = (Some(id), Some(index));
            }

            let loc = self.program[id].loc;
            if self.program[id].has_tag(Flag::Redirect) {
                // dman it bnoarow chekcser
                let os = self.as_literal(out.1.unwrap(), loc)?;
                if let Some(types) = self.program[id].get_tag_mut(Flag::Redirect) {
                    let Expr::Tuple(parts) = &mut types.args.as_mut().unwrap().expr else {
                        err!("bad",)
                    };
                    parts.push(os);
                }
            }
        }

        let is_cl = self.program.comptime_arch == TargetArch::Cranelift && cfg!(feature = "cranelift");
        let func = &mut self.program[id];
        let should_skip = is_cl && func.has_tag(Flag::Skip_Cranelift);
        if !should_skip {
            if func.has_tag(Flag::Test) {
                // TODO: probably want referencable_name=false? but then you couldn't call them from cli so meh.
                self.tests.push(id);
            }
            if func.has_tag(Flag::Test_Broken) {
                // TODO: probably want referencable_name=false? but then you couldn't call them from cli so meh.
                self.tests_broken.push(id);
            }
        }

        if func.has_tag(Flag::Fold) {
            func.set_flag(TryConstantFold, true);
        }
        if func.has_tag(Flag::Export) {
            assert!(!func.any_const_args(), "functions with const arguments cannot be exported.");
            assert!(
                !func.get_flag(FnFlag::AllowRtCapture),
                "functions with runtime captures cannot be exported (declare with '=' instead of '=>')."
            );
            self.export.push(id);
        }

        Ok(out)
    }

    pub fn find_const(&mut self, name: Var<'p>) -> Res<'p, Option<(Values, TypeId)>> {
        if let BigOption::Some(s) = self.program.pool.get_constant(name) {
            if let Some(known) = s.1.ty() {
                if let Expr::Value { value, .. } = &s.0.expr {
                    let ty = s.0.ty;
                    debug_assert!(!ty.is_unknown());
                    debug_assert_eq!(ty, known);
                    return Ok(Some((value.clone(), ty)));
                }
                ice!("constant with known type but unknown value.")
            }
            if matches!(s.0.expr, Expr::Poison) {
                // creating an overload set gets here I think -- Apr 27
                return Ok(None);
            }

            let state = DebugState::ResolveConstant(name);
            self.push_state(&state);
            let (mut val, mut ty) = mem::take(self.program.pool.get_constant(name).unwrap());
            // println!("- {} {} {}", name.log(self.program.pool), ty.log(self.program.pool), val.log(self.program.pool));
            self.program.pool.get_constant(name).unwrap().1 = LazyType::Infer;
            self.infer_types_progress(&mut ty)?;
            self.decl_const(name, &mut ty, &mut val)?;
            self.pop_state(state);
            return self.find_const(name);
        }
        Ok(None)
    }

    pub fn find_const_type(&mut self, var: Var<'p>) -> Res<'p, Option<TypeId>> {
        Ok(self.find_const(var)?.map(|(_, ty)| ty))
    }

    #[track_caller]
    pub fn ensure_resolved_sign(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.program[f].get_flag(ResolvedSign) {
            return Ok(());
        }
        debug_assert!(
            self.program[f].get_flag(NotEvilUninit),
            "ensure_resolved_sign of evil_uninit {}",
            self.log_trace()
        );
        mut_replace!(self.program[f], |mut func: Func<'p>| {
            ResolveScope::resolve_sign(&mut func, self)?; // TODO
            Ok((func, ()))
        });
        Ok(())
    }

    #[track_caller]
    pub fn ensure_resolved_body(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.program[f].get_flag(ResolvedBody) {
            return Ok(());
        }
        debug_assert!(
            self.program[f].get_flag(NotEvilUninit),
            "ensure_resolved_body of evil_uninit {}",
            self.log_trace()
        );
        self.ensure_resolved_sign(f)?;
        mut_replace!(self.program[f], |mut func: Func<'p>| {
            ResolveScope::resolve_body(&mut func, self)?; // TODO
            Ok((func, ()))
        });
        Ok(())
    }

    #[track_caller]
    pub fn func_expr(&mut self, id: FuncId) -> (Expr<'p>, TypeId) {
        if self.program[id].finished_ret.is_some() {
            (Expr::Value { value: (id.as_raw()).into(), coerced: false }, self.program.func_type(id))
        } else {
            (Expr::WipFunc(id), FuncId::get_or_create_type(self.program))
        }
    }

    pub fn compile_expr(&mut self, expr: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, TypeId> {
        // println!("{}", expr.log(self.program.pool));
        let marker = 0;
        unsafe {
            STACK_MIN = STACK_MIN.min(&marker as *const i32 as usize);
            STATS.compile_expr_calls_all += 1;
            if expr.done {
                STATS.compile_expr_calls_with_done_set += 1;
                debug_assert!(!expr.ty.is_unknown());
                return Ok(expr.ty);
            }
        };
        assert!(expr.ty.as_index() < self.program.types.len());

        let old = expr.ty;

        let res = self.compile_expr_inner(expr, requested)?;
        debug_assert!(expr.ty.is_valid());
        debug_assert!(res.is_valid());
        if !expr.ty.is_unknown() {
            self.last_loc = Some(expr.loc);
            // let msg = format!("sanity ICE {} {res:?}", expr.log(self.program.pool)).leak();

            self.type_check_arg(expr.ty, res, "sanity ICE post_expr")?;
        }
        // TODO: its kinda sad to do this every time. better to keep track of pending ones and somehow have safe points you know you're probably not in the middle of mutual recursion.
        self.program.finish_layout_deep(res)?;
        if let Some(requested) = requested {
            debug_assert!(requested.is_valid());
            self.program.finish_layout_deep(requested)?;
            // TODO: its possible to write code that gets here without being caught first.
            //       should fix so everywhere that relies on 'requested' does thier own typecheck because they can give a more contextual error message.
            //       but its annoying becuase this happens first so you cant just sanity check here and i dont want to trust that everyone remembered.
            //       so maybe its better to have more consistant use of the context stack so you always know what you're doing and forwarding typecheck responsibility doesnt mean poor error messages.
            //       but then you have to make sure not to mess up the stack when you hit recoverable errors. and that context has to not be formatted strings since thats slow.
            //       -- Apr 19
            // let msg = format!(
            //     "sanity ICE {} {} vs {}",
            //     expr.log(self.program.pool),
            //     self.program.log_type(res),
            //     self.program.log_type(requested)
            // )
            // .leak();
            // where_the_fuck_am_i(self, expr.loc);
            let msg = "sanity ICE req_expr";
            self.type_check_arg(res, requested, msg)?;
        }

        expr.ty = res;
        if !old.is_unknown() {
            // TODO: make work with new coercion
            // TODO: cant just assert_eq because it does change for rawptr.
            // self.type_check_arg(expr.ty, old, "sanity ICE old_expr")?;
        }

        Ok(res)
    }

    fn break_fn_type(&mut self, requested: Option<TypeId>) -> (Option<TypeId>, Option<TypeId>) {
        let Some(ty) = requested else { return (None, None) };
        let TypeInfo::Fn(ty) = self.program[ty] else { return (None, None) };
        let ret = if ty.ret.is_unknown() { None } else { Some(ty.ret) };
        let arg = if ty.arg.is_unknown() { None } else { Some(ty.arg) };
        (arg, ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr_inner(&mut self, expr: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, TypeId> {
        self.last_loc = Some(expr.loc);
        let loc = expr.loc;
        let prev_ty = expr.ty;
        Ok(match expr.deref_mut() {
            Expr::Cast(inner) => {
                // TODO: you probably never get here cause .done should be set?
                let requested = if inner.ty.is_unknown() { None } else { Some(inner.ty) };
                self.compile_expr(inner, requested)?;
                expr.ty
            }
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::Closure(_) => {
                let (arg, ret) = self.break_fn_type(requested);
                let id = self.promote_closure(expr, arg, ret)?;
                let ty = self.program.fn_type(id).unwrap_or_else(|| FuncId::get_or_create_type(self.program));
                expr.set((id.as_raw()).into(), ty);
                ty
            }
            &mut Expr::WipFunc(id) => {
                self.infer_types(id)?;
                // When Expr::Call compiles its 'f', the closure might not know its types yet, so just say its some const known Fn and figure it out later.
                let ty = self.program.fn_type(id).unwrap_or_else(|| FuncId::get_or_create_type(self.program));
                expr.set((id.as_raw()).into(), ty);
                ty
            }
            Expr::Call(f, arg) => {
                // Compile 'f' as normal, its fine if its a macro that expands to a callable or a closure we don't know the types for yet.
                self.compile_expr(f, None)?;

                if let TypeInfo::FnPtr { ty, .. } = self.program[f.ty] {
                    self.compile_expr(arg, Some(ty.arg))?;
                    expr.done = f.done && arg.done;
                    expr.ty = ty.ret;
                    return Ok(ty.ret);
                }

                if let TypeInfo::Label(arg_ty) = self.program[f.ty] {
                    self.compile_expr(arg, Some(arg_ty))?;
                    expr.done = f.done && arg.done;
                    expr.ty = TypeId::never;
                    return Ok(TypeId::never);
                }

                // If its not a FnPtr, it should be a Fn/FuncId/OverloadSetId
                if let Some(f_id) = self.maybe_direct_fn(f, arg, requested)? {
                    return self.compile_call(expr, f_id);
                }

                self.last_loc = Some(f.loc);
                ice!("tried to call non-function {:?}", f.log(self.program.pool))
            }
            Expr::Block {
                body,
                result: value,
                hoisted_constants,
                ret_label,
                ..
            } => {
                if !*hoisted_constants {
                    self.hoist_constants(body)?;
                    *hoisted_constants = true;
                }

                body.retain(|s| !(matches!(s.stmt, Stmt::Noop) && s.annotations.is_empty())); // Not required but makes debugging easier cause there's less stuff.
                for stmt in body.iter_mut() {
                    self.compile_stmt(stmt)?;
                }
                // body.retain(|s| !(matches!(s.stmt, Stmt::Noop) && s.annotations.is_empty())); // Not required but makes debugging easier cause there's less stuff.

                // TODO: insert drops for locals
                let res = self.compile_expr(value, requested)?;
                if body.is_empty() && ret_label.is_none() {
                    *expr = mem::take(value); // Not required but makes debugging easier cause there's less stuff.
                }
                expr.ty = res;
                res
            }
            Expr::Tuple(values) => {
                assert!(values.len() > 1, "ICE: no trivial tuples");
                let types = requested
                    .and_then(|t| self.program.tuple_types(t))
                    .and_then(|t| if t.len() == values.len() { Some(t) } else { None });
                let mut out = vec![];
                for (i, v) in values.iter_mut().enumerate() {
                    let ty = types.as_ref().and_then(|t| t.get(i).copied());
                    out.push(self.compile_expr(v, ty)?)
                }
                let ty = self.program.tuple_of(out);
                expr.ty = ty;
                ty
            }
            Expr::GetVar(var) => {
                // TODO: dont do the extra lookup if we know its const
                if let BigOption::Some(ty) = self.program.pool.get_var_type(*var) {
                    // Reading a variable. Convert it to `var&[]` so compiling it checks for smaller loads (u8, etc).
                    expr.ty = ty;
                    expr.done = true; // don't recurse on the var expr again.
                    let loc = expr.loc;
                    let ptr_ty = self.program.ptr_type(ty);
                    *expr = FatExpr::synthetic_ty(Expr::SuffixMacro(Flag::Addr.ident(), Box::new(mem::take(expr))), loc, ptr_ty);
                    expr.ty = ptr_ty;
                    expr.done = true;
                    // Note: not using deref_one, because don't want to just remove the ref, we want raw variable expressions to not exist. kinda HACK
                    *expr = FatExpr::synthetic_ty(Expr::SuffixMacro(Flag::Deref.ident(), Box::new(mem::take(expr))), loc, ty);
                    self.compile_expr(expr, requested)?
                } else if let Some((value, ty)) = self.find_const(*var)? {
                    expr.set(value.clone(), ty);
                    expr.done = true;
                    if let Some(req) = requested {
                        if ty != req {
                            let Expr::Value { value, coerced } = &mut expr.expr else{ unreachable!() };
                            assert!(!*coerced, "const mismatch. {} vs {} but already coerced", self.program.log_type(expr.ty), self.program.log_type(req));
                            
                            *coerced = true;
                            if let Some(replacement) = self.coerce_constant(value, ty, req, expr.loc)? {
                                *expr = replacement;
                                return self.compile_expr(expr, requested);
                            }
                            expr.ty = req;
                        }
                    }
                    expr.ty
                } else {
                    let var = *var;
                    where_the_fuck_am_i(self, expr.loc);
                    ice!("Missing resolved variable {}", var.log(self.program.pool),)
                }
            }
            Expr::GetNamed(name) => err!("Undeclared Ident {} {name:?}", self.program.pool.get(*name)), //err!(CErr::UndeclaredIdent(*name)),
            Expr::Value { value, coerced } => {
                debug_assert!(!prev_ty.is_unknown(), "Value expr must have type");
                if let Some(req) = requested {
                    if prev_ty != req {
                        assert!(!*coerced, "const mismatch. {} vs {} but already coerced", self.program.log_type(expr.ty), self.program.log_type(req));
                        
                        *coerced = true;
                        if let Some(replacement) = self.coerce_constant(value, prev_ty, req, loc)? {
                            *expr = replacement;
                            // TODO: instead of making you call compile_expr, have coerce_constant do it (because it can just do the right branch, which is always !fn_ptr currently)
                            return self.compile_expr(expr, requested);
                        }
                        expr.ty = req;
                    }
                }
                
                expr.done = true;
                expr.ty
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = Flag::try_from(*macro_name)?;
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    Flag::If => self.emit_call_if(expr, requested)?,
                    Flag::Loop => self.emit_call_loop(arg)?,
                    Flag::Addr => self.addr_macro(expr, requested)?,
                    // :UnquotePlaceholders
                    Flag::Quote => {
                        let mut arg: FatExpr<'p> = *arg.clone(); // Take the contents of the box, not the box itself!
                        let loc = arg.loc;

                        let mut walk = Unquote {
                            compiler: self,
                            placeholders: vec![],
                        };
                        walk.expr(&mut arg);
                        let mut placeholders = walk.placeholders; // drop walk.

                        let ty = unwrap!(self.program.fat_expr_type, "used quoted ast during bootstrapping");
                        let value = to_values(self.program, arg)?;

                        expr.set(value.clone(), ty);
                        if placeholders.is_empty() {
                            ty
                        } else {
                            placeholders.push(Some(mem::take(expr)));
                            let arg = Box::new(FatExpr::synthetic(
                                Expr::Tuple(placeholders.into_iter().map(|p| p.unwrap()).collect()),
                                loc,
                            ));
                            let arg = FatExpr::synthetic(Expr::SuffixMacro(Flag::Slice.ident(), arg), loc);
                            let f = self.program.find_unique_func(Flag::Unquote_Macro_Apply_Placeholders.ident()).unwrap(); // TODO
                            let _ = self.infer_types(f)?.unwrap();
                            let f = FatExpr::synthetic_ty(Expr::Value { value: (f.as_raw()).into(), coerced: false }, loc, self.program.func_type(f));
                            *expr = FatExpr::synthetic_ty(Expr::Call(Box::new(f), Box::new(arg)), loc, ty);
                            self.compile_expr(expr, requested)?
                        }
                    }
                    Flag::Slice => {
                        let container = self.compile_expr(arg, None)?;
                        let ty = self.program.tuple_types(container);
                        let expect = if let Some(types) = ty {
                            let expect = *unwrap!(types.iter().find(|t| !t.is_unknown()), "all unknown");
                            for t in types {
                                self.type_check_arg(t, expect, "match slice types")?;
                            }
                            expect
                        } else {
                            container
                        };
                        let s_ty = self.create_slice_type(expect, arg.loc)?;
                        expr.done = arg.done;
                        expr.ty = s_ty;
                        s_ty
                    }
                    Flag::Deref => {
                        let requested = requested.map(|t| self.program.ptr_type(t));
                        let ptr = self.compile_expr(arg, requested)?;
                        let ty = unwrap!(self.program.unptr_ty(ptr), "deref not ptr: {}", self.program.log_type(ptr));
                        expr.done = arg.done;
                        expr.ty = ty;
                        ty
                    }
                    Flag::Const_Eval => {
                        self.wip_stack.push((None, ExecStyle::Jit));  // :PushConstFnCtx
                        let res = self.compile_expr(arg, requested)?;
                        let ty = res;
                        let arg = *mem::take(arg);
                        let value = if let Expr::Value { value, .. } = arg.expr {
                            value
                        } else {
                            // TODO: its a bit silly that i have to specifiy the type since the first thing it does is compile it
                            self.immediate_eval_expr(arg, ty)?
                        };
                        expr.set(value.clone(), ty);
                        assert!(self.wip_stack.pop().unwrap().0.is_none());
                        ty
                    }
                    Flag::Tag => {
                        // TODO: auto deref and typecheking
                        let container_ptr = self.compile_expr(arg, None)?;
                        let container_ptr_ty = self.program.raw_type(container_ptr);
                        let depth = self.program.ptr_depth(container_ptr_ty);
                        assert_eq!(
                            depth,
                            1,
                            "!tag ptr must be one level of indirection. {:?}",
                            self.program.log_type(container_ptr_ty)
                        );
                        let ty = self.program.intern_type(TypeInfo::Ptr(TypeId::i64()));
                        expr.done = arg.done;
                        expr.ty = ty;
                        ty
                    }
                    Flag::Fn_Ptr => {
                        // TODO: this should be immediate_eval_expr (which should be updated do the constant check at the beginning anyway).
                        //       currently !fn_ptr can't be an atrbitrary comptime expr which is silly.
                        let ty = self.compile_expr(arg, None)?;
                        if let TypeInfo::Fn(_) = self.program[ty] {
                            let id: FuncId = self.immediate_eval_expr_known(*arg.clone())?;
                            assert!(!self.program[id].any_const_args());
                            // TODO: for now you just need to not make a mistake with calling convention
                            self.add_callee(id);
                            self.ensure_compiled(id, self.exec_style())?;
                            // The backend still needs to do something with this, so just leave it
                            let fn_ty = self.program.func_type(id);
                            let ty = self.program.fn_ty(fn_ty).unwrap();
                            let cc = unwrap2!(self.program[id].cc, "unknown calling convention");
                            let ty = self.program.intern_type(TypeInfo::FnPtr { ty, cc }); // TODO: callconv as part of type
                            arg.set((id.as_raw()).into(), fn_ty);
                            expr.ty = ty;
                            ty
                        } else {
                            err!("!fn_ptr expected const fn not {}", self.program.log_type(ty));
                        }
                    }
                    Flag::From_Bit_Literal => {
                        let int = unwrap!(self.bit_literal(expr), "not int");
                        let ty = self.program.intern_type(TypeInfo::Int(int.0));
                        let mut values = int.1.into();
                        // this fixed a size check in from_values
                        match &mut values {
                            Values::Big(_) => todo!(),
                            Values::Small(_, len) => {
                                match int.0.bit_count {
                                    8 => *len = 1,
                                    16 =>* len = 2,
                                    32 => *len = 4,
                                    _ => {}
                                }
                            }
                        }
                        expr.set(values, ty);
                        ty
                    }
                    Flag::Uninitialized => {
                        assert!(arg.is_raw_unit());
                        assert!(requested.is_some(), "!uninitialized expr must have known type");
                        expr.ty = requested.unwrap();
                        return Ok(requested.unwrap());
                    }
                    Flag::Unquote | Flag::Placeholder => ice!("Unhandled macro {}", self.program.pool.get(*macro_name)),
                    Flag::Builtin => {
                        let Some(name) = arg.as_ident() else {
                            err!("@builtin requires argument",);
                        };
                        let (value, ty) = unwrap!(self.builtin_constant(name), "unknown @builtin: {:?}", self.program.pool.get(name));
                        expr.set(value, ty);
                        return Ok(ty);
                    }
                    Flag::Contextual_Field => {
                        let Some(ty) = requested else {
                            err!(CErr::NeedsTypeHint("!contextual_field (leading dot) requires type hint"))
                        };
                        let Some(name) = arg.as_ident() else {
                            err!("!contextual_field (leading dot) requires name",)
                        };

                        return self.contextual_field(name, expr, ty);
                    }
                    Flag::As => {
                        // TODO: constants?
                        let Expr::Tuple(parts) = &mut arg.expr else {
                            err!("bad !as: {}", arg.log(self.program.pool))
                        };
                        assert!(parts.len() == 2, "bad !as: {}", arg.log(self.program.pool));
                        *expr = self.as_cast_macro(mem::take(&mut parts[0]), mem::take(&mut parts[1]))?;
                        return Ok(expr.ty);
                    }
                    _ => {
                        err!(CErr::UndeclaredIdent(*macro_name))
                    }
                }
            }
            // :PlaceExpr
            Expr::FieldAccess(e, name) => {
                // TODO: this is unfortunate. it means you prewalk instead of letting placeexpr do the recursion
                //       but need to check if its a value that has special fields first.
                let container = self.compile_expr(e, None)?;

                if let Some(val) = e.as_const() {
                    if container == TypeId::ty {
                        let ty: TypeId = from_values(self.program, val.clone())?;
                        return self.contextual_field(*name, expr, ty);
                    }
                    if container == TypeId::scope {
                        let s: ScopeId = from_values(self.program, val.clone())?;
                        let BigOption::Some(var) = self.program.pool.find_in_scope(s, *name) else {
                            err!(CErr::UndeclaredIdent(*name))
                        };
                        assert!(var.kind == VarType::Const);
                        let Some((val, ty)) = self.find_const(var)? else {
                            err!("missing constant {}", var.log(self.program.pool))
                        };
                        expr.set(val.clone(), ty);
                        return Ok(ty);
                    }
                }

                assert!(container != TypeId::scope, "dot syntax on module must be const",);
                // Otherwise its a normal struct/tagged field.
                self.compile_place_expr(expr, requested, true)?;
                self.compile_expr(expr, requested)?; // TODO: this is dumb. its just to get the load/store applied/
                expr.ty
            }
            // TODO: replace these with a more explicit node type?
            Expr::StructLiteralP(pattern) => {
                let requested = requested.or(Some(prev_ty)); // TODO: idk why switching to bytes made me need this. thats disturbing. -- May 26
                let res = self.construct_struct(requested, pattern)?;
                expr.ty = res;
                res
            }
            Expr::PtrOffset { .. } => unreachable!("PtrOffset should be done=true"),
            // err!("Raw struct literal. Maybe you meant to call 'init'?",),
            &mut Expr::String(i) => {
                expr.done = true;
                let bytes = self.program.pool.get(i);
                self.set_literal(expr, (bytes.as_ptr() as *mut i64, bytes.len() as i64))?;
                let byte = u8::get_or_create_type(self.program);
                expr.ty = self.create_slice_type(byte, expr.loc)?;
                expr.ty
            }
            Expr::PrefixMacro { handler, arg, target } => {
                // This allows @a E; instead of @a(E);
                if arg.is_raw_unit() && !target.is_raw_unit() {
                    mem::swap(arg, target);
                }
                let Some(expr_ty) = self.program.fat_expr_type else {
                    *expr = self.builtin_prefix_macro(handler, arg, target)?;
                    return self.compile_expr(expr, requested);
                };
                let arg = mem::take(arg.deref_mut());
                let target = mem::take(target.deref_mut());
                let want = expr_ty;
                let arg_ty = self.program.tuple_of(vec![expr_ty, expr_ty]);
                // TODO: this is dump copy-paste cause i cant easily resovle on type instead of expr
                // TODO: ask for a callable but its hard because i dont know if i want one or two argument version yet. actually i guess i do, just look an target sooner. but im not sure eval will resolve the overload for me yet -- Apr 21
                let os_id: OverloadSetId = self.immediate_eval_expr_known(*handler.clone())?;
                self.compute_new_overloads(os_id, None)?;

                let name = self.program[os_id].name;
                let os = self.program[os_id].ready.iter().filter(|o| (o.ret.is_none()) || o.ret.unwrap() == want);
                let mut os2 = os.clone().filter(|o| o.arg == arg_ty);

                debugln_call!("invoke macro: @{}({}) {}", self.program.pool.get(name), arg.log(self.program.pool), target.log(self.program.pool));
                // If they did '@m(e)' instead of '@m(e) s', prefer a handler that only expects one argument.
                // TODO: should probably distinguish '@m(e) unit' just incase
                let (f, args) = if target.is_raw_unit() {
                    let mut os1 = os.clone().filter(|o| o.arg == want);
                    self.last_loc = Some(arg.loc);
                    let f = unwrap!(
                        os1.next(),
                        "Missing macro overload {} (Expr) -> Expr. maybe you forgot a target expr on the invocation of {}",
                        self.program.pool.get(name),
                        handler.log(self.program.pool)
                    );
                    assert!(os1.next().is_none(), "ambigous macro overload");
                    let f = f.func;
                    (f, vec![Box::into_raw(Box::new(arg)) as i64])
                } else {
                    let f = unwrap!(os2.next(), "missing macro overload {}", self.program.pool.get(name),).func;
                    assert!(os2.next().is_none(), "ambigous macro overload");
                    (f, vec![Box::into_raw(Box::new(arg)) as i64, Box::into_raw(Box::new(target)) as i64])
                };

                assert!(self.program[f].has_tag(Flag::Macro));
                self.infer_types(f)?;
                self.compile(f, ExecStyle::Jit)?;
                self.typecheck_macro_outputs(f, requested)?;

                self.update_cc(f)?;
                self.flush_callees(f)?;
                self.flush_cpu_instruction_cache();
                let ptr = unwrap!(self.aarch64.get_fn(f), "ICE: fn not compiled: {f:?} {}", self.program.pool.get(self.program[f].name));
                let comp_ctx = matches!(self.program[f].cc.unwrap(), CallConv::CCallRegCt);
                let f_ty = self.program[f].finished_ty().unwrap();
                let values = ffi::c::call(self, ptr as usize, f_ty, args, comp_ctx)?;
                let new_expr: FatExpr<'p> = from_values(self.program, values)?;

                *expr = new_expr;
                if expr.done {
                    assert!(!expr.ty.is_unknown());
                }
                debugln_call!("macro result: {}", expr.log(self.program.pool));
                // Now evaluate whatever the macro gave us.
                return self.compile_expr(expr, requested);
            }
        })
    }

    // This is a redundant check but it means you can move the error message to before the macro expansion which might make it more clear.
    // Only works for simple cases where its possible to give a static output type for all invocations of the macro.
    fn typecheck_macro_outputs(&mut self, macro_f: FuncId, requested: Option<TypeId>) -> Res<'p, ()> {
        let Some(requested) = requested else { return Ok(()) };
        let Some(outputs) = &self.program[macro_f].annotations.iter().find(|a| a.name == Flag::Outputs.ident()) else {
            return Ok(());
        };
        let ty = unwrap2!(outputs.args.clone(), "annotation #outputs(T) requires arg T");
        let ty: TypeId = self.immediate_eval_expr_known(ty)?;
        self.type_check_arg(requested, ty, "macro #outputs")?;
        Ok(())
    }

    pub fn enum_constant_macro(&mut self, arg: FatExpr<'p>, mut target: FatExpr<'p>) -> Res<'p, FatExpr<'p>> {
        let loc = arg.loc;
        let ty: TypeId = self.immediate_eval_expr_known(arg.clone())?;

        let mut fields = vec![];
        match &mut target.expr {
            Expr::StructLiteralP(pattern) => {
                for b in &mut pattern.bindings {
                    assert!(b.default.is_some());
                    assert!(matches!(b.lazy(), LazyType::Infer));

                    let expr = unwrap!(b.default.take(), "");
                    let val = self.immediate_eval_expr(expr, ty)?;
                    let name = unwrap!(b.name(), "@enum field missing name??");
                    fields.push((name, val));
                }
            }
            Expr::Tuple(names) => {
                for (i, name) in names.iter().enumerate() {
                    let name = unwrap!(name.as_ident(), "@enum expected ident");
                    fields.push((name, to_values(self.program, i as i64)?)); // TODO: check that expected type is int
                }
            }
            _ => err!("Expected struct literal found {target:?}",),
        }
        let info = TypeInfo::Enum { raw: ty, fields };
        let unique_ty = self.program.intern_type(info);

        self.as_literal(unique_ty, loc)
    }

    // :PlaceExpr
    fn addr_macro(&mut self, macro_expr: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, TypeId> {
        let Expr::SuffixMacro(_, arg) = &mut macro_expr.expr else {
            unreachable!()
        };
        // This is kinda weird. base case because compile_place_expr turns anything into <ptr>[],
        // but the only thing you can do for a var is <var>!addr, so you get stuck in a loop nesting !addr.  -- May 12
        if let Expr::GetVar(var) = arg.deref_mut().deref_mut() {
            let value_ty = unwrap2!(self.program.pool.get_var_type(*var), "Missing var {} (in !addr)", var.log(self.program.pool));
            // TODO: this shouldn't allow let either but i changed how variable refs work for :SmallTypes
            if var.kind == VarType::Const {
                err!("Can only take address of vars not {:?} {}.", var.kind, var.log(self.program.pool))
            }
            let ptr_ty = self.program.ptr_type(value_ty);
            return Ok(ptr_ty);
        }

        self.compile_place_expr(arg, requested, false)?;

        // Note: not assigning to the arg, assinging to the whole macro call. want to replace the !addr.
        *macro_expr = mem::take(arg);
        macro_expr.done = true;
        Ok(macro_expr.ty)
    }

    // HACK: type_of_inner can return None because of errors elsewhere in the compiler (which is a bad idea the way im currently doing it probably) so might leave junk on the context stack.
    pub(crate) fn type_of(&mut self, expr: &mut FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        let s = DebugState::TypeOf;
        self.push_state(&s);
        let before = self.debug_trace.len();
        let res = self.type_of_inner(expr)?;
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
    fn type_of_inner(&mut self, expr: &mut FatExpr<'p>) -> Res<'p, Option<TypeId>> {
        if !expr.ty.is_unknown() {
            return Ok(Some(expr.ty));
        }

        // TODO: this is unfortunate
        if let Some((int, _)) = self.bit_literal(expr) {
            return Ok(Some(self.program.intern_type(TypeInfo::Int(int))));
        }
        Ok(Some(match expr.deref_mut() {
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::WipFunc(_) => return Ok(None),
            Expr::Cast(_) => unreachable!("@as puts type"),
            Expr::Value { value, .. } => unreachable!("{value:?} requires type"),
            Expr::Call(f, arg) => {
                if let Expr::GetVar(i) = f.deref_mut().deref_mut() {
                    if i.kind == VarType::Const {
                        if let Ok(fid) = self.resolve_function(*i, arg, None) {
                            if let Ok(Some(f_ty)) = self.infer_types(fid) {
                                // Need to save this because resolving overloads eats named arguments
                                f.set(to_values(self.program, fid)?, self.program.func_type(fid));
                                return Ok(Some(f_ty.ret));
                            }
                        }
                    } else if let BigOption::Some(ty) = self.program.pool.get_var_type(*i) {
                        if let TypeInfo::FnPtr { ty: f_ty, .. } = self.program[ty] {
                            return Ok(Some(f_ty.ret));
                        }
                    }
                } else if let Ok(Some(ty)) = self.type_of(f) {
                    if let Some(ty) = self.program.fn_ty(ty) {
                        return Ok(Some(ty.ret));
                    }
                }

                return Ok(None);
            }
            Expr::Block { result: e, .. } => return self.type_of(e),
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1);
                let mut types = vec![];
                for v in values {
                    types.push(self.type_of(v)?);
                }
                let before = types.len();
                let types: Vec<_> = types.into_iter().flatten().collect();
                self.last_loc = Some(expr.loc);
                if before != types.len() {
                    return Ok(None);
                }
                self.program.tuple_of(types)
            }
            Expr::FieldAccess(container, _) => {
                if (self.type_of(container)?).is_some() {
                    self.compile_expr(expr, None)?
                } else {
                    return Ok(None);
                }
            }
            Expr::PtrOffset { .. } => {
                unreachable!()
            }
            Expr::Closure(_) => {
                if let Ok(id) = self.promote_closure(expr, None, None) {
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

                // TODO: hack yuck. now that i rely on expr working so it can be passed into quoted things, this is extra ugly.
                match self.compile_expr(handler, Some(TypeId::overload_set)) {
                    Ok(_) => {
                        // TODO: allow Fn and FnPtr as well.
                        let os: OverloadSetId = if let Expr::Value { value, .. } = &handler.expr {
                            from_values(self.program, value.clone())?
                        } else {
                            self.immediate_eval_expr_known(*handler.clone())?
                        };
                        // Note: need to compile first so if something's not ready yet, you dont error in the asm where you just crash.
                        if self.program[os].name == Flag::As.ident() && self.compile_expr(arg, Some(TypeId::ty)).is_ok() {
                            // HACK: sad that 'as' is special
                            let ty: TypeId = self.immediate_eval_expr_known(*arg.clone())?;
                            return Ok(Some(ty));
                        }
                    }
                    Err(e) => ice!("TODO: PrefixMacro handler inference failed. need to make it non-destructive?\n{e:?}",),
                }

                // TODO: seperate applying macros from full commit to compiling it all.
                // TODO: if this fails you might have changed the state.
                match self.compile_expr(expr, None) {
                    Ok(res) => res,
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
                            let value_ty = unwrap2!(self.program.pool.get_var_type(*var), "Missing resolved var (TODO: addr of const?)");
                            self.program.intern_type(TypeInfo::Ptr(value_ty))
                        }
                        &mut Expr::GetNamed(i) => {
                            err!(CErr::UndeclaredIdent(i))
                        }
                        _ => {
                            if (self.type_of(arg)?).is_some() {
                                self.compile_expr(expr, None)?
                            } else {
                                return Ok(None);
                            }
                        } // _ => err!("took address of r-value {}", arg.log(self.program.pool)),
                    },
                    Flag::Deref => {
                        let ptr_ty = self.type_of(arg)?;
                        if let Some(ptr_ty) = ptr_ty {
                            return Ok(self.program.unptr_ty(ptr_ty));
                        }
                        return Ok(None);
                    }
                    Flag::Tag => self.program.ptr_type(TypeId::i64()),
                    Flag::Fn_Ptr => {
                        // if let Some(f_ty) = self.type_of(arg)? {
                        //     if let Some(f_ty) = self.program.fn_ty(f_ty) {
                        //         // TODO: need to know the function because it might specify non-default calling convention.
                        //         return Ok(Some(self.program.intern_type(TypeInfo::FnPtr(f_ty))));
                        //     }
                        // }
                        return Ok(None);
                    }
                    Flag::Quote => unwrap!(self.program.fat_expr_type, "used quoted ast during bootstrapping"),
                    Flag::Loop => TypeId::never,
                    // TODO: there's no reason this couldn't look at the types, but if logic is more complicated (so i dont want to reproduce it) and might fail often (so id be afraid of it getting to a broken state).
                    Flag::If => return Ok(None),
                    Flag::As => {
                        // TODO: sad day
                        if let Expr::Value { value, .. } = &mut arg.expr {
                            let ty = unwrap!(self.program.tuple_types(arg.ty), "TODO: non-trivial pattern matching");
                            assert_eq!(ty[0], TypeId::ty, "@as expected type");
                            let mut reader = ReadBytes { bytes: value.bytes(), i: 0 };
                            let ty_val = chop_prefix(self.program, TypeId::ty, &mut reader)?;
                            assert!(ty.len() == 2);
                            let rest_ty = ty[1];
                            let parts = vec![
                                FatExpr::synthetic_ty(Expr::Value { value: ty_val, coerced: false }, arg.loc, TypeId::ty),
                                FatExpr::synthetic_ty(
                                    Expr::Value {
                                        value: Values::many(reader.bytes[reader.i..].to_vec()), 
                                        coerced: false
                                    },
                                    arg.loc,
                                    rest_ty,
                                ),
                            ];
                            arg.expr = Expr::Tuple(parts);
                        }

                        if let Expr::Tuple(parts) = &mut arg.expr {
                            assert!(parts.len() == 2);
                            if self.compile_expr(&mut parts[0], Some(TypeId::ty)).is_ok() {
                                // HACK: sad that 'as' is special
                                let ty: TypeId = self.immediate_eval_expr_known(parts[0].clone())?;
                                return Ok(Some(ty));
                            }
                        }
                        err!("bad !as",)
                    }
                    _ => match self.compile_expr(expr, None) {
                        Ok(res) => res,
                        Err(e) => ice!("TODO: SuffixMacro inference failed. need to make it non-destructive?\n{e:?}",),
                    },
                }
            }
            Expr::GetVar(var) => {
                let ty = if var.kind == VarType::Const {
                    self.find_const_type(*var)?
                } else {
                    self.program.pool.get_var_type(*var).into()
                };
                unwrap!(ty, "type check missing var {:?} (circular dependency:?)", var.log(self.program.pool))
                //TODO: else return Ok(None)?
            }
            Expr::String(_) => {
                let byte = u8::get_or_create_type(self.program);
                self.create_slice_type(byte, expr.loc)?
            }
        }))
    }

    pub fn set_literal<T: InterpSend<'p>>(&mut self, e: &mut FatExpr<'p>, t: T) -> Res<'p, ()> {
        *e = self.as_literal(t, e.loc)?;
        Ok(())
    }

    // TODO: this really shouldn't return an error -- May 24
    pub fn as_literal<T: InterpSend<'p>>(&mut self, t: T, loc: Span) -> Res<'p, FatExpr<'p>> {
        let ty = T::get_or_create_type(self.program);
        self.program.finish_layout_deep(ty)?;
        let value = to_values(self.program, t)?;
        let mut e = FatExpr::synthetic_ty(Expr::Value { value, coerced: false }, loc, ty);
        e.done = true;
        Ok(e)
    }

    pub fn builtin_constant(&mut self, name: Ident<'p>) -> Option<(Values, TypeId)> {
        let name = self.program.pool.get(name);

        match name {
            "true" => Some((Values::many(vec![1]), TypeId::bool())),
            "false" => Some((Values::many(vec![0]), TypeId::bool())),
            n => self.builtin_type(n).map(|ty| (to_values(self.program, ty).unwrap(), TypeId::ty)),
        }
    }

    pub fn builtin_type(&mut self, name: &str) -> Option<TypeId> {
        let ty = match name {
            "i64" => return Some(TypeId::i64()),
            "f64" => return Some(TypeId::f64()),
            "f32" => return Some(TypeId::f32()),
            "Type" => return Some(TypeId::ty),
            "bool" => return Some(TypeId::bool()),
            "UnknownType" => return Some(TypeId::unknown),
            "Never" => return Some(TypeId::never),
            "rawptr" => return Some(TypeId::voidptr),
            "OverloadSet" => return Some(TypeId::overload_set),
            "LabelId" => return Some(TypeId::label),
            "ScopeId" => return Some(TypeId::scope),
            "FuncId" => return Some(TypeId::func),
            _ => None,
        };
        if let Some(ty) = ty {
            let ty = self.program.intern_type(ty);
            return Some(ty);
        }

        macro_rules! ffi_type {
            ($ty:ty) => {{
                let ty = <$ty>::get_or_create_type(self.program);
                ty
            }};
        }

        Some(match name {
            "Symbol" => ffi_type!(Ident),
            _ => {
                let name = self.program.pool.intern(name);
                self.program.find_ffi_type(name)?
            }
        })
    }

    #[track_caller]
    pub fn infer_types_progress(&mut self, ty: &mut LazyType<'p>) -> Res<'p, bool> {
        let done = match ty {
            LazyType::EvilUnit => panic!(),
            LazyType::Infer => false,
            LazyType::PendingEval(e) => {
                let value = self.immediate_eval_expr(e.clone(), TypeId::ty)?;
                let res: TypeId = from_values(self.program, value)?;
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

    pub fn infer_binding_progress(&mut self, binding: &mut Binding<'p>) -> Res<'p, bool> {
        self.infer_types_progress(&mut binding.ty)
    }

    #[track_caller]
    pub fn infer_pattern(&mut self, bindings: &mut [Binding<'p>]) -> Res<'p, Vec<TypeId>> {
        let mut types = vec![];
        for arg in bindings {
            if let Some(e) = arg.ty.expr_ref() {
                self.last_loc = Some(e.loc);
            }
            if matches!(arg.ty, LazyType::Infer) {
                if let BigOption::Some(value) = &mut arg.default {
                    arg.ty = LazyType::Finished(self.type_of(value)?.unwrap())
                } else {
                    err!("Cannot infer_pattern without type hint",);
                }
            }
            assert!(self.infer_binding_progress(arg)?, "{arg:?}");
            types.push(arg.unwrap());
        }
        Ok(types)
    }

    fn infer_arg(&mut self, func: FuncId) -> Res<'p, TypeId> {
        self.ensure_resolved_sign(func)?;
        // TODO: this looks redundant but if you just check the arg multiple times you don't want to bother attempting the return type multiple times.
        if let BigOption::Some(ty) = self.program[func].finished_arg {
            Ok(ty)
        } else if let Some(ty) = self.infer_types(func)? {
            Ok(ty.arg)
        } else {
            Ok(unwrap2!(self.program[func].finished_arg, "arg not inferred"))
        }
    }

    // Resolve the lazy types for Arg and Ret
    // Ok(None) means return type needs to be infered
    #[track_caller]
    pub(crate) fn infer_types(&mut self, func: FuncId) -> Res<'p, Option<FnType>> {
        // println!("{} {}", self.program.pool.get(self.program[func].name), Location::caller());
        self.ensure_resolved_sign(func)?;
        let f = &self.program[func];
        // TODO: bad things are going on. it changes behavior if this is a debug_assert.
        //       oh fuck its because of the type_of where you can backtrack if you couldn't infer.
        //       so making it work in debug with debug_assert is probably the better outcome.
        assert!(f.get_flag(NotEvilUninit), "{}", self.program.pool.get(f.name));
        if let (BigOption::Some(arg), BigOption::Some(ret)) = (f.finished_arg, f.finished_ret) {
            return Ok(Some(FnType {
                arg,
                ret,
                arity: f.arg.bindings.len() as u16,
            }));
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
            self.program[func].finished_arg = BigOption::Some(arg);
        }
        let ty = if self.program[func].finished_ret.is_none() {
            let mut ret = self.program[func].ret.clone();
            // TODO: now this is dumb because a lot of places dont even get here if its #generic because they don't know if its arg or ret.
            if !self.program[func].get_flag(Generic) && self.infer_types_progress(&mut ret)? {
                self.program[func].ret = ret;
                let ret = self.program[func].ret.unwrap();
                self.program[func].finished_ret = BigOption::Some(ret);

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

    pub fn immediate_eval_expr_known<Ret: InterpSend<'p>>(&mut self, e: FatExpr<'p>) -> Res<'p, Ret> {
        unsafe { STATS.const_eval_node += 1 };
        let ret_ty = Ret::get_or_create_type(self.program);
        self.program.finish_layout(ret_ty)?;
        let values = self.immediate_eval_expr(e, ret_ty)?;
        from_values(self.program, values)
    }

    fn discard(&mut self, f: FuncId) {
        self.program[f].set_flag(NotEvilUninit, false);
    }

    // TODO: look through Expr::Cast
    fn check_quick_eval(&mut self, e: &mut FatExpr<'p>, ret_ty: TypeId) -> Res<'p, Option<Values>> {
        match e.deref_mut() {
            Expr::Block { body, result, .. } => {
                if body.is_empty() {
                    return self.check_quick_eval(result, ret_ty);
                }
            }
            Expr::Value { value, .. } => return Ok(Some(value.clone())),
            Expr::GetVar(var) => {
                if let Some((value, ty)) = self.find_const(*var)? {
                    self.type_check_arg(ty, ret_ty, "quick eval")?;
                    return Ok(Some(value));
                } else {
                    // TODO: -- Apr 24 I think this is always the problem. but what changed??
                    println!("comptime eval const not ready {}", var.log(self.program.pool));
                }
                // fallthrough
            }
            Expr::Tuple(elements) => {
                let types = if ret_ty == TypeId::ty {
                    vec![TypeId::ty; elements.len()]
                } else if let Some(types) = self.program.tuple_types(ret_ty) {
                    types.to_vec()
                } else {
                    unreachable!("{}", self.program.log_type(ret_ty))
                };
                // TODO: this only helps if some can be quick-evaled by special cases above, otherwise makes it worse.
                // TODO:  however.... it debug_asserts it you comment this case out!! -- Apr 30
                //      because of the special casing on types? (Type, Type) === Type
                //      I think actually its because you get here when compiling inline asm at the very beginning.
                //      so you need to be able to do it without all basic ops being ready yet. -- May 27
                let mut values = vec![];
                for (e, ty) in elements.iter().zip(types) {
                    values.push(self.immediate_eval_expr(e.clone(), ty)?)
                }
                if values.len() == 1 {
                    return Ok(Some(values.into_iter().next().unwrap()));
                }

                self.program.finish_layout(ret_ty)?;
                let ty = self.program.raw_type(ret_ty);
                let mut pls = vec![];
                if let TypeInfo::Struct { fields, .. } = &self.program[ty] {
                    if fields.len() == values.len() {
                        for (v, f) in values.into_iter().zip(fields.iter()) {
                            while pls.len() < f.byte_offset {
                                pls.push(0);
                            }
                            pls.extend(v.bytes());
                        }
                        while pls.len() < self.program.get_info(ty).stride_bytes as usize {
                            pls.push(0);
                        }
                        return Ok(Some(Values::many(pls)));
                    }
                    // fallthrough
                }

                // println!("TODO: untested. quick eval to {}", self.program.log_type(ret_ty));
                // TODO: handle alignment
                // for v in values {
                //     pls.extend(v.0);
                // }
                // return Ok(Some(Values::many(pls)));
                // for now just...
                // fallthrough
            }
            Expr::Call(f, arg) => {
                // !slice and !addr can't be const evaled!
                if !matches!(arg.expr, Expr::SuffixMacro(_, _)) {
                    let val_ty = if let Expr::Value { value, .. } = &f.expr {
                        Some((value.clone(), f.ty))
                    } else if let Expr::GetVar(var) = f.expr {
                        self.find_const(var)?
                    } else {
                        None
                    };
                    let f_id = if let Some((val, ty)) = val_ty {
                        if ty == TypeId::overload_set {
                            let ff: OverloadSetId = from_values(self.program, val.clone())?;
                            let ol = &self.program[ff];
                            if ol.pending.is_empty() && ol.ready.len() == 1 {
                                Some(ol.ready[0].func)
                            } else {
                                None
                            }
                        } else if matches!(self.program[ty], TypeInfo::Fn(_)) || ty == FuncId::get_or_create_type(self.program) {
                            Some(val.unwrap_func_id())
                        } else {
                            err!("Expected function but found {:?}", val);
                        }
                    } else {
                        None
                    };
                    if let Some(f) = f_id {
                        debug_assert!(self.program[f].get_flag(NotEvilUninit));
                        // TODO: try to compile it now.
                        // TODO: you do want to allow self.program[f].has_tag(Flag::Ct) but dont have the result here and cant tell which ones need it
                        let is_ready = self.program[f].get_flag(AsmDone);
                        if is_ready && !self.program[f].any_const_args() {
                            // currently this mostly just helps with a bunch of SInt/Unique/UInt calls on easy constants at the beginning.
                            let arg_ty = self.program[f].finished_arg.unwrap();
                            let arg = self.immediate_eval_expr(mem::take(arg), arg_ty)?;
                            return Ok(Some(self.run(f, arg)?));
                        }
                        // fallthrough
                    }
                    // fallthrough
                    // println!("{}", f.log(self.program.pool));
                }

                // fallthrough
            }
            // TODO: @enum field access
            _ => {} // fallthrough
        }
        // println!("{}\n\n\n", e.log(self.program.pool));
        Ok(None)
    }

    fn make_lit_function(&mut self, e: FatExpr<'p>, ret_ty: TypeId) -> Res<'p, FuncId> {
        debug_assert!(!(e.as_suffix_macro(Flag::Slice).is_some() || e.as_suffix_macro(Flag::Addr).is_some()));
        unsafe { STATS.make_lit_fn += 1 };
        let name = Flag::Anon.ident();
        let (arg, ret) = Func::known_args(TypeId::unit, ret_ty, e.loc);
        let mut fake_func = Func::new(name, arg, ret, Some(e.clone()), e.loc, false);
        fake_func.set_flag(ResolvedBody, true);
        fake_func.set_flag(ResolvedSign, true);
        fake_func.finished_arg = BigOption::Some(TypeId::unit);
        fake_func.finished_ret = BigOption::Some(ret_ty);
        fake_func.scope = BigOption::Some(ScopeId::from_index(0));
        self.anon_fn_counter += 1;
        let func_id = self.program.add_func(fake_func);
        self.update_cc(func_id)?;
        Ok(func_id)
    }

    // TODO: coerce_constant?
    pub fn immediate_eval_expr(&mut self, mut e: FatExpr<'p>, ret_ty: TypeId) -> Res<'p, Values> {
        self.wip_stack.push((None, ExecStyle::Jit)); // :PushConstFnCtx
        self.program.finish_layout(ret_ty)?;
        unsafe { STATS.const_eval_node += 1 };
        if let Some(values) = self.check_quick_eval(&mut e, ret_ty)? {
            assert!(self.wip_stack.pop().unwrap().0.is_none());
            return Ok(values);
        }
        let func_id = self.make_lit_function(e, ret_ty)?;
        if let FuncImpl::Normal(body) = &mut self.program[func_id].body {
            let mut stolen_body = mem::take(body);
            if let Err(e) = self.compile_expr(&mut stolen_body, Some(ret_ty)) {
                unsafe {
                    let loc = e.loc.or(self.last_loc);
                    if let Some(loc) = loc {
                        show_error_line(self.program.pool.codemap, loc.low, loc.high);
                    }
                }
                panic!("{e:?}")
            }
            if let Some(values) = self.check_quick_eval(&mut stolen_body, ret_ty)? {
                self.discard(func_id);
                assert!(self.wip_stack.pop().unwrap().0.is_none());
                return Ok(values);
            }
            self.program[func_id].body = FuncImpl::Normal(stolen_body);
        }

        // println!("{}", self.program[func_id].body.log(self.program.pool));

        // TODO: HACK kinda because it might need to be compiled in the function context to notice that its really a constant so this gets double checked which is sad -- May 1
        //       also maybe undo this opt if can't find the asm bug cause its a simpler test case.
        // let mut e = self.program[func_id].body.as_mut().unwrap().clone(); // TODO: no clone so remove
        // if let Some(res) = self.check_quick_eval(&mut e, ret_ty)? {
        //     return Ok(res);
        // }

        let res = self.run(func_id, Values::unit());
        self.discard(func_id);
        assert!(self.wip_stack.pop().unwrap().0.is_none());
        res
    }
    
    // TODO: :coerce_for_const_arg
    // TODO: you can never do this to a constant directly in case its aliased once i have const ptrs. 
    fn coerce_constant(&mut self, value: &mut Values, current: TypeId, requested: TypeId, loc: Span) -> Res<'p, Option<FatExpr<'p>>> {
        if current == requested {
            return Ok(None);
        }
        
        fn int_value(value: &Values, int: IntTypeInfo) -> Res<'static, i128> {
            // TODO: handle signged/unsigned correctly. test!
            let ptr = value.bytes().as_ptr();
            debug_assert_eq!(ptr as usize % value.len(), 0, "Unaligned constant pointer.");
            let i = unsafe {
                if int.signed {
                    match value.len() {
                        1 => *(ptr as *const i8) as i128,
                        2 => *(ptr as *const i16) as i128,
                        4 => *(ptr as *const i32) as i128,
                        8 => *(ptr as *const i64) as i128,
                        _ => err!("bad int size",),
                    }
                } else {
                    match value.len() {
                        1 => *ptr as i128,
                        2 => *(ptr as *const u16) as i128,
                        4 => *(ptr as *const u32) as i128,
                        8 => *(ptr as *const u64) as i128,
                        _ => err!("bad int size",),
                    }
                }
            };
            
            let (min, max) = range(int);
            debug_assert!(i <= max && i >= min, "tried to coerce but value out of bounds for FROM type!?!?!!?");
            Ok(i)
        }
        
        // this currently works but probably only loosly overlaps with being correct because its nap time. 
        fn adjust_int_length(value: &mut Values, int: IntTypeInfo) {
            let Values::Small(value, byte_count) = value else {unreachable!("big value for int {value:?}")};
            let mut want_byte_count = (int.bit_count as u8 + 7) / 8;
            if want_byte_count * 8 != int.bit_count as u8 {
                // weird ints are stored dumby currently 
                want_byte_count = 8;
            }
            let mut i = *byte_count;
            while *byte_count > want_byte_count {
                *byte_count -= 1;
                if int.signed {
                    debug_assert_eq!(*value & (255 << i), 255);
                } else {
                    debug_assert_eq!(*value & (255 << i), 0);
                }
                i -= 1;
            }
            let mut i = *byte_count;
            while *byte_count < want_byte_count {
                *byte_count += 1;
                i += 1;
                if int.signed {
                    debug_assert_eq!(*value & (255 << i), 255);
                } else {
                    debug_assert_eq!(*value & (255 << i), 0);
                }
            }
        }
        
        fn range(int: IntTypeInfo) -> (i128, i128) {
            if int.signed {
                // TODO: UB on i1. 
                let max = (1i128 << (int.bit_count - 1)) - 1;
                (-max, max)
            } else {
                (0, (1i128 << int.bit_count) - 1)
            }
        }
        
        
        #[allow(clippy::single_match)]
        match (&self.program[current], &self.program[requested]) {
            (TypeInfo::Int(have), TypeInfo::Int(requested)) => {
                let (min, max) = range(*requested);
                let v = int_value(value, *have)?; // TODO: sign
                if v <= max && v >= min {
                    // adjust_int_length(value, *requested);
                    return Ok(None)
                }
            }
            (TypeInfo::Int(have), TypeInfo::F32) => {
                let max = (1i128 << f32::MANTISSA_DIGITS) - 1;
                let min = -max; // TODO: is that true? 
                let i = int_value(value, *have)?;
                if i <= max && i >= min {
                    *value = to_values(self.program, i as f32)?;
                    return Ok(None);
                }
            }
            (TypeInfo::Int(have), TypeInfo::F64) => {
                let max = (1i128 << f64::MANTISSA_DIGITS) - 1;
                let min = -max; // TODO: is that true? 
                let i = int_value(value, *have)?;
                if i <= max && i >= min {
                    *value = to_values(self.program, i as f64)?;
                    return Ok(None);
                }
            }
            // TODO: require that its in the float range too
            (TypeInfo::F64, TypeInfo::Int(requested)) => {
                let r = *requested;
                let v: f64 = from_values(self.program, value.clone())?;
                if v == v.floor() {
                    let (min, max) = range(r);
                    let v = v as i128;
                    if v <= max && v >= min {
                        *value = to_values(self.program, v as i64)?; // TODO: rightn umer of bits
                        adjust_int_length(value, r);
                        
                        return Ok(None);
                    }
                }
            }
            (c, TypeInfo::FnPtr { ty, .. }) => {
                if let TypeInfo::Fn(_) = c {
                    // TODO: pull up typechecking logic?
                    let e = FatExpr::synthetic_ty(Expr::Value { value: value.clone(), coerced: true }, loc, current);
                    let e = FatExpr::synthetic(Expr::SuffixMacro(Flag::Fn_Ptr.ident(), Box::new(e)), loc);
                    return Ok(Some(e));
                } else if TypeId::overload_set == current {
                    let ty = *ty;
                    let val = resolve_os(self, value, ty)?;
                    let ty = self.program.intern_type(TypeInfo::Fn(ty));
                    let e = FatExpr::synthetic_ty(Expr::Value { value: val, coerced: true }, loc, ty);
                    let e = FatExpr::synthetic(Expr::SuffixMacro(Flag::Fn_Ptr.ident(), Box::new(e)), loc);
                    return Ok(Some(e));
                }
            }
            (_, TypeInfo::Fn(ty)) => {
                if TypeId::overload_set == current {
                    *value = resolve_os(self, value, *ty)?;
                    return Ok(None);
                }
            }
            (TypeInfo::Fn(_), _) => {
                if TypeId::func == requested {
                    return Ok(None);
                }
            }
            _ => {}
        }
        
        fn resolve_os<'p>(s: &mut Compile<'_, 'p>, value: &mut Values, ty: FnType) -> Res<'p, Values> {
            let os: OverloadSetId = from_values(s.program, value.clone())?; 
            s.compute_new_overloads(os, Some(ty.arity))?;
            // TODO: just filter the iterator.
            let mut overloads = s.program[os].clone(); // sad
        
            overloads
                .ready
                .retain(|f| f.arg == ty.arg && (f.ret.is_none() || f.ret.unwrap() == ty.ret));
            // TODO: You can't just filter here anymore because what if its a Split FuncRef.
            let found = match overloads.ready.len() {
                0 => panic!("Missing overload",),
                1 => overloads.ready[0].func,
                _ => panic!("Ambigous overload \n{:?}", overloads.ready),
            };
            to_values(s.program, found)
        }
        
        // println!("err: Tried to coerce {} to {}", self.program.log_type(current), self.program.log_type(requested));
        err!(CErr::TypeCheck(current, requested, "coerce_constant"))
    }

    pub(crate) fn add_func(&mut self, mut func: Func<'p>) -> Res<'p, FuncId> {
        if func.has_tag(Flag::No_Trace) {
            func.set_flag(FnFlag::NoStackTrace, true);
        }
        let id = self.program.add_func(func);
        Ok(id)
    }

    // TODO: calling this in infer is wrong because it might fail and lose the function
    pub fn promote_closure(&mut self, expr: &mut FatExpr<'p>, req_arg: Option<TypeId>, req_ret: Option<TypeId>) -> Res<'p, FuncId> {
        let Expr::Closure(func) = expr.deref_mut() else { ice!("want closure") };

        // TODO: use :ClosureRequestType
        let f = self.add_func(mem::take(func))?;
        self.ensure_resolved_sign(f)?;

        // If the closure doesn't have type annotations but our caller asked for something specific,
        // insert that as a type annotation and don't bother looking at the body to infer.
        // It will get typechecked later when the callsite actually gets compiled.
        // TODO: this is wrong because it means overloading can't call this function! -- May 5
        if matches!(self.program[f].ret, LazyType::Infer) {
            if let Some(ret) = req_ret {
                self.program[f].ret = LazyType::Finished(ret);
            }
        }

        // like above, if the arg doesn't have a type annotation but we know what we want from context, fill that in.
        if let Some(arg) = req_arg {
            if self.program[f].arg.bindings.iter().any(|b| matches!(b.ty, LazyType::Infer)) {
                if self.program[f].arg.bindings.len() == 1 {
                    self.program[f].arg.bindings[0].ty = LazyType::Finished(arg);
                } else if let Some(types) = self.program.tuple_types(arg) {
                    if types.len() == self.program[f].arg.bindings.len() {
                        for (ty, b) in types.into_iter().zip(self.program[f].arg.bindings.iter_mut()) {
                            if matches!(b.ty, LazyType::Infer) {
                                b.ty = LazyType::Finished(ty);
                            }
                            // TODO: typecheck the ones we're skipping.
                            //       its a massive problem if you tried to pattern match destructure differently than i assumed
                            //       -- May 29
                        }
                    }
                }
            }
        }

        if !self.program[f].get_flag(Generic) && self.infer_types(f)?.is_none() {
            // TODO: i only do this for closures becuase its a pain to thread the &mut result through everything that calls infer_types().
            if let FuncImpl::Normal(body) = &mut self.program[f].body.clone() {
                // debug_
                assert!(self.program[f].get_flag(ResolvedBody), "ICE: closures aren't lazy currently. missing =>?");
                // TODO: this is very suspisious! what if it has captures
                let res = self.type_of(body);
                debug_assert!(res.is_ok(), "{res:?}"); // clearly its fine tho...
                if let Some(ret_ty) = res? {
                    self.program[f].finished_ret = BigOption::Some(ret_ty);
                }
            }
        }

        let (e, ty) = self.func_expr(f);
        expr.expr = e;
        expr.ty = ty;
        Ok(f)
    }

    fn finish_closure(&mut self, expr: &mut FatExpr<'p>) {
        if let Expr::WipFunc(id) = expr.expr {
            let (e, ty) = self.func_expr(id);
            expr.expr = e;
            expr.ty = ty;
        }
    }

    // TODO: adding inline to the func is iffy now that i dont require it to be an inline closure. it will affect other callsites. only want that if this is the only one.
    // TODO: it still doesnt allow the funcs to be any expr because i dont compile them first.
    fn emit_call_if(
        &mut self,
        if_macro: &mut FatExpr<'p>,
        requested: Option<TypeId>, // TODO: allow giving return type to infer
    ) -> Res<'p, TypeId> {
        let Expr::SuffixMacro(_, if_macro_arg) = &mut if_macro.expr else {
            err!("expected !if",)
        };
        let unit = TypeId::unit;
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        let mut unit_expr = self.as_literal((), if_macro_arg.loc)?;
        let Expr::Tuple(parts) = &mut if_macro_arg.expr else {
            ice!("if args must be tuple not {:?}", if_macro_arg);
        };
        let cond = self.compile_expr(&mut parts[0], Some(TypeId::bool()))?;
        self.type_check_arg(cond, TypeId::bool(), "bool cond")?;

        // TODO: this mostly can't happen anymore because you use the if function and params dont get forwarded like that. 
        //       should allow promoting things to constants. 
        // If its constant, don't even bother emitting the other branch
        // TODO: option to toggle this off for testing.
        if let Some(val) = parts[0].as_const() {
            let cond: bool = from_values(self.program, val)?;
            let cond_index = if cond { 1 } else { 2 };
            let Some(branch_body) = self.maybe_direct_fn(&mut parts[cond_index], &mut unit_expr, requested)? else {
                ice!("!if arg must be func not {:?}", parts[cond_index]);
            };

            let branch_arg = self.infer_arg(branch_body)?;
            self.type_check_arg(branch_arg, unit, sig)?;
            self.program[branch_body].set_cc(CallConv::Inline)?; // hack
            self.emit_call_on_unit(branch_body, &mut parts[cond_index], requested)?;
            assert!(self.program[branch_body].finished_ret.is_some());
            // Now we fully dont emit the branch
            // TODO: this is wrong, the cond could have returned a const but still had rt side effects (like if it was a block { blow_up_moon(); true }) -- May 2
            *if_macro = mem::take(&mut parts[cond_index]);

            // need to force the compile again to keep if constant for nested folding.
            return self.compile_expr(if_macro, requested);
        }

        let (true_ty, expect_fn) = if let Some(if_true) = self.maybe_direct_fn(&mut parts[1], &mut unit_expr, requested)? {
            self.program[if_true].set_cc(CallConv::Inline)?; // hack
            let true_arg = self.infer_arg(if_true)?;
            self.type_check_arg(true_arg, unit, sig)?;
            (self.emit_call_on_unit(if_true, &mut parts[1], requested)?, true)
        } else if parts[1].ty.is_unknown() {
            ice!("if second arg must be func not {}", parts[1].log(self.program.pool));
        } else {
            (parts[1].ty, false)
        };
        if expect_fn {
            let Some(if_false) = self.maybe_direct_fn(&mut parts[2], &mut unit_expr, requested.or(Some(true_ty)))? else {
                ice!("if third arg must be func not {:?}", parts[2]);
            };
            self.program[if_false].set_cc(CallConv::Inline)?; // hack
            let false_arg = self.infer_arg(if_false)?;
            self.type_check_arg(false_arg, unit, sig)?;
            let false_ty = self.emit_call_on_unit(if_false, &mut parts[2], requested)?;
            self.type_check_arg(true_ty, false_ty, sig)?;

            self.finish_closure(&mut parts[1]);
            self.finish_closure(&mut parts[2]);
        } else {
            assert!(!parts[2].ty.is_unknown());
            self.type_check_arg(true_ty, parts[2].ty, sig)?;
        }
        Ok(true_ty)
    }

    fn emit_call_loop(&mut self, while_macro_arg: &mut FatExpr<'p>) -> Res<'p, TypeId> {
        if !while_macro_arg.ty.is_unknown() {
            // We've been here before and already replaced closures with calls.
            return Ok(TypeId::unit);
        }

        let sig = "loop(fn(Unit) Unit)";
        let mut unit_expr = self.as_literal((), while_macro_arg.loc)?;

        if let Some(body_fn) = self.maybe_direct_fn(while_macro_arg, &mut unit_expr, Some(TypeId::unit))? {
            self.program[body_fn].set_cc(CallConv::Inline)?; // hack
            let body_arg = self.infer_arg(body_fn)?;
            self.type_check_arg(body_arg, TypeId::unit, sig)?;
            self.finish_closure(while_macro_arg);
            let body_ret = self.emit_call_on_unit(body_fn, while_macro_arg, Some(TypeId::unit))?;
            self.type_check_arg(body_ret, TypeId::unit, sig)?;
        } else {
            todo!("shouldnt get here twice")
        }
        while_macro_arg.ty = TypeId::unit;
        while_macro_arg.done = true;

        Ok(TypeId::never)
    }

    // TODO: I think this can just be (arg, target) = '{ let v: <arg> = <target>; v }'
    //       but it still has special handling in type_of to stop early.
    //       would need better type inference to make that the same, but probably want that anyway.
    //       ideally would also allow macros to infer a type even if they can't run all the way?    -- Apr 21
    pub fn as_cast_macro(&mut self, mut arg: FatExpr<'p>, mut target: FatExpr<'p>) -> Res<'p, FatExpr<'p>> {
        self.compile_expr(&mut arg, Some(TypeId::ty))?;
        let ty: TypeId = self.immediate_eval_expr_known(arg)?;
        assert!(!ty.is_unknown());
        // target.ty = ty;
        // have to do this here because it doesn't pass requested in from saved infered type.  // TODO: maybe it should? -- Apr 21
        // but not calling the normal compile_expr because it has stricter typechecking on the requested type.
        let found = self.compile_expr_inner(&mut target, Some(ty))?;
        self.type_check_arg(found, ty, "@as")?;
        target.ty = found;
        if found != ty {
            let loc = target.loc;
            let done = target.done;
            target = FatExpr::synthetic_ty(Expr::Cast(Box::new(mem::take(&mut target))), loc, ty);
            target.done = done;
        }
        // println!("{} vs {}", self.program.log_type(found), self.program.log_type(ty));
        // println!("{}", target.log(self.program.pool));

        Ok(target)
    }

    #[track_caller]
    pub fn type_check_arg(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        // TODO: dont do this. fix ffi types.
        let found = self.program.raw_type(found);
        let expected = self.program.raw_type(expected);

        if found == expected {
            Ok(())
        } else {
            // println!("{} {}", self.program.log_type(found), self.program.log_type(expected));
            match (&self.program[found], &self.program[expected]) {
                // :Coercion // TODO: only one direction makes sense
                (TypeInfo::Never, _) | (_, TypeInfo::Never) => return Ok(()),
                (TypeInfo::Int(a), TypeInfo::Int(b)) => {
                    if a.bit_count == b.bit_count && a.signed == b.signed {
                        return Ok(());
                    }
                    // :Coercion
                    // TODO: not this!
                    //
                    if a.bit_count == 64 && b.bit_count != 32 && b.bit_count != 16 && b.bit_count != 8 {
                        // && b.bit_count != 64 // TODO: adding this mostly works but breaks --no-fork, so some weird stateful thing?? -- Jun 3
                        return Ok(());
                    }
                    if b.bit_count == 64 && a.bit_count != 32 && a.bit_count != 16 && a.bit_count != 8 {
                        //  && a.bit_count != 64
                        return Ok(());
                    }
                }
                (&TypeInfo::Ptr(f), &TypeInfo::Ptr(e)) => {
                    if self.type_check_arg(f, e, msg).is_ok() {
                        return Ok(());
                    }
                }
                // TODO: not this!
                (TypeInfo::Struct { fields: f, .. }, TypeInfo::Struct { fields: e, .. }) => {
                    assert_eq!(f.len(), e.len());
                    for (f, e) in f.iter().zip(e.iter()) {
                        self.type_check_arg(f.ty, e.ty, msg)?;
                    }
                    return Ok(());
                }

                _ => {}
            }
            err!(CErr::TypeCheck(found, expected, msg))
        }
    }

    pub fn struct_type(&mut self, pattern: &mut Pattern<'p>) -> Res<'p, TypeInfo<'p>> {
        // TODO: maybe const keyword before name in func/struct lets you be generic.
        let _ = self.infer_pattern(&mut pattern.bindings)?;
        let parts: Vec<_> = pattern
            .bindings
            .iter()
            .map(|binding| {
                let name = unwrap!(binding.name.ident(), "field name");
                if binding.kind == VarType::Const {
                    assert!(binding.default.is_some(), "constant field {} must have a value.", self.program.pool.get(name));
                }
                let ty = unwrap!(binding.ty.ty(), "field type not inferred");
                let default = if let BigOption::Some(expr) = binding.default.clone() {
                    // TODO: no clone
                    Some(self.immediate_eval_expr(expr, ty)?)
                } else {
                    None
                };
                Ok((ty, name, default, binding.kind))
            })
            .collect();
        self.program.make_struct(parts.into_iter(), false)
    }

    fn set_deref(&mut self, full_stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        let Stmt::Set { place, value } = &mut full_stmt.stmt else {
            unreachable!()
        };
        match place.deref_mut().deref_mut() {
            // TODO: PrefixMacro is sketchy but makes []->.index work.
            Expr::PrefixMacro {.. } | Expr::GetVar(_) | Expr::FieldAccess(_, _) | Expr::SuffixMacro(_, _) => {
                self.compile_place_expr(place, None, true)?;
                let oldty = place.ty;
                let value_ty = self.compile_expr(value, Some(oldty))?;
                self.type_check_arg(value_ty, oldty, "reassign var")?;
                Ok(())
            }
            Expr::PtrOffset { .. } => unreachable!("compiled twice?"),
            &mut Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;` --"),
        }
    }

    // :PlaceExpr
    // takes any of (<ptr_expr>[] OR <var> OR <ptr>.<field>) and turns it into (<ptr_expr>[])
    #[track_caller]
    fn compile_place_expr(&mut self, place: &mut FatExpr<'p>, requested: Option<TypeId>, want_deref: bool) -> Res<'p, ()> {
        // println!("in: {}", place.log(self.program.pool));
        let loc = place.loc;
        match place.deref_mut().deref_mut() {
            Expr::GetVar(var) => {
                // Could even do this in the parser? -- May 12
                // Note: we no longer do this check here because auto deref creates a temperatoy illegal state that gest removed by deref_one.
                //       emit_bc still checks tho.
                // assert_eq!(var.kind, VarType::Var, "Only 'var' can be addressed (not let/const).");
                let val_ty = self.program.pool.get_var_type(*var);
                let val_ty = unwrap2!(val_ty, "var must be declared: {}", var.log(self.program.pool));
                let ptr_ty = self.program.ptr_type(val_ty);

                *place = FatExpr::synthetic_ty(Expr::SuffixMacro(Flag::Addr.ident(), Box::new(mem::take(place))), loc, ptr_ty);
                place.done = true;
                if want_deref {
                    self.deref_one(place)?;
                    // Don't set done again because you want to go around the main compile_expr another time so special load/store overloads get processed.
                    // TODO: maybe its better to just do that here too and not waste an extra recursion.
                }
            }
            Expr::FieldAccess(container, name) => {
                // TODO: could lookup field and pass down requested
                // Note: compile_expr may have already walked the container because it has to check if its an enum/scope.
                self.compile_place_expr(container, None, false)?;

                // :AutoDeref
                {
                    let raw = self.program.raw_type(container.ty);
                    let TypeInfo::Ptr(mut inner) = self.program[raw] else {
                        err!("PlaceExpr of FieldAccess should be ptr",)
                    };
                    inner = self.program.raw_type(inner);
                    // Pointers never have fields, so the thing behind the pointer, shouldn't be a pointer.
                    // This lets you write `self: *Self; self.name` instead of  `self: *Self; self[].name`.
                    while let TypeInfo::Ptr(next_inner) = self.program[inner] {
                        self.deref_one(container)?;
                        inner = self.program.raw_type(next_inner);
                    }
                }
                let (bytes, field_val_ty) = self.field_access_expr(container, *name)?;
                let field_ptr_ty = self.program.ptr_type(field_val_ty);
                debug_assert!(!matches!(container.expr, Expr::GetVar(_)));
                place.expr = Expr::PtrOffset {
                    ptr: Box::new(mem::take(container)),
                    bytes,
                    name: *name,
                };
                place.done = true;
                if want_deref {
                    place.ty = field_ptr_ty;
                    // Now we have the offset-ed ptr, add back the deref
                    self.deref_one(place)?;

                    // Don't set done again because you want to go around the main compile_expr another time so special load/store overloads get processed.
                } else {
                    // place.done = true;
                    place.ty = field_ptr_ty;
                }
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = Flag::try_from(*macro_name)?;
                match name {
                    Flag::Deref => {
                        // When you see a !deref, treat the expression as a pointer value.
                        let req = requested.map(|r| self.program.ptr_type(r));
                        self.compile_expr(arg, req)?;
                        if want_deref {
                            place.ty = self.program.unptr_ty(arg.ty).unwrap();
                        } else {
                            *place = mem::take(arg);
                        }
                        // Don't set done because you want to go around the main compile_expr another time so special load/store overloads get processed.
                        // place.done = true;
                    }
                    _ => err!("other place expr: {}!{}", arg.log(self.program.pool), self.program.pool.get(*macro_name)),
                }
            }
            &mut Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            Expr::PrefixMacro { .. } => {
                // TODO: this is sketchy but makes []->.index work.
                //       need to think about how requested/want_deref are handled
                self.compile_expr(place, requested)?;
                self.compile_place_expr(place, requested, want_deref)?;
            }
            _ => ice!("TODO: other `place=e;` {}", place.log(self.program.pool)),
        }
        // println!("out: {}", place.log(self.program.pool));
        Ok(())
    }

    fn deref_one(&mut self, ptr: &mut FatExpr<'p>) -> Res<'p, ()> {
        assert!(!ptr.ty.is_unknown(), "unknown type for deref_one");
        let raw = self.program.raw_type(ptr.ty);

        let TypeInfo::Ptr(inner) = self.program[raw] else {
            err!("expected ptr",)
        };

        if let Some(arg) = ptr.as_suffix_macro_mut(Flag::Addr) {
            // this allows auto deref to work on let ptr vars.
            if matches!(arg.expr, Expr::GetVar(_)) {
                // raw var expr is not allowed, we always refer to variables through thier address.
                let loc = ptr.loc;
                *ptr = FatExpr::synthetic_ty(Expr::SuffixMacro(Flag::Deref.ident(), Box::new(mem::take(ptr))), loc, inner);
            } else {
                // Avoid reduntant (whatever)&[].
                *ptr = mem::take(arg);
                if ptr.ty.is_unknown() {
                    ptr.ty = inner; // TODO: this shouldn't happen
                }
            }
        } else {
            let loc = ptr.loc;
            *ptr = FatExpr::synthetic_ty(Expr::SuffixMacro(Flag::Deref.ident(), Box::new(mem::take(ptr))), loc, inner);
        }
        assert!(!ptr.ty.is_unknown(), "unknown type for deref_one");
        Ok(())
    }

    // :PlaceExpr
    fn field_access_expr(&mut self, container_ptr: &mut FatExpr<'p>, name: Ident<'p>) -> Res<'p, (usize, TypeId)> {
        let container_ptr_ty = self.program.raw_type(container_ptr.ty);
        let depth = self.program.ptr_depth(container_ptr_ty);
        if depth != 1 {
            let ty = self.program.log_type(container_ptr_ty);
            err!("index expr ptr must be one level of indirection. {ty}",)
        }
        let container_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "",);

        let raw_container_ty = self.program.raw_type(container_ty);
        self.program.finish_layout(raw_container_ty)?;
        match &self.program[raw_container_ty] {
            TypeInfo::Struct { fields, layout_done, .. } => {
                debug_assert!(*layout_done);
                for f in fields {
                    if f.name == name {
                        container_ptr.done = true;
                        return Ok((f.byte_offset, f.ty));
                    }
                }
                err!("unknown name {} on {:?}", self.program.pool.get(name), self.program.log_type(container_ty));
            }
            TypeInfo::Tagged { cases, .. } => {
                for (f_name, f_ty) in cases.iter() {
                    if *f_name == name {
                        return Ok((8, *f_ty));
                    }
                }
                err!(
                    "unknown name {} on {:?}. \n{cases:?}",
                    self.program.pool.get(name),
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

    fn compile_call(&mut self, expr: &mut FatExpr<'p>, mut fid: FuncId) -> Res<'p, TypeId> {
        let loc = expr.loc;
        let done = expr.done;
        let (f_expr, arg_expr) = if let Expr::Call(f, arg) = expr.deref_mut() { (f, arg) } else { ice!("") };
        self.ensure_resolved_sign(fid)?;

        if self.program[fid].get_flag(Generic) {
            assert!(self.program[fid].any_const_args(), "fn with no const args redundantly marked #generic");
            self.compile_expr(arg_expr, None)?;
            fid = self.curry_const_args(fid, f_expr, arg_expr)?;
        }

        let arg_ty = self.infer_arg(fid)?;
        self.compile_expr(arg_expr, Some(arg_ty))?;
        self.last_loc = Some(loc);
        self.type_check_arg(arg_expr.ty, arg_ty, "fn arg")?;

        // TODO: you really want to compile as much of the body as possible before you start baking things.
        // TODO: if its a pure function you might want to do the call at comptime
        // TODO: make sure I can handle this as well as Nim: https://news.ycombinator.com/item?id=31160234
        if self.program[fid].any_const_args() {
            debug_assert!(!self.program[fid].get_flag(Generic), "#generic was handled above");
            fid = self.curry_const_args(fid, f_expr, arg_expr)?;
        }

        self.update_cc(fid)?; // kinda hack to check if inlined
        let func = &self.program[fid];
        // TODO: some heuristic based on how many times called and how big the body is.
        let force_inline = func.cc == BigOption::Some(CallConv::Inline);
        let deny_inline = func.has_tag(Flag::NoInline);
        assert!(!(force_inline && deny_inline), "{fid:?} is both @inline and @noinline");
        let will_inline = force_inline || func.get_flag(AllowRtCapture) || func.get_flag(MayHaveAquiredCaptures);
        assert!(!(will_inline && deny_inline), "{fid:?} has captures but is @noinline");

        if func.get_flag(FnFlag::UnsafeNoopCast) {
            let ret_ty = func.finished_ret.unwrap();
            // TOOD: ideally you'd do this to run comptime code for checking but its awquard because its on the bootstrap path. -- Jun 1
            // self.ensure_compiled(fid, ExecStyle::Jit)?;
            let done = arg_expr.done;
            // TODO: do this check once per #unsafe_noop function somewhere else. after applying constant arguments.
            assert_eq!(self.program.slot_count(arg_expr.ty), self.program.slot_count(ret_ty));
            expr.expr = Expr::Cast(mem::take(arg_expr));
            expr.ty = ret_ty;
            expr.done = done;
            return Ok(expr.ty);
        }

        if will_inline {
            // If we're just inlining for #inline, compile first so some work on the ast is only done once.
            // note: compile() checks if its ::Inline before actually generating asm so it doesn't waste its time.
            // if its a '=>' function, we can't compile it out of context, and same if it has a const arg of a '=>' function. 
            if !func.get_flag(AllowRtCapture) && !func.get_flag(MayHaveAquiredCaptures) {
                self.compile(fid, self.exec_style())?;
            }

            // TODO: check that you're calling from the same place as the definition.
            self.emit_capturing_call(fid, expr)
        } else if !done && func.get_flag(TryConstantFold) && arg_expr.is_const() {
            // TODO: this should be smarter. like if fn add is #fold, then (1.add(2.add(3))) doesn't need to do the fold at both levels,
            // it can just emit one function for doing the top one. idk if thats better. probably depends how complicated the expression is.
            let ty = self.program.func_type(fid);
            let ret_ty = self.program[fid].finished_ret.unwrap();
            f_expr.set((fid.as_raw()).into(), ty);
            expr.done = true; // don't infinitly recurse!
            expr.ty = ret_ty;

            self.compile(fid, ExecStyle::Jit)?;
            let value = self.immediate_eval_expr(expr.clone(), ret_ty)?;
            expr.set(value, ret_ty);
            Ok(ret_ty)
        } else {
            let res = self.emit_runtime_call(fid, arg_expr)?;
            // Since we've called it, we must know the type by now.
            // TODO: cope with emit_runtime_call baking const args, needs to change the arg expr
            let ty = self.program.func_type(fid);
            f_expr.set((fid.as_raw()).into(), ty);
            
            
            // this fixes functions with all const args the reduce to just a value emitting useless calls to like get the number 65 or whatever if you do ascii("A"). 
            if !deny_inline && arg_expr.is_raw_unit() && !self.program.get_info(res).contains_pointers {
                if let FuncImpl::Normal(FatExpr {
                    expr: Expr::Value { value, .. }, ty: prev_ty, ..
                }) = &self.program[fid].body {
                    // self.type_check_arg(*prev_ty, res, "inline const expr fn")?;
                    expr.expr = Expr::Value { value: value.clone(), coerced: false };
                    expr.ty = *prev_ty;
                    expr.done = true;
                    
                    return Ok(*prev_ty);
                }
            }
            // if you have to remove this, do it for arg_expr at least. 
            expr.done = true;
            
            Ok(res)
        }
    }

    fn const_args_key(&mut self, original_f: FuncId, arg_expr: &mut FatExpr<'p>) -> Res<'p, Values> {
        if self.program[original_f].arg.bindings.len() == 1 {
            let arg_ty = self.compile_expr(arg_expr, self.program[original_f].finished_arg.into())?;
            let value = self.immediate_eval_expr(arg_expr.clone(), arg_ty)?;
            if !matches!(arg_expr.expr, Expr::Value { .. }) {
                arg_expr.set(value.clone(), arg_ty);
            }
            Ok(value)
        } else {
            let check_len = |len| {
                assert_eq!(
                    self.program[original_f].arg.bindings.len(),
                    len,
                    "TODO: non-trivial pattern matching\n{:?} <= {:?} for call to {:?}",
                    self.program[original_f].arg,
                    arg_expr.log(self.program.pool),
                    self.program.pool.get(self.program[original_f].name)
                );
                Ok(())
            };

            let ty = unwrap!(self.program.tuple_types(arg_expr.ty), "TODO: non-trivial pattern matching");
            check_len(ty.len())?;
            match &arg_expr.expr {
                Expr::Value { value, .. } => {
                    // TODO: this is super dumb but better than what I did before. -- May 3 -- May 24
                    let values = value;
                    let mut parts = Vec::with_capacity(values.bytes().len());

                    let mut reader = ReadBytes { bytes: value.bytes(), i: 0 };
                    for ty in ty {
                        let taken = chop_prefix(self.program, ty, &mut reader)?;
                        parts.push(FatExpr::synthetic_ty(Expr::Value { value: taken, coerced: false }, arg_expr.loc, ty))
                    }
                    assert_eq!(reader.bytes.len(), reader.i, "TODO: nontrivial pattern matching");
                    arg_expr.expr = Expr::Tuple(parts);
                }
                Expr::Tuple(parts) => {
                    check_len(parts.len())?;
                }
                _ => err!("TODO: fancier pattern matching",),
            }

            let Expr::Tuple(arg_exprs) = arg_expr.deref_mut() else {
                err!("TODO: pattern match on non-tuple",)
            };
            assert_eq!(arg_exprs.len(), self.program[original_f].arg.bindings.len(), "TODO: non-tuple baked args");

            let mut all_const_args = vec![];
            let mut i = 0;
            while let Some(binding) = self.program[original_f].arg.bindings.get(i) {
                if binding.kind != VarType::Const {
                    i += 1;
                    continue;
                }
                let arg_ty = binding.ty.ty().unwrap_or_else(|| arg_exprs[i].ty);
                let value = self.immediate_eval_expr(arg_exprs[i].clone(), arg_ty)?;
                if !matches!(arg_exprs[i].expr, Expr::Value { .. }) {
                    arg_exprs[i].set(value.clone(), arg_ty);
                }

                all_const_args.extend(value.bytes());
                i += 1;
            }
            Ok(Values::many(all_const_args))
        }
    }

    fn remove_const_args(&mut self, original_f: FuncId, arg_expr: &mut FatExpr<'p>, new_arg_type: TypeId) -> Res<'p, ()> {
        // println!("new_arg_type = {}", self.program.log_type(new_arg_type));
        if self.program[original_f].arg.bindings.len() == 1 {
            assert!(new_arg_type == TypeId::unit);
            self.set_literal(arg_expr, ())
        } else {
            let Expr::Tuple(arg_exprs) = arg_expr.deref_mut() else {
                err!("TODO: pattern match on non-tuple",)
            };
            let mut removed = 0;
            for (i, binding) in self.program[original_f].arg.bindings.iter().enumerate() {
                if let Some(expected) = binding.ty.ty() {
                    let found = arg_exprs[i - removed].ty;
                    self.type_check_arg(found, expected, "const arg")?
                }
                // TODO: else!!!
                if binding.kind == VarType::Const {
                    // TODO: this would be better if i was iterating backwards
                    arg_exprs.remove(i - removed);
                    removed += 1; // TODO: this sucks
                }
            }
            if let Expr::Tuple(v) = arg_expr.deref_mut().deref_mut() {
                if v.is_empty() {
                    assert!(new_arg_type == TypeId::unit);
                    // Note: this started being required when I added fn while.
                    self.set_literal(arg_expr, ())?;
                } else if v.len() == 1 {
                    *arg_expr = mem::take(v.iter_mut().next().unwrap());
                    self.type_check_arg(arg_expr.ty, new_arg_type, "const arg")?;
                } else {
                    arg_expr.ty = TypeId::unknown;
                    let found_type = self.compile_expr(arg_expr, Some(new_arg_type))?;
                    self.type_check_arg(found_type, new_arg_type, "const arg")?;
                }
            } else {
                unreachable!("required tuple above")
            }
            Ok(())
        }
    }

    fn curry_const_args(&mut self, original_f: FuncId, f_expr: &mut FatExpr<'p>, arg_expr: &mut FatExpr<'p>) -> Res<'p, FuncId> {
        let key = (original_f, self.const_args_key(original_f, arg_expr)?);
        if let Some(new_f) = self.program.const_bound_memo.get(&key).copied() {
            let new_arg_type = self.program[new_f].finished_arg.unwrap();
            self.remove_const_args(original_f, arg_expr, new_arg_type)?;
            return Ok(new_f);
        }

        if self.program[original_f].get_flag(AllowRtCapture) {
            debug_assert!(self.program[original_f].get_flag(ResolvedBody));
        } else {
            self.program[original_f].assert_body_not_resolved()?;
        }
        let state = DebugState::Msg(format!("Bake CT Only {original_f:?}"));
        self.push_state(&state);
        // Some part of the argument must be known at comptime.
        // You better hope compile_expr noticed and didn't put it in a stack slot.
        let mut new_func = self.program[original_f].clone();
        // Closures always resolve up front, so they need to renumber the clone.
        // TODO: HACK but closures get renumbered when inlined anyway, so its just the const args that matter. im just being lazy and doing the whole thing redundantly -- May 9
        // normal functions with const args havent had their body resolved yet so don't have to deal with it, we only resolve on the clone.
        // the special case for generics is when args can reference previous ones so they have to resolve sign earlier.

        let needs_renumber = self.program[original_f].get_flag(AllowRtCapture) || self.program[original_f].get_flag(Generic);
        if needs_renumber {
            let mut mapping = Default::default();
            let mut renumber = RenumberVars {
                vars: self.program.next_var,
                mapping: &mut mapping,
            };
            renumber.pattern(&mut new_func.arg);
            renumber.ty(&mut new_func.ret);
            let FuncImpl::Normal(body) = &mut new_func.body else { unreachable!() };
            renumber.expr(body);
            self.program.next_var = renumber.vars;
            if self.program[original_f].get_flag(Generic) && !self.program[original_f].get_flag(ResolvedBody) {
                // the sign has already been resolved so we need to renumber before binding arguments.
                // however, the body hasn't been resolved yet, so we can't just renumber in place.
                // instead, remap the sign as normal and then, insert a new scope containing the remapped variables,
                // and use that as the starting point when we resolve the body of new new function.
                // that way when it iterates up the scopes to resolve names, it will see our remapped shadows instead of the original.
                // this allows #generic argument types that reference the values of other argument.
                // but doing this is a bit creepy because the Var.scope isn't updated to the new one,
                // so they get inserted back in the old one's constants/rt_types again later.
                // that's why its fine when we can't find a remap for something in constants or vars.      -- May 29
                debug_assert!(self.program[original_f].get_flag(ResolvedSign));
                debug_assert!(!self.program[original_f].get_flag(AllowRtCapture));
                self.program[original_f].assert_body_not_resolved()?;
                // TODO: not true for 'name :: fn() = {}' exprs because they don't stop.
                // debug_assert!(
                //     !matches!(body.expr, Expr::Block { .. }),
                //     "Block should be GetParsed because body is not resolved yet. "
                // );
                let id = self.program.pool.dup_scope(new_func.scope.unwrap(), mapping);
                new_func.scope = BigOption::Some(id);
            }
        }
        let new_fid = self.program.add_func(new_func);
        self.ensure_resolved_sign(new_fid)?;
        self.ensure_resolved_body(new_fid)?;

        let func = &self.program[new_fid];

        if self.program[new_fid].arg.bindings.len() == 1 {
            let binding = &self.program[new_fid].arg.bindings[0];
            debug_assert_eq!(binding.kind, VarType::Const);
            let Name::Var(name) = binding.name else {
                err!("arg needs name (unreachable?)",)
            };
            let ty = arg_expr.ty;
            let Expr::Value { value, .. } = arg_expr.expr.clone() else { unreachable!() };
            self.bind_const_arg(new_fid, name, value, ty, arg_expr.loc)?;
            self.set_literal(arg_expr, ())?;

            if self.program[new_fid].get_flag(Generic) {
                self.program[new_fid].set_flag(Generic, false);
                self.infer_types(new_fid)?;
            }
            let ty = self.program.func_type(new_fid);

            f_expr.set(new_fid.as_raw().into(), ty);
            self.program.const_bound_memo.insert(key, new_fid);
            self.pop_state(state);

            Ok(new_fid)
        } else {
            let loc = arg_expr.loc;
            let Expr::Tuple(arg_exprs) = arg_expr.deref_mut() else {
                err!("TODO: pattern match on non-tuple",)
            };
            let pattern = func.arg.clone();
            let is_generic = func.get_flag(Generic);
            let mut removed_count = 0;
            for (i, mut b) in pattern.bindings.into_iter().enumerate() {
                if b.kind != VarType::Const {
                    if is_generic {
                        self.infer_types_progress(&mut b.ty)?;
                        self.program[new_fid].arg.bindings[i - removed_count].ty = b.ty.clone();
                    }
                    continue;
                }
                let Name::Var(name) = b.name else {
                    err!("arg needs name (unreachable?)",)
                };
                let Expr::Value { value, .. } = arg_exprs[i].clone().expr else {
                    unreachable!()
                };
                if !is_generic {
                    b.ty.unwrap();
                }
                self.infer_types_progress(&mut b.ty)?;
                if is_generic {
                    self.program[new_fid].arg.bindings[i].ty = b.ty.clone();
                }
                // bind_const_arg handles adding closure captures.
                // since it needs to do a remap, it gives back the new argument names so we can adjust our bindings acordingly. dont have to deal with it above since there's only one.
                self.bind_const_arg(new_fid, name, value, b.unwrap(), loc)?;
                removed_count += 1;
            }
            debug_assert_ne!(new_fid, original_f);

            if is_generic {
                self.program[new_fid].set_flag(Generic, false);
                let ty = self.infer_types(new_fid)?;
                assert!(ty.is_some(), "failed to infer types of #generic function");
            }

            let ty = self.program.func_type(new_fid);
            f_expr.set((new_fid.as_raw()).into(), ty);
            let f_ty = self.program.fn_ty(ty).unwrap();

            let new_arg_type = self.program[new_fid].finished_arg.unwrap();
            self.remove_const_args(original_f, arg_expr, new_arg_type)?;
            self.type_check_arg(arg_expr.ty, f_ty.arg, "sanity: post bake arg")?;
            self.program.const_bound_memo.insert(key, new_fid);
            self.pop_state(state);
            // Don't need to explicitly force capturing because bind_const_arg added them if any args were closures.
            Ok(new_fid)
        }
    }

    fn emit_call_on_unit(&mut self, cond_fn: FuncId, expr_out: &mut FatExpr<'p>, requested: Option<TypeId>) -> Res<'p, TypeId> {
        let (e, ty) = self.func_expr(cond_fn);
        let get_fn = FatExpr::synthetic_ty(e, expr_out.loc, ty);
        let unit = self.as_literal((), expr_out.loc)?;
        expr_out.expr = Expr::Call(Box::new(get_fn), Box::new(unit));
        expr_out.ty = self.program[cond_fn].finished_ret.unwrap_or(TypeId::unknown);
        self.compile_expr(expr_out, requested)
    }

    // TODO: debug warning for misuse of #inline and #one_ret_pic
    /// The first two can be used for early bootstrapping since they just look at the ast without comptime eval.
    /// - tuple of string literals -> llvm-ir
    /// - tuple of 32-bit int literals -> aarch64 asm ops
    /// - anything else, comptime eval expecting Slice(u32) -> aarch64 asm ops
    pub fn inline_asm_body(&mut self, f: FuncId, asm: &mut FatExpr<'p>) -> Res<'p, FuncImpl<'p>> {
        self.last_loc = Some(self.program[f].loc);
        assert!(
            self.program[f].has_tag(Flag::C) || self.program[f].has_tag(Flag::C_Call) || self.program[f].has_tag(Flag::One_Ret_Pic),
            "inline asm msut specify calling convention. but just: {:?}",
            self.program[f].annotations.iter().map(|a| self.program.pool.get(a.name)).collect::<Vec<_>>()
        );
        if self.program[f].has_tag(Flag::Aarch64) {
            // TODO: :PushConstFnCtx
            // TODO: you can't just compile here because then trying to imm_eval hits a not read asm func i think because of ^ callees.
            //       it recurses and has to emit other asm first but they don't get put in dispatch,
            //       becuase they don't have a thing in the result stack to do callees first.
            let ops = 'o: {
                if let Expr::Tuple(parts) = asm.deref_mut().deref_mut() {
                    let mut ops = Vec::with_capacity(parts.len());
                    for int in parts {
                        let i: u32 = self.immediate_eval_expr_known(int.clone())?; // TODO: sad clone
                        ops.push(i);
                    }
                    break 'o ops;
                }

                let ty = self.type_of(asm)?;
                if let Some(ty) = ty {
                    self.program.finish_layout_deep(ty)?;
                    if let TypeInfo::Struct { fields, .. } = &self.program[ty] {
                        let is_ints = fields.iter().all(|t| matches!(self.program[t.ty], TypeInfo::Int(_)));
                        if is_ints {
                            let len = fields.len();
                            let val = self.immediate_eval_expr(asm.clone(), ty)?;
                            let mut reader = ReadBytes { bytes: val.bytes(), i: 0 };
                            let mut ints = vec![];
                            for _ in 0..len {
                                let op = unwrap!(reader.next_u32(), "");
                                ints.push(op);
                            }
                            assert_eq!(reader.i, val.bytes().len()); // TODO: do this check everywhere.
                            break 'o ints;
                        }
                    }
                }
                todo!("untested");
                // let ops: Vec<u32> = self.immediate_eval_expr_known(asm.clone())?;
                // ops
            };
            Ok(FuncImpl::JittedAarch64(ops))
        } else if self.program[f].has_tag(Flag::Llvm) {
            // TODO: an erorr message here gets swollowed because you're probably in the type_of shit. this is really confusing. need to do beter
            // TODO: if its a string literal just take it
            // TODO: check if they tried to give you something from the stack

            let ir = self.eval_str(asm)?;
            self.program.inline_llvm_ir.push(f);
            Ok(FuncImpl::LlvmIr(ir))
        } else if self.program[f].has_tag(Flag::C) {
            let ir = self.eval_str(asm)?;
            Ok(FuncImpl::CSource(ir))
        } else {
            err!("!asm require arch tag",)
        }
    }

    fn eval_str(&mut self, e: &mut FatExpr<'p>) -> Res<'p, Ident<'p>> {
        // this is really annoying cause i can only refer to the (_, _) version of Str but it already has a cast to Str
        let ty = <(*mut u8, i64) as InterpSend>::get_or_create_type(self.program);
        let a = FatExpr::synthetic_ty(Expr::Cast(Box::new(e.clone())), e.loc, ty);
        let ir: (*mut u8, i64) = self.immediate_eval_expr_known(a)?;
        let ir = unsafe { &*slice::from_raw_parts_mut(ir.0, ir.1 as usize) };
        let std::result::Result::Ok(s) = std::str::from_utf8(ir) else {
            err!("wanted utf8 ",)
        };
        Ok(self.program.pool.intern(s))
    }

    fn construct_struct(&mut self, requested: Option<TypeId>, pattern: &mut Pattern<'p>) -> Res<'p, TypeId> {
        let Some(requested) = requested else {
            err!(CErr::NeedsTypeHint("struct literal"))
        };
        let names: Vec<_> = pattern.flatten_names();
        Ok(mut_replace!(*pattern, |mut pattern: Pattern<'p>| {
            let loc = pattern.loc;
            // TODO: why must this suck so bad
            let values: Option<_> = pattern.flatten_defaults_mut();
            let mut values: Vec<_> = unwrap!(values, "use '=' not ':' for struct literals");
            assert_eq!(names.len(), values.len());
            let raw_container_ty = self.program.raw_type(requested);

            let res = match self.program[raw_container_ty].clone() {
                TypeInfo::Struct { fields, .. } => {
                    for (name, value) in names.iter().zip(&mut values) {
                        // TODO: could guess that they did them in order if i cared about not looping twice.
                        let Some(field) = fields.iter().find(|f| f.name == *name) else {
                            err!("Tried to assign unknown field {}", self.program.pool.get(*name));
                        };
                        let value = self.compile_expr(value, Some(field.ty))?;
                        self.last_loc = Some(loc);
                        self.type_check_arg(value, field.ty, "struct field")?;
                    }

                    // If they're missing some, check for default values.
                    if pattern.bindings.len() != fields.len() {
                        for (i, field) in fields.iter().enumerate() {
                            if names.contains(&field.name) {
                                continue;
                            }
                            let BigOption::Some(value) = field.default.clone() else {
                                err!("Missing required field {}", self.program.pool.get(field.name));
                            };

                            let expr = FatExpr::synthetic_ty(Expr::Value { value, coerced: false }, pattern.loc, field.ty);
                            // TODO: HACK. emit_bc expects them in order
                            pattern.bindings.insert(
                                i,
                                Binding {
                                    name: Name::Ident(field.name),
                                    ty: LazyType::Infer,
                                    default: BigOption::Some(expr),
                                    kind: VarType::Var,
                                },
                            );
                        }
                    }

                    debug_assert_eq!(pattern.bindings.len(), fields.len());

                    requested
                }
                TypeInfo::Tagged { cases, .. } => {
                    assert_eq!(
                        1,
                        values.len(),
                        "{} is an enum, value should have one active varient not {values:?}",
                        self.program.log_type(requested)
                    );
                    // TODO: this needs to be a real err! but we're in a mut_replace
                    let i = cases.iter().position(|f| f.0 == names[0]).unwrap_or_else(|| {
                        panic!(
                            "no field {} exists, expected {:?}",
                            self.program.pool.get(names[0]),
                            cases.iter().map(|n| self.program.pool.get(n.0)).collect::<Vec<_>>()
                        )
                    });
                    let type_hint = cases[i].1;
                    let value = self.compile_expr(values[0], Some(type_hint))?;
                    self.type_check_arg(value, type_hint, "enum case")?;
                    requested
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
        name: Var<'p>,
        ty: &mut LazyType<'p>,
        value: &mut FatExpr<'p>,
        kind: VarType,
        _annotations: &[Annotation<'p>],
    ) -> Res<'p, ()> {
        let no_type = matches!(ty, LazyType::Infer);
        self.infer_types_progress(ty)?;
        self.last_loc = Some(value.loc);

        match kind {
            VarType::Const => self.decl_const(name, ty, value)?,
            VarType::Let | VarType::Var => {
                if kind == VarType::Let && value.expr.as_suffix_macro(Flag::Uninitialized).is_some() {
                    let name = self.program.pool.get(name.name);
                    err!("let bindings cannot be reassigned so '{name}' cannot be !uninitialized",)
                }
                let value = self.compile_expr(value, ty.ty())?;
                let final_ty = if no_type {
                    *ty = LazyType::Finished(value);
                    value
                } else {
                    let ty = ty.unwrap();
                    self.program.finish_layout_deep(value)?;
                    self.program.finish_layout_deep(ty)?;
                    self.type_check_arg(value, ty, "var decl")?;
                    ty
                };

                // TODO: this is so emit_ir which can't mutate program can find it.
                self.program.intern_type(TypeInfo::Ptr(final_ty));

                let prev = self.program.pool.get_var_type(name); // TODO: dumb double lookup when hashmap always gives you the old one
                self.program.pool.put_var_type(name, final_ty);
                assert!(prev.is_none() || prev.unwrap() == final_ty);
                // TODO: should always be none?? but its not a constant and seems to always be the same so its probablby not a super huge deal? -- Apr 23
                //       maybe its just cause im not zeroing the stmt and end up compiling multiple times. -- Apr 25
            }
        }
        Ok(())
    }

    fn decl_const(&mut self, name: Var<'p>, ty: &mut LazyType<'p>, value: &mut FatExpr<'p>) -> Res<'p, ()> {
        // TODO: doing the check here every time is sad.
        if value.expr.as_suffix_macro(Flag::Uninitialized).is_some() {
            let name = self.program.pool.get(name.name);
            err!("const bindings cannot be reassigned so '{name}' cannot be '()!uninitialized'",)
        }

        let rec = if let Some(real_val) = value.expr.as_suffix_macro_mut(Flag::Rec) {
            *value = mem::take(real_val);
            if let Some(ty) = ty.ty() {
                assert_eq!(ty, TypeId::ty);
            }
            // TODO: else!
            let hole = TypeId::from_index(self.program.types.len());
            self.program.types.push(TypeInfo::Placeholder);
            let val = to_values(self.program, hole)?;
            self.save_const_values(name, val, TypeId::ty, value.loc)?;
            Some(hole)
        } else {
            None
        };

        // You don't need to precompile, immediate_eval_expr will do it for you.
        // However, we want to update value.ty on our copy to use below to give constant pointers better type inference.
        // This makes addr_of const for @enum work
        //
        // TODO: :PushConstFnCtx
        // let inferred_type = self.type_of(value)?;
        // println!("{inferred_type:?}");
        let res = self.compile_expr(value, ty.ty())?;
        let mut val = self.immediate_eval_expr(value.clone(), res)?;
        // TODO: clean this up. all the vardecl stuff is kinda messy and I need to be able to reuse for the pattern matching version.
        // TODO: more efficient way of handling overload sets
        // TODO: better syntax for inserting a function into an imported overload set.

        let final_ty = if let Some(expected_ty) = ty.ty() {
            expected_ty
        } else {
            let found = value.ty;
            *ty = LazyType::Finished(found);
            found
        };
        self.program.finish_layout_deep(final_ty)?;

        if let Some(rec) = rec {
            assert_eq!(final_ty, TypeId::ty);
            let ty: TypeId = from_values(self.program, val)?;
            self.program.types[rec.as_index()] = TypeInfo::Named(ty, name.name);
            self.program.types_extra.borrow_mut().truncate(rec.as_index()); // just in case. copy-paste
            val = to_values(self.program, rec)?;
        }

        if final_ty == TypeId::ty {
            let value: TypeId = from_values(self.program, val.clone())?;
            extend_options(&mut self.program.inferred_type_names, value.as_index());
            let n = &mut self.program.inferred_type_names[value.as_index()];
            // TODO: often it will just be self.
            //       need to be smarter about only counting public names? and just show generic returns like that as nested.
            if n.is_none() {
                *n = Some(name.name);
            }
        }

        if rec.is_none() {
            self.save_const_values(name, val, final_ty, value.loc)?;
        }

        Ok(())
    }

    pub fn flush_cpu_instruction_cache(&mut self) {
        let (beg, end) = self.aarch64.bump_dirty(); // this also sets the old page executable.

        // x86 doesn't this. TODO: what about riscv?
        #[cfg(target_arch = "aarch64")]
        {
            // This fixes 'illegal hardware instruction'.
            // sleep(Duration::from_millis(1)) also works (in debug mode). That's really cool, it gives it time to update the other cache because instructions and data are seperate?!
            // Especially fun becuase if you run it in lldb so you break on the error and disassemble... you get perfectly valid instructions because it reads the data cache!
            // https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/caches-and-self-modifying-code
            // https://stackoverflow.com/questions/35741814/how-does-builtin-clear-cache-work
            // https://stackoverflow.com/questions/10522043/arm-clear-cache-equivalent-for-ios-devices
            // https://github.com/llvm/llvm-project/blob/main/compiler-rt/lib/builtins/clear_cache.c
            // https://github.com/apple/darwin-libplatform/blob/main/src/cachecontrol/arm64/cache.s
            // https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/sys_icache_invalidate.3.htmls

            if beg != end {
                unsafe { crate::export_ffi::__clear_cache(beg as *mut libc::c_char, end as *mut libc::c_char) }
            }
        }
    }

    #[track_caller]
    fn save_const(&mut self, name: Var<'p>, val_expr: Expr<'p>, final_ty: TypeId, loc: Span) -> Res<'p, ()> {
        if let BigOption::Some((val, ty)) = self.program.pool.get_constant(name) {
            if matches!(ty, LazyType::Finished(_)) {
                ice!(
                    "tried to re-save constant {}.", //  \nOLD: {}\nNEW: {} // FUCK
                    name.log(self.program.pool),
                    // val.log(self.program.pool),
                    // val_expr.log(self.program.pool)
                );
            }
            if !matches!(val.expr, Expr::Poison) {
                ice!("tried to stomp constant {}", name.log(self.program.pool));
            }
            val.expr = val_expr;
            val.ty = final_ty;
            *ty = LazyType::Finished(final_ty);
        } else {
            // I think this just means we renumbered for a specialization.
            let mut e = FatExpr::synthetic_ty(val_expr, loc, final_ty);
            e.done = true;
            self.program.pool.put_constant(name, e, LazyType::Finished(final_ty));
        }
        Ok(())
    }

    #[track_caller]
    fn save_const_values(&mut self, name: Var<'p>, value: Values, final_ty: TypeId, loc: Span) -> Res<'p, ()> {
        self.save_const(name, Expr::Value { value, coerced: false }, final_ty, loc)
    }

    pub fn bit_literal(&self, expr: &FatExpr<'p>) -> Option<(IntTypeInfo, i64)> {
        let Expr::SuffixMacro(name, arg) = &expr.expr else { return None };
        if *name != Flag::From_Bit_Literal.ident() {
            return None;
        }
        let Expr::Tuple(parts) = arg.deref().deref() else { return None };
        let bit_count = parts[0].clone().as_const()?;
        let Ok(bit_count): Res<'p, i64> = from_values(self.program, bit_count) else {
            return None;
        };
        let val = parts[1].clone().as_const()?;
        let Ok(val): Res<'p, i64> = from_values(self.program, val) else {
            return None;
        };

        Some((IntTypeInfo { bit_count, signed: false }, val))
    }

    fn exec_style(&self) -> ExecStyle {
        self.wip_stack.last().unwrap().1
    }
}

#[repr(i64)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecStyle {
    Jit = 0,
    Aot = 1,
}

pub fn add_unique<T: PartialEq>(vec: &mut Vec<T>, new: T) -> bool {
    if !vec.contains(&new) {
        vec.push(new);
        return true;
    }
    false
}

// i like when my code is rocks not rice
// its a lot more challenging to eat but at least you can Find it

pub struct Unquote<'z, 'a, 'p> {
    pub compiler: &'z mut Compile<'a, 'p>,
    pub placeholders: Vec<Option<FatExpr<'p>>>,
}

// :UnquotePlaceholders
impl<'z, 'a, 'p> WalkAst<'p> for Unquote<'z, 'a, 'p> {
    // TODO: track if we're in unquote mode or placeholder mode.
    fn pre_walk_expr(&mut self, expr: &mut FatExpr<'p>) -> bool {
        let Expr::SuffixMacro(name, arg) = &mut expr.expr else {
            return true;
        };
        if *name == Flag::Unquote.ident() {
            let expr_ty = self.compiler.program.fat_expr_type.expect("used unquote ast while bootstrapping");
            // self.compiler
            //     .compile_expr(self.arg, Some(expr_ty))
            //     .unwrap_or_else(|e| panic!("Expected comple ast but \n{e:?}\n{:?}", arg.log(self.compiler.program.pool))); // TODO
            let loc = arg.loc;
            let placeholder = self.compiler.as_literal(self.placeholders.len() as i64, loc).unwrap();
            let mut placeholder = FatExpr::synthetic(Expr::SuffixMacro(Flag::Placeholder.ident(), Box::new(placeholder)), loc);
            placeholder.ty = expr_ty;
            // Note: take <arg> but replace the whole <expr>
            self.placeholders.push(Some(mem::take(arg.deref_mut())));
            *expr = placeholder;
        } else if *name == Flag::Placeholder.ident() {
            let index: i64 = from_values(self.compiler.program, arg.as_const().unwrap()).unwrap();
            let value = self.placeholders[index as usize].take(); // TODO: make it more obvious that its only one use and the slot is empty.
            *expr = value.unwrap();
        } else if *name == Flag::Quote.ident() {
            // TODO: add a simpler test case than the derive thing (which is what discovered this problem).
            // Don't go into nested !quote. This allows having macros expand to other macro calls without stomping eachother.
            // TODO: feels like you might still end up with two going on at once so need to have a monotonic id number for each expansion stored in the !placeholder.
            //       but so far this is good enough.
            return false;
        }
        true
    }
}
