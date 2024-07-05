#![allow(improper_ctypes_definitions)]

use crate::self_hosted::{resolve_root, show_error_line, ParseErr, SelfHosted};
use crate::unwrap2;
use libc::c_void;

use crate::ast::{
    garbage_loc, CallConv, Expr, FatExpr, FatStmt, Flag, FnFlag, FnType, Func, FuncId, IntTypeInfo, LazyType, OverloadSetId, Pattern, Program,
    ScopeId, TargetArch, TypeId, TypeInfo, TypeMeta, WalkAst,
};
use crate::bc::{BakedVar, BakedVarId, FnBody, PrimSig, Values};
use crate::compiler::{Compile, CompileError, ExecStyle, Res, Unquote, EXPECT_ERR_DEPTH};
use crate::emit_bc::prim_sig;
use crate::ffi::InterpSend;
use crate::logging::{unwrap, PoolLog};
use crate::overloading::where_the_fuck_am_i;
use crate::self_hosted::Ident;
use crate::{assert, emit_bc::emit_bc, err, ice, log_err, make_toplevel, signed_truncate};
use std::fmt::{Debug, Write};
use std::mem::{self, transmute};
use std::ops::{FromResidual, Try};
use std::path::PathBuf;
use std::ptr::{addr_of, null, slice_from_raw_parts, slice_from_raw_parts_mut};
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use crate::export_ffi::BigResult::*;

#[repr(C, i64)]
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum BigOption<T> {
    Some(T),
    #[default]
    None = 1,
}

impl<T> BigOption<T> {
    pub(crate) fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            BigOption::Some(t) => Some(t),
            BigOption::None => None,
        }
    }

    pub(crate) fn as_ref(&self) -> Option<&T> {
        match self {
            BigOption::Some(t) => Some(t),
            BigOption::None => None,
        }
    }
    pub(crate) fn unwrap(self) -> T {
        match self {
            BigOption::Some(t) => t,
            BigOption::None => panic!("Unwrapped missing Option."),
        }
    }

    pub(crate) fn is_none(&self) -> bool {
        match self {
            BigOption::Some(_) => false,
            BigOption::None => true,
        }
    }

    pub(crate) fn is_some(&self) -> bool {
        !self.is_none()
    }

    pub(crate) fn unwrap_or(self, other: T) -> T {
        match self {
            BigOption::Some(t) => t,
            BigOption::None => other,
        }
    }
    pub(crate) fn take(&mut self) -> Option<T>
    where
        T: Default,
    {
        match self {
            BigOption::Some(t) => {
                let t = mem::take(t);
                *self = BigOption::None;
                Some(t)
            }
            BigOption::None => None,
        }
    }
}

impl<T> From<BigOption<T>> for Option<T> {
    fn from(value: BigOption<T>) -> Self {
        match value {
            BigOption::Some(t) => Some(t),
            BigOption::None => None,
        }
    }
}

impl<T> From<Option<T>> for BigOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => BigOption::Some(t),
            None => BigOption::None,
        }
    }
}

#[derive(Clone, Debug)]
#[repr(C, i64)]
pub enum BigResult<T, E> {
    Ok(T),
    Err(E),
}
impl<T, E: Debug> BigResult<T, E> {
    #[track_caller]
    pub fn unwrap(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => panic!("Unwrapped error value: {e:?}"),
        }
    }

    pub fn is_err(&self) -> bool {
        matches!(self, Err(_))
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, Ok(_))
    }

    pub fn unwrap_or_else(self, f: impl FnOnce(E) -> T) -> T {
        match self {
            Ok(t) => t,
            Err(e) => f(e),
        }
    }
    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> BigResult<T, E2> {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(f(e)),
        }
    }
    pub fn map_ok<TT>(self, f: impl FnOnce(T) -> TT) -> BigResult<TT, E> {
        match self {
            Ok(t) => Ok(f(t)),
            Err(e) => Err(e),
        }
    }
}

impl<T, E> From<Result<T, E>> for BigResult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Result::Ok(e) => Ok(e),
            Result::Err(e) => Err(e),
        }
    }
}

impl<'p, T: 'p> BigResult<T, ParseErr<'p>> {
    pub(crate) fn unwrap_log(self, pool: &mut SelfHosted<'p>) -> T {
        self.unwrap_or_else(|e| {
            unsafe {
                show_error_line(pool.codemap, e.span.low, e.span.high);
            }
            panic!("{}", e.msg)
        })
    }
}

impl<'p, T: 'p> BigResult<T, CompileError<'p>> {
    pub(crate) fn unwrap_log(self, pool: &mut SelfHosted<'p>) -> T {
        self.unwrap_or_else(|e| {
            if let Some(loc) = e.loc {
                unsafe {
                    show_error_line(pool.codemap, loc.low, loc.high);
                }
            }
            panic!("{e:?}")
        })
    }
}

pub type BigCErr<'p, T> = BigResult<T, Box<CompileError<'p>>>;

impl<T, E> Try for BigResult<T, E> {
    type Output = T;
    type Residual = E;

    fn from_output(output: Self::Output) -> Self {
        BigResult::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            BigResult::Ok(v) => std::ops::ControlFlow::Continue(v),
            BigResult::Err(v) => std::ops::ControlFlow::Break(v),
        }
    }
}

impl<T, E> FromResidual for BigResult<T, E> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        BigResult::Err(residual)
    }
}

#[repr(C)]
pub struct ImportVTable {
    _intern_string: usize,
    _get_string: usize,
    _a: usize,
    init_compiler: unsafe extern "C" fn(comptime_arch: TargetArch) -> *const Compile<'static, 'static>,
    find_unqiue_func: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, name: Ident<'p>) -> BigOption<FuncId>,
    // TODO: i want the meta program to be tracking these instead.
    get_fns_with_tag: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, tag: Ident<'p>) -> *const [FuncId],
    _b: i64, // todo: remove
    compile_func: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> BigCErr<'p, ()>,
    get_jitted_ptr: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId) -> BigCErr<'p, *const u8>,
    get_function: for<'p> unsafe extern "C" fn(c: &mut Compile<'p, '_>, f: FuncId) -> *const Func<'p>,
    _d: usize,
    _add_file: usize,
    _parse_stmts: usize,
    make_and_resolve_and_compile_top_level: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, body: *const [FatStmt<'p>]) -> BigCErr<'p, ()>,
    make_jitted_exec: unsafe extern "C" fn(c: &mut Compile),
    give_vtable: unsafe extern "C" fn(c: &mut Compile, vtable: *const ExportVTable, userdata: *mut ()),
    get_function_name: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId) -> Ident<'p>,
    comptime_arch: unsafe extern "C" fn() -> (i64, i64),
    _c: usize,
    emit_bc: for<'p> extern "C" fn(compile: &mut Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> Res<'p, FnBody<'p>>,
    get_type_meta: extern "C" fn(compile: &Compile, ty: TypeId) -> TypeMeta,
    debug_log_baked_constant: extern "C" fn(compile: &Compile, id: BakedVarId),
    _get_baked: usize,
    debug_log_bc: for<'p> extern "C" fn(c: &Compile<'_, 'p>, body: &FnBody<'p>),
    _e: usize,
    get_compiler_builtins_source: extern "C" fn() -> &'static str,
    get_cranelift_builtins_source: extern "C" fn() -> &'static str,
    _emit_llvm: usize,
    number_of_functions: unsafe extern "C" fn(c: &mut &mut Program) -> i64,
    _bake_var: usize,
    franca_prim_sig2: for<'p> extern "C" fn(c: &Compile<'_, 'p>, func: &Func<'p>) -> Res<'p, PrimSig<'p>>,
    clone_expr: for<'p> extern "C" fn(e: &FatExpr<'p>) -> FatExpr<'p>,
    clone_type: for<'p> extern "C" fn(e: &LazyType<'p>) -> LazyType<'p>,
    intern_type: for<'p> extern "C" fn(compile: &mut Compile<'_, 'p>, e: TypeInfo<'p>) -> TypeId,
    get_type: for<'p> extern "C" fn(compile: &mut Compile<'_, 'p>, e: TypeId) -> *const TypeInfo<'p>,
    log_type: extern "C" fn(compile: &mut Compile, e: TypeId) -> *const str,
}

#[repr(C)]
#[derive(Clone, Default)]
pub struct ExportVTable {
    pub resolve_comptime_import:
        BigOption<unsafe extern "C" fn(userdata: *mut (), c: &mut Compile, f: FuncId, lib_name: Ident, fn_name: Ident) -> BigOption<*const u8>>,
}

pub static IMPORT_VTABLE: ImportVTable = ImportVTable {
    _intern_string: 1,
    _get_string: 1,
    _a: 0,
    init_compiler: franca_init_compiler,
    find_unqiue_func: franca_find_unique_fn,
    get_fns_with_tag,
    _b: 0,
    compile_func: franca_compile_func,
    get_jitted_ptr,
    get_function: franca_get_function,
    _d: 0,
    _add_file: 1,
    _parse_stmts: 1,
    make_and_resolve_and_compile_top_level,
    make_jitted_exec,
    give_vtable,
    get_function_name,
    comptime_arch,
    _c: 0,
    emit_bc,
    get_type_meta,
    debug_log_baked_constant,
    _get_baked: 1,
    debug_log_bc,
    _e: 0,
    get_compiler_builtins_source,
    get_cranelift_builtins_source,
    _emit_llvm: 1,
    number_of_functions,
    _bake_var: 1,
    franca_prim_sig2,
    clone_expr,
    clone_type,
    intern_type: franca_intern_type,
    get_type,
    log_type: franca_log_type,
};

extern "C" fn franca_log_type(compile: &mut Compile, e: TypeId) -> *const str {
    compile.program.log_type(e).leak() as *const str
}

extern "C" fn get_type<'p>(compile: &mut Compile<'_, 'p>, e: TypeId) -> *const TypeInfo<'p> {
    &compile.program[e] as *const TypeInfo<'p>
}

extern "C" fn franca_intern_type<'p>(comp: &mut Compile<'_, 'p>, ty: TypeInfo<'p>) -> TypeId {
    comp.program.intern_type(ty)
}

extern "C" fn clone_expr<'p>(e: &FatExpr<'p>) -> FatExpr<'p> {
    e.clone()
}

extern "C" fn clone_type<'p>(e: &LazyType<'p>) -> LazyType<'p> {
    e.clone()
}

extern "C" fn franca_prim_sig2<'p>(c: &Compile<'_, 'p>, func: &Func<'p>) -> Res<'p, PrimSig<'p>> {
    let Some(ty) = func.finished_ty() else { err!("fn ty not ready",) };
    let cc = unwrap2!(func.cc, "cc not ready");
    prim_sig(c.program, ty, cc)
}

extern "C" fn debug_log_bc<'p>(c: &Compile<'_, 'p>, body: &FnBody<'p>) {
    println!("{}", body.log(c.program))
}

extern "C" fn debug_log_baked_constant(compile: &Compile, id: BakedVarId) {
    unsafe { println!("{:?}", (*crate::self_hosted::get_baked(compile.program.pool, id)).1) }
}

extern "C" fn get_type_meta(compile: &Compile, ty: TypeId) -> TypeMeta {
    compile.program.get_info(ty)
}

unsafe extern "C" fn comptime_arch() -> (i64, i64) {
    let arch = if cfg!(target_arch = "aarch64") {
        1
    } else if cfg!(target_arch = "x86_64") {
        2
    } else {
        -1
    };
    let os = if cfg!(target_os = "macos") {
        0
    } else if cfg!(target_os = "linux") {
        1
    } else if cfg!(target_os = "windows") {
        2
    } else {
        -1
    };
    (arch, os)
}

unsafe extern "C" fn franca_init_compiler(comptime_arch: TargetArch) -> *const Compile<'static, 'static> {
    if cfg!(not(target_arch = "aarch64")) && comptime_arch == TargetArch::Aarch64 {
        panic!("aarch64 jit is not supported on this architecture"); // TODO: return error instead.
    }
    if cfg!(not(feature = "cranelift")) && comptime_arch == TargetArch::Cranelift {
        panic!("cranelift is not supported by this build of the compiler"); // TODO: return error instead.
    }

    let program = Box::leak(Box::new(Program::new(comptime_arch)));
    let compiler = Box::leak(Box::new(Compile::new(program)));
    compiler as *const Compile
}
unsafe extern "C" fn franca_find_unique_fn<'p>(c: &mut Compile<'_, 'p>, name: Ident<'p>) -> BigOption<FuncId> {
    match c.program.find_unique_func(name) {
        Some(f) => BigOption::Some(f),
        None => BigOption::None,
    }
}
unsafe extern "C" fn get_fns_with_tag(c: &mut Compile, tag: Ident) -> *const [FuncId] {
    let mut found = vec![];
    for (fid, func) in c.program.funcs.iter().enumerate() {
        if func.get_flag(FnFlag::NotEvilUninit) && func.annotations.iter().any(|a| a.name == tag) {
            found.push(FuncId::from_index(fid));
        }
    }
    println!("scanned {} fns", c.program.funcs.len());

    found.leak() as *const [FuncId]
}

unsafe extern "C" fn franca_compile_func<'p>(c: &mut Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> BigCErr<'p, ()> {
    let res = c.compile(f, when);
    if let Err(e) = &res {
        log_err(c, *e.clone());
    }

    res
}

unsafe extern "C" fn get_jitted_ptr<'p>(c: &mut Compile<'_, 'p>, f: FuncId) -> BigCErr<'p, *const u8> {
    c.flush_callees(f)?;
    Ok(unwrap!(c.aarch64.get_fn(f), "not compiled {f:?}"))
}

#[no_mangle]
unsafe extern "C" fn franca_get_function<'p>(c: &mut Compile<'p, '_>, f: FuncId) -> *const Func<'p> {
    &c.program[f] as *const Func
}

unsafe extern "C" fn make_and_resolve_and_compile_top_level<'p>(c: &mut Compile<'_, 'p>, body: *const [FatStmt<'p>]) -> BigCErr<'p, ()> {
    let body = (*body).to_vec();
    let mut global = make_toplevel(c.program.pool, garbage_loc(), body);
    unsafe {
        resolve_root(c.program.pool, &mut global, ScopeId::from_index(0)).unwrap();
    }
    if let Err(e) = c.compile_top_level(global) {
        c.program.pool.print_diagnostic(*e.clone());
        return Err(e);
    }
    Ok(())
}

unsafe extern "C" fn make_jitted_exec(c: &mut Compile) {
    c.flush_cpu_instruction_cache();
}

unsafe extern "C" fn give_vtable(c: &mut Compile, vtable: *const ExportVTable, userdata: *mut ()) {
    c.driver_vtable = (Box::new((*vtable).clone()), userdata)
}

unsafe extern "C" fn get_function_name<'p>(c: &mut Compile<'_, 'p>, f: FuncId) -> Ident<'p> {
    c.program[f].name
}

// TODO: do this myself
// x86 doesn't need this and x86_64-unknown-linux-musl doesn't give me a fake one to link against
#[cfg(target_arch = "aarch64")]
extern "C" {
    pub fn __clear_cache(beg: *mut libc::c_char, end: *mut libc::c_char);
}

// pub fn __clear_cache(beg: *mut libc::c_char, end: *mut libc::c_char);
// IMPORTANT: since compile is repr(C), &mut &mut Program === &mut Compile
pub const COMPILER: &[(&str, *const u8)] = &[
    ("#fold fn tag_value(E: Type, case_name: Symbol) i64", tag_value as *const u8),
    ("#fold fn tag_symbol(E: Type, tag_value: i64) Symbol", tag_symbol as *const u8),
    ("fn number_of_functions() i64", number_of_functions as *const u8),
    // TODO: make FuncId a unique type
    ("fn name(func_id: FuncId) Symbol", function_name as *const u8),
    ("fn index_to_func_id(func_index: i64) FuncId", index_to_func_id as *const u8),
    // TODO: all these type ones could use ffi TypeInfo if i gave it `#ct fn intern_type`
    //       but to do that here instead of current macro message, I'd need to do ffi of enums in a less insane way.
    //       (these functions don't use InterpSend, they just rely on C ABI).
    // Ideally this would just work with tuple syntax but L((a, b), c) === L(a, b, c) !=== L(Ty(a, b), c) because of arg flattening.
    ("fn Ty(fst: Type, snd: Type) Type #fold;", pair_type as *const u8),
    ("fn Ty(fst: Type, snd: Type, trd: Type) Type #fold;", triple_type as *const u8),
    ("fn Ty(fst: Type, snd: Type, trd: Type, frt: Type) Type #fold;", quad_type as *const u8),
    // The type of 'fun(Arg) Ret'. This is a comptime only value.
    // All calls are inlined, as are calls that pass one of these as an argument.
    // Captures of runtime variables are allowed since you just inline everything anyway.
    // Const captures behave as you'd expect from first class closures.
    ("fn Fn(Arg: Type, Ret: Type) Type #fold;", fn_type as *const u8),
    // TODO: include calling convention.
    // Like fun(Arg, Ret) but as a runtime value. Same as a function pointer in c but with less insane syntax :).
    // Use '!addr' on a normal fun value to get create a value of this type.
    // - The function cannot have any runtime variable captures,
    //   but they could be implemented on top of this by taking an environment data pointer as an argument.
    // - The function cannot have any const arguments, they must be baked before creating the pointer.
    ("fn FnPtr(Arg: Type, Ret: Type) Type #fold;", fn_ptr_type as *const u8),
    ("fn FnPtr(Arg: Type, Ret: Type, cc: CallConv) Type #fold;", fn_ptr_type_conv as *const u8),
    // This a null terminated packed string, useful for ffi with old c functions.
    // Currently it doesn't reallocate because all symbols are null terminated but that might change in future. --Apr, 10
    ("#fold fn c_str(s: Symbol) CStr", symbol_to_cstr as *const u8),
    (
        "fn resolve_backtrace_symbol(addr: *i64, out: *RsResolvedSymbol) bool",
        resolve_backtrace_symbol as *const u8,
    ),
    ("fn debug_log_type(ty: Type) void", log_type as *const u8),
    ("fn IntType(bits: i64, signed: bool) Type #fold;", make_int_type as *const u8),
    // measured in bytes
    ("fn size_of(T: Type) i64 #fold", get_size_of as *const u8),
    // ("fn Label(Arg: Type) Type", do_label_type as *const u8),
    // Generated for @BITS to bootstrap encoding for inline asm.
    ("#no_tail fn __shift_or_slice(ints: Slice(i64)) u32", shift_or_slice as *const u8),
    ("fn __save_slice_t(slice_t: Fn(Type, Type)) void", save_slice_t as *const u8),
    ("fn intern_type_ref(ty: *TypeInfo) Type;", intern_type as *const u8),
    // TODO: maybe it would be nice if you could override deref so Type acts like a *TypeInfo.
    ("fn get_type_info(ty: Type) TypeInfo;", get_type_info as *const u8),
    ("fn get_type_info_raw(ty: Type) TypeInfo;", get_type_info_raw as *const u8),
    (
        "fn const_eval(expr: FatExpr, ty: Type, result: rawptr) void;",
        const_eval_any as *const u8,
    ),
    // Calls Compiler::compile_expr
    // Infers the type and avoids some redundant work if you duplicate the ast node in a bunch of places after calling this.
    ("fn compile_ast(value: FatExpr) FatExpr;", compile_ast as *const u8),
    ("fn debug_log_ast(expr: FatExpr) void;", log_ast as *const u8),
    (
        "fn unquote_macro_apply_placeholders(expr: Slice(FatExpr)) FatExpr;",
        unquote_macro_apply_placeholders as *const u8,
    ),
    ("fn get_type_int(e: FatExpr) IntTypeInfo;", get_type_int as *const u8),
    // Convert a pointer to a value into an ast that will produce that value when evaluated.
    // It is illegal to pass a <ty> that does not match the value behind <ptr>.
    ("fn literal_ast(ty: Type, ptr: rawptr) FatExpr;", literal_ast as *const u8),
    ("fn can_assign_types(found: Type, expected: Type) bool;", type_check_arg as *const u8),
    (
        "#macro #outputs(Type) fn enum(Raw: FatExpr, Cases: FatExpr) FatExpr;",
        enum_macro as *const u8,
    ),
    ("#macro fn namespace(block: FatExpr) FatExpr;", namespace_macro as *const u8),
    ("#macro #outputs(Type) fn tagged(cases: FatExpr) FatExpr;", tagged_macro as *const u8),
    ("#macro #outputs(Type) fn struct(fields: FatExpr) FatExpr;", struct_macro as *const u8),
    ("#macro #outputs(Symbol) fn symbol(fields: FatExpr) FatExpr;", symbol_macro as *const u8),
    (
        "#macro #outputs(void) fn assert_compile_error(fields: FatExpr) FatExpr;",
        assert_compile_error_macro as *const u8,
    ),
    ("#macro #outputs(Type) fn type(e: FatExpr) FatExpr;", type_macro as *const u8),
    ("#macro #outputs(u32) fn BITS(parts: FatExpr) FatExpr;", bits_macro as *const u8),
    ("fn __get_comptime_dispatch_ptr() **i64", get_dispatch_ptr as *const u8),
    ("#macro fn as(T: FatExpr, e: FatExpr) FatExpr;", as_macro as *const u8),
    ("#macro fn Fn(Arg: FatExpr, Ret: FatExpr) FatExpr;", fn_type_macro as *const u8),
    ("#macro fn FnPtr(Arg: FatExpr, Ret: FatExpr) FatExpr;", fn_ptr_type_macro as *const u8),
    ("#macro fn Fn(Ret: FatExpr) FatExpr;", fn_type_macro_single as *const u8),
    ("#macro fn FnPtr( Ret: FatExpr) FatExpr;", fn_ptr_type_macro_single as *const u8),
    ("fn __compiler_save_fatexpr_type(t: Type) void;", compiler_save_fatexpr_type as *const u8),
    // ("#fold fn str(s: Symbol) Str", symbol_to_str as *const u8),
    ("fn get_compiler_vtable() *ImportVTable", get_compiler_vtable as *const u8),
    ("fn bake_value(v: BakedVar) BakedVarId", bake_value as *const u8),
    ("fn __save_bake_os(os: OverloadSet) void", save_bake_os as *const u8),
    ("fn get_meta(t: Type) TypeMeta", get_meta as *const u8),
    // TODO: this should be a function, but then that needs a different bootstrapping path.
    ("#macro fn builtin(t: FatExpr) FatExpr", Compile::get_builtin_macro as *const u8),
    (
        "fn __save_function_header(push: Fn(u32, void), pop: Fn(void, void)) void;",
        Compile::save_function_header as *const u8,
    ),
    ("fn has_env_bootstrapped_yet() bool;", has_env_bootstrapped_yet as *const u8), // TODO: remove
    ("fn Tag(Tagged: Type) Type #fold;", get_enum_tag_type as *const u8),
    // :blessed: it looks for @rec by name!! TODO: this is very bad and confusing!! you can't shadow it!! - Jul 1
];

extern "C-unwind" fn has_env_bootstrapped_yet(comp: &mut Compile) -> bool {
    comp.make_slice_t.is_some() && comp.program.fat_expr_type.is_some()
}

extern "C-unwind" fn get_enum_tag_type(comp: &mut Compile, e: TypeId) -> TypeId {
    let raw = comp.program.raw_type(e);
    let TypeInfo::Tagged { tag, .. } = &comp.program[raw] else {
        panic!("Expected @tagged found {}", comp.program.log_type(e))
    };
    *tag
}

// TODO: make an easy way to write these from the language.
//       where you translate the versions that require *ImportVTable.
// TODO: write size_of in terms of this.
extern "C-unwind" fn get_meta(comp: &mut Compile, ty: TypeId) -> TypeMeta {
    comp.program.get_info(ty)
}

extern "C-unwind" fn save_bake_os(comp: &mut Compile, os: OverloadSetId) {
    comp.program.bake_os = Some(os);
}

// TODO: version of this that allows caching? but have to think more about when you're allowed to deduplicate.
extern "C-unwind" fn bake_value(comp: &mut Compile, v: BakedVar) -> BakedVarId {
    unsafe { crate::self_hosted::put_baked(comp.program.pool, v, BigOption::None) }
}

extern "C-unwind" fn get_compiler_vtable(_: &mut Compile) -> *const ImportVTable {
    addr_of!(IMPORT_VTABLE)
}

extern "C-unwind" fn compiler_save_fatexpr_type(compiler: &mut Compile, f: TypeId) {
    compiler.program.fat_expr_type = Some(f);
}
extern "C-unwind" fn save_slice_t(compiler: &mut Compile, f: FuncId) {
    compiler.make_slice_t = Some(f);
}

extern "C-unwind" fn get_size_of(compiler: &mut Compile, ty: TypeId) -> i64 {
    hope(|| compiler.program.finish_layout(ty));
    compiler.program.get_info(ty).stride_bytes as i64
}

pub static STDLIB_PATH: Mutex<Option<PathBuf>> = Mutex::new(None);

const MSG: &str = "//! IMPORTANT: don't try to save #comptime_addr('ASLR junk'), it won't go well. \n";

extern "C" fn get_compiler_builtins_source() -> &'static str {
    let mut out = String::new();
    // writeln!(out, "{}", include_str!("../compiler/driver_api.fr")).unwrap();
    writeln!(out, "{}", MSG).unwrap();
    for (sig, ptr) in COMPILER {
        writeln!(out, "#comptime_addr({}) #ct #c_call {sig};", *ptr as usize).unwrap();
    }
    out.leak() // TODO
}

extern "C" fn get_cranelift_builtins_source() -> &'static str {
    let mut out = String::new();
    writeln!(out, "{}", MSG).unwrap();
    #[cfg(feature = "cranelift")]
    for (sig, ptr) in crate::cranelift::BUILTINS {
        writeln!(out, "#cranelift_emit({}) #c_call {sig};", *ptr as usize).unwrap();
    }
    out.leak() // TODO
}

// TODO: can do some nicer reporting here? maybe this goes away once i can actually do error handling in my language.
fn hope<'p, T>(res: impl FnOnce() -> Res<'p, T>) -> T {
    res().unwrap_or_else(|e| panic!("{e:?}"))
}

fn hopec<'p, T>(comp: &Compile<'_, 'p>, res: impl FnOnce() -> Res<'p, T>) -> T {
    comp.tag_err(res()).unwrap_or_else(|e| {
        log_err(comp, *e);
        panic!("compile error in ffi")
    })
}

extern "C-unwind" fn tag_value<'p>(comp: &mut Compile<'_, 'p>, enum_ty: TypeId, name: Ident<'p>) -> i64 {
    let cases = hope(|| {
        Ok(unwrap!(
            comp.program.get_enum(enum_ty),
            "{} is not enum. (tried tag_value of {})",
            comp.program.log_type(enum_ty),
            comp.program.pool.get(name)
        ))
    });
    let index = cases.iter().position(|f| f.0 == name);
    let index = hope(|| Ok(unwrap!(index, "bad case name id={} {}", name.0, comp.program.pool.get(name))));
    index as i64
}

extern "C-unwind" fn tag_symbol<'p>(program: &&Program<'p>, enum_ty: TypeId, tag_val: i64) -> Ident<'p> {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let case = hope(|| Ok(unwrap!(cases.get(tag_val as usize), "enum tag too high")));
    case.0
}

extern "C-unwind" fn pair_type(program: &mut &mut Program, a: TypeId, b: TypeId) -> TypeId {
    hope(|| {
        assert!(a.as_index() < program.types.len(), "TypeId OOB {:?}", a);
        assert!(b.as_index() < program.types.len(), "TypeId OOB {:?}", b);
        Ok(program.tuple_of(vec![a, b]))
    })
}

extern "C-unwind" fn triple_type(program: &mut &mut Program, a: TypeId, b: TypeId, c: TypeId) -> TypeId {
    hope(|| {
        assert!(a.as_index() < program.types.len(), "TypeId OOB {:?}", a);
        assert!(b.as_index() < program.types.len(), "TypeId OOB {:?}", b);
        assert!(c.as_index() < program.types.len(), "TypeId OOB {:?}", c);
        Ok(program.tuple_of(vec![a, b, c]))
    })
}
extern "C-unwind" fn quad_type(program: &mut &mut Program, a: TypeId, b: TypeId, c: TypeId, d: TypeId) -> TypeId {
    hope(|| Ok(program.tuple_of(vec![a, b, c, d])))
}
extern "C-unwind" fn fn_type(program: &mut &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);
        assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
        let arity = program.tuple_types(arg).map(|t| t.len()).unwrap_or_else(|| 1) as u16;
        Ok(program.intern_type(TypeInfo::Fn(FnType { arg, ret, arity })))
    })
}

extern "C-unwind" fn fn_ptr_type_conv(program: &mut &mut Program, arg: TypeId, ret: TypeId, cc: CallConv) -> TypeId {
    hope(|| {
        assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);
        assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
        let arity = program.tuple_types(arg).map(|t| t.len()).unwrap_or_else(|| 1) as u16;
        Ok(program.intern_type(TypeInfo::FnPtr {
            ty: FnType { arg, ret, arity },
            cc,
        }))
    })
}

extern "C-unwind" fn fn_ptr_type(program: &mut &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);
        assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
        program.finish_layout(arg)?;
        program.finish_layout(ret)?;
        let cc = CallConv::CCallReg;
        let arity = program.tuple_types(arg).map(|t| t.len()).unwrap_or_else(|| 1) as u16;
        Ok(program.intern_type(TypeInfo::FnPtr {
            ty: FnType { arg, ret, arity },
            cc,
        }))
    })
}

// Note: currently StringPool guarentees that they're all null terminated but I don't want to promise that to the language so wrap in this function.
extern "C-unwind" fn symbol_to_cstr<'p>(program: &mut &mut Program<'p>, symbol: i64) -> *const u8 {
    let symbol: Ident<'p> = unsafe { transmute(symbol as u32) };
    program.pool.get_c_str(symbol)
}

/// This must be kept in sync with the definition in unwind.fr!
pub struct RsResolvedSymbol {
    line: i64,
    owned_name: *const u8,
    name_len: i64,
}

extern "C-unwind" fn resolve_backtrace_symbol(_: &mut &mut Program, addr: *mut c_void, out: &mut RsResolvedSymbol) -> i64 {
    let mut success = 0;
    #[cfg(feature = "backtracers")]
    backtrace::resolve(addr, |symbol| {
        out.line = symbol.lineno().map(|v| v as i64).unwrap_or(-1);
        // out.col = symbol.colno().map(|v| v as i64).unwrap_or(-1);
        if let Some(name) = symbol.name() {
            let mut bytes = name.to_string().into_bytes();
            if bytes.is_empty() {
                out.owned_name = null();
                out.name_len = 0;
            } else {
                bytes.shrink_to_fit();
                out.name_len = bytes.len() as i64;
                out.owned_name = Box::leak(bytes.into_boxed_slice()).as_ptr()
            }
        } else {
            out.owned_name = null();
            out.name_len = 0;
        }
        success = 1;
    });
    success
}

extern "C-unwind" fn log_type(p: &mut &mut Program, a: TypeId) {
    println!("{a:?} = {}", p.log_type(a));
}

extern "C-unwind" fn log_ast<'p>(p: &mut Compile<'_, 'p>, a: FatExpr<'p>) {
    println!("{}", a.log(p.program.pool));
    where_the_fuck_am_i(p, a.loc);
}

extern "C" fn number_of_functions(program: &mut &mut Program) -> i64 {
    program.funcs.len() as i64
}

extern "C-unwind" fn function_name<'p>(program: &mut &mut Program<'p>, f: FuncId) -> Ident<'p> {
    program[f].name
}

extern "C-unwind" fn index_to_func_id(_: &mut &mut Program, f: usize) -> FuncId {
    FuncId::from_index(f)
}

extern "C-unwind" fn make_int_type(program: &mut &mut Program<'_>, bit_count: i64, signed: bool) -> TypeId {
    program.intern_type(TypeInfo::Int(crate::ast::IntTypeInfo { bit_count, signed }))
}

fn return_from_ffi(compile: &mut Compile) {
    // Since we just called into a compiler function, it might have emitted new code and left it in write mode...
    // but our caller might be jitted code (like a macro), so before returning to them, we need to make sure they're executable.
    // this avoids a `exc_bad_access (code=2, <some code address>)`. The problem only happened in the lox demo, not my small tests, so its fairly rare.
    // -- May 30
    // TODO: its wasteful to leave the map in exec mode if we're going to be writing to it again soon.
    compile.flush_cpu_instruction_cache();
}

extern "C-unwind" fn as_macro<'p>(compile: &mut Compile<'_, 'p>, arg: FatExpr<'p>, target: FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(arg.loc);
    let res = compile.as_cast_macro(arg, target);
    hopec(compile, || res)
}

extern "C-unwind" fn enum_macro<'p>(compile: &mut Compile<'_, 'p>, arg: FatExpr<'p>, target: FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(arg.loc);
    let res = compile.enum_constant_macro(arg, target);
    return_from_ffi(compile);
    res.unwrap()
}

extern "C-unwind" fn intern_type<'p>(compile: &mut Compile<'_, 'p>, ty: &TypeInfo<'p>) -> TypeId {
    compile.program.intern_type(ty.clone())
}

extern "C-unwind" fn get_type_info<'p>(compile: &Compile<'_, 'p>, ty: TypeId) -> TypeInfo<'p> {
    compile.program[ty].clone()
}
extern "C-unwind" fn get_type_info_raw<'p>(compile: &Compile<'_, 'p>, ty: TypeId) -> TypeInfo<'p> {
    let ty = compile.program.raw_type(ty);
    compile.program[ty].clone()
}

// TODO: at least use hope(|| ...) instead of unwrapping so much.
//       need to think of better error handling story.

extern "C-unwind" fn const_eval_any<'p>(compile: &mut Compile<'_, 'p>, mut expr: FatExpr<'p>, ty: TypeId, addr: usize) {
    // TODO: immediate_eval_expr doesn't do a type check. -- Apr 27
    compile.compile_expr(&mut expr, Some(ty)).unwrap();

    match compile.immediate_eval_expr(expr, ty) {
        Ok(val) => {
            let bytes = val.bytes();
            debug_assert_eq!(bytes.len(), compile.program.get_info(ty).stride_bytes as usize);
            let out = unsafe { &mut *slice_from_raw_parts_mut(addr as *mut u8, bytes.len()) };
            return_from_ffi(compile);
            out.copy_from_slice(bytes);
        }
        Err(e) => panic!("{e:?}"),
    }
}

extern "C-unwind" fn compile_ast<'p>(compile: &mut Compile<'_, 'p>, mut expr: FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(expr.loc);
    let res = compile.compile_expr(&mut expr, None);
    hopec(compile, || res);
    return_from_ffi(compile);
    expr
}

// :UnquotePlaceholders
extern "C-unwind" fn unquote_macro_apply_placeholders<'p>(compile: &mut Compile<'_, 'p>, args: *mut [FatExpr<'p>]) -> FatExpr<'p> {
    // println!(
    //     "ptr={} len={} ptr%align={}",
    //     args.as_mut_ptr() as usize,
    //     args.len(),
    //     args.as_mut_ptr() as usize % mem::align_of::<FatExpr>()
    // );
    // for a in unsafe { &mut *args } {
    //     // println!("{}", unsafe { &*a }.log(compile.program.pool));
    //     // unsafe { show_error_line(compile.program.pool.codemap, a.loc.low, a.loc.high) }
    // }

    let mut args = unsafe { &*args }.to_vec();
    let doo = || {
        let mut template = unwrap!(args.pop(), "template arg");
        let mut walk = Unquote {
            compiler: compile,
            placeholders: args.into_iter().map(Some).collect(),
        };
        // TODO: rename to handle or idk so its harder to accidently call the walk one directly which is wrong but sounds like it should be right.
        walk.expr(&mut template);
        let placeholders = walk.placeholders;
        assert!(placeholders.iter().all(|a| a.is_none()), "didnt use all arguments");
        compile.program.pool.renumber_expr(&mut template, BigOption::None);

        Ok(template)
    }; // topdpdwkp[aspefiwfe]e

    let res = doo();
    compile.tag_err(res).unwrap_or_else(|e| panic!("{e:?}"))
}

extern "C-unwind" fn get_type_int<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> IntTypeInfo {
    hope(|| {
        match &mut arg.expr {
            Expr::Call(_, _) => {
                if let Some((int, _)) = compile.bit_literal(&arg) {
                    return Ok(int);
                }
            }
            Expr::Value { .. } => err!("todo",),
            _ => {
                let ty = compile.compile_expr(&mut arg, None)?;
                let ty = compile.program.raw_type(ty);
                if let TypeInfo::Int(int) = compile.program[ty] {
                    return_from_ffi(compile);
                    return Ok(int);
                }
                err!("expected expr of int type not {}", compile.program.log_type(ty));
            }
        }
        err!("expected binary literal not {arg:?}",);
    })
}

extern "C-unwind" fn literal_ast<'p>(compile: &Compile<'_, 'p>, ty: TypeId, ptr: usize) -> FatExpr<'p> {
    let bytes = compile.program.get_info(ty).stride_bytes as usize;
    let value = unsafe { &*slice_from_raw_parts(ptr as *const u8, bytes) };
    let value = Values::many(value.to_vec());
    // TODO: zero_padding
    let loc = compile.last_loc.unwrap_or_else(garbage_loc); // TODO: caller should pass it in?
    FatExpr::synthetic_ty(Expr::Value { value, coerced: false }, loc, ty)
}

extern "C-unwind" fn type_check_arg(compile: &Compile, found: TypeId, expected: TypeId) -> bool {
    compile.type_check_arg(found, expected, "").is_ok()
}

// TODO: what if struct const fields were lazy like normal values and then could use that instead of this.
extern "C-unwind" fn namespace_macro<'p>(compile: &mut Compile<'_, 'p>, mut block: FatExpr<'p>) -> FatExpr<'p> {
    let loc = block.loc;
    // give any other macros a chance to expand.
    let ty = compile.program.intern_type(TypeInfo::Fn(FnType {
        arg: TypeId::unit,
        ret: TypeId::unit,
        arity: 1,
    }));
    let found_ty = compile.compile_expr(&mut block, Some(ty)).unwrap();
    let id = block.as_const().unwrap().unwrap_func_id();
    debug_assert_eq!(ty, found_ty);
    compile.compile(id, ExecStyle::Jit).unwrap();
    let func = &mut compile.program[id];
    let s = func.scope.unwrap();

    return_from_ffi(compile);
    compile.as_literal(s, loc).unwrap()
}

pub extern "C-unwind" fn tagged_macro<'p>(compile: &mut Compile<'_, 'p>, mut cases: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        if let Expr::StructLiteralP(pattern) = &mut cases.expr {
            let mut tag_fields = vec![];
            for (i, b) in pattern.bindings.iter_mut().enumerate() {
                if b.default.is_none() && matches!(b.ty, LazyType::Infer) {
                    // @tagged(s: i64, n) is valid and infers n as void.
                    b.ty = LazyType::Finished(TypeId::unit);
                }
                tag_fields.push((b.name().unwrap(), Values::one(i as i64))); // :tag_enums_are_sequential
            }
            let ty = compile.struct_type(pattern)?;
            let TypeInfo::Struct { fields, .. } = ty else { unreachable!() };

            let tag = TypeInfo::Enum {
                raw: TypeId::i64(),
                fields: tag_fields,
                sequentual: true,
            };
            let tag = compile.program.intern_type(tag);

            let ty = TypeInfo::Tagged {
                cases: fields.into_iter().map(|f| (f.name, f.ty)).collect(),
                tag,
            };
            let ty = compile.program.intern_type(ty);

            compile.set_literal(&mut cases, ty)?;
        } else {
            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", cases);
        }
        Ok(())
    });
    return_from_ffi(compile);

    cases
}

pub extern "C-unwind" fn struct_macro<'p>(compile: &mut Compile<'_, 'p>, mut fields: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        if let Expr::StructLiteralP(pattern) = &mut fields.expr {
            let ty = compile.struct_type(pattern)?;
            let ty = compile.program.intern_type(ty);
            compile.set_literal(&mut fields, ty)?;
        } else {
            err!("expected map literal: (name: Type, ... ) but found {:?}", fields);
        }
        Ok(())
    });
    return_from_ffi(compile);

    fields
}

extern "C-unwind" fn symbol_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        // TODO: use match
        let value = match &mut arg.expr {
            &mut Expr::GetNamed(i) => i,
            &mut Expr::GetVar(v) => v.name,
            &mut Expr::String(i) => i,
            Expr::PrefixMacro { target, .. } => {
                // TODO: check that handler is @as
                if let Expr::String(i) = target.expr {
                    i
                } else {
                    // ice!("Expected identifier found {arg:?}")
                    todo!()
                }
            }
            _ => ice!("Expected identifier found {arg:?}"),
        };
        compile.set_literal(&mut arg, value)?;
        Ok(())
    });
    return_from_ffi(compile);

    arg
}

extern "C-unwind" fn assert_compile_error_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        // TODO: this can still have side-effects on the compiler state tho :(
        let before = compile.debug_trace.len();
        unsafe {
            EXPECT_ERR_DEPTH.fetch_add(1, Ordering::SeqCst);
        }
        let res = compile.compile_expr(&mut arg, None);
        unsafe {
            EXPECT_ERR_DEPTH.fetch_sub(1, Ordering::SeqCst);
        }
        assert!(res.is_err());
        while compile.debug_trace.len() > before {
            compile.debug_trace.pop();
        }

        compile.set_literal(&mut arg, ())?;
        Ok(())
    });
    return_from_ffi(compile);

    arg
}

pub extern "C-unwind" fn type_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        // Note: this does not evaluate the expression.
        // TODO: warning if it has side effects. especially if it does const stuff.
        let ty = compile.compile_expr(&mut arg, None)?;
        compile.set_literal(&mut arg, ty)?;
        Ok(())
    });
    return_from_ffi(compile);

    arg
}

extern "C-unwind" fn bits_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        let Expr::Tuple(parts) = arg.expr else {
            err!("Expected @Bits(Tuple...)",)
        };
        let shift_or_slice = compile.program.find_unique_func(Flag::__Shift_Or_Slice.ident()).unwrap();
        compile.compile(shift_or_slice, ExecStyle::Jit)?;

        let mut new_args = Vec::with_capacity(parts.len() * 3);
        let loc = arg.loc;
        let mut shift = 32;
        for mut int in parts {
            let mut ty = get_type_int(compile, int.clone()); // TODO: redundant work cause of clone

            shift -= ty.bit_count;
            assert!(shift >= 0, "expected 32 bits. TODO: other sizes.");
            if let Some((_, v)) = compile.bit_literal(&int) {
                // Not signed_truncate-ing here because shift_or_slice does it.
                assert!(v < 1 << ty.bit_count, "{v} {}", ty.bit_count);
                int = compile.as_literal(v, loc)?;
            }
            int = FatExpr::synthetic_ty(Expr::Cast(Box::new(mem::take(&mut int))), loc, TypeId::i64());
            new_args.push(int);
            let mut sh = compile.as_literal(shift, loc)?;
            sh.ty = TypeId::i64();
            new_args.push(sh);
            // HACK
            if ty.signed {
                ty.bit_count *= -1;
            }
            let mut size = compile.as_literal(ty.bit_count, loc)?;
            size.ty = TypeId::i64();
            new_args.push(size);
        }

        if shift != 0 {
            err!("shift != 0; expected 32 bits. TODO: other sizes.",);
        }

        arg.expr = Expr::Tuple(new_args);
        let (func, f_ty) = compile.func_expr(shift_or_slice);
        let func = FatExpr::synthetic_ty(func, loc, f_ty);
        let arg = FatExpr::synthetic(Expr::SuffixMacro(Flag::Slice.ident(), Box::new(arg)), loc);
        let ty = u32::get_type(compile.program);
        Ok(FatExpr::synthetic_ty(Expr::Call(Box::new(func), Box::new(arg)), loc, ty))
    })
}

extern "C-unwind" fn shift_or_slice(compiler: &mut Compile, ptr: *const i64, len: usize) -> i64 {
    hopec(compiler, || {
        assert!(len < 32, "{} {}", ptr as usize, len);
        let ints = unsafe { &*slice_from_raw_parts(ptr, len) };
        let mut acc = 0;
        for i in 0..ints.len() / 3 {
            let mut x = ints[i * 3];
            let sh = ints[i * 3 + 1];
            let size_info = ints[i * 3 + 2]; // HACK. masking fixed maual_mmap. u16 has garabge memory where read as u64.
            let bit_count = size_info.abs();
            let is_signed = size_info < 0;
            if is_signed {
                x = signed_truncate(x, bit_count);
            }
            assert!(sh < 32, "{} {}", ptr as usize, len);
            // assert!(x << sh <= 1 << 32, "{x:#x} << {sh}");
            acc |= (x & ((1 << bit_count) - 1)) << sh;
        }
        assert!(acc <= u32::MAX as i64, "{acc:x} is not a valid u32: {ints:?}");
        Ok(acc)
    })
}

extern "C-unwind" fn get_dispatch_ptr(comp: &mut Compile) -> *mut i64 {
    comp.aarch64.get_dispatch() as usize as *mut i64
}

extern "C-unwind" fn make_fn_type<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, ret: FatExpr<'p>) -> Res<'p, FnType> {
    let types = match &mut arg.expr {
        Expr::StructLiteralP(parts) => {
            parts.if_empty_add_unit();
            compile.infer_pattern(&mut parts.bindings)?
        }
        Expr::Tuple(parts) => {
            let mut types = vec![];
            for e in parts {
                let t: TypeId = compile.immediate_eval_expr_known(e.clone())?;
                types.push(t);
            }
            types
        }
        _ => {
            // err!("expected @fn(name: type, name: type) type not {}", arg.log(compile.pool)),
            let t: TypeId = compile.immediate_eval_expr_known(arg.clone())?;
            vec![t]
        }
    };
    let arity = types.len() as u16;
    let arg = compile.program.tuple_of(types);
    let ret: TypeId = compile.immediate_eval_expr_known(ret)?;
    let f_ty = FnType { arg, ret, arity };
    return_from_ffi(compile);
    Ok(f_ty)
}
extern "C-unwind" fn fn_type_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>, ret: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        let f_ty = make_fn_type(compile, &mut arg, ret)?;
        let ty = compile.program.intern_type(TypeInfo::Fn(f_ty));
        compile.set_literal(&mut arg, ty)?;
        Ok(())
    });
    return_from_ffi(compile);

    arg
}

extern "C-unwind" fn fn_ptr_type_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>, ret: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        let f_ty = make_fn_type(compile, &mut arg, ret)?;
        let ty = compile.program.intern_type(TypeInfo::FnPtr {
            ty: f_ty,
            cc: CallConv::CCallReg,
        });
        compile.set_literal(&mut arg, ty)?;
        Ok(())
    });
    return_from_ffi(compile);

    arg
}

extern "C-unwind" fn fn_ptr_type_macro_single<'p>(compile: &mut Compile<'_, 'p>, ret: FatExpr<'p>) -> FatExpr<'p> {
    let loc = ret.loc;
    fn_ptr_type_macro(
        compile,
        FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![], loc }), loc),
        ret,
    )
}

extern "C-unwind" fn fn_type_macro_single<'p>(compile: &mut Compile<'_, 'p>, ret: FatExpr<'p>) -> FatExpr<'p> {
    let loc = ret.loc;
    fn_type_macro(
        compile,
        FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![], loc }), loc),
        ret,
    )
}
