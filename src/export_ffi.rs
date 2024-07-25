#![allow(improper_ctypes_definitions)]

use crate::self_hosted::{resolve_root, show_error_line, ParseErr, SelfHosted};

use crate::ast::{
    garbage_loc, CallConv, Expr, FatExpr, FatStmt, Flag, FnType, Func, FuncId, IntTypeInfo, LazyType, Pattern, Program, ScopeId, TargetArch, TypeId,
    TypeInfo, TypeMeta, WalkAst,
};
use crate::bc::{from_values, Values};
use crate::compiler::{want, Compile, CompileError, ExecStyle, Res, ResultType, Unquote, EXPECT_ERR_DEPTH};
use crate::ffi::InterpSend;
use crate::logging::{unwrap, PoolLog};
use crate::overloading::where_the_fuck_am_i;
use crate::self_hosted::Ident;
use crate::{assert, err, ice, log_err, make_toplevel};
use std::fmt::{Debug, Write};
use std::mem::{self, transmute};
use std::ops::{FromResidual, Try};
use std::path::PathBuf;
use std::ptr::{slice_from_raw_parts, slice_from_raw_parts_mut};
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

    pub(crate) fn expect(self, arg: &str) -> T {
        match self {
            BigOption::Some(t) => t,
            BigOption::None => panic!("Missing Value. {arg}"),
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
    pub(crate) fn unwrap(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => panic!("Unwrapped error value: {e:?}"),
        }
    }

    pub(crate) fn is_err(&self) -> bool {
        matches!(self, Err(_))
    }

    pub(crate) fn is_ok(&self) -> bool {
        matches!(self, Ok(_))
    }

    pub(crate) fn unwrap_or_else(self, f: impl FnOnce(E) -> T) -> T {
        match self {
            Ok(t) => t,
            Err(e) => f(e),
        }
    }
    pub(crate) fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> BigResult<T, E2> {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(f(e)),
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
    init_compiler: unsafe extern "C" fn(comptime_arch: TargetArch, build_options_ptr: usize) -> *const Compile<'static, 'static>,
    find_unqiue_func: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, name: Ident<'p>) -> BigOption<FuncId>,
    _get_fns_with_tag: usize,
    _b: i64, // todo: remove
    compile_func: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> BigCErr<'p, ()>,
    get_jitted_ptr: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId) -> BigCErr<'p, *const u8>,
    _get_function: usize,
    _d: usize,
    _add_file: usize,
    _parse_stmts: usize,
    make_and_resolve_and_compile_top_level: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, body: *const [FatStmt<'p>]) -> BigCErr<'p, ()>,
    make_jitted_exec: unsafe extern "C" fn(c: &mut Compile),
    give_vtable: unsafe extern "C" fn(c: &mut Compile, vtable: *const ExportVTable, userdata: *mut ()),
    _k: usize,
    comptime_arch: unsafe extern "C" fn() -> (i64, i64),
    _c: usize,
    _l: usize,
    _m: usize,
    __a: usize,
    _f: usize,
    __b: usize,
    _e: usize,
    get_compiler_builtins_source: extern "C" fn() -> &'static str,
    _i: usize,
    _emit_llvm: usize,
    _j: usize,
    _bake_var: usize,
    _h: usize,
    clone_expr: for<'p> extern "C" fn(e: &FatExpr<'p>) -> FatExpr<'p>,
    clone_type: for<'p> extern "C" fn(e: &LazyType<'p>) -> LazyType<'p>,
    _o: usize,
    _n: usize,
    log_type: extern "C" fn(compile: &mut Compile, e: TypeId) -> *const str,
    check_for_new_aot_bake_overloads: for<'p> extern "C" fn(comp: &mut Compile<'_, 'p>) -> Res<'p, ()>,
    clone_func: for<'p> extern "C" fn(e: &Func<'p>) -> Func<'p>,
}

#[repr(C)]
#[derive(Clone, Default)]
pub struct ExportVTable {
    pub resolve_comptime_import:
        BigOption<unsafe extern "C" fn(userdata: *mut (), c: &mut Compile, f: FuncId, lib_name: Ident, fn_name: Ident) -> BigOption<*const u8>>,
}

pub static IMPORT_VTABLE: ImportVTable = ImportVTable {
    _intern_string: 0xbeefbeefbeef,
    _get_string: 0xbeefbeefbeef,
    _a: 0,
    init_compiler: franca_init_compiler,
    find_unqiue_func: franca_find_unique_fn,
    _get_fns_with_tag: 0xbeefbeefbeef,
    _b: 0,
    compile_func: franca_compile_func,
    get_jitted_ptr,
    _get_function: 1,
    _d: 0,
    _add_file: 0xbeefbeefbeef,
    _parse_stmts: 0xbeefbeefbeef,
    make_and_resolve_and_compile_top_level,
    make_jitted_exec,
    give_vtable,
    _k: 0xbeefbeefbeef,
    comptime_arch,
    _c: 0,
    _l: 0,
    _m: 0xbeefbeefbeef,
    __a: 0,
    _f: 0xbeefbeefbeef,
    __b: 0xbeefbeefbeef,
    _e: 0,
    get_compiler_builtins_source,
    _i: 0,
    _emit_llvm: 0xbeefbeefbeef,
    _j: 0,
    _bake_var: 0xbeefbeefbeef,
    _h: 0,
    clone_expr,
    clone_type,
    _o: 0,
    _n: 0,
    log_type: franca_log_type,
    check_for_new_aot_bake_overloads,
    clone_func,
};

// TODO: cant just call the thing because lifetime?
extern "C" fn check_for_new_aot_bake_overloads<'p>(compile: &mut Compile<'_, 'p>) -> Res<'p, ()> {
    compile.check_for_new_aot_bake_overloads()
}
extern "C" fn franca_log_type(compile: &mut Compile, e: TypeId) -> *const str {
    compile.program.log_type(e).leak() as *const str
}

extern "C" fn clone_expr<'p>(e: &FatExpr<'p>) -> FatExpr<'p> {
    e.clone()
}

extern "C" fn clone_type<'p>(e: &LazyType<'p>) -> LazyType<'p> {
    e.clone()
}

extern "C" fn clone_func<'p>(e: &Func<'p>) -> Func<'p> {
    e.clone()
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

unsafe extern "C" fn franca_init_compiler(comptime_arch: TargetArch, build_options_ptr: usize) -> *const Compile<'static, 'static> {
    if cfg!(not(target_arch = "aarch64")) && comptime_arch == TargetArch::Aarch64 {
        panic!("aarch64 jit is not supported on this architecture"); // TODO: return error instead.
    }

    let program = Box::leak(Box::new(Program::new(comptime_arch, build_options_ptr)));
    let compiler = Box::leak(Box::new(Compile::new(program)));
    compiler as *const Compile
}
unsafe extern "C" fn franca_find_unique_fn<'p>(c: &mut Compile<'_, 'p>, name: Ident<'p>) -> BigOption<FuncId> {
    match c.program.find_unique_func(name) {
        Some(f) => BigOption::Some(f),
        None => BigOption::None,
    }
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
    Ok(unwrap!(c.get_fn(f), "not compiled {f:?}"))
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

// pub(crate) fn __clear_cache(beg: *mut libc::c_char, end: *mut libc::c_char);
// IMPORTANT: since compile is repr(C), &mut &mut Program === &mut Compile
pub const COMPILER: &[(&str, *const u8)] = &[
    ("#fold fn tag_value(E: Type, case_name: Symbol) i64", tag_value as *const u8),
    ("#fold fn tag_symbol(E: Type, tag_value: i64) Symbol", tag_symbol as *const u8),
    ("fn name(func_id: FuncId) Symbol", function_name as *const u8),
    ("fn index_to_func_id(func_index: i64) FuncId", index_to_func_id as *const u8),
    // TODO: all these type ones could use ffi TypeInfo if i gave it `#ct fn intern_type`
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
    // This a null terminated packed string, useful for ffi with old c functions.
    // Currently it doesn't reallocate because all symbols are null terminated but that might change in future. --Apr, 10
    ("#fold fn c_str(s: Symbol) CStr", symbol_to_cstr as *const u8),
    ("fn debug_log_type(ty: Type) void", log_type as *const u8),
    ("fn IntType(bits: i64, signed: bool) Type #fold;", make_int_type as *const u8),
    // measured in bytes
    ("fn size_of(T: Type) i64 #fold", get_size_of as *const u8),
    // ("fn Label(Arg: Type) Type", do_label_type as *const u8),
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
    // ("#fold fn str(s: Symbol) Str", symbol_to_str as *const u8),
    ("fn get_meta(t: Type) TypeMeta", get_meta as *const u8),
    // TODO: this should be a function, but then that needs a different bootstrapping path.
    ("#macro fn builtin(t: FatExpr) FatExpr", Compile::get_builtin_macro as *const u8),
    ("fn Tag(Tagged: Type) Type #fold;", get_enum_tag_type as *const u8),
    // :blessed: it looks for @rec by name!! TODO: this is very bad and confusing!! you can't shadow it!! - Jul 1
];

extern "C-unwind" fn get_enum_tag_type(comp: &mut Compile, e: TypeId) -> TypeId {
    let raw = comp.program.raw_type(e);
    let TypeInfo::Tagged { tag, .. } = &comp.program[raw] else {
        println!("{}", comp.log_trace());
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

extern "C-unwind" fn get_size_of(compiler: &mut Compile, ty: TypeId) -> i64 {
    hope(|| compiler.program.finish_layout(ty));
    compiler.program.get_info(ty).stride_bytes as i64
}

pub static STDLIB_PATH: Mutex<Option<PathBuf>> = Mutex::new(None);

const MSG: &str = "//! IMPORTANT: don't try to save #comptime_addr('ASLR junk'), it won't go well. \n";

extern "C" fn get_compiler_builtins_source() -> &'static str {
    let mut out = String::new();
    writeln!(out, "{}", MSG).unwrap();
    for (sig, ptr) in COMPILER {
        writeln!(out, "#comptime_addr({}) #ct #c_call {sig};", *ptr as usize).unwrap();
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
    match &comp.program[enum_ty] {
        TypeInfo::Enum { raw, fields, .. } => {
            // TODO: this is kinda dumb, could be in meta.fr but this us just easier right now. -- Jul 8

            if !(matches!(comp.program[*raw], TypeInfo::Int(_))) {
                panic!("tag_value on @enum (not @tagged) that is not an int!")
            }
            // TODO: assert 8 bytes
            let index = fields.iter().position(|f| f.0 == name);
            let index = hope(|| {
                Ok(unwrap!(
                    index,
                    "bad case name id={} {}. expected {:?}",
                    name.0,
                    comp.program.pool.get(name),
                    fields.iter().map(|f| comp.program.pool.get(f.0)).collect::<Vec<_>>(),
                ))
            });
            let i: i64 = from_values(comp.program, fields[index].1.clone()).unwrap();
            i
        }
        TypeInfo::Tagged { cases, .. } => {
            let index = cases.iter().position(|f| f.0 == name);
            let index = hope(|| {
                Ok(unwrap!(
                    index,
                    "tagged type, bad case name id={} {}. expected {:?}",
                    name.0,
                    comp.program.pool.get(name),
                    cases.iter().map(|f| comp.program.pool.get(f.0)).collect::<Vec<_>>(),
                ))
            });
            index as i64
        }
        &TypeInfo::Named(ty, _) => tag_value(comp, ty, name),
        _ => {
            panic!(
                "{} is not enum. (tried tag_value of {})",
                comp.program.log_type(enum_ty),
                comp.program.pool.get(name)
            )
        }
    }
}

extern "C-unwind" fn tag_symbol<'p>(program: &&Program<'p>, enum_ty: TypeId, tag_val: i64) -> Ident<'p> {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let case = hope(|| Ok(unwrap!(cases.get(tag_val as usize), "enum tag too high")));
    case.0
}

extern "C-unwind" fn pair_type(program: &mut &mut Program, a: TypeId, b: TypeId) -> TypeId {
    hope(|| {
        // assert!(a.as_index() < program.types.len(), "TypeId OOB {:?}", a);// should always be true i just can't be bothered to do ffi now that self hosted owns it
        // assert!(b.as_index() < program.types.len(), "TypeId OOB {:?}", b);
        Ok(program.tuple_of(vec![a, b]))
    })
}

extern "C-unwind" fn triple_type(program: &mut &mut Program, a: TypeId, b: TypeId, c: TypeId) -> TypeId {
    hope(|| {
        // assert!(a.as_index() < program.types.len(), "TypeId OOB {:?}", a);
        // assert!(b.as_index() < program.types.len(), "TypeId OOB {:?}", b);// should always be true i just can't be bothered to do ffi now that self hosted owns it
        // assert!(c.as_index() < program.types.len(), "TypeId OOB {:?}", c);
        Ok(program.tuple_of(vec![a, b, c]))
    })
}
extern "C-unwind" fn quad_type(program: &mut &mut Program, a: TypeId, b: TypeId, c: TypeId, d: TypeId) -> TypeId {
    hope(|| Ok(program.tuple_of(vec![a, b, c, d])))
}
extern "C-unwind" fn fn_type(program: &mut &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        // assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);  // should always be true i just can't be bothered to do ffi now that self hosted owns it
        // assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
        let arity = program.tuple_types(arg).map(|t| t.len()).unwrap_or_else(|| 1) as u16;
        Ok(program.intern_type(TypeInfo::Fn(FnType { arg, ret, arity })))
    })
}

extern "C-unwind" fn fn_ptr_type(program: &mut &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        // assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);// should always be true i just can't be bothered to do ffi now that self hosted owns it
        // assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
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

extern "C-unwind" fn log_type(p: &mut &mut Program, a: TypeId) {
    println!("{a:?} = {} = {:?}", p.log_type(a), p[a]);
}

extern "C-unwind" fn log_ast<'p>(p: &mut Compile<'_, 'p>, a: &mut FatExpr<'p>) {
    println!("{}", a.log(p.program.pool));
    where_the_fuck_am_i(p, a.loc);
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

extern "C-unwind" fn as_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, target: &mut FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(arg.loc);
    let res = compile.as_cast_macro(mem::take(arg), mem::take(target));
    hopec(compile, || res)
}

extern "C-unwind" fn enum_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, target: &mut FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(arg.loc);
    let res = compile.enum_constant_macro(mem::take(arg), mem::take(target));
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

extern "C-unwind" fn const_eval_any<'p>(compile: &mut Compile<'_, 'p>, expr: &mut FatExpr<'p>, ty: TypeId, addr: usize) {
    // TODO: immediate_eval_expr doesn't do a type check. -- Apr 27
    compile.compile_expr(expr, want(ty)).unwrap();

    match compile.immediate_eval_expr(mem::take(expr), ty) {
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

extern "C-unwind" fn compile_ast<'p>(compile: &mut Compile<'_, 'p>, expr: &mut FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(expr.loc);
    let res = compile.compile_expr(expr, ResultType::None);
    hopec(compile, || res);
    return_from_ffi(compile);
    mem::take(expr)
}

// :UnquotePlaceholders
extern "C-unwind" fn unquote_macro_apply_placeholders<'p>(compile: &mut Compile<'_, 'p>, args: *mut [FatExpr<'p>]) -> FatExpr<'p> {
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

extern "C-unwind" fn get_type_int<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>) -> IntTypeInfo {
    hope(|| {
        match &mut arg.expr {
            Expr::Call(_, _) => {
                if let Some((int, _)) = compile.bit_literal(arg) {
                    return Ok(int);
                }
            }
            Expr::Value { .. } => err!("todo",),
            _ => {
                let ty = compile.compile_expr(arg, ResultType::None)?;
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
extern "C-unwind" fn namespace_macro<'p>(compile: &mut Compile<'_, 'p>, block: &mut FatExpr<'p>) -> FatExpr<'p> {
    let loc = block.loc;
    // give any other macros a chance to expand.
    let ty = compile.program.intern_type(TypeInfo::Fn(FnType {
        arg: TypeId::unit,
        ret: TypeId::unit,
        arity: 1,
    }));
    let found_ty = compile.compile_expr(block, want(ty)).unwrap();
    let id = block.as_const().unwrap().unwrap_func_id();
    debug_assert_eq!(ty, found_ty);
    compile.compile(id, ExecStyle::Jit).unwrap();
    let func = &mut compile.program[id];
    let s = func.scope.unwrap();

    return_from_ffi(compile);
    compile.as_literal(s, loc).unwrap()
}

pub extern "C-unwind" fn tagged_macro<'p>(compile: &mut Compile<'_, 'p>, cases_expr: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        if let Expr::StructLiteralP(pattern) = &mut cases_expr.expr {
            let mut tag_fields = vec![];
            let mut cases = vec![];
            for (i, b) in pattern.bindings.iter_mut().enumerate() {
                // TODO: allow as default so you can use .Name like you can with void?
                //       then need to store default in TypeInfo::Tagged as well. -- Jul 5
                assert!(b.default.is_none(), "dont use = with @tagged");
                if matches!(b.ty, LazyType::Infer) {
                    // @tagged(s: i64, n) is valid and infers n as void.
                    b.ty = LazyType::Finished(TypeId::unit);
                } else {
                    assert!(compile.infer_binding_progress(b)?, "failed to infer @tagged type");
                }
                let name = b.name().unwrap();
                tag_fields.push((name, Values::one(i as i64))); // :tag_enums_are_sequential
                let ty = unwrap!(b.ty.ty(), "ICE: field type not inferred");
                cases.push((name, ty))
            }

            let tag = TypeInfo::Enum {
                raw: TypeId::i64(),
                fields: tag_fields,
                sequentual: true,
            };
            let tag = compile.program.intern_type(tag);

            let ty = TypeInfo::Tagged { cases, tag };
            let ty = compile.program.intern_type(ty);

            compile.set_literal(cases_expr, ty)?;
        } else {
            err!("@tagged expected map literal like `(name: Type, ...)` but found {:?}", cases_expr);
        }
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(cases_expr)
}

pub extern "C-unwind" fn struct_macro<'p>(compile: &mut Compile<'_, 'p>, fields: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        if let Expr::StructLiteralP(pattern) = &mut fields.expr {
            let ty = compile.struct_type(pattern)?;
            let ty = compile.program.intern_type(ty);
            compile.set_literal(fields, ty)?;
        } else {
            err!("expected map literal: (name: Type, ... ) but found {:?}", fields);
        }
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(fields)
}

extern "C-unwind" fn symbol_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>) -> FatExpr<'p> {
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
        compile.set_literal(arg, value)?;
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(arg)
}

extern "C-unwind" fn assert_compile_error_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        // TODO: this can still have side-effects on the compiler state tho :(
        let before = compile.debug_trace.len();
        unsafe {
            EXPECT_ERR_DEPTH.fetch_add(1, Ordering::SeqCst);
        }
        let res = compile.compile_expr(arg, ResultType::None);
        unsafe {
            EXPECT_ERR_DEPTH.fetch_sub(1, Ordering::SeqCst);
        }
        assert!(res.is_err());
        while compile.debug_trace.len() > before {
            compile.debug_trace.pop();
        }

        compile.set_literal(arg, ())?;
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(arg)
}

pub extern "C-unwind" fn type_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        // Note: this does not evaluate the expression.
        // TODO: warning if it has side effects. especially if it does const stuff.
        let ty = compile.compile_expr(arg, ResultType::None)?;
        compile.set_literal(arg, ty)?;
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(arg)
}

extern "C-unwind" fn bits_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        let Expr::Tuple(parts) = &mut arg.expr else {
            err!("Expected @Bits(Tuple...)",)
        };
        let shift_or_slice = compile.program.find_unique_func(Flag::__Shift_Or_Slice.ident()).unwrap();
        compile.compile(shift_or_slice, ExecStyle::Jit)?;

        let mut new_args = Vec::with_capacity(parts.len() * 3);
        let loc = arg.loc;
        let mut shift = 32;
        for int in parts {
            let mut ty = get_type_int(compile, int); // TODO: redundant work cause of clone

            shift -= ty.bit_count;
            assert!(shift >= 0, "expected 32 bits. TODO: other sizes.");
            // don't need this since we need to handle arbirary expressions anyway.
            // if let Some((_, v)) = compile.bit_literal(&int) {
            //     // Not signed_truncate-ing here because shift_or_slice does it.
            //     assert!(v < 1 << ty.bit_count, "{v} {}", ty.bit_count);
            //     int = compile.as_literal(v, loc)?;
            // }
            *int = FatExpr::synthetic_ty(Expr::Cast(Box::new(mem::take(int))), loc, TypeId::i64());
            new_args.push(mem::take(int));
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
        let arg = FatExpr::synthetic(Expr::Slice(Box::new(mem::take(arg))), loc);
        let ty = u32::get_type(compile.program);
        Ok(FatExpr::synthetic_ty(Expr::Call(Box::new(func), Box::new(arg)), loc, ty))
    })
}

extern "C-unwind" fn get_dispatch_ptr(_: &mut Compile) -> *mut i64 {
    todo!("get_dispatch_ptr")
    // comp.aarch64.get_dispatch() as usize as *mut i64
}

extern "C-unwind" fn make_fn_type<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, ret: &mut FatExpr<'p>) -> Res<'p, FnType> {
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
    let ret: TypeId = compile.immediate_eval_expr_known(mem::take(ret))?;
    let f_ty = FnType { arg, ret, arity };
    return_from_ffi(compile);
    Ok(f_ty)
}
extern "C-unwind" fn fn_type_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, ret: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        let f_ty = make_fn_type(compile, arg, ret)?;
        let ty = compile.program.intern_type(TypeInfo::Fn(f_ty));
        compile.set_literal(arg, ty)?;
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(arg)
}

extern "C-unwind" fn fn_ptr_type_macro<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, ret: &mut FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        let f_ty = make_fn_type(compile, arg, ret)?;
        let ty = compile.program.intern_type(TypeInfo::FnPtr {
            ty: f_ty,
            cc: CallConv::CCallReg,
        });
        compile.set_literal(arg, ty)?;
        Ok(())
    });
    return_from_ffi(compile);

    mem::take(arg)
}

extern "C-unwind" fn fn_ptr_type_macro_single<'p>(compile: &mut Compile<'_, 'p>, ret: &mut FatExpr<'p>) -> FatExpr<'p> {
    let loc = ret.loc;
    fn_ptr_type_macro(
        compile,
        &mut FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![], loc }), loc),
        ret,
    )
}

extern "C-unwind" fn fn_type_macro_single<'p>(compile: &mut Compile<'_, 'p>, ret: &mut FatExpr<'p>) -> FatExpr<'p> {
    let loc = ret.loc;
    fn_type_macro(
        compile,
        &mut FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![], loc }), loc),
        ret,
    )
}
