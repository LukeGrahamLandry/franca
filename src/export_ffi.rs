#![allow(improper_ctypes_definitions)]

use codemap::{File, Span};
use interp_derive::Reflect;
use libc::c_void;

use crate::ast::{
    garbage_loc, CallConv, Expr, FatExpr, FatStmt, Flag, FnFlag, FnType, Func, FuncId, IntTypeInfo, LazyType, OverloadSetId, Pattern, Program,
    ScopeId, TargetArch, TypeId, TypeInfo, WalkAst,
};
use crate::bc::{to_values, ReadBytes, Values};
use crate::compiler::{Compile, CompileError, ExecStyle, Res, Unquote, EXPECT_ERR_DEPTH};
use crate::ffi::InterpSend;
use crate::lex::Lexer;
use crate::logging::{unwrap, PoolLog};
use crate::overloading::where_the_fuck_am_i;
use crate::parse::Parser;
use crate::pool::{Ident, StringPool};
use crate::scope::ResolveScope;
use crate::{assert, err, ice, log_err, make_toplevel, signed_truncate, Stats, STATS};
use std::fmt::{Debug, Write};
use std::hint::black_box;
use std::mem::{self, transmute, ManuallyDrop};
use std::ops::{FromResidual, Try};
use std::path::PathBuf;
use std::ptr::{addr_of, null, slice_from_raw_parts, slice_from_raw_parts_mut};
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};
use std::{fs, io};

use crate::export_ffi::BigResult::*;

#[repr(C, i64)]
#[derive(Clone, Debug)]
pub enum BigOption<T> {
    Some(T),
    None,
}

#[derive(Clone, Debug)]
#[repr(C, i64)]
pub enum BigResult<T, E> {
    Ok(T),
    Err(E),
}
impl<T, E: Debug> BigResult<T, E> {
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
    pub fn map_err(self, f: impl FnOnce(E) -> E) -> Self {
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
    intern_string: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, s: *const str) -> Ident<'p>,
    get_string: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, s: Ident<'p>) -> *const str,
    get_stats: unsafe extern "C" fn() -> *const Stats,
    init_compiler: unsafe extern "C" fn(comptime_arch: TargetArch) -> *const Compile<'static, 'static>,
    find_unqiue_func: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, name: Ident<'p>) -> BigOption<FuncId>,
    // TODO: i want the meta program to be tracking these instead.
    get_fns_with_tag: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, tag: Ident<'p>) -> *const [FuncId],
    // TODO: you can't just give it the slice from get_exports/tests because that will alias the vec that might grow, but the idea is this will go away soon anyway.
    emit_c: for<'p> unsafe extern "C" fn(
        out: *mut ManuallyDrop<Res<'p, *const str>>,
        c: &mut Compile<'_, 'p>,
        fns: *const [FuncId],
        add_test_runner: bool,
    ),
    compile_func: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> BigCErr<'p, ()>,
    get_jitted_ptr: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, f: FuncId) -> BigCErr<'p, *const u8>,
    get_function: for<'p> unsafe extern "C" fn(c: &mut Compile<'p, '_>, f: FuncId) -> *const Func<'p>,
    lookup_filename: unsafe extern "C" fn(c: &mut Compile, span: Span) -> *const str,
    add_file: unsafe extern "C" fn(c: &mut Compile, name: &str, content: &str) -> Arc<File>,
    parse_stmts: for<'p> unsafe extern "C" fn(out: *mut ManuallyDrop<BigCErr<'p, *const [FatStmt<'p>]>>, c: &mut Compile<'_, 'p>, f: Arc<File>),
    make_and_resolve_and_compile_top_level: for<'p> unsafe extern "C" fn(c: &mut Compile<'_, 'p>, body: *const [FatStmt<'p>]) -> BigCErr<'p, ()>,
    make_jitted_exec: unsafe extern "C" fn(c: &mut Compile),
}

pub static IMPORT_VTABLE: ImportVTable = ImportVTable {
    intern_string: franca_intern_string,
    get_string: franca_get_string,
    get_stats: franca_get_stats,
    init_compiler: franca_init_compiler,
    find_unqiue_func: franca_find_unique_fn,
    get_fns_with_tag,
    emit_c: franca_emit_c,
    compile_func: franca_compile_func,
    get_jitted_ptr,
    get_function: franca_get_function,
    lookup_filename,
    add_file,
    parse_stmts,
    make_and_resolve_and_compile_top_level,
    make_jitted_exec,
};

unsafe extern "C" fn franca_intern_string<'p>(c: &mut Compile<'_, 'p>, s: *const str) -> Ident<'p> {
    c.pool.intern(&*s)
}
unsafe extern "C" fn franca_get_string<'p>(c: &mut Compile<'_, 'p>, s: Ident<'p>) -> *const str {
    c.pool.get(s) as *const str
}
unsafe extern "C" fn franca_get_stats() -> *const Stats {
    addr_of!(STATS)
}
unsafe extern "C" fn franca_init_compiler(comptime_arch: TargetArch) -> *const Compile<'static, 'static> {
    let pool = Box::leak(Box::new(StringPool::default()));
    let program = Box::leak(Box::new(Program::new(pool, comptime_arch)));
    let compiler = Box::leak(Box::new(Compile::new(pool, program)));
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
unsafe extern "C" fn franca_emit_c<'p>(
    out: *mut ManuallyDrop<Res<'p, *const str>>,
    c: &mut Compile<'_, 'p>,
    fns: *const [FuncId],
    add_test_runner: bool,
) {
    #[cfg(feature = "c-backend")]
    {
        *out = ManuallyDrop::new(crate::c::emit_c(c, (*fns).to_vec(), add_test_runner).map_ok(|s| s.leak() as *const str));
    }

    #[cfg(not(feature = "c-backend"))]
    {
        // TODO: this should be an error but its annoying to make them with indirect return cause can't use the macro
        panic!("not compiled with c backend")
    }
}

unsafe extern "C" fn franca_compile_func<'p>(c: &mut Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> BigCErr<'p, ()> {
    c.compile(f, when)
}

unsafe extern "C" fn get_jitted_ptr<'p>(c: &mut Compile<'_, 'p>, f: FuncId) -> BigCErr<'p, *const u8> {
    Ok(unwrap!(c.aarch64.get_fn(f), "not compiled {f:?}"))
}

unsafe extern "C" fn franca_get_function<'p>(c: &mut Compile<'p, '_>, f: FuncId) -> *const Func<'p> {
    &c.program[f] as *const Func
}

unsafe extern "C" fn lookup_filename(c: &mut Compile, span: Span) -> *const str {
    c.parsing.codemap.look_up_span(span).file.name().to_string().leak() as *const str
}

unsafe extern "C" fn add_file(c: &mut Compile, name: &str, content: &str) -> Arc<File> {
    c.parsing.codemap.add_file(name.to_string(), content.to_string())
}

unsafe extern "C" fn parse_stmts<'p>(out: *mut ManuallyDrop<BigCErr<'p, *const [FatStmt<'p>]>>, comp: &mut Compile<'_, 'p>, file: Arc<File>) {
    assert_eq!(mem::size_of::<BigCErr<'p, *const [FatStmt<'p>]>>(), 24);
    let lex = Lexer::new(file.clone(), comp.program.pool, file.span);
    let stmts = match Parser::parse_stmts(&mut comp.parsing, lex, comp.pool) {
        Ok(s) => s,
        Err(e) => {
            *out = ManuallyDrop::new(Err(e));
            return;
        }
    };
    mem::forget(file); // the franca code that called us doesn't know it has to clone the Arc.
    *out = ManuallyDrop::new(Ok(stmts.leak()));
}

unsafe extern "C" fn make_and_resolve_and_compile_top_level<'p>(c: &mut Compile<'_, 'p>, body: *const [FatStmt<'p>]) -> BigCErr<'p, ()> {
    let body = (*body).to_vec();
    let mut global = make_toplevel(c.pool, garbage_loc(), body);
    ResolveScope::run(&mut global, c, ScopeId::from_index(0))?;
    c.compile_top_level(global)?;
    Ok(())
}

unsafe extern "C" fn make_jitted_exec(c: &mut Compile) {
    c.aarch64.make_exec();
    c.flush_cpu_instruction_cache();
}

// This lets rust _declare_ a flat_call like its normal
// Ideally you could do this with a generic but you can't be generic over a function value whose type depends on other generic parameters.
macro_rules! bounce_flat_call {
    ($Arg:ty, $Ret:ty, $f:ident) => {{
        // weird hack becuase you can't concat_idents! for a function name and you can't declare an unnamed function as an expression.
        mod $f {
            use super::*;
            // TODO: can't do this because of lifetimes. you still get the error message if it doesn't compile so its not a big deal but I find it less clear.
            // const F: fn(compile: &mut Compile, a: $Arg) -> $Ret = $f; // force a typecheck

            pub extern "C-unwind" fn bounce(retptr: *mut u8, compile: &mut Compile<'_, '_>, argptr: *mut u8, arg_count: i64, ret_count: i64) {
                debugln!("bounce_flat_call {}", stringify!($f));
                let ty = <$Arg>::get_or_create_type(compile.program);
                compile.program.finish_layout_deep(ty).unwrap();
                let ty = <$Ret>::get_or_create_type(compile.program);
                compile.program.finish_layout_deep(ty).unwrap();
                debug_assert_eq!(arg_count, <$Arg>::size_bytes(compile.program) as i64, "bad arg count. expected {}", stringify!($Arg));
                debug_assert_eq!(ret_count, <$Ret>::size_bytes(compile.program) as i64, "bad ret count. expected {}", stringify!($Ret));

                // debug_assert_eq!(argptr as usize % <$Arg>::align_bytes(compile.program) as usize, 0, "bad arg align for {}", stringify!($Arg));
                // debug_assert_eq!(retptr as usize % <$Ret>::align_bytes(compile.program) as usize, 0, "bad ret align for {}", stringify!($Ret));
                unsafe {
                    let argslice = &mut *slice_from_raw_parts_mut(argptr, arg_count as usize);
                    debugln!("bounce ARG: {argslice:?}");
                    let arg: $Arg = hopec(compile, || Ok(unwrap!(<$Arg>::deserialize_from_ints(compile.program, &mut ReadBytes { bytes: argslice, i: 0}), "deserialize arg")));
                    let ret: $Ret = $f(compile, arg);
                    let ret = ret.serialize_to_ints_one(compile.program);
                    let out = &mut *slice_from_raw_parts_mut(retptr, ret_count as usize);
                    out.fill(0); // TODO: remove
                    out.copy_from_slice(&ret);
                    argslice.fill(0); // TODO: remove
                    debugln!("bounce RET: {out:?}");
                }
                // Since we just called into a compiler function, it might have emitted new code and left it in write mode...
                // but our caller might be jitted code (like a macro), so before returning to them, we need to make sure they're executable.
                // this avoids a `exc_bad_access (code=2, <some code address>)`. The problem only happened in the lox demo, not my small tests, so its fairly rare.
                // -- May 30
                // TODO: i probably have to do this before returning from any export_ffi function, not just flat_call ones.
                //       its wasteful to leave the map in exec mode if we're going to be writing to it again soon.
                compile.flush_cpu_instruction_cache();
                compile.aarch64.make_exec();
            }
        }

        $f::bounce
    }};
}

// TODO: use the right int types
// TODO: parse header files for signatures, but that doesn't help when you want to call it at comptime so need the address.
pub const LIBC: &[(&str, *const u8)] = &[
    // #[cfg(target_arch = "aarch64")]
    // ("fn __clear_cache(beg: rawptr, beg: rawptr) Unit", __clear_cache as *const u8),

    // #[cfg(target_os = "macos")]
    // ("fn _NSGetArgc() *i64", _NSGetArgc as *const u8), // TODO: i32
    // #[cfg(target_os = "macos")]
    // ("fn _NSGetArgv() **CStr", _NSGetArgv as *const u8),
    ("fn temp_start_raw_terminal(fd: Fd) Unit", start_raw as *const u8),
    ("fn temp_end_raw_terminal(fd: Fd) Unit", end_raw as *const u8),
];

// Based on man termios and
// https://stackoverflow.com/questions/421860/capture-characters-from-standard-input-without-waiting-for-enter-to-be-pressed
pub extern "C" fn start_raw(fd: i32) {
    use libc::*;
    unsafe {
        let mut term: termios = mem::zeroed();
        tcgetattr(fd, &mut term);
        term.c_lflag &= !(ICANON | ECHO); // dont read in lines | dont show what you're typing
        term.c_cc[VMIN] = 0; // its ok for a read to return nothing
        term.c_cc[VTIME] = 0; // dont wait at all
        tcsetattr(fd, TCSANOW, &term);
    }
}

pub extern "C" fn end_raw(fd: i32) {
    use libc::*;
    unsafe {
        let mut term: termios = mem::zeroed();
        tcgetattr(fd, &mut term);
        term.c_lflag |= ICANON | ECHO;
        tcsetattr(fd, TCSADRAIN, &term);
    }
}
// tcgetattr(0, &mut term);
// term.c_lflag |= ICANON | ECHO;
// tcsetattr(0, TCSADRAIN, &term);

// thank you rust very cool. TODO: non-macos
#[cfg(target_os = "macos")]
extern "C" {
    // These functions are in crt_externs.h.
    fn _NSGetArgc() -> *mut libc::c_int;
    fn _NSGetArgv() -> *mut *mut *mut libc::c_char;
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
    ("fn Ptr(Inner: Type) Type #fold", do_ptr_type as *const u8),
    ("fn operator_star_prefix(Inner: Type) Type #fold", do_ptr_type as *const u8),
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
    ("#fold fn int(s: Symbol) i64", symbol_to_int as *const u8), // TODO: this should be a noop
    (
        "fn resolve_backtrace_symbol(addr: *i64, out: *RsResolvedSymbol) bool",
        resolve_backtrace_symbol as *const u8,
    ),
    ("fn debug_log_type(ty: Type) Unit", log_type as *const u8),
    ("fn IntType(bits: i64, signed: bool) Type #fold;", make_int_type as *const u8),
    // measured in bytes
    ("fn size_of(T: Type) i64 #fold", get_size_of as *const u8),
    ("fn Label(Arg: Type) Type", do_label_type as *const u8),
    // useful when everything's broken so can't even compile the one defined in the language.
    ("fn debug_log_int(i: i64) Unit", debug_log_int as *const u8),
    ("fn debug_log_str(s: Str) Unit", debug_log_str as *const u8),
    // Generated for @BITS to bootstrap encoding for inline asm.
    ("#no_tail fn __shift_or_slice(ints: Slice(i64)) u32", shift_or_slice as *const u8),
    ("fn __save_slice_t(slice_t: Fn(Type, Type)) Unit", save_slice_t as *const u8),
    ("fn __save_cstr_t(T: Type) Unit", save_cstr_t as *const u8),
    ("fn intern_type_ref(ty: *TypeInfo) Type;", intern_type as *const u8),
];

extern "C-unwind" fn save_slice_t(compiler: &mut Compile, f: FuncId) {
    compiler.make_slice_t = Some(f);
}
extern "C-unwind" fn save_cstr_t(compiler: &mut Compile, t: TypeId) {
    compiler.program.save_cstr_t = Some(t);
}

extern "C-unwind" fn get_size_of(compiler: &mut Compile, ty: TypeId) -> i64 {
    hope(|| compiler.program.finish_layout(ty));
    compiler.program.get_info(ty).stride_bytes as i64
}

pub static STDLIB_PATH: Mutex<Option<PathBuf>> = Mutex::new(None);

pub fn get_include_std(name: &str) -> Option<String> {
    let msg = "//! IMPORTANT: don't try to save #comptime_addr('ASLR junk'), it won't go well. \n";
    match name {
        "libc" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            writeln!(
                out,
                "const OpenFlag = @enum(i64) (Read = {}, Write = {}, ReadWrite = {}, Create = {}, Truncate = {});",
                libc::O_RDONLY,
                libc::O_WRONLY,
                libc::O_RDWR,
                libc::O_CREAT,
                libc::O_TRUNC,
            )
            .unwrap();
            // TODO: this should be a bitflag. presumably PROT_NONE is always 0.
            //       distinguish between flags whose value is shifting (i.e. 0/1/2 vs 1/2/4)
            writeln!(
                out,
                "const MapProt = @enum(i64) (Exec = {}, Read = {}, Write = {}, None = {});",
                libc::PROT_EXEC,
                libc::PROT_READ,
                libc::PROT_WRITE,
                libc::PROT_NONE
            )
            .unwrap();
            writeln!(
                out,
                "const MapFlag = @enum(i64) (Private = {}, Anonymous = {});",
                libc::MAP_PRIVATE,
                libc::MAP_ANONYMOUS,
            )
            .unwrap();
            for (sig, ptr) in LIBC {
                writeln!(out, "#comptime_addr({}) #dyn_link #c_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        "compiler" => {
            let mut out = String::new();
            writeln!(
                out,
                "const CallConv = @enum(i64) (C = {}, Flat = {});",
                CallConv::CCallReg as u8,
                CallConv::Flat as u8,
            )
            .unwrap();
            writeln!(out, "{}", msg).unwrap();
            for (sig, ptr) in COMPILER {
                writeln!(out, "#comptime_addr({}) #ct #c_call {sig};", *ptr as usize).unwrap();
            }
            for (sig, ptr) in COMPILER_FLAT {
                writeln!(out, "#comptime_addr({}) #ct #flat_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        #[cfg(feature = "cranelift")]
        "codegen_cranelift_basic" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            for (sig, ptr) in crate::cranelift::BUILTINS {
                writeln!(out, "#cranelift_emit({}) #c_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        #[cfg(not(feature = "cranelift"))]
        "codegen_cranelift_basic" => Some(String::new()),
        _ => {
            // if let Some((_, src)) = INCLUDE_STD.iter().find(|(check, _)| name == *check) {
            //     return Some(src.to_string());
            // }

            let path = STDLIB_PATH.lock();
            let path = path.as_ref().unwrap().as_ref();
            if let Some(path) = path {
                let path = path.join(name);
                if let std::result::Result::Ok(src) = fs::read_to_string(&path) {
                    return Some(src);
                }

                // TODO: have a different macro for this (cwd)
                if let std::result::Result::Ok(src) = fs::read_to_string(name) {
                    return Some(src);
                }

                println!("Missing path {path:?}");
            } else {
                // TODO: have a different macro for this (cwd)
                if let std::result::Result::Ok(src) = fs::read_to_string(name) {
                    return Some(src);
                }
                println!("STDLIB_PATH not set.");
            }
            None
        }
    }
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

extern "C-unwind" fn tag_value<'p>(program: &&Program<'p>, enum_ty: TypeId, name: Ident<'p>) -> i64 {
    let cases = hope(|| {
        Ok(unwrap!(
            program.get_enum(enum_ty),
            "{} is not enum. (tried tag_value of {})",
            program.log_type(enum_ty),
            program.pool.get(name)
        ))
    });
    let index = cases.iter().position(|f| f.0 == name);
    let index = hope(|| Ok(unwrap!(index, "bad case name")));
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
        let cc = if program.get_info(arg).size_slots > 8 {
            CallConv::Flat
        } else {
            CallConv::CCallReg
        };
        let arity = program.tuple_types(arg).map(|t| t.len()).unwrap_or_else(|| 1) as u16;
        Ok(program.intern_type(TypeInfo::FnPtr {
            ty: FnType { arg, ret, arity },
            cc,
        }))
    })
}

// Note: currently StringPool guarentees that they're all null terminated but I don't want to promise that to the language so wrap in this function.
extern "C-unwind" fn symbol_to_cstr(program: &mut &mut Program, symbol: i64) -> *const u8 {
    let symbol = symbol as u32;
    hope(|| {
        let symbol = program
            .pool
            .upcast(symbol) // TODO: return an error instead.
            .unwrap_or_else(|| program.pool.intern(&format!("invalid symbol {symbol}")));
        let s = program.pool.get_c_str(symbol);
        Ok(s)
    })
}

extern "C-unwind" fn symbol_to_int(_: &mut &mut Program, symbol: u32) -> i64 {
    symbol as i64
}

#[derive(Reflect)]
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

extern "C-unwind" fn number_of_functions(program: &mut &mut Program) -> i64 {
    program.funcs.len() as i64
}

extern "C-unwind" fn function_name<'p>(program: &mut &mut Program<'p>, f: FuncId) -> Ident<'p> {
    program[f].name
}

extern "C-unwind" fn index_to_func_id(_: &mut &mut Program, f: usize) -> FuncId {
    FuncId::from_index(f)
}

extern "C-unwind" fn get_errno() -> i32 {
    io::Error::last_os_error().raw_os_error().unwrap_or(0)
}

extern "C-unwind" fn make_int_type(program: &mut &mut Program<'_>, bit_count: i64, signed: bool) -> TypeId {
    program.intern_type(TypeInfo::Int(crate::ast::IntTypeInfo { bit_count, signed }))
}

extern "C-unwind" fn do_ptr_type(program: &mut &mut Program<'_>, ty: TypeId) -> TypeId {
    program.ptr_type(ty)
}

extern "C-unwind" fn do_label_type(program: &mut &mut Program<'_>, ty: TypeId) -> TypeId {
    program.intern_type(TypeInfo::Label(ty))
}

extern "C-unwind" fn debug_log_int(_: &mut &mut Program<'_>, i: i64) {
    println!("{i}");
}

extern "C-unwind" fn debug_log_str(_: &mut &mut Program<'_>, s: &str) {
    println!("{s}");
}

extern "C-unwind" fn test_flat_call(ret: *mut u8, compile: &mut Compile, arg: *mut u8, arg_count: i64, ret_count: i64) {
    debug_assert!(arg_count == 3 * 8);
    debug_assert!(ret_count == 8);
    let _ = black_box(compile.program.assertion_count); // dereference the pointer.
    unsafe {
        let s = &mut *slice_from_raw_parts_mut(arg as *mut i64, 3);
        *(ret as *mut i64) = (s[0] * s[1]) + s[2];
    }
}
extern "C-unwind" fn test_flat_call_callback(ret: *mut u8, compile: &mut Compile<'_, '_>, arg: *mut u8, arg_count: i64, ret_count: i64) {
    debug_assert!(arg_count == 8);
    debug_assert!(ret_count == 8);
    let _ = black_box(compile.program.assertion_count); // dereference the pointer.
    unsafe {
        let addr = *(arg as *mut usize);
        // TODO: i love this. could do even better if i looked at ranges of memory in my arenas probably.
        debug_assert!(
            addr % 4 == 0 && (addr as u32 & FuncId::MASK == 0 || FuncId::from_raw(addr as u32).as_index() > compile.program.funcs.len()),
            "thats a weird ptr my dude, did you mean to call {:?}?",
            FuncId::from_raw(addr as u32)
        );
        let f: FlatCallFn = transmute(addr);
        let ret = ret as *mut i64;
        *ret = do_flat_call(compile, f, ((10i64, 5i64), 7i64));
        debug_assert_eq!(*ret, 57);
    }
}

pub type FlatCallFn = extern "C-unwind" fn(ret: *mut u8, program: &mut Compile<'_, '_>, arg: *mut u8, arg_count: i64, ret_count: i64);

// This lets rust _call_ a flat_call like its normal
pub fn do_flat_call<'p, Arg: InterpSend<'p>, Ret: InterpSend<'p>>(compile: &mut Compile<'_, 'p>, f: FlatCallFn, arg: Arg) -> Ret {
    Arg::get_or_create_type(compile.program); // sigh
    Ret::get_or_create_type(compile.program); // sigh
    let mut arg = arg.serialize_to_ints_one(compile.program);
    let mut ret = vec![0u8; Ret::size_bytes(compile.program)];
    f(ret.as_mut_ptr(), compile, arg.as_mut_ptr(), arg.len() as i64, ret.len() as i64);
    Ret::deserialize_from_ints(compile.program, &mut ReadBytes { bytes: &ret, i: 0 }).unwrap()
}

// This the interpreter call a flat_call without knowing its types
pub fn do_flat_call_values<'p>(compile: &mut Compile<'_, 'p>, f: FlatCallFn, arg: Values, ret_type: TypeId) -> Res<'p, Values> {
    let ret_count = compile.program.get_info(ret_type).stride_bytes as usize;
    debugln_call!("IN: {arg:?} addr=0x{:x}", f as usize);
    let mut ret = vec![0u8; ret_count];
    f(
        ret.as_mut_ptr(),
        compile,
        // TODO: decide if flat call is allowed to mutate its args.
        arg.bytes().as_ptr() as *mut u8,
        arg.bytes().len() as i64,
        ret.len() as i64,
    );
    debugln_call!("OUT: {ret:?}");
    Ok(Values::many(ret))
}

fn test_flat_call2(_: &mut Compile, ((a, b), c): ((i64, i64), i64)) -> i64 {
    a * b + c
}

type Unit = ();

// TODO: this needs to be less painful. want to have it just parse rust signetures and expose them.
// TODO: documenet which ones can only be used in macros because they need the 'result: &mut FnWip'
pub const COMPILER_FLAT: &[(&str, FlatCallFn)] = &[
    ("fn test_flat_call_fma(a: i64, b: i64, add_this: i64) i64;", test_flat_call),
    ("fn test_flat_call_callback(addr: rawptr) i64;", test_flat_call_callback),
    (
        "fn test_flat_call_fma2(a: i64, b: i64, add_this: i64) i64;",
        bounce_flat_call!(((i64, i64), i64), i64, test_flat_call2),
    ),
    // TODO: maybe it would be nice if you could override deref so Type acts like a *TypeInfo.
    ("fn get_type_info(ty: Type) TypeInfo;", bounce_flat_call!(TypeId, TypeInfo, get_type_info)),
    (
        "fn get_type_info_raw(ty: Type) TypeInfo;",
        bounce_flat_call!(TypeId, TypeInfo, get_type_info_raw),
    ),
    (
        "fn const_eval(expr: FatExpr, ty: Type, result: rawptr) Unit;",
        bounce_flat_call!(((FatExpr, TypeId), usize), Unit, const_eval_any),
    ),
    // Calls Compiler::compile_expr
    // Infers the type and avoids some redundant work if you duplicate the ast node in a bunch of places after calling this.
    (
        "fn compile_ast(value: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, compile_ast),
    ),
    ("fn debug_log_ast(expr: FatExpr) Unit;", bounce_flat_call!(FatExpr, Unit, log_ast)),
    (
        "fn unquote_macro_apply_placeholders(expr: Slice(FatExpr)) FatExpr;",
        bounce_flat_call!(Vec<FatExpr>, FatExpr, unquote_macro_apply_placeholders),
    ),
    (
        "fn get_type_int(e: FatExpr) IntTypeInfo;",
        bounce_flat_call!(FatExpr, IntTypeInfo, get_type_int),
    ),
    // Convert a pointer to a value into an ast that will produce that value when evaluated.
    // It is illegal to pass a <ty> that does not match the value behind <ptr>.
    (
        "fn literal_ast(ty: Type, ptr: rawptr) FatExpr;",
        bounce_flat_call!((TypeId, usize), FatExpr, literal_ast),
    ),
    (
        "fn can_assign_types(found: Type, expected: Type) bool;",
        bounce_flat_call!((TypeId, TypeId), bool, type_check_arg),
    ),
    (
        "#macro #outputs(Type) fn enum(Raw: FatExpr, Cases: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, enum_macro),
    ),
    (
        "#macro fn namespace(block: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, namespace_macro),
    ),
    (
        "#macro #outputs(Type) fn tagged(cases: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, tagged_macro),
    ),
    (
        "#macro #outputs(Type) fn struct(fields: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, struct_macro),
    ),
    (
        "#macro #outputs(Symbol) fn symbol(fields: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, symbol_macro),
    ),
    (
        "#macro #outputs(Unit) fn assert_compile_error(fields: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, assert_compile_error_macro),
    ),
    (
        "#macro #outputs(Type) fn type(e: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, type_macro),
    ),
    (
        "#macro #outputs(u32) fn BITS(parts: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, bits_macro),
    ),
    (
        "fn __get_comptime_dispatch_ptr() **i64",
        bounce_flat_call!((), *mut i64, get_dispatch_ptr),
    ),
    (
        "#macro fn resolve(function_type: FatExpr, overload_set_id: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, resolve_os),
    ),
    (
        "#macro fn as(T: FatExpr, e: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, as_macro),
    ),
    (
        "#macro fn Fn(Arg: FatExpr, Ret: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, fn_type_macro),
    ),
    (
        "#macro fn FnPtr(Arg: FatExpr, Ret: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, fn_ptr_type_macro),
    ),
    (
        "#macro fn Fn(Ret: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, fn_type_macro_single),
    ),
    (
        "#macro fn FnPtr( Ret: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, fn_ptr_type_macro_single),
    ),
];

fn as_macro<'p>(compile: &mut Compile<'_, 'p>, (arg, target): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    compile.last_loc = Some(arg.loc);
    let res = compile.as_cast_macro(arg, target);
    hopec(compile, || res)
}

fn enum_macro<'p>(compile: &mut Compile<'_, 'p>, (arg, target): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    compile.last_loc = Some(arg.loc);
    let res = compile.enum_constant_macro(arg, target);
    res.unwrap()
}

extern "C-unwind" fn intern_type<'p>(compile: &mut Compile<'_, 'p>, ty: &TypeInfo<'p>) -> TypeId {
    compile.program.intern_type(ty.clone())
}

fn get_type_info<'p>(compile: &Compile<'_, 'p>, ty: TypeId) -> TypeInfo<'p> {
    compile.program[ty].clone()
}
fn get_type_info_raw<'p>(compile: &Compile<'_, 'p>, ty: TypeId) -> TypeInfo<'p> {
    let ty = compile.program.raw_type(ty);
    compile.program[ty].clone()
}

// TODO: at least use hope(|| ...) instead of unwrapping so much.
//       need to think of better error handling story.

fn const_eval_any<'p>(compile: &mut Compile<'_, 'p>, ((mut expr, ty), addr): ((FatExpr<'p>, TypeId), usize)) {
    // TODO: immediate_eval_expr doesn't do a type check. -- Apr 27
    compile.compile_expr(&mut expr, Some(ty)).unwrap();

    match compile.immediate_eval_expr(expr, ty) {
        Ok(val) => {
            let bytes = val.bytes();
            debug_assert_eq!(bytes.len(), compile.program.get_info(ty).stride_bytes as usize);
            let out = unsafe { &mut *slice_from_raw_parts_mut(addr as *mut u8, bytes.len()) };
            out.copy_from_slice(bytes);
        }
        Err(e) => panic!("{e:?}"),
    }
}

fn compile_ast<'p>(compile: &mut Compile<'_, 'p>, mut expr: FatExpr<'p>) -> FatExpr<'p> {
    compile.last_loc = Some(expr.loc);
    let res = compile.compile_expr(&mut expr, None);
    hopec(compile, || res);
    expr
}

// :UnquotePlaceholders
fn unquote_macro_apply_placeholders<'p>(compile: &mut Compile<'_, 'p>, mut args: Vec<FatExpr<'p>>) -> FatExpr<'p> {
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
        compile.program.next_var = template.renumber_vars(compile.program.next_var, &mut Default::default(), compile);

        Ok(template)
    }; // topdpdwkp[aspefiwfe]e

    let res = doo();
    compile.tag_err(res).unwrap_or_else(|e| panic!("{e:?}"))
}

fn get_type_int<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> IntTypeInfo {
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
                    return Ok(int);
                }
                err!("expected expr of int type not {}", compile.program.log_type(ty));
            }
        }
        err!("expected binary literal not {arg:?}",);
    })
}

fn literal_ast<'p>(compile: &Compile<'_, 'p>, (ty, ptr): (TypeId, usize)) -> FatExpr<'p> {
    let bytes = compile.program.get_info(ty).stride_bytes as usize;
    let value = unsafe { &*slice_from_raw_parts(ptr as *const u8, bytes) };
    let value = Values::many(value.to_vec());
    let loc = compile.last_loc.unwrap_or_else(garbage_loc); // TODO: caller should pass it in?
    FatExpr::synthetic_ty(Expr::Value { value }, loc, ty)
}

fn type_check_arg(compile: &Compile, (found, expected): (TypeId, TypeId)) -> bool {
    compile.type_check_arg(found, expected, "").is_ok()
}

// TODO: what if struct const fields were lazy like normal values and then could use that instead of this.
fn namespace_macro<'p>(compile: &mut Compile<'_, 'p>, mut block: FatExpr<'p>) -> FatExpr<'p> {
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
    compile.as_literal(s, loc).unwrap()
}

fn tagged_macro<'p>(compile: &mut Compile<'_, 'p>, mut cases: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        if let Expr::StructLiteralP(pattern) = &mut cases.expr {
            for b in &mut pattern.bindings {
                if b.default.is_none() && matches!(b.ty, LazyType::Infer) {
                    // @tagged(s: i64, n) is valid and infers n as Unit.
                    b.ty = LazyType::Finished(TypeId::unit);
                }
            }
            let ty = compile.struct_type(pattern)?;
            let ty = compile.program.to_enum(ty);
            compile.set_literal(&mut cases, ty)?;
        } else {
            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", cases);
        }
        Ok(())
    });

    cases
}

fn struct_macro<'p>(compile: &mut Compile<'_, 'p>, mut fields: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        if let Expr::StructLiteralP(pattern) = &mut fields.expr {
            let ty = compile.struct_type(pattern)?;
            let ty = compile.program.intern_type(ty);
            compile.set_literal(&mut fields, ty)?;
        } else {
            err!("expected map literal: .{{ name: Type, ... }} but found {:?}", fields);
        }
        Ok(())
    });

    fields
}

fn symbol_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
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

    arg
}

fn assert_compile_error_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
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

    arg
}

fn type_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
    hope(|| {
        // Note: this does not evaluate the expression.
        // TODO: warning if it has side effects. especially if it does const stuff.
        let ty = compile.compile_expr(&mut arg, None)?;
        compile.set_literal(&mut arg, ty)?;
        Ok(())
    });

    arg
}

fn bits_macro<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
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
                assert!(v < 1 << ty.bit_count);
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

fn get_dispatch_ptr(comp: &mut Compile, _: ()) -> *mut i64 {
    comp.aarch64.get_dispatch() as usize as *mut i64
}
fn resolve_os<'p>(comp: &mut Compile<'_, 'p>, (f_ty, os): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    let loc = os.loc;
    let ty: TypeId = hope(|| comp.immediate_eval_expr_known(f_ty));
    let os: OverloadSetId = hope(|| comp.immediate_eval_expr_known(os));
    let f_ty = comp.program.fn_ty(ty).expect("@resolve arg should be function type.");

    hope(|| comp.compute_new_overloads(os, None));
    // TODO: just filter the iterator.
    let mut overloads = comp.program[os].clone(); // sad

    overloads
        .ready
        .retain(|f| f.arg == f_ty.arg && (f.ret.is_none() || f.ret.unwrap() == f_ty.ret));
    // TODO: You can't just filter here anymore because what if its a Split FuncRef.
    let found = match overloads.ready.len() {
        0 => panic!("Missing overload",),
        1 => overloads.ready[0].func,
        _ => panic!("Ambigous overload \n{:?}", overloads.ready),
    };
    let val = to_values(comp.program, found).unwrap();
    FatExpr::synthetic_ty(Expr::Value { value: val }, loc, ty)
}

fn make_fn_type<'p>(compile: &mut Compile<'_, 'p>, arg: &mut FatExpr<'p>, ret: FatExpr<'p>) -> Res<'p, FnType> {
    assert!(matches!(arg.expr, Expr::StructLiteralP(_)), "expected @fn(name: type, name: type) type");
    let Expr::StructLiteralP(parts) = &mut arg.expr else { unreachable!() };
    parts.if_empty_add_unit();
    let types = compile.infer_pattern(&mut parts.bindings)?;
    let arity = parts.bindings.len() as u16;
    let arg = compile.program.tuple_of(types);
    let ret: TypeId = compile.immediate_eval_expr_known(ret)?;
    let f_ty = FnType { arg, ret, arity };
    Ok(f_ty)
}
fn fn_type_macro<'p>(compile: &mut Compile<'_, 'p>, (mut arg, ret): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    hope(|| {
        let f_ty = make_fn_type(compile, &mut arg, ret)?;
        let ty = compile.program.intern_type(TypeInfo::Fn(f_ty));
        compile.set_literal(&mut arg, ty)?;
        Ok(())
    });

    arg
}

fn fn_ptr_type_macro<'p>(compile: &mut Compile<'_, 'p>, (mut arg, ret): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    hope(|| {
        let f_ty = make_fn_type(compile, &mut arg, ret)?;
        let ty = compile.program.intern_type(TypeInfo::FnPtr {
            ty: f_ty,
            cc: CallConv::CCallReg,
        });
        compile.set_literal(&mut arg, ty)?;
        Ok(())
    });

    arg
}

fn fn_ptr_type_macro_single<'p>(compile: &mut Compile<'_, 'p>, ret: FatExpr<'p>) -> FatExpr<'p> {
    let loc = ret.loc;
    fn_ptr_type_macro(
        compile,
        (FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![], loc }), loc), ret),
    )
}

fn fn_type_macro_single<'p>(compile: &mut Compile<'_, 'p>, ret: FatExpr<'p>) -> FatExpr<'p> {
    let loc = ret.loc;
    fn_type_macro(
        compile,
        (FatExpr::synthetic(Expr::StructLiteralP(Pattern { bindings: vec![], loc }), loc), ret),
    )
}
