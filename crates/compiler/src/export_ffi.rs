#![allow(improper_ctypes_definitions)]

use interp_derive::Reflect;
use libc::c_void;

use crate::ast::{garbage_loc, Expr, FatExpr, FnType, FuncId, IntTypeInfo, Program, TypeId, TypeInfo, WalkAst};
use crate::bc::{values_from_ints, Value, Values};
use crate::compiler::{bit_literal, Compile, Res, Unquote};
use crate::err;
use crate::ffi::InterpSend;
use crate::logging::{unwrap, PoolLog};
use crate::pool::Ident;
use std::fmt::Write;
use std::hint::black_box;
use std::mem::transmute;
use std::path::PathBuf;
use std::ptr::{null, slice_from_raw_parts, slice_from_raw_parts_mut};
use std::sync::Mutex;
use std::{fs, io};

// This lets rust _declare_ a flat_call like its normal
// Ideally you could do this with a generic but you can't be generic over a function value whose type depends on other generic parameters.
macro_rules! bounce_flat_call {
    ($Arg:ty, $Ret:ty, $f:ident) => {{
        // weird hack becuase you can't concat_idents! for a function name and you can't declare an unnamed function as an expression.
        mod $f {
            use super::*;
            // TODO: can't do this because of lifetimes. you still get the error message if it doesn't compile so its not a big deal but I find it less clear.
            // const F: fn(compile: &mut Compile, a: $Arg) -> $Ret = $f; // force a typecheck

            pub extern "C-unwind" fn bounce(compile: &mut Compile<'_, '_>, argptr: *mut i64, arg_count: i64, retptr: *mut i64, ret_count: i64) {
                debug_assert_eq!(arg_count, <$Arg>::size() as i64, "bad arg count. expected {}", stringify!($Arg));
                debug_assert_eq!(ret_count, <$Ret>::size() as i64, "bad ret count. expected {}", stringify!($Ret));
                unsafe {
                    let argslice = &mut *slice_from_raw_parts_mut(argptr, arg_count as usize);
                    let arg: $Arg = <$Arg>::deserialize_from_ints(&mut argslice.iter().copied()).unwrap();
                    let ret: $Ret = $f(compile, arg);
                    let ret = ret.serialize_to_ints_one();
                    let out = &mut *slice_from_raw_parts_mut(retptr, ret_count as usize);
                    out.fill(0); // TODO: remove
                    out.copy_from_slice(&ret);
                    argslice.fill(0); // TODO: remove
                }
            }
        }

        $f::bounce
    }};
}

// TODO: parse header files for signatures, but that doesn't help when you want to call it at comptime so need the address.
pub const LIBC: &[(&str, *const u8)] = &[
    ("fn write(fd: Fd, buf: Ptr(u8), size: usize) isize", libc::write as *const u8),
    ("fn getchar() i32", libc::getchar as *const u8),
    ("fn putchar(c: i64) i32", libc::putchar as *const u8),  // TODO: c: i32
    ("fn exit(status: i64) Never", libc::exit as *const u8), // TODO: status: i32
    ("fn malloc(size: usize) VoidPtr", libc::malloc as *const u8),
    ("fn free(ptr: VoidPtr) Unit", libc::free as *const u8),
    ("fn system(null_terminated_cmd: Ptr(u8)) i32", libc::system as *const u8),
    ("fn open(null_terminated_path: Ptr(u8), flags: i32) Fd", libc::open as *const u8),
    ("fn close(fd: Fd) i32", libc::close as *const u8),
    ("fn rand() i32", libc::rand as *const u8),
    ("fn get_errno() i32", get_errno as *const u8),
    ("fn dlopen(name: CStr, flag: i64) DlHandle", libc::dlopen as *const u8),
    ("fn dlsym(lib: DlHandle, name: CStr) VoidPtr", libc::dlsym as *const u8),
    ("fn dlclose(lib: DlHandle) i64", libc::dlclose as *const u8),
    (
        "fn mmap(addr: VoidPtr, len: i64, prot: i64, flags: i64, fd: Fd, offset: i64) VoidPtr",
        libc::mmap as *const u8,
    ),
    ("fn munmap(addr: VoidPtr, len: i64) i64", libc::munmap as *const u8),
    ("fn mprotect(addr: VoidPtr, len: i64, prot: i64) i64", libc::mprotect as *const u8),
    ("fn __clear_cache(beg: VoidPtr, beg: VoidPtr) Unit", __clear_cache as *const u8),
    (
        "fn clock_gettime(clock_id: i64, time_spec: VoidPtr) Unit",
        libc::clock_gettime as *const u8,
    ),
];

// TODO: do this myself
extern "C" {
    pub fn __clear_cache(beg: *mut libc::c_char, end: *mut libc::c_char);
}

// pub fn __clear_cache(beg: *mut libc::c_char, end: *mut libc::c_char);
// IMPORTANT: since compile is repr(C), &mut &mut Program === &mut Compile
pub const COMPILER: &[(&str, *const u8)] = &[
    ("fn Ptr(Inner: Type) Type", do_ptr_type as *const u8),
    ("#no_memo fn Unique(Backing: Type) Type", do_unique_type as *const u8),
    ("fn tag_value(E: Type, case_name: Symbol) i64", tag_value as *const u8),
    ("fn tag_symbol(E: Type, tag_value: i64) Symbol", tag_symbol as *const u8),
    ("fn number_of_functions() i64", number_of_functions as *const u8),
    // TODO: make FuncId a unique type
    ("fn name(func_id: FuncId) Symbol", function_name as *const u8),
    ("fn index_to_func_id(func_index: i64) FuncId", index_to_func_id as *const u8),
    // TODO: all these type ones could use ffi TypeInfo if i gave it `#ct fn intern_type`
    //       but to do that here instead of current macro message, I'd need to do ffi of enums in a less insane way.
    //       (these functions don't use InterpSend, they just rely on C ABI).
    // Ideally this would just work with tuple syntax but L((a, b), c) === L(a, b, c) !=== L(Ty(a, b), c) because of arg flattening.
    ("fn Ty(fst: Type, snd: Type) Type;", pair_type as *const u8),
    // The type of 'fun(Arg) Ret'. This is a comptime only value.
    // All calls are inlined, as are calls that pass one of these as an argument.
    // Captures of runtime variables are allowed since you just inline everything anyway.
    // Const captures behave as you'd expect from first class closures.
    ("fn Fn(Arg: Type, Ret: Type) Type;", fn_type as *const u8),
    // TODO: include calling convention.
    // Like fun(Arg, Ret) but as a runtime value. Same as a function pointer in c but with less insane syntax :).
    // Use '!addr' on a normal fun value to get create a value of this type.
    // - The function cannot have any runtime variable captures,
    //   but they could be implemented on top of this by taking an environment data pointer as an argument.
    // - The function cannot have any const arguments, they must be baked before creating the pointer.
    ("fn FnPtr(Arg: Type, Ret: Type) Type;", fn_ptr_type as *const u8),
    // TODO: this return a packed string, with the length divided by 8
    //       so its truncated to amultiple of 8 chars but with an unknown amount of memory hanging off the end that you can't free.
    //       WORTHLESS  --Apr, 10
    ("fn sym_to_str(s: Symbol) Str", symbol_to_str as *const u8),
    // This a null terminated packed string, useful for ffi with old c functions.
    // Currently it doesn't reallocate because all symbols are null terminated but that might change in future. --Apr, 10
    ("fn c_str(s: Symbol) CStr", symbol_to_cstr as *const u8),
    ("fn int(s: Symbol) i64", symbol_to_int as *const u8), // TODO: this should be a noop
    (
        "fn resolve_backtrace_symbol(addr: *u32, out: *RsResolvedSymbol) bool",
        resolve_backtrace_symbol as *const u8,
    ),
    ("fn debug_log_type(ty: Type) Unit", log_type as *const u8),
    ("fn IntType(bits: i64, signed: bool) Type;", make_int_type as *const u8),
];

pub static STDLIB_PATH: Mutex<Option<PathBuf>> = Mutex::new(None);

pub fn get_include_std(name: &str) -> Option<String> {
    let msg = "//! IMPORTANT: don't try to save #comptime_addr('ASLR junk'), it won't go well. \n";
    match name {
        "libc" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            writeln!(
                out,
                "const OpenFlag = @enum(i32) (Read = {}, Write = {}, ReadWrite = {});",
                libc::O_RDONLY,
                libc::O_WRONLY,
                libc::O_RDWR
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
            writeln!(out, "const DlHandle = VoidPtr; const CStr = Unique$Ptr(i64);").unwrap();
            for (sig, ptr) in LIBC {
                writeln!(out, "#pub #comptime_addr({}) #dyn_link #c_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        "compiler" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            for (sig, ptr) in COMPILER {
                writeln!(out, "#pub #comptime_addr({}) #ct #c_call {sig};", *ptr as usize).unwrap();
            }
            for (sig, ptr) in COMPILER_FLAT {
                writeln!(out, "#pub #comptime_addr({}) #ct #flat_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        _ => {
            // if let Some((_, src)) = LIB.iter().find(|(check, _)| name == *check) {
            //     return Some(src.to_string());
            // }
            let path = STDLIB_PATH.lock();
            let path = path.as_ref().unwrap().as_ref();
            if let Some(path) = path {
                let path = path.join(name);
                if let Ok(src) = fs::read_to_string(&path) {
                    return Some(src);
                }

                // TODO: have a different macro for this (cwd)
                if let Ok(src) = fs::read_to_string(name) {
                    return Some(src);
                }

                println!("Missing path {path:?}");
            } else {
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

extern "C-unwind" fn tag_value<'p>(program: &&Program<'p>, enum_ty: TypeId, name: Ident<'p>) -> i64 {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
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
        Ok(program.intern_type(TypeInfo::Tuple(vec![a, b])))
    })
}

extern "C-unwind" fn fn_type(program: &mut &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);
        assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
        Ok(program.intern_type(TypeInfo::Fn(FnType { arg, ret })))
    })
}

extern "C-unwind" fn fn_ptr_type(program: &mut &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        assert!(arg.as_index() < program.types.len(), "TypeId OOB {:?}", arg);
        assert!(ret.as_index() < program.types.len(), "TypeId OOB {:?}", ret);
        Ok(program.intern_type(TypeInfo::FnPtr(FnType { arg, ret })))
    })
}

// TODO: test abi
extern "C-unwind" fn symbol_to_str(program: &mut &mut Program, symbol: i64) -> (*const u8, i64) {
    let symbol = symbol as u32;
    hope(|| {
        let symbol = unwrap!(program.pool.upcast(symbol), "invalid symbol");
        let s = program.pool.get(symbol);
        // TODO: fix len stuff
        Ok((s.as_ptr(), (s.len()) as i64 / 2 - 1))
    })
}

// Note: currently StringPool guarentees that they're all null terminated but I don't want to promise that to the language so wrap in this function.
extern "C-unwind" fn symbol_to_cstr(program: &mut &mut Program, symbol: i64) -> *const u8 {
    let symbol = symbol as u32;
    hope(|| {
        let symbol = unwrap!(program.pool.upcast(symbol), "invalid symbol");
        let s = program.pool.get_c_str(symbol);
        Ok(s)
    })
}

extern "C-unwind" fn symbol_to_int(_: &mut &mut Program, symbol: u32) -> i64 {
    symbol as i64
}

#[repr(C)]
#[derive(Reflect)]
pub struct RsResolvedSymbol {
    line: i64,
    owned_name: *const u8,
    name_len: i64,
}

extern "C-unwind" fn resolve_backtrace_symbol(_: &mut &mut Program, addr: *mut c_void, out: &mut RsResolvedSymbol) -> i64 {
    let mut success = 0;
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
    println!("{}", p.log_type(a));
}

extern "C-unwind" fn log_ast<'p>(p: &mut Compile<'_, 'p>, a: FatExpr<'p>) {
    println!("{}", a.log(p.program.pool));
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

extern "C-unwind" fn do_unique_type(program: &mut &mut Program<'_>, ty: TypeId) -> TypeId {
    program.unique_ty(ty)
}

extern "C-unwind" fn test_flat_call(compile: &mut Compile, arg: *mut i64, arg_count: i64, ret: *mut i64, ret_count: i64) {
    assert!(arg_count == 3);
    assert!(ret_count == 1);
    let _ = black_box(compile.program.assertion_count); // dereference the pointer.
    unsafe {
        let s = &mut *slice_from_raw_parts_mut(arg, arg_count as usize);
        *ret = (s[0] * s[1]) + s[2];
    }
}
extern "C-unwind" fn test_flat_call_callback(compile: &mut Compile<'_, '_>, arg: *mut i64, arg_count: i64, ret: *mut i64, ret_count: i64) {
    assert!(arg_count == 1);
    assert!(ret_count == 1);
    let _ = black_box(compile.program.assertion_count); // dereference the pointer.
    unsafe {
        let addr = *arg as usize;
        let f: FlatCallFn = transmute(addr);
        *ret = do_flat_call(compile, f, ((10, 5), 7));
        assert_eq!(*ret, 57);
    }
}

pub type FlatCallFn = extern "C-unwind" fn(program: &mut Compile<'_, '_>, arg: *mut i64, arg_count: i64, ret: *mut i64, ret_count: i64);

// This lets rust _call_ a flat_call like its normal
pub fn do_flat_call<'p, Arg: InterpSend<'p>, Ret: InterpSend<'p>>(compile: &mut Compile<'_, 'p>, f: FlatCallFn, arg: Arg) -> Ret {
    let mut arg = arg.serialize_to_ints_one();
    let mut ret = vec![0i64; Ret::size()];
    f(compile, arg.as_mut_ptr(), arg.len() as i64, ret.as_mut_ptr(), ret.len() as i64);
    Ret::deserialize_from_ints(&mut ret.into_iter()).unwrap()
}

// This the interpreter call a flat_call without knowing its types
pub fn do_flat_call_values<'p>(compile: &mut Compile<'_, 'p>, f: FlatCallFn, arg: Values, ret_type: TypeId) -> Res<'p, Values> {
    let ret_count = compile.ready.sizes.slot_count(compile.program, ret_type);
    let mut arg = compile.aarch64.constants.store_to_ints(arg.vec().iter());
    let mut ret = vec![0i64; ret_count];
    f(compile, arg.as_mut_ptr(), arg.len() as i64, ret.as_mut_ptr(), ret.len() as i64);
    let mut out = vec![];
    values_from_ints(compile, ret_type, &mut ret.into_iter(), &mut out)?;
    assert_eq!(out.len(), ret_count);
    Ok(out.into())
}

fn test_flat_call2(_: &mut Compile, ((a, b), c): ((i64, i64), i64)) -> i64 {
    a * b + c
}

type Unit = ();

// TODO: this needs to be less painful. want to have it just parse rust signetures and expose them.
// TODO: documenet which ones can only be used in macros because they need the 'result: &mut FnWip'
pub const COMPILER_FLAT: &[(&str, FlatCallFn)] = &[
    ("fn test_flat_call_fma(a: i64, b: i64, add_this: i64) i64;", test_flat_call),
    ("fn test_flat_call_callback(addr: VoidPtr) i64;", test_flat_call_callback),
    (
        "fn test_flat_call_fma2(a: i64, b: i64, add_this: i64) i64;",
        bounce_flat_call!(((i64, i64), i64), i64, test_flat_call2),
    ),
    ("fn intern_type(ty: TypeInfo) Type;", bounce_flat_call!(TypeInfo, TypeId, intern_type)),
    // TODO: maybe it would be nice if you could override deref so Type acts like a *TypeInfo.
    ("fn get_type_info(ty: Type) TypeInfo;", bounce_flat_call!(TypeId, TypeInfo, get_type_info)),
    (
        "fn const_eval(expr: FatExpr, ty: Type, result: VoidPtr) Unit;",
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
        "fn literal_ast(ty: Type, ptr: VoidPtr) FatExpr;",
        bounce_flat_call!((TypeId, usize), FatExpr, literal_ast),
    ),
    (
        "fn can_assign_types(found: Type, expected: Type) bool;",
        bounce_flat_call!((TypeId, TypeId), bool, type_check_arg),
    ),
    (
        "#macro #outputs(Type) fun enum(Raw: FatExpr, Cases: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, enum_macro),
    ),
    (
        "#macro fun as(T: FatExpr, value: FatExpr) FatExpr;",
        bounce_flat_call!((FatExpr, FatExpr), FatExpr, as_macro),
    ),
    (
        "#macro fun namespace(block: FatExpr) FatExpr;",
        bounce_flat_call!(FatExpr, FatExpr, namespace_macro),
    ),
];

fn enum_macro<'p>(compile: &mut Compile<'_, 'p>, (arg, target): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    let result = compile.pending_ffi.pop().unwrap().unwrap();
    let res = compile.enum_constant_macro(arg, target);
    compile.pending_ffi.push(Some(result));
    res.unwrap()
}

fn as_macro<'p>(compile: &mut Compile<'_, 'p>, (arg, target): (FatExpr<'p>, FatExpr<'p>)) -> FatExpr<'p> {
    let result = compile.pending_ffi.pop().unwrap().unwrap();
    let res = compile.as_cast_macro(unsafe { &mut *result }, arg, target);
    compile.pending_ffi.push(Some(result));
    res.unwrap()
}

fn intern_type<'p>(compile: &mut Compile<'_, 'p>, ty: TypeInfo<'p>) -> TypeId {
    compile.program.intern_type(ty)
}

fn get_type_info<'p>(compile: &Compile<'_, 'p>, ty: TypeId) -> TypeInfo<'p> {
    compile.program[ty].clone()
}

// TODO: at least use hope(|| ...) instead of unwrapping so much.
//       need to think of better error handling story.

fn const_eval_any<'p>(compile: &mut Compile<'_, 'p>, ((mut expr, ty), addr): ((FatExpr<'p>, TypeId), usize)) {
    let result = compile.pending_ffi.pop().unwrap().unwrap();
    // TODO: immediate_eval_expr doesn't do a type check. -- Apr 27
    compile.compile_expr(unsafe { &mut *result }, &mut expr, Some(ty)).unwrap();

    match compile.immediate_eval_expr(expr, ty) {
        Ok(val) => {
            let slots = compile.ready.sizes.slot_count(compile.program, ty);
            let val = compile.aarch64.constants.store_to_ints(val.vec().iter());
            debug_assert_eq!(val.len(), slots);
            let out = unsafe { &mut *slice_from_raw_parts_mut(addr as *mut i64, val.len()) };
            out.copy_from_slice(&val);
        }
        Err(e) => panic!("{e:?}"),
    }
    compile.pending_ffi.push(Some(result));
}

fn compile_ast<'p>(compile: &mut Compile<'_, 'p>, mut expr: FatExpr<'p>) -> FatExpr<'p> {
    let result = compile.pending_ffi.pop().unwrap().unwrap();
    compile.compile_expr(unsafe { &mut *result }, &mut expr, None).unwrap();
    compile.pending_ffi.push(Some(result));
    expr
}

fn unquote_macro_apply_placeholders<'p>(compile: &mut Compile<'_, 'p>, mut args: Vec<FatExpr<'p>>) -> FatExpr<'p> {
    let result = compile.pending_ffi.pop().unwrap().unwrap();

    let mut template = args.pop().unwrap();
    let mut walk = Unquote {
        compiler: compile,
        placeholders: args.into_iter().map(Some).collect(),
        result: unsafe { &mut *result },
    };
    // TODO: rename to handle or idk so its harder to accidently call the walk one directly which is wrong but sounds like it should be right.
    walk.expr(&mut template);
    let placeholders = walk.placeholders;
    assert!(placeholders.iter().all(|a| a.is_none()), "didnt use all arguments");
    compile.program.next_var = template.renumber_vars(compile.program.next_var);

    compile.pending_ffi.push(Some(result));
    template
}

fn get_type_int<'p>(compile: &mut Compile<'_, 'p>, mut arg: FatExpr<'p>) -> IntTypeInfo {
    let result = compile.pending_ffi.pop().unwrap().unwrap();
    let res = hope(|| {
        match &mut arg.expr {
            Expr::Call(_, _) => {
                if let Some((int, _)) = bit_literal(&arg, compile.pool) {
                    return Ok(int);
                }
            }
            Expr::Value { .. } => err!("todo",),
            _ => {
                let ty = unwrap!(compile.type_of(unsafe { &mut *result }, &mut arg)?, "");
                let ty = compile.program.raw_type(ty);
                if let TypeInfo::Int(int) = compile.program[ty] {
                    return Ok(int);
                }
                err!("expected expr of int type not {}", compile.program.log_type(ty));
            }
        }
        err!("expected binary literal not {arg:?}",);
    });
    compile.pending_ffi.push(Some(result));
    res
}

fn literal_ast<'p>(compile: &mut Compile<'_, 'p>, (ty, ptr): (TypeId, usize)) -> FatExpr<'p> {
    let slots = compile.ready.sizes.slot_count(compile.program, ty);
    let value = unsafe { &*slice_from_raw_parts(ptr as *const i64, slots) };
    let mut out = vec![];
    values_from_ints(compile, ty, &mut value.iter().copied(), &mut out).unwrap();
    let value: Values = out.into();
    let loc = compile.last_loc.unwrap_or_else(garbage_loc); // TODO: caller should pass it in?
    FatExpr::synthetic(Expr::Value { ty, value }, loc)
}

fn type_check_arg(compile: &Compile, (found, expected): (TypeId, TypeId)) -> bool {
    compile.type_check_arg(found, expected, "").is_ok()
}

// TODO: what if struct const fields were lazy like normal values and then could use that instead of this.
fn namespace_macro<'p>(compile: &mut Compile<'_, 'p>, mut block: FatExpr<'p>) -> FatExpr<'p> {
    let result = compile.pending_ffi.pop().unwrap().unwrap();
    let loc = block.loc;
    // give any other macros a chance to expand.
    compile.compile_expr(unsafe { &mut *result }, &mut block, Some(TypeId::unit())).unwrap();

    let (s, block) = if let Expr::Block {
        resolved: Some((s, b)),
        result,
        ..
    } = block.expr
    {
        debug_assert!(result.is_raw_unit());
        (s, b)
    } else {
        panic!("expected block for @namespace not {}", block.log(compile.pool))
    };

    compile.pending_ffi.push(Some(result));

    FatExpr::value(Values::Many(vec![Value::I64(s.as_raw()), Value::I64(block as i64)]), TypeId::scope(), loc)
}
