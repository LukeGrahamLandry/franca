#![allow(improper_ctypes_definitions)]

use interp_derive::{InterpSend, Reflect};
use libc::c_void;

use crate::ast::{FnType, FuncId, Program, TypeId, TypeInfo};
use crate::bc::Value;
use crate::compiler::Res;
use crate::logging::unwrap;
use crate::pool::Ident;
use std::fmt::Write;
use std::path::PathBuf;
use std::ptr::null;
use std::sync::Mutex;
use std::{fs, io, slice};

// TODO: parse header files for signatures, but that doesn't help when you want to call it at comptime so need the address.
pub const LIBC: &[(&str, *const u8)] = &[
    ("@env fn write(fd: Fd, buf: Ptr(u8), size: usize) isize", libc::write as *const u8),
    ("@env fn getchar() i32", libc::getchar as *const u8),
    ("@env fn putchar(c: i64) i32", libc::putchar as *const u8),  // TODO: c: i32
    ("@env fn exit(status: i64) Never", libc::exit as *const u8), // TODO: status: i32
    ("fn malloc(size: usize) VoidPtr", libc::malloc as *const u8),
    ("fn free(ptr: VoidPtr) Unit", libc::free as *const u8),
    ("@env fn system(null_terminated_cmd: Ptr(u8)) i32", libc::system as *const u8),
    ("@env fn open(null_terminated_path: Ptr(u8), flags: i32) Fd", libc::open as *const u8),
    ("@env fn close(fd: Fd) i32", libc::close as *const u8),
    ("@env fn rand() i32", libc::rand as *const u8),
    ("@env fn get_errno() i32", get_errno as *const u8),
    ("@env fn dlopen(name: CStr, flag: i64) DlHandle", libc::dlopen as *const u8),
    ("@env fn dlsym(lib: DlHandle, name: CStr) VoidPtr", libc::dlsym as *const u8),
    ("@env fn dlclose(lib: DlHandle) i64", libc::dlclose as *const u8),
    ("@env fn puts(s: CStr) i64", libc::puts as *const u8),
    (
        "@env fn mmap(addr: VoidPtr, len: i64, prot: i64, flags: i64, fd: Fd, offset: i64) VoidPtr",
        libc::mmap as *const u8,
    ),
    ("fn munmap(addr: VoidPtr, len: i64) i64", libc::munmap as *const u8),
    ("fn mprotect(addr: VoidPtr, len: i64, prot: i64) i64", libc::mprotect as *const u8),
];

pub const COMPILER: &[(&str, *const u8)] = &[
    ("fn Ptr(Inner: Type) Type", Program::ptr_type as *const u8),
    ("fn Unique(Backing: Type) Type", Program::unique_ty as *const u8),
    ("fn tag_value(E: Type, case_name: Symbol) i64", tag_value as *const u8),
    ("fn tag_symbol(E: Type, tag_value: i64) Symbol", tag_symbol as *const u8),
    ("fn assert_eq(_: i64, __: i64) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: Type, __: Type) Unit", assert_equ32 as *const u8),
    ("fn assert_eq(_: bool, __: bool) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: Symbol, __: Symbol) Unit", assert_equ32 as *const u8), // TODO: subtyping
    ("fn print_int(v: i64) Unit", print_int as *const u8),
    ("fn number_of_functions() i64", number_of_functions as *const u8),
    // TODO: make FuncId a unique type
    ("fn name(func_id: FuncId) Symbol", function_name as *const u8),
    ("fn assert_eq(_: f64, __: f64) Unit", assert_eqf64 as *const u8),
    // TODO: all these type ones could use ffi TypeInfo if i gave it `@ct fn intern_type`
    //       but to do that here instead of current macro message, I'd need to do ffi of enums in a less insane way.
    //       (these functions don't use InterpSend, they just rely on C ABI).
    // Ideally this would just work with tuple syntax but L((a, b), c) === L(a, b, c) !=== L(Ty(a, b), c) because of arg flattening.
    ("fn Ty(fst: Type, snd: Type) Type;", pair_type as *const u8),
    // The type of '@pub fn(Arg) Ret'. This is a comptime only value.
    // All calls are inlined, as are calls that pass one of these as an argument.
    // Captures of runtime variables are allowed since you just inline everything anyway.
    // Const captures behave as you'd expect from first class closures.
    ("fn Fn(Arg: Type, Ret: Type) Type;", fn_type as *const u8),
    // TODO: include calling convention.
    // Like @pub fn(Arg, Ret) but as a runtime value. Same as a function pointer in c but with less insane syntax :).
    // Use '!addr' on a normal @pub fn value to get create a value of this type.
    // - The function cannot have any runtime variable captures,
    //   but they could be implemented on top of this by taking an environment data pointer as an argument.
    // - The function cannot have any const arguments, they must be baked before creating the pointer.
    ("fn FnPtr(Arg: Type, Ret: Type) Type;", fn_ptr_type as *const u8),
];

pub const COMPILER_LATE: &[(&str, *const u8)] = &[
    // TODO: this return a packed string, with the length divided by 8
    //       so its truncated to amultiple of 8 chars but with an unknown amount of memory hanging off the end that you can't free.
    //       WORTHLESS  --Apr, 10
    ("@no_interp fn sym_to_str(s: Symbol) Str", symbol_to_str as *const u8),
    // This a null terminated packed string, useful for ffi with old c functions.
    // Currently it doesn't reallocate because all symbols are null terminated but that might change in future. --Apr, 10
    ("@no_interp fn c_str(s: Symbol) CStr", symbol_to_cstr as *const u8),
    ("fn int(s: Symbol) i64", symbol_to_int as *const u8), // TODO: this should be a noop
    (
        "fn resolve_backtrace_symbol(addr: *u32, out: *RsResolvedSymbol) bool",
        resolve_backtrace_symbol as *const u8,
    ),
];

pub static STDLIB_PATH: Mutex<Option<PathBuf>> = Mutex::new(None);

pub fn get_include_std(name: &str) -> Option<String> {
    let msg = "//! IMPORTANT: don't try to save @comptime_addr('ASLR junk'), it won't go well. \n";
    match name {
        "libc" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            writeln!(
                out,
                "@pub const OpenFlag = @enum(i32) (Read = {}, Write = {}, ReadWrite = {});",
                libc::O_RDONLY,
                libc::O_WRONLY,
                libc::O_RDWR
            )
            .unwrap();
            // TODO: this should be a bitflag. presumably PROT_NONE is always 0.
            //       distinguish between flags whose value is shifting (i.e. 0/1/2 vs 1/2/4)
            writeln!(
                out,
                "@pub const MapProt = @enum(i64) (Exec = {}, Read = {}, Write = {}, None = {});",
                libc::PROT_EXEC,
                libc::PROT_READ,
                libc::PROT_WRITE,
                libc::PROT_NONE
            )
            .unwrap();
            writeln!(
                out,
                "@pub const MapFlag = @enum(i64) (Private = {}, Anonymous = {});",
                libc::MAP_PRIVATE,
                libc::MAP_ANONYMOUS,
            )
            .unwrap();
            writeln!(out, "@pub const DlHandle = VoidPtr; @pub const CStr = Unique$Ptr(i64);").unwrap();
            for (sig, ptr) in LIBC {
                writeln!(out, "@pub @comptime_addr({}) @dyn_link @c_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        "compiler" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            for (sig, ptr) in COMPILER {
                writeln!(out, "@pub @comptime_addr({}) @ct @c_call {sig};", *ptr as usize).unwrap();
            }
            Some(out)
        }
        "compiler_late" => {
            let mut out = String::new();
            writeln!(out, "{}", msg).unwrap();
            for (sig, ptr) in COMPILER_LATE {
                writeln!(out, "@pub @comptime_addr({}) @ct @c_call {sig};", *ptr as usize).unwrap();
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
                println!("Missing path {path:?}");
            } else {
                println!("STDLIB_PATH not set.");
            }
            None
        }
    }
}

// macro_rules! _result_ffi {
//     ($out:ident, $all:ident, $T:ty, $t_name:expr) => {{
//         extern fn is_ok(r: &mut Res<'_, $T>) -> bool {
//             r.is_ok()
//         }
//         extern fn unwrap<'a>(r: &'a mut Res<'_, $T>) -> &'a mut $T {
//             r.as_mut().unwrap()
//         }
//         writeln!($all, "{} = Opaque({}, {}),", $t_name, size_of::<$T>(), align_of::<$T>()).unwrap();
//         writeln!($out, "@comptime_addr({}) @c_call fn is_ok(r: Ptr(CRes.{}[])) bool;", is_ok as *const u8 as usize, $t_name).unwrap();
//         writeln!($out, "@comptime_addr({0}) @c_call fn unwrap(r: Ptr(CRes.{1}[])) Ptr({1});", unwrap as *const u8 as usize, $t_name).unwrap();
//     }};
// }

// TODO: can do some nicer reporting here? maybe this goes away once i can actually do error handling in my language.
fn hope<'p, T>(res: impl FnOnce() -> Res<'p, T>) -> T {
    res().unwrap_or_else(|e| panic!("{e:?}"))
}

extern "C-unwind" fn tag_value<'p>(program: &Program<'p>, enum_ty: TypeId, name: Ident<'p>) -> i64 {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let index = cases.iter().position(|f| f.0 == name);
    let index = hope(|| Ok(unwrap!(index, "bad case name")));
    index as i64
}

extern "C-unwind" fn tag_symbol<'p>(program: &Program<'p>, enum_ty: TypeId, tag_val: i64) -> Ident<'p> {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let case = hope(|| Ok(unwrap!(cases.get(tag_val as usize), "enum tag too high")));
    case.0
}

extern "C-unwind" fn pair_type(program: &mut Program, a: TypeId, b: TypeId) -> TypeId {
    hope(|| {
        assert!(a.0 < program.types.len() as u32, "TypeId OOB {}", a.0);
        assert!(b.0 < program.types.len() as u32, "TypeId OOB {}", b.0);
        Ok(program.intern_type(TypeInfo::Tuple(vec![a, b])))
    })
}

extern "C-unwind" fn fn_type(program: &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        assert!(arg.0 < program.types.len() as u32, "TypeId OOB {}", arg.0);
        assert!(ret.0 < program.types.len() as u32, "TypeId OOB {}", ret.0);
        Ok(program.intern_type(TypeInfo::Fn(FnType { arg, ret })))
    })
}

extern "C-unwind" fn fn_ptr_type(program: &mut Program, arg: TypeId, ret: TypeId) -> TypeId {
    hope(|| {
        assert!(arg.0 < program.types.len() as u32, "TypeId OOB {}", arg.0);
        assert!(ret.0 < program.types.len() as u32, "TypeId OOB {}", ret.0);
        Ok(program.intern_type(TypeInfo::FnPtr(FnType { arg, ret })))
    })
}

// Supports bool, i64, and Type which all have the same repr.
// TODO: track call site
extern "C-unwind" fn assert_eq(program: &mut Program, a: i64, b: i64) {
    hope(|| Ok(assert_eq!(a, b)));
    program.assertion_count += 1;
}

extern "C-unwind" fn assert_equ32(program: &mut Program, a: u32, b: u32) {
    hope(|| Ok(assert_eq!(a, b)));
    program.assertion_count += 1;
}

extern "C-unwind" fn assert_eqf64(program: &mut Program, a: f64, b: f64) {
    hope(|| Ok(assert_eq!(a, b)));
    program.assertion_count += 1;
}

// TODO: test abi
extern "C-unwind" fn symbol_to_str(program: &mut Program, symbol: i64) -> (*const u8, i64) {
    let symbol = symbol as u32;
    hope(|| {
        let symbol = unwrap!(program.pool.upcast(symbol), "invalid symbol");
        let s = program.pool.get(symbol);
        // TODO: fix len stuff
        Ok((s.as_ptr(), (s.len()) as i64 / 2 - 1))
    })
}

// Note: currently StringPool guarentees that they're all null terminated but I don't want to promise that to the language so wrap in this function.
extern "C-unwind" fn symbol_to_cstr(program: &mut Program, symbol: i64) -> *const u8 {
    let symbol = symbol as u32;
    hope(|| {
        let symbol = unwrap!(program.pool.upcast(symbol), "invalid symbol");
        let s = program.pool.get(symbol);
        Ok(s.as_ptr())
    })
}

extern "C-unwind" fn symbol_to_int(_: &mut Program, symbol: u32) -> i64 {
    symbol as i64
}

#[repr(C)]
#[derive(Reflect)]
pub struct RsResolvedSymbol {
    line: i64,
    owned_name: *const u8,
    name_len: i64,
}

extern "C-unwind" fn resolve_backtrace_symbol(_: &mut Program, addr: *mut c_void, out: &mut RsResolvedSymbol) -> i64 {
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

// TODO: fix alloc overload selection on asm
extern "C-unwind" fn print_int(_: &mut Program, a: i64) {
    println!("{a}");
}

extern "C-unwind" fn number_of_functions(program: &mut Program) -> i64 {
    program.funcs.len() as i64
}

extern "C-unwind" fn function_name<'p>(program: &mut Program<'p>, f: FuncId) -> Ident<'p> {
    program[f].name
}

extern "C-unwind" fn get_errno() -> i32 {
    io::Error::last_os_error().raw_os_error().unwrap_or(0)
}

#[cfg(target_arch = "aarch64")]
pub fn copy_to_mmap_exec(code: Vec<u32>) -> (Box<memmap2::Mmap>, *const u8) {
    // TODO: emit into this thing so don't have to copy.
    let mut map = memmap2::MmapOptions::new().len(code.len() * 4).map_anon().unwrap();
    let bytes = code.as_ptr() as *const u8;
    let bytes = unsafe { slice::from_raw_parts(bytes, code.len() * 4) };
    map.copy_from_slice(bytes);
    let map = map.make_exec().unwrap();
    let ptr = map.as_ptr();
    (Box::new(map), ptr)
}

#[derive(Debug, Clone, InterpSend)]
pub struct CmdResult {
    pub status: i32,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}
