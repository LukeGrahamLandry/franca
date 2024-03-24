#![allow(improper_ctypes_definitions)]

use crate::ast::{Program, TypeId, TypeInfo};
use crate::compiler::Res;
use crate::logging::unwrap;
use crate::pool::Ident;
use std::fmt::Write;
use std::slice;

macro_rules! stdlib {
    ($name:expr) => {
        (concat!($name, ".fr"), include_str!(concat!("../../../lib/", $name, ".fr")))
    };
}

static LIB: &[(&str, &str)] = &[
    stdlib!("lib"),
    stdlib!("core"),
    stdlib!("codegen/aarch64/basic.gen"),
    stdlib!("codegen/llvm/basic"),
    stdlib!("collections"),
    stdlib!("system"),
    stdlib!("ast"),
    stdlib!("macros"),
    stdlib!("codegen/aarch64/instructions"),
    stdlib!("codegen/aarch64/basic"),
    stdlib!("codegen/wasm/instructions"),
];

// TODO: parse header files for signatures, but that doesn't help when you want to call it at comptime so need the address.
pub const LIBC: &[(&str, *const u8)] = &[
    ("@env fn write(fd: Fd, buf: Ptr(u8), size: usize) isize", libc::write as *const u8),
    ("@env fn getchar() i32", libc::getchar as *const u8),
    ("@env fn putchar(c: i32) i32", libc::putchar as *const u8),
    ("@env fn exit(status: i32) Never", libc::exit as *const u8),
    ("fn malloc(size: usize) VoidPtr", libc::malloc as *const u8),
    ("fn free(ptr: VoidPtr) Unit", libc::free as *const u8),
    ("@env fn system(null_terminated_cmd: Ptr(u8)) i32", libc::system as *const u8),
    ("@env fn open(null_terminated_path: Ptr(u8), flags: i32) Fd", libc::open as *const u8),
    ("@env fn close(fd: Fd) i32", libc::close as *const u8),
];

pub const COMPILER: &[(&str, *const u8)] = &[
    ("fn Ptr(Inner: Type) Type", Program::ptr_type as *const u8),
    ("fn Unique(Backing: Type) Type", Program::unique_ty as *const u8),
    ("fn tag_value(E: Type, case_name: Symbol) i64", tag_value as *const u8),
    ("fn tag_symbol(E: Type, tag_value: i64) Symbol", tag_symbol as *const u8),
    ("fn assert_eq(_: i64, __: i64) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: Type, __: Type) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: bool, __: bool) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: Symbol, __: Symbol) Unit", assert_eq as *const u8), // TODO: subtyping
    ("fn Array(T: Type, count: usize) Type", array_type as *const u8),
];

pub const COMPILER_LATE: &[(&str, *const u8)] = &[("@no_interp fn str(s: Symbol) Str", symbol_to_str as *const u8)];

pub fn get_include_std(name: &str) -> Option<String> {
    if let Some((_, src)) = LIB.iter().find(|(check, _)| name == *check) {
        return Some(src.to_string());
    }

    let mut out = String::new();
    writeln!(out, "//! IMPORTANT: don't try to save @comptime_addr('ASLR junk'), it won't go well. \n").unwrap();
    match name {
        "libc" => {
            for (sig, ptr) in LIBC {
                writeln!(out, "@comptime_addr({}) @dyn_link @c_call {sig};", *ptr as usize).unwrap();
            }
        }
        "compiler" => {
            writeln!(
                out,
                "const OpenFlag = @enum(i32) (Read = {}, Write = {}, ReadWrite = {});",
                libc::O_RDONLY,
                libc::O_WRONLY,
                libc::O_RDWR
            )
            .unwrap();
            for (sig, ptr) in COMPILER {
                writeln!(out, "@comptime_addr({}) @ct @c_call {sig};", *ptr as usize).unwrap();
            }
        }
        "compiler_late" => {
            for (sig, ptr) in COMPILER_LATE {
                writeln!(out, "@comptime_addr({}) @ct @c_call {sig};", *ptr as usize).unwrap();
            }
        }
        _ => return None,
    }

    Some(out)
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
fn hope<'p, T>(res: impl Fn() -> Res<'p, T>) -> T {
    res().unwrap_or_else(|e| panic!("{e:?}"))
}

pub extern "C" fn tag_value<'p>(program: &Program<'p>, enum_ty: TypeId, name: Ident<'p>) -> i64 {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let index = cases.iter().position(|f| f.0 == name);
    let index = hope(|| Ok(unwrap!(index, "bad case name")));
    index as i64
}

pub extern "C" fn tag_symbol<'p>(program: &Program<'p>, enum_ty: TypeId, tag_val: i64) -> Ident<'p> {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let case = hope(|| Ok(unwrap!(cases.get(tag_val as usize), "enum tag too high")));
    case.0
}

// Supports bool, i64, and Type which all have the same repr.
// TODO: track call site
pub extern "C" fn assert_eq(program: &mut Program, a: i64, b: i64) {
    hope(|| Ok(assert_eq!(a, b)));
    program.assertion_count += 1;
}

// TODO: test. need to allow indexing tuple elements for this to be usable.
// TODO: more efficient storage. TypeInfo::Array(Type, usize) but then need to cleanup everywhere that specifically handles a tuple.
pub extern "C" fn array_type(program: &mut Program, ty: TypeId, count: usize) -> TypeId {
    let types = vec![ty; count];
    program.intern_type(TypeInfo::Tuple(types))
}

// TODO: test abi
pub extern "C" fn symbol_to_str(program: &mut Program, symbol: i64) -> (*const u8, i64) {
    hope(|| {
        let _symbol = unwrap!(program.pool.upcast(symbol), "invalid symbol");
        todo!(); // TODO: bytes or expand into int array
                 // Ok(("abcd".as_ptr(), 1))
    })
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