#![allow(improper_ctypes_definitions)]

use crate::ast::{Program, TypeId};
use crate::pool::Ident;
use std::fmt::Write;
use std::slice;
use crate::compiler::Res;
use crate::logging::{unwrap, ice, err};

// TODO: parse header files for signatures, but that doesn't help when you want to call it at comptime so need the address.
pub const LIBC: &[(&str, *const u8)] = &[
    ("fn write(fd: i32, buf: Ptr(u8), size: usize) isize", libc::write as *const u8),
    ("fn getchar() i32", libc::getchar as *const u8),
    ("fn putchar(c: i32) i32", libc::putchar as *const u8),
    ("fn exit(status: i32) Never", libc::exit as *const u8),
    ("fn malloc(size: usize) VoidPtr", libc::malloc as *const u8),
    ("fn free(ptr: VoidPtr) Unit", libc::free as *const u8),
    ("fn system(null_terminated_cmd: Ptr(u8)) i32", libc::system as *const u8),
];

pub const COMPILER: &[(&str, *const u8)] = &[
    ("fn Ptr(Inner: Type) Type", Program::ptr_type as *const u8),
    ("fn Unique(Backing: Type) Type", Program::unique_ty as *const u8),
    ("fn tag_value(E: Type, case_name: Symbol) i64", tag_value as *const u8),
    ("fn tag_symbol(E: Type, tag_value: i64) Symbol", tag_symbol as *const u8),
    ("fn assert_eq(_: i64, __: i64) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: Type, __: Type) Unit", assert_eq as *const u8),
    ("fn assert_eq(_: bool, __: bool) Unit", assert_eq as *const u8),
];

pub fn get_special_functions() -> String {
    let mut out = String::new();
    writeln!(out, "//! IMPORTANT: don't try to save @comptime_addr('ASLR junk'), it won't go well. \n").unwrap();
    for (sig, ptr) in COMPILER {
        writeln!(out, "@comptime_addr({}) @ct @c_call {sig};", *ptr as usize).unwrap();
    }
    writeln!(out).unwrap();
    for (sig, ptr) in LIBC {
        writeln!(out, "@comptime_addr({}) @dyn_link @c_call {sig};", *ptr as usize).unwrap();
    }
    out
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
    res().unwrap_or_else(|e| {
        panic!("{e:?}")
    })
}

pub extern fn tag_value<'p>(program: &Program<'p>, enum_ty: TypeId, name: Ident<'p>) -> i64 {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let index = cases.iter().position(|f| f.0 == name);
    let index = hope(|| Ok(unwrap!(index, "bad case name")));
    index as i64
}

pub extern fn tag_symbol<'p>(program: &Program<'p>, enum_ty: TypeId, tag_val: i64) -> Ident<'p> {
    let cases = hope(|| Ok(unwrap!(program.get_enum(enum_ty), "{} is not enum.", program.log_type(enum_ty))));
    let case =  hope(|| Ok(unwrap!(cases.get(tag_val as usize),  "enum tag too high")));
    case.0
}

// Supports bool, i64, and Type which all have the same repr.
// TODO: track call site
pub extern fn assert_eq<'p>(program: &mut Program, a: i64, b: i64) {
    hope(|| Ok(assert_eq!(a, b)));
    program.assertion_count += 1;
}

#[cfg(target_arch = "aarch64")]
pub fn copy_to_mmap_exec(code: Vec<u32>) -> (Box<memmap2::Mmap>, *const u8) {
    // TODO: emit into this thing so don't have to copy.
    let mut map = memmap2::MmapOptions::new()
        .len(code.len() * 4)
        .map_anon()
        .unwrap();
    let bytes = code.as_ptr() as *const u8;
    let bytes = unsafe { slice::from_raw_parts(bytes, code.len() * 4) };
    map.copy_from_slice(bytes);
    let map = map.make_exec().unwrap();
    let ptr = map.as_ptr();
    (Box::new(map), ptr)
}
