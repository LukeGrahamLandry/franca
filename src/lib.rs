#![feature(ptr_metadata)]
#![feature(iter_array_chunks)]
#![feature(vec_into_raw_parts)]
#![feature(slice_ptr_get)]
#![feature(closure_track_caller)]
#![feature(sync_unsafe_cell)]
#![feature(naked_functions)]
#![feature(const_trait_impl)]
#![feature(try_trait_v2)]
#![feature(str_from_raw_parts)]
// bro if you can tell you could compile it more efficiently why don't you just compile it more efficiently
#![allow(clippy::format_collect)]
extern crate core;

struct MyAllocator;

pub const ARENA_SIZE: usize = 1 << 30;

#[cfg(feature = "be_thread_safe")]
thread_local! {
     pub static MEM: Cell<*mut u8> = Cell::new(alloc_arena());
}

#[cfg(not(feature = "be_thread_safe"))]
pub static MEM: NotThreadSafe = NotThreadSafe(UnsafeCell::new(null_mut()));

pub struct NotThreadSafe(UnsafeCell<*mut u8>);
unsafe impl Sync for NotThreadSafe {}

impl NotThreadSafe {
    pub fn get(&self) -> *mut u8 {
        unsafe { *self.0.get() }
    }

    pub fn set(&self, v: *mut u8) {
        unsafe { *self.0.get() = v };
    }
}

fn alloc_arena() -> *mut u8 {
    unsafe {
        libc::mmap(
            null_mut(),
            ARENA_SIZE,
            libc::PROT_WRITE | libc::PROT_READ,
            libc::MAP_ANON | libc::MAP_PRIVATE,
            -1,
            0,
        ) as *mut u8
    }
}

unsafe impl GlobalAlloc for MyAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut ptr = MEM.get();
        if cfg!(not(feature = "be_thread_safe")) && ptr.is_null() {
            ptr = alloc_arena();
        }
        let ptr = ptr.add(ptr.align_offset(layout.align().max(8)));
        MEM.set(ptr.add(layout.size()));
        ptr
    }

    unsafe fn dealloc(&self, _: *mut u8, _: Layout) {
        // leak it
    }
}

// TODO: i now rely on not freeing things :( -- Jun 12
// TODO: #[cfg(not(feature="free_memory"))]
// Apr 29. this makes (release mode) tests ~20% faster.
#[global_allocator]
static GLOBAL: MyAllocator = MyAllocator;

pub type Map<K, V> = HashMap<K, V>;

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {{
        if $crate::bc_to_asm::TRACE_ASM {
            print!($($arg)*);
        }
    }};
}

#[macro_export]
macro_rules! debugln {
    ($($arg:tt)*) => {{
        if $crate::bc_to_asm::TRACE_ASM  {
            println!($($arg)*);
        }
    }};
}

#[macro_export]
macro_rules! debugln_call {
    ($($arg:tt)*) => {{
        if $crate::bc_to_asm::TRACE_CALLS  {
            println!($($arg)*);
        }
    }};
}

use std::alloc::GlobalAlloc;
use std::alloc::Layout;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::ptr::null_mut;

use crate::self_hosted::Span;
use bc::Values;
use export_ffi::BigOption;
use export_ffi::STDLIB_PATH;
use self_hosted::SelfHosted;

macro_rules! mut_replace {
    ($value:expr, $f:expr) => {{
        let temp = mem::take(&mut $value);
        #[allow(clippy::redundant_closure_call)]
        let res: Res<_> = $f(temp);
        if res.is_err() {
            println!("WARNING: mut_replace hit err. something will be lost!! {}. {res:?}", Location::caller());
        }
        let (temp, out) = res?;
        $value = temp;
        out
    }};
}

pub mod ast;
pub mod bc;
pub mod bc_to_asm;

pub mod compiler;
#[cfg(feature = "cranelift")]
pub mod cranelift;
pub mod emit_bc;
pub mod export_ffi;
pub mod ffi;
pub mod logging;
pub mod overloading;

pub mod self_hosted;

use crate::{
    ast::{Expr, FatExpr, FatStmt, Func, TypeId},
    compiler::{Compile, CompileError},
};

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Stats {
    pub ast_expr_nodes_all: usize,
    pub fn_body_resolve: usize,
    pub make_lit_fn: usize,
    pub parser_queue: usize,
    pub parser_did: usize,
    pub jit_mprotect: usize,
    pub ast_expr_nodes_parser_only: usize,
    pub compile_expr_calls_all: usize,
    pub compile_expr_calls_with_done_set: usize,
    pub expr_fmt: usize,
    pub const_eval_node: usize,
    pub jit_call: usize,
    pub bytecodes: usize,
}

pub static mut STATS: Stats = Stats {
    ast_expr_nodes_all: 0,
    fn_body_resolve: 0,
    make_lit_fn: 0,
    parser_queue: 0,
    jit_mprotect: 0,
    parser_did: 0,
    ast_expr_nodes_parser_only: 0,
    compile_expr_calls_all: 0,
    compile_expr_calls_with_done_set: 0,
    expr_fmt: 0,
    const_eval_node: 0,
    jit_call: 0,
    bytecodes: 0,
};

// TODO: feature = "bundle_stdlib"
/*
macro_rules! include_std {
    ($name:expr) => {
        (concat!($name, ".fr"), include_str!(concat!("../../../lib/", $name, ".fr")))
    };
}

pub const INCLUDE_STD: &[(&str, &str)] = &[
    include_std!("core"),
    include_std!("ast"),
    include_std!("fmt"),
    include_std!("list"),
    include_std!("map"),
    include_std!("mem"),
    include_std!("option"),
    include_std!("pointers"),
    include_std!("prelude"),
    include_std!("macros"),
    include_std!("testing"),
    include_std!("slice"),
    include_std!("system"),
    include_std!("codegen/aarch64/basic"),
    include_std!("codegen/aarch64/basic.gen"),
    include_std!("codegen/aarch64/instructions"),
    include_std!("codegen/aarch64/unwind"),
    include_std!("codegen/bf/instructions"),
    include_std!("codegen/llvm/basic"),
    include_std!("codegen/wasm/instructions"),
];
*/

pub static mut STACK_START: usize = 0;
pub static mut JITTED_PAGE: (usize, usize) = (0, 0);
pub static mut MY_CONST_DATA: (usize, usize) = (0, 0);
pub static mut STACK_MIN: usize = usize::MAX;
pub static mut COMPILER_CTX_PTR: usize = 0;
pub static mut MMAP_ARENA_START: usize = 0;

pub static MY_STRING: &str = "Hello World";

#[inline(never)]
pub fn where_am_i() {
    let marker = 0;
    println!("ADDR OF RUST FUNCTION: {}", where_am_i as usize);
    println!("ADDR OF RUST CONSTANT DATA: {}", MY_STRING.as_ptr() as usize);
    println!(
        "ADDR OF RUST STACK: {} to {} (min: {})",
        unsafe { STACK_START },
        &marker as *const i32 as usize,
        unsafe { STACK_MIN }
    );
    println!("COMPILER_CTX_PTR: {}", unsafe { COMPILER_CTX_PTR });
    println!("ADDR OF MMAP ARENA: {} to {} ", unsafe { MMAP_ARENA_START }, MEM.get() as usize);
    println!("ADDR OF MMAP JITTED: {} to {}", unsafe { JITTED_PAGE.0 }, unsafe {
        JITTED_PAGE.0 + JITTED_PAGE.1
    });
    println!("ADDR OF MMAP CONST DATA: {} to {}", unsafe { MY_CONST_DATA.0 }, unsafe {
        MY_CONST_DATA.0 + MY_CONST_DATA.1
    });
}

// I'd rather include it in the binary but I do this so I don't have to wait for the compiler to recompile every time I change the lib
// (maybe include_bytes in a seperate crate would make it better)
// I also like that users can put the lib somewhere an edit it for thier program. I dont want the compiler to just force its blessed version.
// But I also don't want it to be like c where you just get whatever the system happens to have.
pub fn find_std_lib() -> bool {
    fn check(mut p: PathBuf) -> bool {
        p.push("lib");
        p.push("franca_stdlib_1.fr");
        if p.exists() {
            p.pop();
            let mut path = STDLIB_PATH.lock().unwrap();
            *path = Some(p);
            return true;
        }
        false
    }

    // if a project wants to supply its own version, that should take priority.
    if let Ok(mut p) = env::current_dir() {
        if check(p.clone()) {
            return true;
        }
        p.push("franca");
        if check(p.clone()) {
            return true;
        }
        p.pop();
        p.push("vendor/franca");
        if check(p.clone()) {
            return true;
        }
    }

    if let Ok(mut p) = env::current_exe() {
        p.pop();
        p.push("franca");
        if check(p.clone()) {
            return true;
        }
        // exe might be in franca/target/release/franca or franca/target/debug/deps/compiler-21be1aa281dbe5d6, so go up
        for _ in 0..5 {
            p.pop();
            if check(p.clone()) {
                return true;
            }
        }
    }

    let p = PathBuf::from("/Users/luke/Documents/mods/infered");
    if check(p) {
        return true;
    }

    false
}

pub fn log_err<'p>(interp: &Compile<'_, 'p>, e: CompileError<'p>) {
    println!("ERROR");
    println!("{}", e.trace);

    println!("Internal: {}", e.internal_loc.unwrap());
    let message = e.reason.log(interp.program, interp.program.pool);
    println!("{message}");
    interp.program.pool.print_diagnostic(e);
}

pub fn make_toplevel<'p>(pool: &SelfHosted<'p>, user_span: Span, stmts: Vec<FatStmt<'p>>) -> Func<'p> {
    let name = pool.intern("@toplevel@");
    let body = Some(FatExpr::synthetic(
        Expr::Block {
            body: stmts,
            result: Box::new(FatExpr::synthetic_ty(
                Expr::Value {
                    value: Values::unit(),
                    coerced: false,
                },
                user_span,
                TypeId::unit,
            )),
            ret_label: BigOption::None,
            hoisted_constants: false,
        },
        user_span,
    ));

    let (g_arg, g_ret) = Func::known_args(TypeId::unit, TypeId::unit, user_span);
    Func::new(name, g_arg, g_ret, body, user_span, false)
}

pub fn timestamp() -> f64 {
    use std::time::SystemTime;
    SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64()
}

macro_rules! impl_index_imm {
    ($container:ty, $idx:ty, $elem:ty, $field:ident) => {
        impl<'p> std::ops::Index<$idx> for $container {
            type Output = $elem;

            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index.as_index()]
            }
        }
    };
}

macro_rules! impl_index {
    ($container:ty, $idx:ty, $elem:ty, $field:ident) => {
        $crate::impl_index_imm!($container, $idx, $elem, $field);

        impl<'p> std::ops::IndexMut<$idx> for $container {
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index.as_index()]
            }
        }
    };
}

pub(crate) use impl_index;
pub(crate) use impl_index_imm;

pub fn extend_options<T>(v: &mut Vec<Option<T>>, index: usize) {
    if v.len() > index {
        return;
    }

    let count = index - v.len() + 1;
    v.reserve(count);
    for _ in 0..count {
        v.push(None);
    }
}
pub fn extend_options2<T>(v: &mut Vec<BigOption<T>>, index: usize) {
    if v.len() > index {
        return;
    }

    let count = index - v.len() + 1;
    v.reserve(count);
    for _ in 0..count {
        v.push(BigOption::None);
    }
}

fn pops<T>(v: &mut Vec<T>, count: usize) {
    for _ in 0..count {
        v.pop().unwrap();
    }
}

// There must be a not insane way to do this but i gave up and read the two's complement wikipedia page.
/// Convert an i64 to an i<bit_count> with the (64-<bit_count>) leading bits 0.
fn signed_truncate(mut x: i64, bit_count: i64) -> i64 {
    debug_assert!(x > -(1 << (bit_count)) && (x < (1 << (bit_count))));
    let mask = (1 << bit_count) - 1;
    if x < 0 {
        x *= -1;
        x = !x;
        x += 1;
        x &= mask;
    }
    x
}

// Feels like this might be useful for representing padding?
// But maybe it makes more sense to do it in chunks since its generally all at the end (when not repr(C)?).
#[repr(C, i64)]
#[derive(Clone, Debug)]
pub enum BitSet {
    Small([usize; 2]),
    Big(Vec<usize>),
}

const BITS: usize = usize::BITS as usize;
impl BitSet {
    /// Whatever capacity you can get without allocating.
    pub fn empty() -> BitSet {
        BitSet::Small([0, 0])
    }

    pub fn with_capacity(size: usize) -> Self {
        if size <= 128 {
            BitSet::Small([0, 0])
        } else {
            BitSet::Big(vec![0; size / BITS + 1])
        }
    }

    pub fn get(&self, i: usize) -> bool {
        let v = match self {
            BitSet::Small(v) => v.as_ref(),
            BitSet::Big(v) => v.as_ref(),
        };
        if i >= v.len() * BITS {
            return false;
        }
        let index = i / BITS;
        let bit = i % BITS;
        (v[index] & (1 << bit)) != 0
    }

    pub fn put(&mut self, i: usize, value: bool) {
        let v = match self {
            BitSet::Small(v) => v.as_mut(),
            BitSet::Big(v) => v.as_mut(),
        };
        let index = i / BITS;
        let bit = i % BITS;
        if value {
            v[index] |= (value as usize) << bit;
        } else {
            v[index] &= !((value as usize) << bit);
        }
    }

    pub fn set(&mut self, i: usize) {
        self.insert(i, true)
    }

    pub fn insert(&mut self, i: usize, value: bool) {
        match self {
            BitSet::Small(v) => {
                if i >= 128 {
                    *self = Self::Big(v.to_vec());
                    self.insert(i, value);
                } else {
                    self.put(i, value);
                }
            }
            BitSet::Big(v) => {
                while i >= (v.len() * BITS) {
                    v.push(0);
                }
            }
        }
        self.put(i, value)
    }

    pub fn clear(&mut self) {
        match self {
            BitSet::Small(v) => {
                *v = [0, 0];
            }
            BitSet::Big(v) => {
                v.clear();
            }
        }
    }
}

impl Default for BitSet {
    fn default() -> Self {
        Self::empty()
    }
}
