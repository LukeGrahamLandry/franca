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
#![allow(improper_ctypes_definitions)] // this ones fair but temporary
extern crate core;

struct MyAllocator;

#[no_mangle]
extern "C" fn main() {
    unsafe {
        self_hosted_main(addr_of!(IMPORT_VTABLE), false);
    }
    unreachable!();
}

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
    pub(crate) fn get(&self) -> *mut u8 {
        unsafe { *self.0.get() }
    }

    pub(crate) fn set(&self, v: *mut u8) {
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

pub const TRACE_CALLS: bool = false;

#[macro_export]
macro_rules! debugln_call {
    ($($arg:tt)*) => {{
        if $crate::TRACE_CALLS  {
            println!($($arg)*);
        }
    }};
}

use std::alloc::GlobalAlloc;
use std::alloc::Layout;
use std::cell::UnsafeCell;
use std::collections::HashMap;

use std::ptr::addr_of;
use std::ptr::null_mut;

use crate::self_hosted::Span;
use bc::Values;
use export_ffi::BigOption;
use export_ffi::IMPORT_VTABLE;
use self_hosted::self_hosted_main;
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
pub mod compiler;
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
    pub(crate) fn_body_resolve: usize,
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

pub(crate) fn log_err<'p>(interp: &Compile<'_, 'p>, e: CompileError<'p>) {
    println!("Internal: {}", e.internal_loc.unwrap());
    let message = e.reason.log(interp.program, interp.program.pool);
    println!("{message}");
    interp.program.pool.print_diagnostic(e);
}

pub(crate) fn make_toplevel<'p>(pool: &SelfHosted<'p>, user_span: Span, stmts: Vec<FatStmt<'p>>) -> Func<'p> {
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

pub(crate) fn extend_options<T>(v: &mut Vec<Option<T>>, index: usize) {
    if v.len() > index {
        return;
    }

    let count = index - v.len() + 1;
    v.reserve(count);
    for _ in 0..count {
        v.push(None);
    }
}
