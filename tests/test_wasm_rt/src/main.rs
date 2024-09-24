use wasmtime::{Caller, Engine, Linker, Module, Store};

fn main() {
    let mut args = std::env::args();
    args.next().unwrap();
    let file = args.next().expect("pass path to wasm file");
    let engine = Engine::default();
    let module = Module::from_file(&engine, file).unwrap();

    let mut linker = Linker::new(&engine);
    linker.func_wrap("env", "write", write).unwrap();
    linker.func_wrap("env", "mmap", mmap).unwrap();
    linker.func_wrap("env", "abort", abort).unwrap();
    linker.func_wrap("env", "munmap", munmap).unwrap();
    linker
        .func_wrap("env", "clock_gettime", clock_gettime)
        .unwrap();
    linker.func_wrap("env", "malloc", malloc).unwrap();
    linker.func_wrap("env", "free", free).unwrap();
    linker.func_wrap("env", "memcpy", memcpy).unwrap(); // TODO: ugh. don't generate calls to this

    let mut store = Store::new(&engine, HostCtx::default());
    let instance = linker.instantiate(&mut store, &module).unwrap();
    match instance.get_typed_func::<(), i64>(&mut store, "main") {
        Ok(f) => {
            f.call(&mut store, ()).unwrap();
        }
        Err(_) => {
            let f = instance
                .get_typed_func::<(), ()>(&mut store, "main")
                .unwrap();
            f.call(&mut store, ()).unwrap();
        }
    }
}

use core::str;
use std::{
    mem::zeroed,
    process::exit,
    ptr::{slice_from_raw_parts, slice_from_raw_parts_mut},
};

#[derive(Default)]
pub struct HostCtx {
    bump: i64,
}

fn get_str(mut caller: Caller<'_, HostCtx>, ptr: i32, len: i64) -> &str {
    let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
    unsafe {
        let start = memory.data_ptr(&caller).offset(ptr as isize);
        str::from_utf8(&*slice_from_raw_parts(start, len as usize)).unwrap()
    }
}

fn alloc(mut caller: Caller<'_, HostCtx>, len: i64) -> i32 {
    let heap_base = caller
        .get_export("__heap_base")
        .unwrap()
        .into_global()
        .unwrap()
        .get(&mut caller)
        .unwrap_i32();
    let start = heap_base as i64 + caller.data().bump;
    caller.data_mut().bump += len;
    // TODO: why is it not mutable? i can mutate it just fine in the browser
    // let heap_end = caller.get_export("__heap_end").unwrap().into_global().unwrap();
    // let end = heap_end.get(&mut caller).unwrap_i32();
    // if start + len > end as i64 {
    //     let page_size = 65536;
    //     let pages = len / page_size + 1;
    //     let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
    //     memory.grow(&mut caller, pages as u64).unwrap();
    //     heap_end.set(&mut caller, Val::I32((end as i64 + pages * page_size) as i32)).unwrap();
    // }

    start as i32
}

pub fn write(caller: Caller<'_, HostCtx>, fd: i32, ptr: i32, len: i64) -> i64 {
    let msg = get_str(caller, ptr, len);
    match fd {
        1 => print!("{}", msg),
        2 => eprint!("{}", msg),
        _ => panic!("can only write to stdout/stderr. {}", msg),
    };
    len
}

pub fn clock_gettime(mut caller: Caller<'_, HostCtx>, clock_id: i64, ptr: i32) {
    let memory = caller.get_export("memory").unwrap().into_memory().unwrap();
    unsafe {
        let mut p = zeroed::<libc::timespec>();
        libc::clock_gettime(clock_id as u32, &mut p);
        let ptr = memory.data_ptr(&caller).offset(ptr as isize);
        *(ptr as *mut libc::timespec) = p;
    }
}

pub fn abort() {
    exit(1);
}

pub fn mmap(
    caller: Caller<'_, HostCtx>,
    addr: i32,
    len: i64,
    prot: i64,
    flags: i64,
    fd: i32,
    offset: i64,
) -> i32 {
    alloc(caller, len)
}

pub fn munmap(caller: Caller<'_, HostCtx>, addr: i32, len: i64) -> i64 {
    // TOD0
    0
}

pub fn malloc(caller: Caller<'_, HostCtx>, len: i64) -> i32 {
    let len = len + 16 - len % 16;
    alloc(caller, len)
}

pub fn free(caller: Caller<'_, HostCtx>, ptr: i32) {
    // TODO
}

pub fn memcpy(mut caller: Caller<'_, HostCtx>, dest: i32, src: i32, len: i32) -> i32 {
    let memory = caller.get_export("memory").unwrap().into_memory().unwrap();

    unsafe {
        let dest = memory.data_ptr(&caller).offset(dest as isize);
        let src = memory.data_ptr(&caller).offset(src as isize);
        let src = &mut *slice_from_raw_parts_mut(src, len as usize);
        let dest = &*slice_from_raw_parts_mut(dest, len as usize);
        src.copy_from_slice(dest);
    }
    dest
}
