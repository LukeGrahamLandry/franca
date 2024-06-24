#![feature(slice_ptr_get)]
#![feature(pattern)]
#![feature(thread_sleep_until)]

use franca::{
    ast::{garbage_loc, FuncId, Program, ScopeId, TargetArch},
    bc::{to_values, Values},
    compiler::{Compile, ExecStyle, Res},
    export_ffi::{ImportVTable, IMPORT_VTABLE},
    find_std_lib, log_err,
    logging::PoolLog,
    make_toplevel,
    scope::ResolveScope,
    self_hosted::self_hosted_main,
    timestamp, MEM, MMAP_ARENA_START, STACK_START, STATS,
};
use std::{
    env,
    ffi::CString,
    fs::{self, File},
    io::Read,
    mem::transmute,
    os::fd::FromRawFd,
    panic::{set_hook, take_hook},
    path::PathBuf,
    process::exit,
    ptr::addr_of,
    str::pattern::Pattern,
};

// TODO: Instead of cli args, what if the arg was a string of code to run so 'franca "start_lsp()"' would concat that on some compiler_cli.txt and run it.
//       Make sure theres some prefix that lets you run/compile the next arg as a file path for shabang line.
//       Maybe that implies I should have syntax for a call that taking the rest of the line as an argument without needing a close paren.
//       Or just passs remaining cli args as args of a function.
//       Because it seems nicer if things are composable and the compiler functions don't assume they can just read the args except the top level one which you can define in userland.
// TODO: repl(). works nicely with ^ so you could experiment but then always be able to run things as a command when working with other tools

const SHOW_MEM_REGIONS: bool = false;

fn main() {
    let marker = 0;
    unsafe { STACK_START = &marker as *const i32 as usize };
    unsafe { MMAP_ARENA_START = MEM.get() as usize };

    // If you get random garbage that looks like a pointer, this can help you figure out where its coming from.
    if SHOW_MEM_REGIONS {
        let prev = take_hook();
        set_hook(Box::new(move |arg| {
            franca::where_am_i();
            prev(arg);
        }));
    }

    unsafe {
        self_hosted_main(addr_of!(IMPORT_VTABLE));
    }
    unreachable!();
}
