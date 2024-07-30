#![feature(slice_ptr_get)]
#![feature(pattern)]
#![feature(thread_sleep_until)]

use franca::{export_ffi::IMPORT_VTABLE, self_hosted::self_hosted_main};
use std::ptr::addr_of;

// TODO: Instead of cli args, what if the arg was a string of code to run so 'franca "start_lsp()"' would concat that on some compiler_cli.txt and run it.
//       Make sure theres some prefix that lets you run/compile the next arg as a file path for shabang line.
//       Maybe that implies I should have syntax for a call that taking the rest of the line as an argument without needing a close paren.
//       Or just passs remaining cli args as args of a function.
//       Because it seems nicer if things are composable and the compiler functions don't assume they can just read the args except the top level one which you can define in userland.
// TODO: repl(). works nicely with ^ so you could experiment but then always be able to run things as a command when working with other tools

fn main() {
    unsafe {
        self_hosted_main(addr_of!(IMPORT_VTABLE), false);
    }
    unreachable!();
}
