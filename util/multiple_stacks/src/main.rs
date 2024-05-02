use core::arch::global_asm;
use core::ptr::null_mut;

global_asm!(
    "_test_stack_shit:",
    // setup new stack
    "mov x16, sp",
    "mov sp, x0",
    "sub sp, sp, 32",
    // save the old stack and our return info on the new stack.
    "str x16, [sp]",
    "str lr, [sp, #8]",
    "str fp, [sp, #16]",
    // call a user function
    "mov x0, x2",
    "blr x1",
    // restore return info
    "ldr x16, [sp]", // x16 is allowed to be scratch during function calls in arm c abi so callback can have a return value.
    "ldr lr, [sp, #8]",
    "ldr fp, [sp, #16]",
    // restore stack
    "add sp, sp, 32",
    "mov sp, x16",
    "ret",
);

extern "C" {
    fn test_stack_shit(_new_sp: usize, callback: extern "C" fn(ctx: *mut ()), ctx: *mut ());
}

extern "C" fn do_stuff(ctx: *mut ()) {
    let my_secret_value = [31415926535897u64, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    // std::hint::black_box(&my_secret_value);  // not needed
    println!(
        "Hello world. My stack is at {}, My context ptr is {ctx:p}",
        &my_secret_value as *const u64 as usize
    );
}

fn main() {
    let stack_size = 1 << 10;
    let new_stack = vec![0u64; stack_size];
    let stack_top = new_stack.as_ptr() as usize + (stack_size * 8);
    assert_eq!(stack_top % 16, 0); // arm wants sp to be 16 byte aligned
    dbg!(stack_top);
    do_stuff(null_mut());
    unsafe {
        test_stack_shit(stack_top, do_stuff, null_mut());
    }
    do_stuff(null_mut());

    let mut found = 0;
    for (idx, v) in new_stack.iter().enumerate() {
        if *v == 31415926535897u64 {
            found += 1;
            println!("Found: {:?}", &new_stack[idx..idx + 10]);
        }
    }
    assert_eq!(found, 1);
}
