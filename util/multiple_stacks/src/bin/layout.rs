struct Inner {
    big: i64,
    small: u8,
}

struct Nested {
    inner: Inner,
    last: u8,
}

struct Together {
    big: i64,
    small: u8,
    last: u8,
}

use std::ptr::addr_of;
fn main() {
    {
        let value: Nested = unsafe { std::mem::uninitialized() };
        println!("Nested: \n");
        let start = (&value as *const Nested as usize);
        println!("big: {}", (addr_of!(value.inner.big) as usize) - start);
        println!("small: {}", (&value.inner.small as *const u8 as usize) - start);
        println!("last: {}", (&value.last as *const u8 as usize) - start);
    }
    {
        let value: Together = unsafe { std::mem::uninitialized() };
        println!("Together: \n");
        let start = (&value as *const Together as usize);
        println!("big: {}", (&value.big as *const i64 as usize) - start);
        println!("small: {}", (&value.small as *const u8 as usize) - start);
        println!("last: {}", (&value.last as *const u8 as usize) - start);
    }
}
