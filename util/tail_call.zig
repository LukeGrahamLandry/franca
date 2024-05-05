pub fn main() void {
    const N = 999999;
    @import("std").debug.print("{}\n", .{add_slow(0, N)});   
}

fn add_slow(a: i64, b: i64) i64 {
    return if(b == 0) a else @call(.always_tail, add_slow, .{a + 1, b - 1});
}
