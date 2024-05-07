/// am i allowed to just write random shit off the end of the stack? 

pub fn main() void {
    const sp = get_stack_pointer();
    for (sp, 0..100) |*ptr, _| {
        @import("std").debug.print("{}, ", .{ptr.*});
        ptr.* = 100;
    }
    
    @import("std").debug.print("\nHello World\n", .{});
}

extern "C" fn get_stack_pointer() [*c]u8;

comptime {
    asm (
        \\.global _get_stack_pointer;
        \\_get_stack_pointer:
        \\  mov x0, sp
        \\  mov x1, #2024
        \\  sub x0, x0, x1
        \\  ret
    );
}