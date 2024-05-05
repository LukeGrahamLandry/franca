const std = @import("std");

extern "c" fn strerror(e: usize) [*c] u8;

pub fn main() void {
    for (0..100000) |i| {
        const s = std.mem.span(strerror(i));
        if(!std.mem.startsWith(u8, s, "Unknown")) {
            std.debug.print("{}. {s}\n", .{i, s});
        }
    }  
}
