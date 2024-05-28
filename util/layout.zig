const Inner = struct  {
    big: i64,
    small: u8,
};

const Nested = struct {
    inner: Inner,
    last: u8,
};

const Together = struct {
    big: i64,
    small: u8,
    last: u8,
};

const print = @import("std").debug.print;
pub fn main() void {
    {
        var value: Nested = undefined;
        print("Nested: \n", .{});
        const start = @intFromPtr(&value);
        print("big: {}\n", .{ (@intFromPtr(&value.inner.big) ) - start});
        print("small: {}\n", .{ (@intFromPtr(&value.inner.small) ) - start});
        print("last: {}\n", .{ (@intFromPtr(&value.last) ) - start});
    }
    {
        var value: Together = undefined;
        print("Together: \n", .{});
        const start = @intFromPtr(&value);
        print("big: {}\n", .{ (@intFromPtr(&value.big) ) - start});
        print("small: {}\n", .{ (@intFromPtr(&value.small) ) - start});
        print("last: {}\n", .{ (@intFromPtr(&value.last) ) - start});
    }
}
