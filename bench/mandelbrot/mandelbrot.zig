
const Pos = struct  {
    x: f64,
    y: f64,
    pub fn add(self: Pos, b: Pos)  Pos {
            return .{
                .x = self.x + b.x,
                .y = self.y + b.y,
            };
        }
       pub fn sub(self: Pos, b: Pos) Pos {
            return .{
                .x=self.x - b.x,
                .y=self.y - b.y,
            };
        }
       pub fn mul(self: Pos, b: Pos) Pos {
            return .{
                .x= self.x * b.x,
                .y= self.y * b.y,
            };
        }
       pub fn div(self: Pos, b: Pos) Pos {
            return .{
                .x= self.x / b.x,
                .y=self.y / b.y,
            };
        }
};


fn mandelbrot(c: Pos, z_: Pos, steps: i64)  i64 {
    var z = z_;
    var i: i64 = 0;
    var zSq = z.mul(z);
    while (i < steps and zSq.x + zSq.y < 4.0) {
        z.y = z.x * 2.0 * z.y;
        z.x = zSq.x - zSq.y;
        z = z.add(c);
        zSq = z.mul(z);
        i = i + 1;
    }
    return i;
}

const std = @import("std");

pub fn main() !void {
    // let start = timestamp();
    const max_steps = 45;
    const width = 70;
    const height = 35;
    const x_speed = 0.03;
    const y_speed = 0.06;
    const x_start = 0.0 - 1.5;
    const y_start = 0.0 - 1.0;

    const A = std.heap.GeneralPurposeAllocator(.{});
    var a: A = .{};
    var out = try std.ArrayList(u8).initCapacity(a.allocator(), width * height);
    var pos: Pos = .{ .x= x_start, .y= y_start };
    
    for( 0..height) |_| { 
        for( 0..width) |_| { 
            const steps = mandelbrot(pos, .{ .x= 0.0, .y= 0.0 }, max_steps);
            if (steps == max_steps) {
                try out.appendSlice("@");
            } else {
               try  out.appendSlice(" ");
            }
            pos.x = pos.x + x_speed;
        }
        try out.appendSlice("|");
        try out.appendSlice("\n");
        pos.x = x_start;
        pos.y = pos.y + y_speed;
    }
    try std.io.getStdOut().writeAll(out.items);
    // let end = timestamp();
    // println!("Finished running main() in {:.5} seconds.", end - start);
}

// pub fn timestamp() -> f64 {
//     use std::time::SystemTime;
//     SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64()
// }
