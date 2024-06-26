const std = @import("std");
const safeUnreachable = @import("util.zig").safeUnreachable;

pub fn main() !void {
    const val: i32 = 4;

    std.debug.print("Start\n", .{});
    if (val == 5) {
        std.debug.print("Succes\n", .{});
    } else {
        safeUnreachable(@src());
    }
    std.debug.print("End\n", .{});
}
