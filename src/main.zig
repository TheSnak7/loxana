const std = @import("std");
const safeUnreachable = @import("util.zig").safeUnreachable;
const Chunk = @import("chunk.zig").Chunk;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);

    const gpa = general_purpose_allocator.allocator();

    var chunk = try Chunk.init(gpa);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.9);
    try chunk.writeOp(.op_constant, 123);
    try chunk.writeByte(constant, 123);
    try chunk.writeOp(.op_return, 123);

    const stdout = std.io.getStdOut().writer();
    try chunk.dissamble(stdout, "Test chunk");
}
