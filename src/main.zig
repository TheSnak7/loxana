const std = @import("std");
const safeUnreachable = @import("util.zig").safeUnreachable;
const Chunk = @import("chunk.zig").Chunk;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);

    const gpa = general_purpose_allocator.allocator();

    var chunk = try Chunk.init(gpa);
    defer chunk.deinit();

    const constant = try chunk.addConstant(1.2);
    try chunk.writeOp(.op_constant, 123);
    try chunk.write(u8, @intCast(constant), 123);

    const con2 = try chunk.addConstant(1.3);

    try chunk.writeOp(.op_constant, 124);
    try chunk.write(u8, @intCast(con2), 124);

    try chunk.writeOp(.op_constant_long, 124);
    try chunk.write(u24, con2, 124);

    try chunk.writeOp(.op_return, 125);

    const stdout = std.io.getStdOut().writer();
    try chunk.disassemble(stdout, "Test chunk");
}
