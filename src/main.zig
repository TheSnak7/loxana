const std = @import("std");
const safeUnreachable = @import("util.zig").safeUnreachable;
const Chunk = @import("chunk.zig").Chunk;
const VM = @import("vm.zig");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);

    const gpa = general_purpose_allocator.allocator();

    var vm = VM.init(gpa);
    defer vm.deinit();

    var chunk = try Chunk.init(gpa);
    defer chunk.deinit();

    //const constant = try chunk.addConstant(1.2);
    //try chunk.writeOp(.constant, 123);
    //try chunk.write(u8, @intCast(constant), 123);

    //try chunk.writeOp(.negate, 125);
    //try chunk.writeOp(.ret, 125);

    try chunk.writeAssembly(.{
        1.2,
        2,
    }, .{
        .{ .constant, "0", 123 },
        .{ .constant, "0", 124 },
        .{ .add, 125 },
        .{ .negate, 125 },
        .{ .constant, "1", 125 },
        .{ .divide, 125 },
        .{ .ret, 125 },
    });

    //FIXME: wrap all errors
    _ = try vm.interpret(&chunk);

    //const stdout = std.io.getStdOut().writer();
    //try chunk.disassemble(stdout, "Test chunk");
}
