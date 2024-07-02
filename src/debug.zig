const std = @import("std");
const Value = @import("value.zig").Value;
const OpCode = @import("opcode.zig").OpCode;
const ch = @import("chunk.zig");
const Chunk = ch.Chunk;
const ConstantIndex = ch.Chunk.ConstantIndex;

pub fn disassemble(self: *Chunk, writer: anytype, name: []const u8) !void {
    _ = try writer.print("== {s} ==\n", .{name});
    var offset: u32 = 0;
    while (offset < self.bytes.items.len) {
        const instruction_length = try self.printInstruction(writer, offset);
        offset += instruction_length;
    }
}

pub fn printInstruction(self: *const Chunk, writer: anytype, offset: u32) !u32 {
    try writer.print("{d:0>4} ", .{offset});

    if (offset > 0 and self.getLine(offset) == self.getLine(offset - 1)) {
        try writer.print("   | ", .{});
    } else {
        try writer.print("{d: >4} ", .{self.getLine(offset)});
    }

    const opcode: OpCode = @enumFromInt(self.bytes.items[offset]);

    try writer.print("{s: <16}", .{@tagName(opcode)});

    return switch (opcode) {
        .ret, .negate, .add, .subtract, .multiply, .divide => {
            _ = try writer.print("\n", .{});
            return 1;
        },
        .constant => try self.printConstantInstruction(writer, opcode, offset, u8),
        .constant_long => try self.printConstantInstruction(writer, opcode, offset, u24),
    };
}
pub fn printConstantInstruction(self: *const Chunk, writer: anytype, opcode: OpCode, offset: u32, constant_type: type) !u32 {
    _ = constant_type;
    const constant: ConstantIndex = self.bytes.items[offset + 1];
    _ = try writer.print(" {d: >6} ", .{constant});

    try printValue(writer, self.constants.items[constant]);
    _ = try writer.print("\n", .{});

    return opcode.instructionLen();
}

pub fn printValue(writer: anytype, val: Value) !void {
    _ = try writer.print("{d}", .{val});
}

pub fn dumpConstantPool(self: *const Chunk) void {
    std.debug.print("---- Constants ----\n", .{});
    for (self.constants.items, 0..) |constant, i| {
        std.debug.print("  {}:  {}\n", .{ i, constant });
    }
}
