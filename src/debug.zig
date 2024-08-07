const std = @import("std");
const Value = @import("Value.zig").Value;
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
        .ret,
        .negate,
        .add,
        .subtract,
        .multiply,
        .divide,
        .nil,
        .op_false,
        .op_true,
        .not,
        .equal,
        .greater,
        .less,
        .print,
        .pop,
        => {
            _ = try writer.print("\n", .{});
            return 1;
        },
        .jump,
        .jump_if_false,
        => try self.printJumpInstruction(writer, opcode, 1, offset),
        .loop,
        => try self.printJumpInstruction(writer, opcode, -1, offset),
        .get_global,
        .define_global,
        .set_global,
        .constant,
        => try self.printConstantInstruction(writer, opcode, offset, u8),
        .set_local,
        .get_local,
        => try self.printByteInstruction(writer, opcode, offset),
        .constant_long => try self.printConstantInstruction(writer, opcode, offset, u24),
    };
}
pub fn printJumpInstruction(self: *const Chunk, writer: anytype, opcode: OpCode, sign: i8, offset: u32) !u32 {
    const jump: i16 = std.mem.bytesAsValue(i16, self.bytes.items[(offset + 1)..(offset + 3)]).*;

    try writer.print(" {d} -> {d}", .{
        offset,
        @as(i16, @intCast(offset + 3)) + sign * jump,
    });
    _ = try writer.print("\n", .{});

    return opcode.instructionLen();
}

pub fn printConstantInstruction(self: *const Chunk, writer: anytype, opcode: OpCode, offset: u32, constant_type: type) !u32 {
    _ = constant_type;
    const constant: ConstantIndex = self.bytes.items[offset + 1];
    _ = try writer.print(" [{d}] ", .{constant});

    try printValue(writer, self.constants.items[constant]);
    _ = try writer.print("\n", .{});

    return opcode.instructionLen();
}

pub fn printByteInstruction(self: *const Chunk, writer: anytype, opcode: OpCode, offset: u32) !u8 {
    const slot = self.bytes.items[@intCast(offset + 1)];
    try writer.print(" {d: >6}", .{slot});
    _ = try writer.print("\n", .{});

    return opcode.instructionLen();
}

pub fn printValue(writer: anytype, val: Value) !void {
    _ = try writer.print("{any}", .{val});
}

pub fn dumpConstantPool(self: *const Chunk) void {
    std.debug.print("---- Constants ----\n", .{});
    for (self.constants.items, 0..) |constant, i| {
        std.debug.print("  {}:  {}\n", .{ i, constant });
    }
}
