const std = @import("std");
const Value = @import("value.zig").Value;
const Writer = std.io.AnyWriter;

//Using op_ to avoid annoying name collisions with Zig keywords
pub const OpCode = enum(u8) {
    op_return,
    op_constant,

    pub fn instructionLen(self: OpCode) u8 {
        return switch (self) {
            .op_return => 1,
            .op_constant => 2,
        };
    }
};

pub const Chunk = struct {
    const ConstantIndex = u8;

    bytes: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(u32),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) !Chunk {
        const bytes = std.ArrayList(u8).init(alloc);
        const constants = std.ArrayList(Value).init(alloc);
        const lines = std.ArrayList(u32).init(alloc);
        return .{
            .bytes = bytes,
            .constants = constants,
            .lines = lines,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.bytes.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeOp(self: *Chunk, instruction: OpCode, line: u32) !void {
        try self.bytes.append(@intFromEnum(instruction));
        try self.lines.append(line);
    }

    pub fn writeByte(self: *Chunk, val: u8, line: u32) !void {
        try self.bytes.append(val);
        try self.lines.append(line);
    }

    pub fn addConstant(self: *Chunk, val: Value) !ConstantIndex {
        try self.constants.append(val);
        if (self.constants.items.len > std.math.maxInt(u8)) {
            @panic("Exceeded constant pool");
        }
        return @intCast(self.constants.items.len - 1);
    }

    pub fn dissamble(self: *Chunk, writer: anytype, name: []const u8) !void {
        _ = try writer.print("== {s} ==\n", .{name});
        var offset: u32 = 0;
        while (offset < self.bytes.items.len) {
            const opcode: OpCode = @enumFromInt(self.bytes.items[offset]);

            offset += try self.printInstruction(writer, opcode, offset);
        }
    }

    fn printInstruction(self: *Chunk, writer: anytype, opcode: OpCode, offset: u32) !u32 {
        try writer.print("{d:0>4} ", .{offset});

        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            try writer.print("   | ", .{});
        } else {
            try writer.print("{d: >4} ", .{self.lines.items[offset]});
        }

        try writer.print("{s: <16}", .{@tagName(opcode)});

        return switch (opcode) {
            .op_return => {
                _ = try writer.print("\n", .{});
                return 1;
            },
            .op_constant => try printConstantInstruction(self, writer, opcode, offset),
        };
    }
    fn printConstantInstruction(self: *Chunk, writer: anytype, opcode: OpCode, offset: u32) !u32 {
        const constant = self.bytes.items[offset + 1];
        _ = try writer.print(" {d: >4} ", .{constant});

        try printValue(writer, self.constants.items[constant]);
        _ = try writer.print("\n", .{});

        return opcode.instructionLen();
    }

    fn printValue(writer: anytype, val: Value) !void {
        _ = try writer.print("{d}", .{val});
    }
};
