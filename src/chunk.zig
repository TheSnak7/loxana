const std = @import("std");
const Value = @import("value.zig").Value;
const Writer = std.io.AnyWriter;

//Using op_ to avoid annoying name collisions with Zig keywords
pub const OpCode = enum(u8) {
    op_return,
    op_constant,
    op_constant_long,

    pub fn instructionLen(self: OpCode) u8 {
        return switch (self) {
            .op_return => 1,
            .op_constant => 2,
            .op_constant_long => 4,
        };
    }
};

pub const Chunk = struct {
    const ConstantIndex = u24;
    const Line = struct {
        intruction_count: u32,
        line_number: u32,
    };

    bytes: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(Line),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) !Chunk {
        const bytes = std.ArrayList(u8).init(alloc);
        const constants = std.ArrayList(Value).init(alloc);
        const lines = std.ArrayList(Line).init(alloc);
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
        try self.insertLineNumber(line);
    }

    pub fn write(self: *Chunk, valtype: type, val: valtype, line: u32) !void {
        const size = switch (valtype) {
            u8, i8 => 1,
            u16, i16 => 2,
            u24, i24 => 3,
            u32, i32 => 4,
            u40, i40 => 5,
            u48, i48 => 6,
            u56, i56 => 7,
            u64, i64 => 8,
            else => @compileError("Unsupported type: " ++ @typeName(valtype)),
        };

        const bytes = std.mem.toBytes(val);
        try self.bytes.appendSlice(bytes[0..size]);
        for (size) |_| {
            try self.insertLineNumber(line);
        }
    }

    pub fn insertLineNumber(self: *Chunk, line: u32) !void {
        const last_opt = self.lines.getLastOrNull();
        if (last_opt) |last| {
            if (last.line_number == line) {
                self.lines.items[self.lines.items.len - 1].intruction_count += 1;
            } else {
                try self.lines.append(.{
                    .intruction_count = 1,
                    .line_number = line,
                });
            }
        } else {
            try self.lines.append(.{
                .intruction_count = 1,
                .line_number = line,
            });
        }
    }

    pub fn addConstant(self: *Chunk, val: Value) !ConstantIndex {
        try self.constants.append(val);
        if (self.constants.items.len > std.math.maxInt(ConstantIndex)) {
            @panic("Exceeded constant pool");
        }
        return @intCast(self.constants.items.len - 1);
    }

    pub fn read(self: *const Chunk, valtype: type, first_byte_index: u32) valtype {
        const ptr = &self.bytes.items[first_byte_index];
        return std.mem.bytesAsValue(valtype, ptr).*;
    }

    pub fn getLine(self: *Chunk, instruction_index: u32) u32 {
        var counted_instructions: u32 = 0;
        var counted_line_blocks: u32 = 0;
        while (true) {
            counted_instructions += self.lines.items[counted_line_blocks].intruction_count;
            if (counted_instructions > instruction_index) {
                return self.lines.items[counted_line_blocks].line_number;
            }
            counted_line_blocks += 1;
        }
    }

    pub fn disassemble(self: *Chunk, writer: anytype, name: []const u8) !void {
        _ = try writer.print("== {s} ==\n", .{name});
        var offset: u32 = 0;
        while (offset < self.bytes.items.len) {
            const opcode: OpCode = @enumFromInt(self.bytes.items[offset]);

            const instruction_length = try self.printInstruction(writer, opcode, offset);
            offset += instruction_length;
        }
    }

    fn printInstruction(self: *Chunk, writer: anytype, opcode: OpCode, offset: u32) !u32 {
        try writer.print("{d:0>4} ", .{offset});

        if (offset > 0 and self.getLine(offset) == self.getLine(offset - 1)) {
            try writer.print("   | ", .{});
        } else {
            try writer.print("{d: >4} ", .{self.getLine(offset)});
        }

        try writer.print("{s: <16}", .{@tagName(opcode)});

        return switch (opcode) {
            .op_return => {
                _ = try writer.print("\n", .{});
                return 1;
            },
            .op_constant => try self.printConstantInstruction(writer, opcode, offset, u8),
            .op_constant_long => try self.printConstantInstruction(writer, opcode, offset, u24),
        };
    }
    fn printConstantInstruction(self: *Chunk, writer: anytype, opcode: OpCode, offset: u32, constant_type: type) !u32 {
        const constant: ConstantIndex = if (constant_type == u8) self.bytes.items[offset + 1] else self.read(u24, offset + 1);
        _ = try writer.print(" {d: >6} ", .{constant});

        try printValue(writer, self.constants.items[constant]);
        _ = try writer.print("\n", .{});

        return opcode.instructionLen();
    }

    fn printValue(writer: anytype, val: Value) !void {
        _ = try writer.print("{d}", .{val});
    }

    fn dumpConstantPool(self: *const Chunk) void {
        std.debug.print("---- Constants ----\n", .{});
        for (self.constants.items, 0..) |constant, i| {
            std.debug.print("  {}:  {}\n", .{ i, constant });
        }
    }
};
