const std = @import("std");
const Value = @import("value.zig").Value;
const OpCode = @import("opcode.zig").OpCode;
const Debug = @import("debug.zig");

pub const Chunk = struct {
    pub usingnamespace Debug;

    pub const ConstantIndex = u24;
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

    pub fn writeAssembly(self: *Chunk, cons: anytype, asmb: anytype) !void {
        var constants: [cons.len]ConstantIndex = undefined;
        inline for (cons, 0..) |constant, idx| {
            constants[idx] = try self.addConstant(constant);
        }

        inline for (asmb) |tuple| {
            const operand = tuple[0];
            const line = tuple[tuple.len - 1];

            try self.writeOp(operand, line);

            inline for (1..tuple.len - 1) |idx| {
                const constant = try std.fmt.parseInt(ConstantIndex, tuple[idx], 10);
                try self.write(u8, @intCast(constant), line);
            }
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

    pub fn getLine(self: *const Chunk, instruction_index: u32) u32 {
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
};
