const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("value.zig").Value;
const Config = @import("config.zig");

const DebugWriter = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        return std.debug.print(fmt, args);
    }
};

const Self = @This();

chunk: *Chunk = undefined,
// FIXME: replace with pointer arithmetic
ip: u32 = 0,
stack: [Config.stack_size]Value = undefined,
//FIXME: replace with pointer arithmetic
stack_top: u32 = 0,
alloc: std.mem.Allocator,

pub const InterpretResultTag = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const InterpretResult = union(InterpretResultTag) {
    ok: void,
    compile_error: i32,
    runtime_error: i32,
};

pub fn init(alloc: std.mem.Allocator) Self {
    return .{
        .alloc = alloc,
    };
}

fn resetVM(self: *Self) void {
    self.resetStack();
}

pub fn deinit(self: *Self) void {
    _ = self;
}

fn resetStack(self: *Self) void {
    self.stack_top = 0;
}

//This function should wrap all errors
pub fn interpret(self: *Self, chunk: *Chunk) !InterpretResult {
    self.chunk = chunk;
    self.ip = 0;

    return runCode(self);
}

inline fn push(self: *Self, val: Value) void {
    self.stack[self.stack_top] = val;
    self.stack_top += 1;
}
inline fn pop(self: *Self) Value {
    self.stack_top -= 1;
    return self.stack[self.stack_top];
}

inline fn readByte(self: *Self) u8 {
    const ret = self.chunk.bytes.items[self.ip];
    self.ip += 1;
    return ret;
}

inline fn readConstant(self: *Self) Value {
    return self.chunk.constants.items[self.readByte()];
}

fn runCode(self: *Self) !InterpretResult {
    while (true) {
        if (Config.debug_trace_exectution) {
            std.debug.print("          ", .{});
            for (0..self.stack_top) |idx| {
                std.debug.print("[ {} ]", .{self.stack[idx]});
            }
            std.debug.print("\n", .{});

            _ = try self.chunk.printInstruction(DebugWriter, self.ip);
        }

        const opcode: OpCode = @enumFromInt(self.readByte());

        // Implementation of instructions follows the book for now
        // FIXME: Improve performance when simple compiler and benchmarking is available
        switch (opcode) {
            .constant => {
                const constant = self.readConstant();
                self.push(constant);
            },
            .ret => {
                _ = try Chunk.printValue(DebugWriter, self.pop());
                std.debug.print("\n", .{});
                return InterpretResult.ok;
            },
            .add => {
                const b = self.pop();
                const a = self.pop();
                self.push(a + b);
            },
            .subtract => {
                const b = self.pop();
                const a = self.pop();
                self.push(a - b);
            },
            .multiply => {
                const b = self.pop();
                const a = self.pop();
                self.push(a * b);
            },
            .divide => {
                const b = self.pop();
                const a = self.pop();
                self.push(a / b);
            },
            .negate => {
                self.push(-self.pop());
            },
            .constant_long => {
                @panic("Unsupported instruction: op-c-long");
            },
        }
    }
}
