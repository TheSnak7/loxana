const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("value.zig").Value;
const Config = @import("config.zig");
const Parser = @import("Parser.zig");

const DebugWriter = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        return std.debug.print(fmt, args);
    }
};

const VM = @This();

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

pub fn init(alloc: std.mem.Allocator) VM {
    return .{
        .alloc = alloc,
    };
}

fn resetVM(self: *VM) void {
    self.resetStack();
}

pub fn deinit(self: *VM) void {
    _ = self;
}

fn resetStack(self: *VM) void {
    self.stack_top = 0;
}

pub fn interpret(self: *VM, alloc: std.mem.Allocator, src: []const u8) !VM.InterpretResult {
    var chunk = try Chunk.init(alloc);
    defer chunk.deinit();

    //FIXME: super hacky
    var parser: Parser = undefined;

    if (!(try parser.compile(src, &chunk))) {
        return InterpretResult{ .compile_error = 99 };
    }

    self.chunk = &chunk;
    self.ip = 0;

    const result = self.runCode();

    return result;
}

inline fn push(self: *VM, val: Value) void {
    self.stack[self.stack_top] = val;
    self.stack_top += 1;
}
inline fn pop(self: *VM) Value {
    self.stack_top -= 1;
    return self.stack[self.stack_top];
}

inline fn readByte(self: *VM) u8 {
    const ret = self.chunk.bytes.items[self.ip];
    self.ip += 1;
    return ret;
}

inline fn readConstant(self: *VM) Value {
    return self.chunk.constants.items[self.readByte()];
}

fn runCode(self: *VM) !InterpretResult {
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
