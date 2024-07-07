const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("Value.zig").Value;
const Object = @import("Object.zig");
const Config = @import("config.zig");
const Parser = @import("Parser.zig");

const DebugWriter = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        return std.debug.print(fmt, args);
    }
};

pub const VM = struct {
    chunk: *Chunk = undefined,
    // FIXME: replace with pointer arithmetic
    ip: u32 = 0,
    stack: [Config.stack_size]Value = undefined,
    //FIXME: replace with pointer arithmetic
    stack_top: u32 = 0,
    strings: std.StringHashMap(*Object.String),
    globals: std.AutoHashMap(*Object.String, Value),
    alloc: std.mem.Allocator,
    objects: ?*Object,

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
        const strings = std.StringHashMap(*Object.String).init(alloc);
        const globals = std.AutoHashMap(*Object.String, Value).init(alloc);
        return .{
            .alloc = alloc,
            .objects = null,
            .strings = strings,
            .globals = globals,
        };
    }

    fn resetVM(self: *VM) void {
        self.resetStack();
    }

    pub fn deinit(self: *VM) void {
        std.debug.print("Running cleanup\n", .{});
        self.freeObjects();
        self.strings.deinit();
        self.globals.deinit();
    }

    fn freeObjects(self: *VM) void {
        var objNode = self.objects;
        while (objNode) |obj| {
            const next = obj.next;
            self.freeObject(obj);
            objNode = next;
        }
    }

    fn freeObject(self: *VM, obj: *Object) void {
        switch (obj.tag) {
            .string => {
                const string: *Object.String = @fieldParentPtr("obj", obj);
                self.alloc.free(string.bytes);
                self.alloc.destroy(string);
            },
        }
    }

    fn resetStack(self: *VM) void {
        self.stack_top = 0;
    }

    pub fn interpret(self: *VM, alloc: std.mem.Allocator, src: []const u8) !VM.InterpretResult {
        var chunk = try Chunk.init(alloc);
        defer chunk.deinit();

        //FIXME: super hacky
        var parser: Parser = undefined;

        if (!(try parser.compile(self, src, &chunk))) {
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

    inline fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - distance - 1];
    }

    inline fn readByte(self: *VM) u8 {
        const ret = self.chunk.bytes.items[self.ip];
        self.ip += 1;
        return ret;
    }

    inline fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    inline fn readString(self: *VM) *Object.String {
        return self.readConstant().asString();
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
            // TODO: Improve performance when simple compiler and benchmarking is available
            switch (opcode) {
                .constant => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .ret => {
                    return InterpretResult.ok;
                },
                .nil => self.push(Value.getNil()),
                .op_true => self.push(Value.fromBool(true)),
                .op_false => self.push(Value.fromBool(false)),
                .pop => _ = self.pop(),
                .get_global => {
                    const name = self.readString();
                    const value = self.globals.get(name);
                    if (value) |v| {
                        self.push(v);
                    } else {
                        self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                        return InterpretResult{ .runtime_error = 33 };
                    }
                },
                .define_global => {
                    const name = self.readString();
                    const val = self.peek(0);
                    try self.globals.put(name, val);
                    _ = self.pop();
                },
                .set_global => {
                    const name = self.readString();
                    const value = self.peek(0);
                    if (self.globals.get(name) == null) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                        return InterpretResult{ .runtime_error = 33 };
                    }
                    try self.globals.put(name, value);
                },
                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.fromBool(Value.areEqual(a, b)));
                },
                .not => self.push(Value.fromBool(self.pop().isFalsey())),
                .add => {
                    if (self.peek(0).isString() and self.peek(1).isString()) {
                        try self.concatenate();
                    } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        const b = self.pop().number;
                        const a = self.pop().number;
                        self.push(Value.fromNum(a + b));
                    } else {
                        self.runtimeError("Operand must be a number", .{});
                        return InterpretResult{ .runtime_error = 33 };
                    }
                },
                .subtract => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value{ .number = a - b });
                },
                .multiply => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value{ .number = a * b });
                },
                .divide => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value{ .number = a / b });
                },
                .greater => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value.fromBool(a > b));
                },
                .less => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value.fromBool(a < b));
                },
                .negate => {
                    switch (self.peek(0)) {
                        .number => self.push(Value{ .number = -self.pop().number }),
                        else => {
                            self.runtimeError("Operand must be a number", .{});
                            return InterpretResult{ .runtime_error = 33 };
                        },
                    }
                },
                .print => {
                    //TODO: Add standard and error Writers to vm
                    const stdout = std.io.getStdOut().writer();
                    try stdout.print("{}\n", .{self.pop()});
                },
                .constant_long => {
                    @panic("Unsupported instruction: op-c-long");
                },
            }
        }
    }

    inline fn concatenate(self: *VM) !void {
        const b = self.pop().object.asString();
        const a = self.pop().object.asString();

        const len = a.bytes.len + b.bytes.len;
        const chars = try self.alloc.alloc(u8, len);
        @memcpy(chars[0..a.bytes.len], a.bytes);
        @memcpy(chars[a.bytes.len..], b.bytes);

        const res = try Object.String.take(self, chars);
        self.push(Value.fromObject(&res.obj));
    }

    inline fn twoNumsOnTop(self: *VM) bool {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operand must be numbers.", .{});
            return false;
        }
        return true;
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);
        std.debug.print("\n", .{});

        const instruction = self.ip - 1;
        const line = self.chunk.getLine(instruction);
        std.debug.print("[line {}] in script\n", .{line});
        self.resetStack();
    }
};
