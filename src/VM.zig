const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("Value.zig").Value;
const Object = @import("Object.zig");
const Config = @import("config.zig");
const sp = @import("spcompiler.zig");
const Parser = sp.Parser;
const CompilationContext = sp.CompilationContext;

const DebugWriter = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        return std.debug.print(fmt, args);
    }
};

const CallFrame = struct {
    function: *Object.Function,
    ip: [*]u8,
    slots: [*]Value,

    inline fn readByte(self: *CallFrame) u8 {
        const ret = self.ip[0];
        self.ip += 1;
        return ret;
    }

    inline fn readU16(self: *CallFrame) u16 {
        const val = std.mem.bytesToValue(u16, self.ip);
        self.ip += 2;
        return val;
    }

    inline fn readConstant(self: *CallFrame) Value {
        return self.function.chunk.constants.items[self.readByte()];
    }

    inline fn readString(self: *CallFrame) *Object.String {
        return self.readConstant().asString();
    }
};

pub const VM = struct {
    frames: [Config.frames_stack_size]CallFrame = undefined,
    frameCount: u32,
    stack: [Config.stack_size]Value = undefined,
    //FIXME: replace with pointer arithmetic
    stack_top: [*]Value,
    strings: std.StringHashMap(*Object.String),
    globals: std.AutoHashMap(*Object.String, Value),
    alloc: std.mem.Allocator,
    ext: ExternFuncs,
    objects: ?*Object,
    //FIXME: Find a better system for this
    pointers_initialized: bool = false,

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

    pub const ExternFuncs = struct {
        stdout: std.io.AnyWriter,
    };

    pub fn init(alloc: std.mem.Allocator, funcs: ExternFuncs) VM {
        const strings = std.StringHashMap(*Object.String).init(alloc);
        const globals = std.AutoHashMap(*Object.String, Value).init(alloc);
        return .{
            .alloc = alloc,
            .ext = funcs,
            .objects = null,
            .strings = strings,
            .globals = globals,
            .frameCount = 0,
            .stack_top = undefined,
            .pointers_initialized = false,
        };
    }

    //FIXME: Heap allocate VM to avoid dangling pointers by copy in init
    pub fn setupPointers(self: *VM) void {
        self.resetStack();
        self.pointers_initialized = true;
    }

    fn resetVM(self: *VM) void {
        self.resetStack();
    }

    pub fn deinit(self: *VM) void {
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
                const string = obj.asString();
                self.alloc.free(string.bytes);
                self.alloc.destroy(string);
            },
            .function => {
                const function = obj.asFunction();
                function.destroy(self.alloc);
            },
        }
    }

    fn resetStack(self: *VM) void {
        self.stack_top = self.stack[0..].ptr;

        self.frameCount = 0;
    }

    pub fn interpret(self: *VM, src: []const u8) !VM.InterpretResult {
        if (!self.pointers_initialized) {
            @panic("Forgot to setupPointers()");
        }

        var context = try CompilationContext.init(self);
        defer context.deinit();

        const function: *Object.Function = try context.compile(src) orelse {
            return InterpretResult{ .compile_error = 99 };
        };

        self.push(function.obj.asValue());
        var frame = &self.frames[self.frameCount];
        self.frameCount += 1;
        frame.function = function;
        frame.ip = function.chunk.bytes.items.ptr;
        frame.slots = self.stack[0..].ptr;

        const result = self.runCode();

        return result;
    }

    inline fn push(self: *VM, val: Value) void {
        self.stack_top[0] = val;
        self.stack_top += 1;
    }
    inline fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    inline fn peek(self: *VM, distance: usize) Value {
        return (self.stack_top - distance - 1)[0];
    }

    fn runCode(self: *VM) !InterpretResult {
        const frame = &self.frames[self.frameCount - 1];

        while (true) {
            if (Config.debug_trace_exectution) {
                std.debug.print("          ", .{});

                const stack_size: usize = (@intFromPtr(self.stack_top) - @intFromPtr(&self.stack[0])) / @sizeOf(Value);
                for (0..stack_size) |idx| {
                    std.debug.print("[ {} ]", .{self.stack[idx]});
                }
                std.debug.print("\n", .{});

                const offset: u32 = @intCast(@intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.bytes.items.ptr));
                _ = try frame.function.chunk.printInstruction(DebugWriter, offset);
            }

            const opcode: OpCode = @enumFromInt(frame.readByte());

            // Implementation of instructions follows the book for now
            // TODO: Improve performance when simple compiler and benchmarking is available
            switch (opcode) {
                .constant => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                .ret => {
                    return InterpretResult.ok;
                },
                .jump => {
                    const offset = frame.readU16();
                    frame.ip += offset;
                },
                .jump_if_false => {
                    const offset = frame.readU16();
                    if (self.peek(0).isFalsey()) {
                        frame.ip += offset;
                    }
                },
                .loop => {
                    const offset = frame.readU16();
                    frame.ip -= offset;
                },
                .nil => self.push(Value.getNil()),
                .op_true => self.push(Value.fromBool(true)),
                .op_false => self.push(Value.fromBool(false)),
                .pop => _ = self.pop(),
                .get_local => {
                    const slot = frame.readByte();
                    const local = frame.slots[slot];
                    self.push(local);
                },
                .set_local => {
                    const slot = frame.readByte();
                    frame.slots[slot] = self.peek(0);
                },
                .get_global => {
                    const name = frame.readString();
                    const value = self.globals.get(name);
                    if (value) |v| {
                        self.push(v);
                    } else {
                        self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
                        return InterpretResult{ .runtime_error = 33 };
                    }
                },
                .define_global => {
                    const name = frame.readString();
                    const val = self.peek(0);
                    try self.globals.put(name, val);
                    _ = self.pop();
                },
                .set_global => {
                    const name = frame.readString();
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
                        const b = self.pop().asNumber();
                        const a = self.pop().asNumber();
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
                    self.push(Value.fromNum(a - b));
                },
                .multiply => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value.fromNum(a * b));
                },
                .divide => {
                    if (!self.twoNumsOnTop()) return InterpretResult{ .runtime_error = 33 };
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(Value.fromNum(a / b));
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
                        .number => self.push(Value.fromNum(-(self.pop().asNumber()))),
                        else => {
                            self.runtimeError("Operand must be a number", .{});
                            return InterpretResult{ .runtime_error = 33 };
                        },
                    }
                },
                .print => {
                    //TODO: Add error writer to vm
                    try self.ext.stdout.print("{}\n", .{self.pop()});
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

        const frame = &self.frames[self.frameCount - 1];
        const instruction: u32 = @intCast(@intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.bytes.items.ptr) - 1);
        const line = frame.function.chunk.getLine(instruction);
        std.debug.print("[line {}] in script\n", .{line});
        self.resetStack();
    }
};
