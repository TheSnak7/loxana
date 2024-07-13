const std = @import("std");
const Value = @import("Value.zig").Value;
const VM = @import("VM.zig").VM;
const Chunk = @import("chunk.zig").Chunk;
const Object = @This();

pub const Type = enum {
    string,
    function,
};

tag: Type,
next: ?*Object,

pub inline fn isObjType(val: *const Value, tag: Type) bool {
    return val.*.isObject() and val.*.object.tag == tag;
}

pub fn allocateObject(vm: *VM, obj_type: type, type_tag: Type) !*Object {
    const ptr = try vm.alloc.create(obj_type);
    ptr.obj.tag = type_tag;

    ptr.obj.next = vm.objects;
    vm.objects = &ptr.obj;

    return &ptr.obj;
}

pub inline fn getType(val: *const Value) Type {
    return val.object.obj_type;
}

pub inline fn asValue(self: *Object) Value {
    return Value.fromObject(self);
}

pub inline fn asString(self: *Object) *String {
    return @fieldParentPtr("obj", self);
}

pub inline fn asFunction(self: *Object) *Function {
    return @fieldParentPtr("obj", self);
}

pub const String = struct {
    obj: Object,
    bytes: []const u8,

    pub fn format(string: String, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}", .{string.bytes});
    }

    pub fn take(vm: *VM, bytes: []const u8) !*String {
        return allocate(vm, bytes);
    }

    pub fn copy(vm: *VM, str: []const u8) !*String {
        //Check whether string is already interned
        const interned = vm.strings.get(str);
        if (interned) |s| return s;

        const heapChars = try vm.alloc.alloc(u8, str.len);
        @memcpy(heapChars, str);
        return try allocate(vm, heapChars);
    }

    //TODO: unify api names
    pub fn allocate(vm: *VM, bytes: []const u8) !*String {
        const obj = try allocateObject(vm, String, .string);
        var string = obj.asString();
        string.bytes = bytes;

        _ = try vm.strings.put(bytes, string);

        return string;
    }
};

pub const Function = struct {
    obj: Object,
    arity: u8,
    //TODO?: Turn this into a managed pointer?
    chunk: Chunk,
    name: ?*String,

    pub fn format(func: Function, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("<fn {s}>", .{if (func.name) |n| n.bytes else "<script>"});
    }

    //TODO? Change naming to init/deinit?
    pub fn create(vm: *VM) !*Function {
        const obj = try allocateObject(vm, Function, .function);
        const function = obj.asFunction();
        function.arity = 0;
        function.name = null;
        function.chunk = try Chunk.init(vm.alloc);

        return function;
    }

    pub fn destroy(self: *Function, alloc: std.mem.Allocator) void {
        self.chunk.deinit();
        alloc.destroy(self);
    }
};
