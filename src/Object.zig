const std = @import("std");
const Value = @import("Value.zig").Value;
const VM = @import("VM.zig").VM;
const Object = @This();

pub const Type = enum {
    string,
};

tag: Type,
next: ?*Object,

pub inline fn getType(val: *const Value) Type {
    return val.object.obj_type;
}

pub inline fn asValue(self: *Object) Value {
    return Value.fromObject(self);
}

pub const String = struct {
    obj: Object,
    bytes: []const u8,

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

    pub fn allocate(vm: *VM, bytes: []const u8) !*String {
        const obj = try allocateObject(vm, String, .string);
        var string = obj.asString();
        string.bytes = bytes;

        _ = try vm.strings.put(bytes, string);

        return string;
    }
};

pub inline fn isObjType(val: *const Value, tag: Type) bool {
    return val.*.isObject() and val.*.object.tag == tag;
}

pub inline fn asString(self: *Object) *String {
    return @fieldParentPtr("obj", self);
}

pub fn allocateObject(vm: *VM, obj_type: type, type_tag: Type) !*Object {
    const ptr = try vm.alloc.create(obj_type);
    ptr.obj.tag = type_tag;

    ptr.obj.next = vm.objects;
    vm.objects = &ptr.obj;

    return &ptr.obj;
}
