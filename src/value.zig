const std = @import("std");
const Object = @import("Object.zig");

pub const Tag = enum {
    boolean,
    nil,
    number,
    object,
};

pub const Value = union(Tag) {
    boolean: bool,
    nil: void,
    number: f64,
    object: *Object,

    pub fn format(value: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value) {
            .number => |num| try writer.print("{d}", .{num}),
            .nil => try writer.print("nil", .{}),
            .boolean => |b| try writer.print("{s}", .{if (b) "true" else "false"}),
            .object => |obj| {
                switch (obj.tag) {
                    .string => try writer.print("\"{s}\"", .{obj.asString().bytes}),
                }
            },
        }
        return;
    }

    pub inline fn getNil() Value {
        return Value.nil;
    }

    pub inline fn fromBool(b: bool) Value {
        return Value{ .boolean = b };
    }

    pub inline fn fromNum(num: f64) Value {
        return Value{ .number = num };
    }

    pub inline fn fromObject(obj: *Object) Value {
        return Value{ .object = obj };
    }

    pub inline fn isNumber(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .number;
    }

    pub inline fn isBool(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .boolean;
    }

    pub inline fn isNil(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .nil;
    }

    pub inline fn isObject(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .object;
    }

    pub inline fn isString(value: *const Value) bool {
        return Object.isObjType(value, .string);
    }

    pub inline fn isFalsey(value: *const Value) bool {
        return value.isNil() or (value.isBool() and !value.boolean);
    }

    pub inline fn asObj(value: *const Value) *Object {
        return value.object;
    }

    pub inline fn asString(value: *const Value) *Object.String {
        return value.asObj().asString();
    }

    pub inline fn areEqual(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;

        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .nil => true,
            .number => a.number == b.number,
            .object => a.object == b.object,
        };
    }
};
