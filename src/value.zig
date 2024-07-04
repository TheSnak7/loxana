const std = @import("std");

pub const Tag = enum {
    boolean,
    nil,
    number,
};

pub const Value = union(Tag) {
    boolean: bool,
    nil: void,
    number: f64,

    pub fn format(value: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (value) {
            .number => try writer.print("{d}", .{value.number}),
            .nil => try writer.print("nil", .{}),
            .boolean => try writer.print("{s}", .{if (value.boolean) "true" else "false"}),
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

    pub inline fn isNumber(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .number;
    }

    pub inline fn isBool(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .boolean;
    }

    pub inline fn isNil(value: *const Value) bool {
        return std.meta.activeTag(value.*) == .nil;
    }

    pub inline fn isFalsey(value: *const Value) bool {
        return value.isNil() or (value.isBool() and !value.boolean);
    }

    pub inline fn areEqual(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;

        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .nil => true,
            .number => a.number == b.number,
        };
    }
};
