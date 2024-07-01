const std = @import("std");

pub const Token = struct {
    tag: Tag,
    name: []const u8,
    line: u32,

    pub const Tag = enum {
        left_paren,
        right_paren,
        left_brace,
        right_brace,
        comma,
        dot,
        minus,
        plus,
        semicolon,
        slash,
        star,

        bang,
        bang_equal,
        equal,
        equal_equal,
        greater,
        greater_equal,
        less,
        less_equal,

        identifier,
        string,
        number,

        keyword_and,
        keyword_class,
        keyword_else,
        keyword_false,
        keyword_for,
        keyword_fun,
        keyword_if,
        keyword_nil,
        keyword_or,
        keyword_print,
        keyword_return,
        keyword_super,
        keyword_this,
        keyword_true,
        keyword_var,
        keyword_while,

        invalid,
        eof,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(
        .{
            .{ "and", .keyword_and },
            .{ "class", .keyword_class },
            .{ "else", .keyword_else },
            .{ "false", .keyword_false },
            .{ "for", .keyword_for },
            .{ "fun", .keyword_fun },
            .{ "if", .keyword_if },
            .{ "nil", .keyword_nil },
            .{ "or", .keyword_or },
            .{ "print", .keyword_print },
            .{ "return", .keyword_return },
            .{ "super", .keyword_super },
            .{ "this", .keyword_this },
            .{ "true", .keyword_true },
            .{ "var", .keyword_var },
            .{ "while", .keyword_while },
        },
    );

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub fn format(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("<Token \"{s}\" ({s}) at line {}>", .{ value.name, @tagName(value.tag), value.line });
    }

    pub fn debugFormat(value: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("<Token \"{s}\" ({s}) at line {}>\n", .{ value.name, @tagName(value.tag), value.line });
    }

    pub fn toString(value: @This(), alloc: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "<Token \"{s}\" ({s}) at line {}>", .{ value.name, @tagName(value.tag), value.line });
    }
};
