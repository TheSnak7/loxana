const std = @import("std");
const Tokenizer = @import("tokenizer.zig");

pub fn compile(src: []const u8) !void {
    var scanner: Tokenizer = Tokenizer.init(src);
    var line: u32 = std.math.maxInt(u32);
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{d: >4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{s: >18} {s}\n", .{ @tagName(token.tag), token.name });
        if (token.tag == .eof) break;
    }
}
