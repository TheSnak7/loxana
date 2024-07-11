const std = @import("std");
const VM = @import("VM.zig").VM;
const t = std.testing;

fn runCode(src: []const u8, writer: std.io.AnyWriter) !void {
    const alloc = t.allocator;

    const funcs: VM.ExternFuncs = .{
        .stdout = writer,
    };

    var vm = VM.init(alloc, funcs);
    defer vm.deinit();

    const result = try vm.interpret(alloc, src);
    std.debug.assert(result == VM.InterpretResult.ok);
}

fn testCode(result: []const u8, code: []const u8) !void {
    var buffer = std.ArrayList(u8).init(t.allocator);
    defer buffer.deinit();

    var counting_writer = std.io.countingWriter(buffer.writer());

    _ = try runCode(
        code,
        counting_writer.writer().any(),
    );

    try t.expectEqualStrings(result, buffer.items[0..counting_writer.bytes_written]);
}

test "simple print" {
    const code = "print 1;";
    const result = "1\n";

    try testCode(result, code);
}

test "simple add" {
    const code = "print 1 + 2;";
    const result = "3\n";

    try testCode(result, code);
}

test "simple mul" {
    const code = "print 5 * 4;";
    const result = "20\n";

    try testCode(result, code);
}

test "simple div" {
    const code = "print 8 / 4;";
    const result = "2\n";

    try testCode(result, code);
}

test "simple brackets" {
    const code = "print 5 * (1 + 2);";
    const result = "15\n";

    try testCode(result, code);
}

test "multiply and add" {
    const code = "print 9 + 2 * 4;";
    const result = "17\n";

    try testCode(result, code);
}

test "equa" {
    const code = "print 9 + 2 * 4;";
    const result = "17\n";

    try testCode(result, code);
}
