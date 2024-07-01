const std = @import("std");
const clap = @import("clap");
const safeUnreachable = @import("util.zig").safeUnreachable;
const Chunk = @import("chunk.zig").Chunk;
const VM = @import("vm.zig");
const compile = @import("compiler.zig").compile;

//var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
//defer std.debug.assert(general_purpose_allocator.deinit() == .ok);

//const gpa = general_purpose_allocator.allocator();

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);

    const gpa = general_purpose_allocator.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\<FILE>...
        \\
    );

    //const YesNo = enum { yes, no };
    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        printHelp();
    }
    if (res.positionals.len == 0) {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();
        try repl(stdout, stdin);
    }

    for (res.positionals) |pos| {
        try runFile(gpa, pos);
    }
}

fn printHelp() void {
    std.debug.print("Usage: loxana [path]\n", .{});
}

var lineBuf: [1024]u8 = undefined;
fn repl(stdout: anytype, stdin: anytype) !void {
    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterOrEof(&lineBuf, '\n') orelse {
            @panic("Unexpected null bytes read");
        };
        _ = try interpret(line);
    }
}

fn runFile(alloc: std.mem.Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().readFileAlloc(alloc, path, 1000_000_000);

    const result = try interpret(file);

    switch (result) {
        .compile_error => std.process.exit(65),
        .runtime_error => std.process.exit(70),
        .ok => return,
    }
}

fn interpret(src: []const u8) !VM.InterpretResult {
    try compile(src);
    return VM.InterpretResult.ok;
}

test "run all tests" {
    _ = @import("tests.zig");
}
