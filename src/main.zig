const std = @import("std");
const clap = @import("clap");
const safeUnreachable = @import("util.zig").safeUnreachable;
const Chunk = @import("chunk.zig").Chunk;
const VM = @import("VM.zig").VM;
const interpret = VM.interpret;

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
        try repl(gpa, stdout, stdin);
    }

    for (res.positionals) |pos| {
        try runFile(gpa, pos);
    }
}

fn printHelp() void {
    std.debug.print("Usage: loxana [path]\n", .{});
}

var lineBuf: [1024]u8 = undefined;
fn repl(alloc: std.mem.Allocator, stdout: anytype, stdin: anytype) !void {
    const funcs: VM.ExternFuncs = .{
        .stdout = std.io.getStdOut().writer().any(),
    };

    var vm = VM.init(alloc, funcs);
    vm.setupPointers();

    while (true) {
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterOrEof(&lineBuf, '\n') orelse {
            @panic("Unexpected null bytes read");
        };
        const is_exit = std.mem.startsWith(u8, line, "exit");
        if (is_exit) {
            vm.deinit();
            return;
        }

        _ = try vm.interpret(line);
    }
}

fn runFile(alloc: std.mem.Allocator, path: []const u8) !void {
    const src = try loadFile(alloc, path);
    defer alloc.free(src);

    const result = try runSrc(alloc, src);

    switch (result) {
        .compile_error => std.process.exit(65),
        .runtime_error => std.process.exit(70),
        .ok => return,
    }
}

fn runSrc(alloc: std.mem.Allocator, src: []const u8) !VM.InterpretResult {
    const funcs: VM.ExternFuncs = .{
        .stdout = std.io.getStdOut().writer().any(),
    };

    var vm = VM.init(alloc, funcs);
    vm.setupPointers();
    defer vm.deinit();

    const result = try vm.interpret(src);

    return result;
}

fn loadFile(alloc: std.mem.Allocator, path: []const u8) ![]const u8 {
    const src = try std.fs.cwd().readFileAlloc(alloc, path, 1000_000_000);
    return src;
}

test "run all tests" {
    _ = @import("tests.zig");
}
