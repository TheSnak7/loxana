const std = @import("std");
const optimizeUnreachable = false;

pub inline fn safeUnreachable(info: std.builtin.SourceLocation) noreturn {
    if (optimizeUnreachable) {
        unreachable;
    } else {
        @panic(std.fmt.comptimePrint("Unreachable panicked in function: {s} in file: {s}:{d}:{d}", .{ info.fn_name, info.file, info.line, info.column }));
    }
}
