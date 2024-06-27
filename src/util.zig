const std = @import("std");
const optimize_unreachable = false;

pub inline fn safeUnreachable(info: std.builtin.SourceLocation) noreturn {
    if (optimize_unreachable) {
        unreachable;
    } else {
        @panic(std.fmt.comptimePrint("Unreachable panicked in function: {s} in file: {s}:{d}:{d}", .{ info.fn_name, info.file, info.line, info.column }));
    }
}
