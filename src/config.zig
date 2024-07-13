const std = @import("std");

pub const debug_trace_exectution = false;
pub const debug_print_code = false;

//TODO?: Make this dynamic
pub const frames_stack_size = 256;

pub const stack_size = frames_stack_size * (std.math.maxInt(u8) + 1);
