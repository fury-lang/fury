const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
}
