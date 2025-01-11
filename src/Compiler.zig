const std = @import("std");
const Parser = @import("Parser.zig");

const Compiler = @This();

alloc: std.mem.Allocator,
span_start: std.ArrayList(u8),
span_end: std.ArrayList(u8),
ast_node: std.ArrayList(Parser.AstNode),

blocks: std.ArrayList(Parser.Block),

source: []const u8,
