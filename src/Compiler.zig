const std = @import("std");
const Parser = @import("Parser.zig");

const Compiler = @This();

alloc: std.mem.Allocator,
span_start: std.ArrayList(u8),
span_end: std.ArrayList(u8),
ast_node: std.ArrayList(Parser.AstNode),

blocks: std.ArrayList(Parser.Block),

source: []const u8,

file_offsets: std.ArrayList(struct { fname: []const u8, offset: usize, end: usize }),

pub fn new(alloc: std.mem.Allocator) Compiler {
    return Compiler{
        .alloc = alloc,
        .span_start = std.ArrayList(u8).init(alloc),
        .span_end = std.ArrayList(u8).init(alloc),
        .ast_node = std.ArrayList(Parser.AstNode).init(alloc),
        .blocks = std.ArrayList(Parser.Block).init(alloc),
        .source = undefined,
        .file_offsets = std.ArrayList(struct { fname: []const u8, offset: usize, end: usize }).init(alloc),
    };
}

pub fn print(self: *Compiler) void {
    if (self.ast_node.items.len == 0) {
        std.debug.print("<empty>", .{});
    } else {
        // TODO print helper
    }

    std.debug.print("Nodes:\n", .{});
    std.debug.print("num nodes: {}\n", .{self.ast_node.items.len});
    std.debug.print("{any}\n", .{self.ast_node.getLast()});
    for (self.ast_node.items, 0..) |node, node_id| {
        std.debug.print("{d} {any}\n", .{ node_id, node });
    }

    std.debug.print("Blocks:\n", .{});
    for (self.blocks.items, 0..) |block, block_id| {
        std.debug.print("{d} {any}\n", .{ block_id, block });
    }
}
