const std = @import("std");
const Parser = @import("Parser.zig");
const Errors = @import("Errors.zig");

const Compiler = @This();

alloc: std.mem.Allocator,
span_start: std.ArrayList(u8),
span_end: std.ArrayList(u8),
ast_node: std.ArrayList(Parser.AstNode),

blocks: std.ArrayList(Parser.Block),

source: []const u8,

file_offsets: std.ArrayList(struct { fname: []const u8, offset: usize, end: usize }),

errors: std.ArrayList(Errors.SourceError),

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

pub fn pushNode(self: *Compiler, ast_node: Parser.NodeId) !Parser.NodeId {
    try self.ast_node.append(ast_node);
    return self.ast_node.items.len - 1;
}

pub fn createNode(self: *Compiler, ast_node: Parser.AstNode, span_start: usize, span_end: usize) !Parser.NodeId {
    try self.span_start.append(span_start);
    try self.span_end.append(span_end);
    return try self.pushNode(ast_node);
}

pub fn getNode(self: *Compiler, node_id: Parser.NodeId) Parser.AstNode {
    return self.ast_node.items[node_id];
}

pub fn numAstNodes(self: *Compiler) usize {
    return self.ast_node.items.len;
}

pub fn getSource(self: *Compiler, node_id: Parser.NodeId) []const u8 {
    return self.source[self.span_start.items[node_id]..self.span_end.items[node_id]];
}
