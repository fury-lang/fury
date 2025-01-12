const std = @import("std");
const Parser = @import("Parser.zig");
const Errors = @import("Errors.zig");

const Compiler = @This();

alloc: std.mem.Allocator,
span_start: std.ArrayList(usize),
span_end: std.ArrayList(usize),
ast_node: std.ArrayList(Parser.AstNode),

blocks: std.ArrayList(Parser.Block),

source: []const u8,

file_offsets: std.ArrayList(File),

errors: std.ArrayList(Errors.SourceError),

const File = struct {
    fname: []const u8,
    offset: usize,
    end: usize,
};

pub fn new(alloc: std.mem.Allocator) Compiler {
    return Compiler{
        .alloc = alloc,
        .span_start = std.ArrayList(usize).init(alloc),
        .span_end = std.ArrayList(usize).init(alloc),
        .ast_node = std.ArrayList(Parser.AstNode).init(alloc),
        .blocks = std.ArrayList(Parser.Block).init(alloc),
        .source = "",
        .file_offsets = std.ArrayList(File).init(alloc),
        .errors = std.ArrayList(Errors.SourceError).init(alloc),
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

pub fn printErrors(self: *Compiler, err: *Errors.SourceError) !void {
    const span_start = self.span_start.items[err.node_id];
    const span_end = self.span_end.items[err.node_id];

    var file_name: []const u8 = "unknown";
    var file_span_start: usize = 0;
    var file_span_end: usize = 0;

    for (self.file_offsets.items) |file| {
        if (span_start >= file.offset and span_start < file.end) {
            file_name = file.fname;
            file_span_start = file.offset;
            file_span_end = file.end;
            break;
        }
    }

    const line_ext = self.lineExtents(span_start, file_span_start, file_span_end);

    const line_number_width = (try std.fmt.allocPrint(self.alloc, "{d}", .{line_ext[0]})).len;

    var max_number_width = line_number_width;
    if (line_ext[1] + 1 < file_span_end) {
        const next_line_number = self.lineExtents(line_ext[1] + 1, file_span_start, file_span_end);
        max_number_width = (try std.fmt.allocPrint(self.alloc, "{d}", .{next_line_number[0]})).len;
    }

    for (0..(max_number_width + 2)) |_| {
        std.debug.print("─", .{});
    }

    std.debug.print("┬─ \x1b[0;36m{s}:{d}:{d}\x1b[0m\n", .{ file_name, line_ext[0], span_start - line_ext[1] + 1 });

    // Previous line in the source code, if available
    if (line_ext[1] > file_span_start + 1) {
        const prev_line_ext = self.lineExtents(line_ext[1] - 1, file_span_start, file_span_end);
        const prev_line_number_str = try std.fmt.allocPrint(self.alloc, "{d}", .{prev_line_ext[0]});

        for (0..(max_number_width - prev_line_number_str.len)) |_| {
            std.debug.print(" ", .{});
        }

        std.debug.print(" {s} │ {s}\n", .{ prev_line_number_str, self.source[prev_line_ext[1]..prev_line_ext[2]] });
    }

    // Line being highlighted
    for (0..(max_number_width - line_number_width)) |_| {
        std.debug.print(" ", .{});
    }

    std.debug.print(" {d} │ {s}\n", .{ line_ext[0], self.source[line_ext[1]..line_ext[2]] });

    for (0..(max_number_width + 2)) |_| {
        std.debug.print(" ", .{});
    }

    std.debug.print("│", .{});

    for (0..(span_start - line_ext[1] + 1)) |_| {
        std.debug.print(" ", .{});
    }

    switch (err.severity) {
        Errors.Severity.Error => {
            std.debug.print("\x1b[0;31m", .{});
            for (span_start..span_end) |_| {
                std.debug.print("╍", .{});
            }
            std.debug.print(" error: {s}\n", .{err.message});
        },
        Errors.Severity.Note => {
            std.debug.print("\x1b[0;34m", .{});
            for (span_start..span_end) |_| {
                std.debug.print("╍", .{});
            }
            std.debug.print(" note: {s}\n", .{err.message});
        },
    }
    std.debug.print("\x1b[0m", .{});

    // Next line after error, for context
    if (line_ext[2] < file_span_end) {
        const next_line_ext = self.lineExtents(line_ext[2] + 1, file_span_start, file_span_end);
        std.debug.print(" {d} │ {s}\n", .{ next_line_ext[0], self.source[next_line_ext[1]..next_line_ext[2]] });
    }

    for (0..(max_number_width + 2)) |_| {
        std.debug.print("─", .{});
    }

    std.debug.print("┴─\n", .{});
}

pub fn lineExtents(self: *Compiler, span_position: usize, file_span_start: usize, file_span_end: usize) [3]usize {
    const contents = self.source;

    const line_number = std.mem.count(u8, contents[0..span_position], "\n");
    var line_start = span_position;

    if (line_start > file_span_start and contents[line_start] == '\n') {
        line_start -= 1;
    }

    while (line_start > file_span_start and contents[line_start] != '\n') {
        line_start -= 1;
    }

    if (contents[line_start] == '\n') {
        line_start += 1;
    }

    var line_end = span_position;

    while (line_end < file_span_end and contents[line_end] != '\n') {
        line_end += 1;
    }

    return [_]usize{ line_number, line_start, line_end };
}

pub fn pushNode(self: *Compiler, ast_node: Parser.AstNode) !Parser.NodeId {
    try self.ast_node.append(ast_node);
    return self.ast_node.items.len - 1;
}

pub fn createNode(self: *Compiler, ast_node: Parser.AstNode, span_start: usize, span_end: usize) !Parser.NodeId {
    try self.span_start.append(span_start);
    try self.span_end.append(span_end);
    return try self.pushNode(ast_node);
}

pub fn spanOffset(self: *Compiler) usize {
    return self.source.len;
}

pub fn addFile(self: *Compiler, file_name: []const u8) !void {
    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const span_offset = self.source.len;
    const content = try self.alloc.alloc(u8, file_size);
    _ = try file.read(content);

    try self.file_offsets.append(File{ .fname = file_name, .offset = span_offset, .end = span_offset + file_size });

    const new_source = try std.fmt.allocPrint(self.alloc, "{s}\n{s}", .{ self.source, content });
    self.source = new_source;
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
