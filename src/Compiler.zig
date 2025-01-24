const std = @import("std");
const Parser = @import("Parser.zig");
const Errors = @import("Errors.zig");
const Typechecker = @import("Typechecker.zig");
const LifetimeChecker = @import("LifetimeChecker.zig");

const Compiler = @This();

// Core information, indexed by NodeId
alloc: std.mem.Allocator,
span_start: std.ArrayList(usize),
span_end: std.ArrayList(usize),
ast_node: std.ArrayList(Parser.AstNode),
node_types: std.ArrayList(Typechecker.TypeId),
node_lifetimes: std.ArrayList(LifetimeChecker.AllocationLifetime),

// Blocks, indexed by BlockId
blocks: std.ArrayList(Parser.Block),

source: []const u8,

file_offsets: std.ArrayList(File),

// Definitions:
// indexed by VarId
variables: std.ArrayList(Typechecker.Variable),
// indexed by FuncId
functions: std.ArrayList(Typechecker.Function),
// indexed by TypeId
types: std.ArrayList(Typechecker.Type),
// indexed by ModuleId
modules: std.ArrayList(Typechecker.Module),

// Memory reclamation
exiting_blocks: std.AutoHashMap(Parser.NodeId, std.ArrayList(Parser.BlockId)),

// Methods on types
methods_on_type: std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.FuncId)),
virtual_methods_on_type: std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.FuncId)),

// Use/def
call_resolution: std.AutoHashMap(Parser.NodeId, CallTarget),
var_resolution: std.AutoHashMap(Parser.NodeId, Typechecker.VarId),
fun_resolution: std.AutoHashMap(Parser.NodeId, Typechecker.FuncId),
type_resolution: std.AutoHashMap(Parser.NodeId, Typechecker.TypeId),

base_classes: std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.TypeId)),

errors: std.ArrayList(Errors.SourceError),

const File = struct {
    fname: []const u8,
    offset: usize,
    end: usize,
};

pub const CaseOffset = usize;

const EnumConstructor = struct {
    type_id: Typechecker.TypeId,
    case_offset: CaseOffset,
};

pub const CallTarget = union(enum) {
    function: Typechecker.FuncId,
    enum_constructor: EnumConstructor,
    node_id: Parser.NodeId,
};

pub fn new(alloc: std.mem.Allocator) Compiler {
    return Compiler{
        .alloc = alloc,
        .span_start = std.ArrayList(usize).init(alloc),
        .span_end = std.ArrayList(usize).init(alloc),
        .ast_node = std.ArrayList(Parser.AstNode).init(alloc),
        .node_types = std.ArrayList(Typechecker.TypeId).init(alloc),
        .node_lifetimes = std.ArrayList(LifetimeChecker.AllocationLifetime).init(alloc),
        .blocks = std.ArrayList(Parser.Block).init(alloc),
        .source = "",
        .file_offsets = std.ArrayList(File).init(alloc),
        .variables = std.ArrayList(Typechecker.Variable).init(alloc),
        .functions = std.ArrayList(Typechecker.Function).init(alloc),
        .types = std.ArrayList(Typechecker.Type).init(alloc),
        .modules = std.ArrayList(Typechecker.Module).init(alloc),
        .exiting_blocks = std.AutoHashMap(Parser.NodeId, std.ArrayList(Parser.BlockId)).init(alloc),
        .errors = std.ArrayList(Errors.SourceError).init(alloc),
        .call_resolution = std.AutoHashMap(Parser.NodeId, CallTarget).init(alloc),
        .var_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.VarId).init(alloc),
        .fun_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.FuncId).init(alloc),
        .type_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.TypeId).init(alloc),
        .base_classes = std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.TypeId)).init(alloc),
        .methods_on_type = std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.FuncId)).init(alloc),
        .virtual_methods_on_type = std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.FuncId)).init(alloc),
    };
}

pub fn printLifetime(self: *Compiler, node_id: usize) void {
    switch (self.node_lifetimes.items[node_id]) {
        .@"return" => std.debug.print("return", .{}),
        .unknown => std.debug.print("unknown", .{}),
        .scope => |scope| std.debug.print("Scope [ level: {} ]", .{scope.level}),
        .param => |param| std.debug.print("Param [ var_id: {} ]", .{param.var_id}),
    }
}

pub fn print(self: *Compiler) void {
    if (self.ast_node.items.len == 0) {
        std.debug.print("<empty>", .{});
    } else {
        // TODO print helper
    }

    std.debug.print("Nodes:\n", .{});
    std.debug.print("num nodes: {}\n", .{self.ast_node.items.len});
    for (self.ast_node.items, 0..) |node, node_id| {
        std.debug.print("{d} {any}, {s},    (lifetime: ", .{ node_id, node, self.prettyType(self.node_types.items[node_id]) catch unreachable });
        self.printLifetime(node_id);
        std.debug.print(")\n", .{});
        // std.debug.print("{d} {s} -> {s},    type: {s},    lifetime: {any}\n", .{ node_id, @tagName(node), self.getSource(node_id), self.prettyType(self.node_types.items[node_id]) catch unreachable, self.node_lifetimes.items[node_id] });
    }

    std.debug.print("\nBlocks:\n", .{});
    std.debug.print("num blocks: {}\n", .{self.blocks.items.len});
    for (self.blocks.items, 0..) |block, block_id| {
        std.debug.print("{d} nodes: [", .{block_id});
        for (block.nodes.items) |node| {
            std.debug.print("{d}, ", .{node});
        }
        if (block.may_locally_allocate) |allocate| {
            std.debug.print("] may_locally_allocates: {d}, \n", .{allocate});
        } else {
            std.debug.print("] may_locally_allocates: null\n", .{});
        }
    }

    std.debug.print("\nVariables:\n", .{});
    std.debug.print("num variables: {}\n", .{self.variables.items.len});
    for (self.variables.items, 0..) |_var, var_id| {
        std.debug.print("{d} name: {s}, type_id: {}, is_mutable: {}, defined_in: {s}, defined_node: {}\n", .{ var_id, self.getSource(_var.name), _var.ty, _var.is_mutable, self.getSource(_var.where_defined), _var.where_defined });
    }

    std.debug.print("\nFunctions:\n", .{});
    std.debug.print("num functions: {}\n", .{self.functions.items.len});
    for (self.functions.items, 0..) |fun, fun_id| {
        // std.debug.print("{s}\n", .{self.getSource(fun.name)});
        std.debug.print("{d} {s}\n", .{ fun_id, self.getSource(fun.name) });
    }

    std.debug.print("\nTypes:\n", .{});
    std.debug.print("num types: {}\n", .{self.types.items.len});
    for (self.types.items, 0..) |ty, ty_id| {
        // std.debug.print("{d} {any}\n", .{ ty_id, ty });
        std.debug.print("{d} {s}\n", .{ ty_id, @tagName(ty) });
    }

    std.debug.print("\nCall resolution:\n", .{});
    std.debug.print("num calls: {}\n", .{self.call_resolution.count()});
    var iter = self.call_resolution.iterator();
    while (iter.next()) |call| {
        const node_id_source = self.getSource(call.key_ptr.*);
        var call_target: []u8 = "";
        switch (call.value_ptr.*) {
            .function => {
                call_target = std.fmt.allocPrint(self.alloc, "function {d}", .{call.value_ptr.*.function}) catch unreachable;
            },
            .node_id => {
                call_target = @constCast(self.getSource(call.value_ptr.*.node_id));
            },
            else => {},
        }
        std.debug.print("node_id: {s} -> target: {s}\n", .{ node_id_source, call_target });
    }

    std.debug.print("\n", .{});
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
    // +1 for the newline at the start of the file
    const span_offset = self.source.len + 1;
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

pub fn getType(self: *Compiler, type_id: Typechecker.TypeId) Typechecker.Type {
    return self.types.items[type_id];
}

pub fn getMutType(self: *Compiler, type_id: Typechecker.TypeId) *Typechecker.Type {
    return &self.types.items[type_id];
}

pub fn getNodeType(self: *Compiler, node_id: Parser.NodeId) Parser.NodeId {
    return self.node_types.items[node_id];
}

pub fn setNodeType(self: *Compiler, node_id: Parser.NodeId, type_id: Typechecker.TypeId) void {
    self.node_types.items[node_id] = type_id;
}

pub fn getVariable(self: *Compiler, var_id: Typechecker.VarId) Typechecker.Variable {
    return self.variables.items[var_id];
}

pub fn getVariableName(self: *Compiler, var_id: Typechecker.VarId) []const u8 {
    return self.getSource(self.variables.items[var_id].name);
}

pub fn pushType(self: *Compiler, ty: Typechecker.Type) !Typechecker.TypeId {
    try self.types.append(ty);
    return self.types.items.len - 1;
}

pub fn hasMain(self: *Compiler) bool {
    for (self.functions.items) |fun| {
        if (std.mem.eql(u8, self.getSource(fun.name), "main")) {
            return true;
        }
    }
    return false;
}

pub fn getUnderlyingTypeId(self: *Compiler, type_id: Typechecker.TypeId) Typechecker.TypeId {
    const ty = self.getType(type_id);
    return switch (ty) {
        .pointer => |pt| pt.target,
        else => type_id,
    };
}

pub fn resizeNodeTypes(self: *Compiler, size: usize, type_id: Typechecker.TypeId) !void {
    const old_len = self.node_types.items.len;
    try self.node_types.resize(size);
    for (self.node_types.items, 0..) |*ty, idx| {
        if (idx >= old_len) {
            ty.* = type_id;
        }
    }
}

pub fn resizeNodeLifetimes(self: *Compiler, size: usize) !void {
    const old_len = self.node_lifetimes.items.len;
    try self.node_lifetimes.resize(size);
    for (self.node_lifetimes.items, 0..) |*life, idx| {
        if (idx >= old_len) {
            life.* = .{
                .unknown = Parser.Void.void,
            };
        }
    }
}

pub fn getNodeLifetime(self: *Compiler, node_id: Parser.NodeId) LifetimeChecker.AllocationLifetime {
    return self.node_lifetimes.items[node_id];
}

pub fn setNodeLifetime(self: *Compiler, node_id: Parser.NodeId, allocation_lifetime: LifetimeChecker.AllocationLifetime) void {
    self.node_lifetimes.items[node_id] = allocation_lifetime;
}

pub fn findType(self: *Compiler, ty: Typechecker.Type) Typechecker.TypeId {
    for (self.types.items, 0..) |*t, idx| {
        if (std.meta.eql(t.*, ty)) {
            return idx;
        }
    }

    const error_msg = std.fmt.allocPrint(self.alloc, "internal error: can't find {any} as a TypeId", .{ty}) catch unreachable;
    @panic(error_msg);
}

pub fn findOrCreateType(self: *Compiler, ty: Typechecker.Type) !Typechecker.TypeId {
    for (self.types.items, 0..) |*t, idx| {
        if (std.meta.eql(t.*, ty)) {
            return idx;
        }
    }

    _ = try self.pushType(ty);
    return self.types.items.len - 1;
}

pub fn isTypeVariable(self: *Compiler, type_id: Typechecker.TypeId) bool {
    const ty = self.types.items[type_id];
    return switch (ty) {
        .type_variable => true,
        else => false,
    };
}

pub fn resolveType(self: *Compiler, type_id: Typechecker.TypeId, local_inferences: *std.ArrayList(Typechecker.TypeId)) Typechecker.TypeId {
    const ty = self.getType(type_id);
    return switch (ty) {
        .fun_local_type_val => |tt| local_inferences.items[tt.offset],
        else => type_id,
    };
}

pub fn resolveNodeType(self: *Compiler, node_id: Parser.NodeId, local_inferences: *std.ArrayList(Typechecker.TypeId)) Typechecker.TypeId {
    switch (self.types.items[self.node_types.items[node_id]]) {
        .fun_local_type_val => |ty| return local_inferences.items[ty.offset],
        .raw_buffer => |inner_type_id| {
            const resolved_inner_type_id = self.resolveType(inner_type_id, local_inferences);
            return self.findType(.{ .raw_buffer = resolved_inner_type_id });
        },
        else => return self.node_types.items[node_id],
    }
}

pub fn freshTypeVariable(self: *Compiler, node_id: Parser.NodeId) !Typechecker.TypeId {
    try self.types.append(Typechecker.Type{ .type_variable = node_id });
    return self.types.items.len - 1;
}

pub fn isCopyableType(self: *Compiler, type_id: Typechecker.TypeId) bool {
    const ty = self.getType(type_id);
    return switch (ty) {
        .bool => true,
        .i64 => true,
        .f64 => true,
        else => false,
    };
}

pub fn findPointerTo(self: *Compiler, type_id: Typechecker.TypeId) ?Typechecker.TypeId {
    for (self.types.items, 0..) |ty, found_type_id| {
        switch (ty) {
            .pointer => |pt| {
                if (pt.target == type_id) {
                    return found_type_id;
                }
            },
            else => {},
        }
    }
    return null;
}

pub fn isAllocatorType(self: *Compiler, type_id: Typechecker.TypeId) bool {
    switch (self.types.items[type_id]) {
        .pointer => |ptr_ty| return self.isAllocatorType(ptr_ty.target),
        .@"struct" => |s| return s.is_allocator,
        else => return false,
    }
}

pub fn methodsOnType(self: *Compiler, idx: Typechecker.TypeId) std.ArrayList(Typechecker.FuncId) {
    return self.methods_on_type.get(idx).?;
}

pub fn virtualMethodsOnType(self: *Compiler, idx: Typechecker.TypeId) std.ArrayList(Typechecker.FuncId) {
    const virt_method = self.virtual_methods_on_type.get(idx);
    if (virt_method) |methods| {
        return methods;
    } else {
        return std.ArrayList(Typechecker.FuncId).init(self.alloc);
    }
}

pub fn insertMethodsOnType(self: *Compiler, type_id: Typechecker.TypeId, methods: std.ArrayList(Typechecker.FuncId)) !void {
    try self.methods_on_type.put(type_id, methods);
}

pub fn insertVirtualMethodsOnType(self: *Compiler, type_id: Typechecker.TypeId, methods: std.ArrayList(Typechecker.FuncId)) !void {
    if (!(self.methods_on_type.count() == 0)) {
        try self.virtual_methods_on_type.put(type_id, methods);
    }
}

pub fn hasUnsatisfiedVirtualMethods(self: *Compiler, idx: Typechecker.TypeId) bool {
    if (!(self.virtualMethodsOnType(idx).items.len == 0)) {
        return true;
    }

    // TODO flatten the list
    // for (self.base_classes.get(idx).items) |base_class| {
    //     if (!self.hasFullySatisfiesVirtualMethods(base_class)) {
    //         return true;
    //     }
    // }

    return false;
}

pub fn prettyType(self: *Compiler, type_id: Typechecker.TypeId) ![]const u8 {
    switch (self.getType(type_id)) {
        .bool => return "bool",
        .c_char => return "c_char",
        .c_external_type => |node_id| {
            const s = try std.fmt.allocPrint(self.alloc, "extern({s})", .{self.getSource(node_id)});
            return s;
        },
        .c_int => return "c_int",
        .c_size_t => return "c_size_t",
        .c_string => return "c_string",
        .c_void_ptr => return "c_void_ptr",
        .@"enum" => {
            //FIXME: give this a name
            return "enum";
        },
        .fun => |fun| {
            var output = std.ArrayList(u8).init(self.alloc);
            var first = true;
            for (fun.params.items) |param| {
                if (!first) {
                    try output.appendSlice(", ");
                } else {
                    first = false;
                }
                const v_id = try std.fmt.allocPrint(self.alloc, "{d}", .{self.getVariable(param.var_id).ty});
                try output.appendSlice(v_id);
            }

            try output.appendSlice(") -> ");
            try output.appendSlice(try self.prettyType(fun.ret));
            return try output.toOwnedSlice();
        },
        .i64 => return "i64",
        .pointer => |pt| {
            var output: []const u8 = "";
            switch (pt.pointer_type) {
                .Owned => output = "owned ",
                .Shared => output = "shared ",
                else => {},
            }
            output = try std.fmt.allocPrint(self.alloc, "{s} {s}", .{ output, try self.prettyType(pt.target) });
            if (pt.optional) {
                output = try std.fmt.allocPrint(self.alloc, "{s}?", .{output});
            }
            return output;
        },
        .range => |id| {
            return try std.fmt.allocPrint(self.alloc, "range({s})", .{try self.prettyType(id)});
        },
        .raw_buffer => |id| {
            return try std.fmt.allocPrint(self.alloc, "[{s}]", .{try self.prettyType(id)});
        },
        .@"struct" => |_| {
            // FIXME: need more info
            return try std.fmt.allocPrint(self.alloc, "struct()", .{});
        },
        .fun_local_type_val => |ty| {
            return try std.fmt.allocPrint(self.alloc, "<local typevar: {d}>", .{ty.offset});
        },
        .type_variable => |id| {
            return try std.fmt.allocPrint(self.alloc, "<{s}>", .{self.getSource(id)});
        },
        .unknown => return "unknown",
        .void => return "void",
        else => return "pretty type not implemented",
    }
}
