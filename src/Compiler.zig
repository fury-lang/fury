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

// `modules` and `module_resolution` are populated by the typechecker
// `module_lookup` and `module_lookup_use` are populated by the parser
// lookup the node_id of the parsed block representing a module using it's absolute path
module_lookup: std.StringHashMap(Parser.NodeId),
// lookup the id of the block (eg the entire module's loaded source) from the node_id of the path in the use statement
module_lookup_use: std.AutoHashMap(Parser.NodeId, Parser.NodeId),

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

// lookup the id of a Module from the id of the block (value field from module_lookup_use) in the ast
module_resolution: std.AutoHashMap(Parser.NodeId, Typechecker.ModuleId),
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

pub fn init(alloc: std.mem.Allocator) Compiler {
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
        .module_lookup = std.StringHashMap(Parser.NodeId).init(alloc),
        .module_lookup_use = std.AutoHashMap(Parser.NodeId, Parser.NodeId).init(alloc),
        .exiting_blocks = std.AutoHashMap(Parser.NodeId, std.ArrayList(Parser.BlockId)).init(alloc),
        .errors = std.ArrayList(Errors.SourceError).init(alloc),
        .call_resolution = std.AutoHashMap(Parser.NodeId, CallTarget).init(alloc),
        .var_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.VarId).init(alloc),
        .fun_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.FuncId).init(alloc),
        .type_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.TypeId).init(alloc),
        .module_resolution = std.AutoHashMap(Parser.NodeId, Typechecker.ModuleId).init(alloc),
        .base_classes = std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.TypeId)).init(alloc),
        .methods_on_type = std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.FuncId)).init(alloc),
        .virtual_methods_on_type = std.AutoHashMap(Typechecker.TypeId, std.ArrayList(Typechecker.FuncId)).init(alloc),
    };
}

pub fn deinit(self: *Compiler) void {
    self.span_start.deinit();
    self.span_end.deinit();

    for (self.ast_node.items) |node| {
        switch (node) {
            .fun_type => |fun_type| fun_type.params.deinit(),
            .fun => |fun| fun.lifetime_annotations.deinit(),
            .params => |params| params.deinit(),
            .@"struct" => |s| {
                s.fields.deinit();
                s.methods.deinit();
            },
            .@"enum" => |e| {
                e.cases.deinit();
                e.methods.deinit();
            },
            .enum_case => |e_case| {
                if (e_case.payload) |payload| {
                    payload.deinit();
                }
            },
            .call => |call| call.args.deinit(),
            .match => |match| match.match_arms.deinit(),
            .raw_buffer => |buffer| buffer.deinit(),
            else => {},
        }
    }
    self.ast_node.deinit();

    self.node_types.deinit();
    self.node_lifetimes.deinit();

    for (self.blocks.items) |block| {
        block.nodes.deinit();
    }
    self.blocks.deinit();

    self.file_offsets.deinit();
    self.variables.deinit();

    for (self.functions.items) |fun| {
        fun.params.deinit();
        fun.lifetime_annotations.deinit();
        fun.type_params.deinit();
        fun.inference_vars.deinit();
    }
    self.functions.deinit();

    for (self.types.items) |ty| {
        switch (ty) {
            .fun => |f| f.params.deinit(),
            .@"struct" => |s| {
                s.generic_params.deinit();
                s.fields.deinit();
            },
            .@"enum" => |e| {
                e.generic_params.deinit();
                for (e.variants.items) |variant| {
                    switch (variant) {
                        .@"struct" => |var_st| var_st.params.deinit(),
                        else => {},
                    }
                }
                e.variants.deinit();
            },
            else => {},
        }
    }
    self.types.deinit();

    for (self.modules.items) |*module| {
        module.scope.deinit();
    }
    self.modules.deinit();

    self.module_lookup.deinit();
    self.module_lookup_use.deinit();

    var exiting_iter = self.exiting_blocks.iterator();
    while (exiting_iter.next()) |exit_entry| {
        exit_entry.value_ptr.*.deinit();
    }
    self.exiting_blocks.deinit();

    var method_iter = self.methods_on_type.iterator();
    while (method_iter.next()) |method_entry| {
        method_entry.value_ptr.*.deinit();
    }
    self.methods_on_type.deinit();

    var virtual_iter = self.virtual_methods_on_type.iterator();
    while (virtual_iter.next()) |virtual_entry| {
        virtual_entry.value_ptr.*.deinit();
    }
    self.virtual_methods_on_type.deinit();

    self.call_resolution.deinit();
    self.var_resolution.deinit();
    self.fun_resolution.deinit();
    self.type_resolution.deinit();
    self.module_resolution.deinit();
    var base_classes_iter = self.base_classes.iterator();
    while (base_classes_iter.next()) |class_entry| {
        class_entry.value_ptr.*.deinit();
    }
    self.base_classes.deinit();

    for (self.errors.items) |err| {
        self.alloc.free(err.message);
    }
    self.errors.deinit();
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
        std.debug.print("{}, {any}  (", .{ node_id, node });
        // std.debug.print("\n", .{});
        // std.debug.print("{d} {any},    (lifetime: ", .{ node_id, node });
        // std.debug.print("{}: {s}: ", .{ node_id, self.getSource(node_id) });
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
    for (self.variables.items, 0..) |variable, var_id| {
        std.debug.print("{d} name: {s}, type_id: {}, is_mutable: {}, defined_in: {s}, defined_node: {}\n", .{ var_id, self.getSource(variable.name), variable.ty, variable.is_mutable, self.getSource(variable.where_defined), variable.where_defined });
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

pub fn addModule(self: *Compiler, module_block: Parser.NodeId, module: Typechecker.Module) !Typechecker.ModuleId {
    const module_id = self.modules.items.len;
    try self.modules.append(module);
    try self.module_resolution.put(module_block, module_id);
    return module_id;
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
            .pointer => |ptr_ty| {
                if (ptr_ty.target == type_id) {
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
    if (self.methods_on_type.get(idx)) |list| {
        return list;
    } else {
        return std.ArrayList(Typechecker.FuncId).init(self.alloc);
    }
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
    if (!(methods.items.len == 0)) {
        try self.virtual_methods_on_type.put(type_id, methods);
    }
}

pub fn replaceNode(self: *Compiler, node_id: Parser.NodeId, target_ty: Parser.NodeId) !Parser.NodeId {
    const node = self.getNode(node_id);
    const span_start = self.span_start.items[node_id];
    const span_end = self.span_end.items[node_id];

    var new_node_id = self.numAstNodes();
    const new_node = Parser.AstNode{ .type_coercion = .{
        .source_node = new_node_id,
        .target_type = target_ty,
    } };
    new_node_id = try self.createNode(node, span_start, span_end);

    if (self.var_resolution.fetchRemove(node_id)) |entry| {
        try self.var_resolution.put(new_node_id, entry.value);
    }

    try self.node_types.append(Typechecker.UNKNOWN_TYPE_ID);
    self.ast_node.items[node_id] = new_node;

    // swapping
    const tmp = self.node_types.items[node_id];
    self.node_types.items[node_id] = self.node_types.items[new_node_id];
    self.node_types.items[new_node_id] = tmp;

    return new_node_id;
}

pub fn hasUnsatisfiedVirtualMethods(self: *Compiler, idx: Typechecker.TypeId) bool {
    const methods = self.virtualMethodsOnType(idx);
    if (!(methods.items.len == 0)) {
        return true;
    }

    const base_classes = self.base_classes.get(idx);
    if (base_classes) |classes| {
        for (classes.items) |class| {
            if (self.fullySatifiesVirtualMethods(idx, class)) |cond| {
                if (!cond) {
                    return true;
                }
            }
        }
    }

    return false;
}

pub fn fullySatifiesVirtualMethods(self: *Compiler, type_id: Typechecker.TypeId, base_class: Typechecker.TypeId) ?bool {
    const ty_id = self.getUnderlyingTypeId(type_id);
    const base_id = self.getUnderlyingTypeId(base_class);

    const virtual_methods = self.virtual_methods_on_type.get(base_id);
    if (virtual_methods == null) {
        return null;
    }

    const methods = self.methodsOnType(ty_id);

    var virtual_methods_map = std.StringHashMap(Typechecker.FuncId).init(self.alloc);
    for (virtual_methods.?.items) |id| {
        const node_id = self.functions.items[id].name;
        virtual_methods_map.put(self.getSource(node_id), id) catch unreachable;
    }

    // TODO use set here in future
    var methods_names = std.StringHashMap(Typechecker.FuncId).init(self.alloc);
    for (methods.items) |id| {
        const node_id = self.functions.items[id].name;
        if (methods_names.get(self.getSource(node_id))) |_| {} else {
            methods_names.put(self.getSource(node_id), id) catch unreachable;
        }
    }

    var iter = virtual_methods_map.iterator();
    while (iter.next()) |entry| {
        if (!methods_names.contains(entry.key_ptr.*)) {
            return false;
        }
    }

    return true;
}

pub fn getSourcePath(self: *Compiler, node_id: Parser.NodeId) []const u8 {
    const position = self.span_start.items[node_id];
    for (self.file_offsets.items) |file| {
        if (position >= file.offset and position < file.end) {
            return file.fname;
        }
    }

    @panic("position should always be a valid offset into source content that's already been loaded");
}

pub fn isGenericType(self: *Compiler, type_id: Typechecker.TypeId, list: std.ArrayList(Typechecker.TypeId)) !bool {
    // The `seen` parameter is used to protect the recursion from going infinite. Once we see a type,
    // before we destructure it, we log that we have seen it so we do not check it again.
    var seen = try list.clone();
    for (seen.items) |item| {
        if (item == type_id) {
            return false;
        }
    }

    try seen.append(type_id);

    switch (self.getType(type_id)) {
        .bool, .c_char, .c_external_type, .c_string, .c_void_ptr, .void, .i64, .f64, .c_int, .c_size_t => return false,
        .type_variable, .fun_local_type_val, .unknown => return true,
        .range => |x| return try self.isGenericType(x, try seen.clone()),
        .raw_buffer => |x| return try self.isGenericType(x, try seen.clone()),
        .fun => |f| {
            for (f.params.items) |x| {
                const variable = self.getVariable(x.var_id);
                if (try self.isGenericType(variable.ty, try seen.clone())) {
                    return true;
                }
                return try self.isGenericType(f.ret, try seen.clone());
            }
        },
        .pointer => |pt| return try self.isGenericType(pt.target, try seen.clone()),
        .@"struct" => |s| {
            if (!(s.generic_params.items.len == 0)) return true;
            for (s.fields.items) |field| {
                if (try self.isGenericType(field.ty, try seen.clone())) {
                    return true;
                }
            }
            return false;
        },
        .@"enum" => |e| {
            if (!(e.generic_params.items.len == 0)) return true;
            for (e.variants.items) |item| {
                switch (item) {
                    .simple => return false,
                    .single => |single| return try self.isGenericType(single.param, try seen.clone()),
                    .@"struct" => |s| {
                        for (s.params.items) |param| {
                            if (try self.isGenericType(param.ty, try seen.clone())) {
                                return true;
                            }
                        }
                    },
                }
            }
            return false;
        },
    }

    unreachable;
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
        .pointer => |ptr_ty| {
            var output: []const u8 = "";
            switch (ptr_ty.pointer_type) {
                .Owned => output = "owned ",
                .Shared => output = "shared ",
                else => {},
            }
            output = try std.fmt.allocPrint(self.alloc, "{s} {s}", .{ output, try self.prettyType(ptr_ty.target) });
            if (ptr_ty.optional) {
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
