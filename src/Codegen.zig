const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Typechecker = @import("Typechecker.zig");

const Codegen = @This();

alloc: std.mem.Allocator,
compiler: Compiler,

pub fn new(alloc: std.mem.Allocator, compiler: Compiler) !Codegen {
    return Codegen{
        .alloc = alloc,
        .compiler = compiler,
    };
}

pub fn codegenTypename(self: *Codegen, type_id: Typechecker.TypeId, local_inferences: *std.ArrayList(Typechecker.TypeId), output: *std.ArrayList(u8)) anyerror!void {
    switch (self.compiler.getType(type_id)) {
        .fun_local_type_val => |ty| {
            try self.codegenTypename(local_inferences.items[ty.offset], local_inferences, output);
        },
        .@"struct" => unreachable,
        .@"enum" => unreachable,
        .pointer => |pt| {
            try self.codegenTypename(pt.target, local_inferences, output);
            try output.appendSlice("*");
        },
        .fun => {
            try output.appendSlice("fun_ty_");
            const id = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
            try output.appendSlice(id);
        },
        .c_external_type => unreachable,
        .raw_buffer => unreachable,
        else => {
            if (type_id == Typechecker.VOID_TYPE_ID) {
                try output.appendSlice("void");
            } else if (type_id == Typechecker.I64_TYPE_ID) {
                try output.appendSlice("int64_t");
            } else if (type_id == Typechecker.F64_TYPE_ID) {
                try output.appendSlice("double");
            } else if (type_id == Typechecker.C_STRING_TYPE_ID) {
                try output.appendSlice("const char*");
            } else if (type_id == Typechecker.C_VOID_PTR_TYPE_ID) {
                try output.appendSlice("void*");
            } else if (type_id == Typechecker.C_INT_TYPE_ID) {
                try output.appendSlice("int");
            } else if (type_id == Typechecker.C_SIZE_T_TYPE_ID) {
                try output.appendSlice("size_t");
            } else if (type_id == Typechecker.C_CHAR_TYPE_ID) {
                try output.appendSlice("char");
            } else if (type_id == Typechecker.BOOL_TYPE_ID) {
                try output.appendSlice("bool");
            } else if (type_id == Typechecker.UNKNOWN_TYPE_ID) {
                @panic("(unknown) type should be resolved before codegen");
            } else {
                @panic("unknown type");
            }
        },
    }
}

pub fn codegenUserPredecls(self: *Codegen, output: *std.ArrayList(u8)) !void {
    for (self.compiler.types.items, 0..) |ty, idx| {
        switch (ty) {
            .@"struct" => unreachable,
            .@"enum" => unreachable,
            .fun => |fun| {
                const params = fun.params;
                const ret = fun.ret;

                for (params.items) |param| {
                    const var_id = param.var_id;
                    const type_id = self.compiler.getVariable(var_id).ty;
                    if (self.compiler.isTypeVariable(type_id)) continue;
                }

                if (self.compiler.isTypeVariable(ret)) continue;

                try output.appendSlice("typedef ");
                var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                try self.codegenTypename(ret, &local_inference, output);
                try output.appendSlice("(*fun_ty_");
                const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(idx_str);
                // FIXME: we may not always have an allocation_id
                try output.appendSlice(")(long");
                for (params.items) |param| {
                    try output.appendSlice(", ");
                    const var_type_id = self.compiler.getVariable(param.var_id).ty;
                    var local_inference_0 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(var_type_id, &local_inference_0, output);
                }
                try output.appendSlice(");\n");
            },
            else => {},
        }
    }
}

pub fn codegenUserTypes(self: *Codegen, output: *std.ArrayList(u8)) !void {
    for (self.compiler.types.items, 0..) |ty, idx| {
        switch (ty) {
            .@"struct" => unreachable,
            .@"enum" => unreachable,
            .raw_buffer => unreachable,
            .fun => |fun| {
                const params = fun.params;
                const ret = fun.ret;

                for (params.items) |param| {
                    const var_id = param.var_id;
                    const type_id = self.compiler.getVariable(var_id).ty;
                    if (self.compiler.isTypeVariable(type_id)) continue;
                }

                if (self.compiler.isTypeVariable(ret)) continue;

                try output.appendSlice("typedef ");
                var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                try self.codegenTypename(ret, &local_inference, output);
                try output.appendSlice("(*fun_ty_");
                const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(idx_str);
                // FIXME: we may not always have an allocation_id
                try output.appendSlice(")(long");
                for (params.items) |param| {
                    try output.appendSlice(", ");
                    const var_type_id = self.compiler.getVariable(param.var_id).ty;
                    var local_inference_0 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(var_type_id, &local_inference_0, output);
                }
                try output.appendSlice(");\n");
            },
            else => {},
        }
    }
}

pub fn codegenFunSignature(self: *Codegen, fun_id: Typechecker.FuncId, params: *std.ArrayList(Typechecker.Param), return_type: Typechecker.TypeId, output: *std.ArrayList(u8), is_extern_c: bool) !void {
    try self.codegenTypename(return_type, &self.compiler.functions.items[fun_id].inference_vars, output);
    try output.append(' ');

    if (is_extern_c) {
        try output.appendSlice(self.compiler.getSource(self.compiler.functions.items[fun_id].name));
        try output.append('(');
    } else {
        try output.appendSlice("/* ");
        try output.appendSlice(self.compiler.getSource(self.compiler.functions.items[fun_id].name));
        try output.appendSlice(" */ ");

        try output.appendSlice("function_");
        const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{fun_id});
        try output.appendSlice(idx_str);
        try output.append('(');

        try output.appendSlice("long allocation_id");
    }

    var first = is_extern_c;
    for (params.items) |param| {
        if (!first) {
            try output.appendSlice(", ");
        } else {
            first = false;
        }

        const variable_ty = self.compiler.getVariable(param.var_id).ty;
        try self.codegenTypename(variable_ty, &self.compiler.functions.items[fun_id].inference_vars, output);

        try output.append(' ');
        try output.appendSlice("variable_");
        const var_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{param.var_id});
        try output.appendSlice(var_id_str);
    }

    try output.append(')');
}

pub fn codegenAnnotation(self: *Codegen, node_id: Parser.NodeId, output: *std.ArrayList(u8)) !void {
    _ = self;
    _ = node_id;
    // TODO check lifetime scope

    try output.appendSlice("allocation_id + 1");
}

pub fn codegenNode(self: *Codegen, node_id: Parser.NodeId, local_inferences: *std.ArrayList(Typechecker.TypeId), output: *std.ArrayList(u8)) anyerror!void {
    switch (self.compiler.getNode(node_id)) {
        .c_string, .c_char, .float => {
            const src = self.compiler.getSource(node_id);
            try output.appendSlice(src);
        },
        .int => {
            const src = self.compiler.getSource(node_id);
            try output.appendSlice(src);
            try output.appendSlice("LL");
        },
        .none => try output.appendSlice("NULL"),
        .name => {
            if (self.compiler.var_resolution.get(node_id)) |var_id| {
                try output.appendSlice("/* ");
                try output.appendSlice(self.compiler.getSource(self.compiler.getVariable(var_id).name));

                try output.appendSlice(" */");

                try output.appendSlice("variable_");
                const var_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{var_id});
                try output.appendSlice(var_id_str);
            } else if (self.compiler.fun_resolution.get(node_id)) |fun_id| {
                try output.appendSlice("/* ");
                try output.appendSlice(self.compiler.getSource(self.compiler.functions.items[fun_id].name));

                try output.appendSlice(" */");

                try output.appendSlice("function_");
                const fun_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{fun_id});
                try output.appendSlice(fun_id_str);
            } else {
                const src = self.compiler.getSource(node_id);
                try output.appendSlice(src);
            }
        },
        .let => unreachable,
        .plus => try output.append('+'),
        .minus => try output.append('-'),
        .multiply => try output.append('*'),
        .divide => try output.append('/'),
        .assignment => try output.append('='),
        .less_than => try output.append('<'),
        .less_than_or_equal => try output.appendSlice("<="),
        .equals => try output.appendSlice("=="),
        .not_equals => try output.appendSlice("!="),
        .greater_than => try output.append('>'),
        .@"and" => try output.appendSlice("&&"),
        .@"or" => try output.appendSlice("||"),
        .bitwise_or => try output.append('|'),
        .shift_left => try output.appendSlice("<<"),
        .shift_right => try output.appendSlice(">>"),
        .greater_than_or_equal => try output.appendSlice(">="),
        .add_assignment => try output.appendSlice("+="),
        .subtract_assignment => try output.appendSlice("-="),
        .multiply_assignment => try output.appendSlice("*="),
        .divide_assignment => try output.appendSlice("/="),
        .binary_op => |_| {
            // const lhs = bin_op.left;
            // const op = bin_op.op;
            // const rhs = bin_op.right;

            unreachable;
        },
        .call => |call| {
            const head = call.head;
            const args = call.args;

            var call_target: Compiler.CallTarget = undefined;
            if (self.compiler.call_resolution.get(head)) |c_tar| {
                call_target = c_tar;
            } else {
                @panic("internal error: missing call resolution in codegen");
            }

            switch (call_target) {
                .function => |fun_id| {
                    const fun = self.compiler.functions.items[fun_id];
                    if (fun_id == 0) {
                        // special case for println
                        const ty = self.compiler.resolveNodeType(args.items[0], local_inferences);
                        if (ty == Typechecker.C_STRING_TYPE_ID) {
                            try output.appendSlice("printf(\"%s\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                        } else if (ty == Typechecker.I64_TYPE_ID) {
                            try output.appendSlice("printf(\"%lli\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                        } else if (ty == Typechecker.F64_TYPE_ID) {
                            try output.appendSlice("printf(\"%lf\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                        } else if (ty == Typechecker.BOOL_TYPE_ID) {
                            try output.appendSlice("printf(\"%s\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                            try output.appendSlice("? \"true\" : \"false\"");
                        } else if (ty == Typechecker.C_INT_TYPE_ID) {
                            try output.appendSlice("printf(\"%i\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                        } else if (ty == Typechecker.C_SIZE_T_TYPE_ID) {
                            try output.appendSlice("printf(\"%li\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                        } else if (ty == Typechecker.C_CHAR_TYPE_ID) {
                            try output.appendSlice("printf(\"%c\\n\", ");
                            try self.codegenNode(args.items[0], local_inferences, output);
                        } else {
                            @panic("unknown type for printf");
                        }
                        try output.appendSlice(");\n");
                        return;
                    }

                    var first = true;
                    if (fun.body) |_| {
                        try output.appendSlice("/* ");
                        try output.appendSlice(self.compiler.getSource(fun.name));
                        try output.appendSlice(" */");

                        try output.appendSlice("function_");
                        const fun_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{fun_id});
                        try output.appendSlice(fun_id_str);
                        try output.append('(');

                        try self.codegenAnnotation(node_id, output);
                        first = false;
                    } else if (fun.is_extern) {
                        try output.appendSlice(self.compiler.getSource(fun.name));
                        try output.append('(');
                    } else {
                        // TODO check for member access
                        try output.appendSlice(self.compiler.getSource(fun.name));
                        try output.append('(');

                        try self.codegenAnnotation(node_id, output);
                        first = false;
                    }

                    // TODO check for member access

                    for (args.items) |arg| {
                        if (!first) {
                            try output.appendSlice(", ");
                        } else {
                            first = false;
                        }

                        try self.codegenNode(arg, local_inferences, output);
                    }
                    try output.append(')');
                },
                .node_id => {
                    try output.append('(');
                    try self.codegenNode(head, local_inferences, output);
                    try output.append(')');
                    try output.append('(');

                    try self.codegenAnnotation(node_id, output);

                    for (args.items) |arg| {
                        try output.appendSlice(", ");
                        try self.codegenNode(arg, local_inferences, output);
                    }

                    try output.append(')');
                },
            }
        },
        .new => unreachable,
        .namespaced_lookup => unreachable,
        .named_value => unreachable,
        .@"break" => unreachable,
        .member_access => unreachable,
        .raw_buffer => unreachable,
        .index => unreachable,
        .statement => |stmt| {
            try self.codegenNode(stmt, local_inferences, output);
            try output.appendSlice(";\n");
        },
        .@"if" => unreachable,
        .@"while" => unreachable,
        .@"for" => unreachable,
        .@"defer" => unreachable,
        .match => unreachable,
        .block => {
            try self.codegenBlock(node_id, local_inferences, output);
        },
        .unsage_block => unreachable,
        .true => try output.appendSlice("true"),
        .false => try output.appendSlice("false"),
        .fun, .@"struct", .@"enum", .expern_type => {
            // ignore this, as we handle it elsewhere
        },
        .type_coercion => unreachable,
        else => @panic("unsupported node"),
    }
}

pub fn codegenBlock(self: *Codegen, block: Parser.NodeId, local_inferences: *std.ArrayList(Typechecker.TypeId), output: *std.ArrayList(u8)) !void {
    switch (self.compiler.getNode(block)) {
        .block => |block_id| {
            for (self.compiler.blocks.items[block_id].nodes.items) |node_id| {
                switch (self.compiler.getNode(node_id)) {
                    .@"return" => |return_expr| {
                        if (return_expr) |ret| {
                            try self.codegenTypename(self.compiler.getNodeType(ret), local_inferences, output);
                            try output.appendSlice("return_expr = ");
                            try self.codegenNode(ret, local_inferences, output);
                            try output.appendSlice(";\n");
                        }

                        // TODO check for freeing allocator level

                        if (return_expr) |_| {
                            try output.appendSlice("return return_expr;\n");
                        } else {
                            try output.appendSlice("return;\n");
                        }

                        return;
                    },
                    else => {
                        try self.codegenNode(node_id, local_inferences, output);
                        try output.appendSlice(";\n");
                    },
                }
                // TODO check for freeing allocator level
            }
        },
        else => @panic("codegen of a block that isn't a block"),
    }
}

pub fn codegenFunDecls(self: *Codegen, output: *std.ArrayList(u8)) !void {
    for (self.compiler.functions.items, 0..) |fun, idx| {
        if (idx == 0) continue;
        var params = fun.params;
        const type_params = fun.type_params;
        const return_type = fun.return_type;
        const body = fun.body;

        // TODO generics
        var has_generics_in_signature = false;
        if (type_params.items.len > 0) {
            has_generics_in_signature = true;
        }

        if (!has_generics_in_signature) {
            try self.codegenFunSignature(idx, &params, return_type, output, (body == null));
            try output.appendSlice(";\n");
        }
    }

    try output.append('\n');

    for (self.compiler.functions.items, 0..) |fun, idx| {
        if (idx == 0) continue;
        var params = fun.params;
        const type_params = fun.type_params;
        const return_type = fun.return_type;
        const body = fun.body;
        var inference_vars = fun.inference_vars;

        // TODO generics
        var has_generics_in_signature = false;
        if (type_params.items.len > 0) {
            has_generics_in_signature = true;
        }

        if (!has_generics_in_signature) {
            if (body) |b| {
                try self.codegenFunSignature(idx, &params, return_type, output, false);
                try output.appendSlice(" {\n");
                try self.codegenBlock(b, &inference_vars, output);
                try output.appendSlice("}\n");
            }
        }
    }
}

pub fn codegen(self: *Codegen) ![]const u8 {
    var output: std.ArrayList(u8) = std.ArrayList(u8).init(self.alloc);

    const allocator = try std.fs.cwd().openFile("allocator/allocator.c", .{});
    defer allocator.close();

    const file_size = try allocator.getEndPos();
    const allocator_src = try self.alloc.alloc(u8, file_size);
    _ = try allocator.read(allocator_src);

    try output.appendSlice(allocator_src);

    try output.appendSlice("struct Allocator *allocator;\n");

    try self.codegenUserPredecls(&output);
    try self.codegenUserTypes(&output);
    try self.codegenFunDecls(&output);

    for (self.compiler.functions.items, 0..) |fun, idx| {
        const name = self.compiler.getSource(fun.name);

        if (std.mem.eql(u8, name, "main")) {
            try output.appendSlice("int main() {\n");
            try output.appendSlice("allocator = create_allocator(100);\n");
            try output.appendSlice("function_");
            const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
            try output.appendSlice(idx_str);
            try output.appendSlice("(0);\n}\n");
        }
    }

    return try output.toOwnedSlice();
}
