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
        .@"struct" => {
            try output.appendSlice("struct struct_");
            const id = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
            try output.appendSlice(id);
        },
        .@"enum" => {
            try output.appendSlice("struct enum_");
            const id = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
            try output.appendSlice(id);
        },
        .pointer => |pt| {
            try self.codegenTypename(pt.target, local_inferences, output);
            try output.appendSlice("*");
        },
        .fun => {
            try output.appendSlice("fun_ty_");
            const id = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
            try output.appendSlice(id);
        },
        .c_external_type => |c_ext| {
            const ty = self.compiler.getSource(c_ext);
            try output.appendSlice(ty);
        },
        .raw_buffer => |inner_ty| {
            try self.codegenTypename(inner_ty, local_inferences, output);
            try output.appendSlice("*");
        },
        else => {
            if (type_id == Typechecker.VOID_TYPE_ID) {
                try output.appendSlice("void");
            } else if (type_id == Typechecker.I64_TYPE_ID) {
                // check why we got error without the extra space
                try output.appendSlice("int64_t ");
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
                // check why we got error without the extra space
                try output.appendSlice("bool ");
            } else if (type_id == Typechecker.UNKNOWN_TYPE_ID) {
                @panic("(unknown) type should be resolved before codegen");
            } else {
                @panic("unknown type");
            }
        },
    }
}

pub fn codegenAllocatorFunction(self: *Codegen, type_id: Typechecker.TypeId, fields: *std.ArrayList(Typechecker.TypeField), is_allocator: bool, base_classes0: *?std.ArrayList(Typechecker.TypeId), output: *std.ArrayList(u8)) !void {
    const ptr_type = self.compiler.getType(type_id);
    const tagname = @tagName(ptr_type);
    if (!std.mem.eql(u8, tagname, "pointer")) {
        @panic("internal error: pointer to unknown type");
    }

    try output.appendSlice("struct struct_");
    const inner_type_id = try std.fmt.allocPrint(self.alloc, "{d}", .{ptr_type.pointer.target});
    try output.appendSlice(inner_type_id);
    try output.appendSlice("* allocator_");
    try output.appendSlice(inner_type_id);
    try output.append('(');
    try output.appendSlice("long allocation_id");

    for (fields.items, 0..) |type_field, idx| {
        try output.appendSlice(", ");
        var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
        try self.codegenTypename(type_field.ty, &local_inference, output);
        try output.append(' ');
        try output.appendSlice("field_");
        const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
        try output.appendSlice(f_id);
        try output.appendSlice(" /*");
        try output.appendSlice(type_field.name);
        try output.appendSlice(" */");
    }

    var base_classes: std.ArrayList(Typechecker.TypeId) = undefined;
    if (base_classes0.* == null) {
        base_classes = std.ArrayList(Typechecker.TypeId).init(self.alloc);
    } else {
        base_classes = base_classes0.*.?;
    }
    for (base_classes.items) |base_class| {
        const base_type = self.compiler.getType(base_class);
        const base_tag = @tagName(base_type);
        if (!std.mem.eql(u8, base_tag, "struct")) {
            @panic("base classes should be struct Types");
        }

        for (base_type.@"struct".fields.items, 0..) |type_field, idx| {
            try output.appendSlice(", ");
            var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
            try self.codegenTypename(type_field.ty, &local_inference, output);
            try output.append(' ');
            const b_id = try std.fmt.allocPrint(self.alloc, "{d}", .{base_class});
            const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
            try output.appendSlice("base_");
            try output.appendSlice(b_id);
            try output.appendSlice("_field_");
            try output.appendSlice(f_id);
            try output.appendSlice(" /*");
            try output.appendSlice(type_field.name);
            try output.appendSlice(" */");
        }
    }
    try output.appendSlice(") {\n");

    var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
    try self.codegenTypename(type_id, &local_inference, output);
    try output.appendSlice(" tmp = (");
    var local_inference0 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
    try self.codegenTypename(type_id, &local_inference0, output);
    try output.appendSlice(")allocate(allocator, sizeof(struct struct_");
    try output.appendSlice(inner_type_id);
    try output.appendSlice("), allocation_id);\n");

    if (is_allocator) {
        try output.appendSlice("tmp->__allocation_id__ = allocation_id;\n");
    }

    try output.appendSlice("initializer_");
    try output.appendSlice(inner_type_id);
    try output.appendSlice("(tmp");

    for (fields.*.items, 0..) |type_field, idx| {
        try output.appendSlice(", field_");
        const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
        try output.appendSlice(f_id);
        try output.appendSlice(" /* ");
        try output.appendSlice(type_field.name);
        try output.appendSlice(" */");
    }

    for (base_classes.items) |base_class| {
        const base_type = self.compiler.getType(base_class);
        if (@TypeOf(base_type) != @TypeOf(Typechecker.Type.@"struct")) {
            @panic("base classes should be struct Types");
        }

        for (base_type.@"struct".fields.items, 0..) |type_field, idx| {
            const b_id = try std.fmt.allocPrint(self.alloc, "{d}", .{base_class});
            const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
            try output.appendSlice("base_");
            try output.appendSlice(b_id);
            try output.appendSlice("_field_");
            try output.appendSlice(f_id);
            try output.appendSlice(" /* ");
            try output.appendSlice(type_field.name);
            try output.appendSlice(" */");
        }
    }
    try output.appendSlice(");\n");

    try output.appendSlice("return tmp;\n}\n");
}

pub fn codegenInitializerFunction(self: *Codegen, type_id: Typechecker.TypeId, fields: *std.ArrayList(Typechecker.TypeField), base_classes: *?std.ArrayList(Typechecker.TypeId), output: *std.ArrayList(u8)) !void {
    const ptr_type = self.compiler.getType(type_id);
    const tagname = @tagName(ptr_type);
    if (!std.mem.eql(u8, tagname, "pointer")) {
        @panic("internal error: pointer to unknown type");
    }

    const inner_type_id = try std.fmt.allocPrint(self.alloc, "{d}", .{ptr_type.pointer.target});
    try output.appendSlice("void initializer_");
    try output.appendSlice(inner_type_id);
    try output.appendSlice("(struct struct_");
    try output.appendSlice(inner_type_id);
    try output.appendSlice("* tmp");

    for (fields.*.items, 0..) |type_field, idx| {
        try output.appendSlice(", ");
        var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
        try self.codegenTypename(type_field.ty, &local_inference, output);
        try output.appendSlice(" field_");
        const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
        try output.appendSlice(f_id);
        try output.appendSlice(" /* ");
        try output.appendSlice(type_field.name);
        try output.appendSlice(" */ ");
    }

    if (base_classes.*) |base_classes0| {
        for (base_classes0.items) |base_class| {
            const base_type = self.compiler.getType(base_class);
            switch (base_type) {
                .@"struct" => {
                    for (base_type.@"struct".fields.items, 0..) |type_field, idx| {
                        try output.appendSlice(", ");
                        var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                        try self.codegenTypename(type_field.ty, &local_inference, output);
                        try output.append(' ');
                        try output.appendSlice("base_field_");
                        const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                        try output.appendSlice(f_id);
                        try output.appendSlice(" /* ");
                        try output.appendSlice(type_field.name);
                        try output.appendSlice(" */ ");
                    }
                },
                else => {
                    // TODO
                },
            }
        }
    }
    try output.appendSlice(") {\n");

    if (base_classes.*) |base_classes0| {
        const base_class = base_classes0.items;
        try output.appendSlice("initializer_");
        const base_id = try std.fmt.allocPrint(self.alloc, "{d}", .{base_class});
        try output.appendSlice(base_id);
        try output.appendSlice("(&tmp->baseclass");

        for (base_classes0.items) |base| {
            const base_type = self.compiler.getType(base);
            switch (base_type) {
                .@"struct" => {
                    for (base_type.@"struct".fields.items, 0..) |type_field, idx| {
                        try output.appendSlice("base_field_");
                        const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                        try output.appendSlice(f_id);
                        try output.appendSlice(" /* ");
                        try output.appendSlice(type_field.name);
                        try output.appendSlice(" */ ");
                    }
                },
                else => {
                    // TODO
                },
            }
        }
        try output.appendSlice(");\n");

        // TODO virtual table
        // for (base_classes0.items, 0..) |base, depth| {

        // }
    }

    for (fields.items, 0..) |type_field, idx| {
        const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
        try output.appendSlice("tmp->field_");
        try output.appendSlice(f_id);
        try output.appendSlice(" = ");
        try output.appendSlice("field_");
        try output.appendSlice(f_id);
        try output.appendSlice(" /* ");
        try output.appendSlice(type_field.name);
        try output.appendSlice(" */ ;\n");
    }

    try output.appendSlice("\n}\n");
}

pub fn codegenUserPredecls(self: *Codegen, output: *std.ArrayList(u8)) !void {
    for (self.compiler.types.items, 0..) |ty, idx| {
        switch (ty) {
            .@"struct" => |_struct| {
                if (!(_struct.generic_params.items.len == 0)) {
                    // Don't codegen generic functions. Instead, only codegen their instantiations
                    continue;
                }

                try output.appendSlice("struct struct_");
                const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(f_id);
                try output.appendSlice(";\n");
            },
            .@"enum" => |e| {
                const generics_params = e.generic_params;
                if (!(generics_params.items.len == 0)) {
                    // Don't codegen generic functions. Instead, only codegen their instantiations
                    continue;
                }

                try output.appendSlice("struct enum_");
                const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(f_id);
                try output.appendSlice(";\n");
            },
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
            .@"struct" => |_struct| {
                if (!(_struct.generic_params.items.len == 0)) {
                    // Don't codegen generic functions. Instead, only codegen their instantiations
                    continue;
                }

                // TODO virtual methods

                try output.appendSlice("struct struct_");
                const id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(id);
                try output.appendSlice("{\n");

                if (_struct.is_allocator) {
                    var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(Typechecker.I64_TYPE_ID, &local_inference, output);
                    try output.append(' ');
                    try output.appendSlice("__allocation_id__;\n");
                }

                const base_classes = self.compiler.base_classes.get(idx);
                if (base_classes) |classes| {
                    const _ty = classes.items[0];
                    var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(_ty, &local_inference, output);
                    try output.appendSlice(" baseclass;\n");
                }

                // TODO vitual methods

                for (_struct.fields.items, 0..) |type_field, _idx| {
                    var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(type_field.ty, &local_inference, output);
                    try output.append(' ');
                    try output.appendSlice(" field_");
                    const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{_idx});
                    try output.appendSlice(f_id);
                    try output.appendSlice(" /*");
                    try output.appendSlice(type_field.name);
                    try output.appendSlice(" */ ;\n");
                }

                try output.appendSlice("};\n");

                // TODO vtables predecls

                if (self.compiler.findPointerTo(idx)) |ptr| {
                    try self.codegenInitializerFunction(ptr, @constCast(&_struct.fields), @constCast(&base_classes), output);
                    if (!self.compiler.hasUnsatisfiedVirtualMethods(idx)) {
                        try self.codegenAllocatorFunction(ptr, @constCast(&_struct.fields), _struct.is_allocator, @constCast(&base_classes), output);
                    }
                } else {
                    @panic("internal error: can't find pointer to type");
                }
            },
            .@"enum" => |e| {
                const generic_params = e.generic_params;
                const cases = e.variants;

                if (!(generic_params.items.len == 0)) {
                    // Don't codegen generic functions. Instead, only codegen their instantiations
                    continue;
                }

                try output.appendSlice("struct enum_");
                const id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(id);
                try output.appendSlice("{\n");
                try output.appendSlice("int arm_id;\n");
                try output.appendSlice("union {\n");
                for (cases.items, 0..) |case, _idx| {
                    switch (case) {
                        .single => |single| {
                            var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                            try self.codegenTypename(single.param, &local_inference, output);
                            try output.append(' ');
                            try output.appendSlice("case_");
                            const _id = try std.fmt.allocPrint(self.alloc, "{d}", .{_idx});
                            try output.appendSlice(_id);
                            try output.appendSlice(" /*");
                            try output.appendSlice(single.name);
                            try output.appendSlice(" */");
                        },
                        .@"struct" => |s| {
                            // FIXME!! This will name collide because of C naming resolution
                            try output.appendSlice("struct /*");
                            try output.appendSlice(s.name);
                            try output.appendSlice(" */ {\n");

                            for (s.params.items, 0..) |arg, arg_idx| {
                                var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                                try self.codegenTypename(arg.ty, &local_inference, output);
                                try output.append(' ');
                                try output.appendSlice("case_");
                                const _id = try std.fmt.allocPrint(self.alloc, "{}_{} /* ", .{ _idx, arg_idx });
                                try output.appendSlice(_id);
                                try output.appendSlice(arg.name);
                                try output.appendSlice(" */ ;\n");
                            }

                            try output.appendSlice("};\n");
                        },
                        .simple => |_| {
                            // ignore because it is encoded into the arm_id above
                        },
                    }
                }

                try output.appendSlice("};\n");
                try output.appendSlice("};\n");

                for (cases.items, 0..) |case, case_offset| {
                    var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(idx, &local_inference, output);
                    try output.appendSlice("* enum_case_");
                    const id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                    try output.appendSlice(id_str);
                    try output.append('_');
                    const case_off = try std.fmt.allocPrint(self.alloc, "{d}", .{case_offset});
                    try output.appendSlice(case_off);
                    try output.appendSlice("(int allocation_id");

                    switch (case) {
                        .single => |single| {
                            try output.appendSlice(", ");
                            var local_inference0 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                            try self.codegenTypename(single.param, &local_inference0, output);
                            try output.appendSlice(" arg");
                        },
                        .@"struct" => |s| {
                            for (s.params.items, 0..) |param, param_idx| {
                                try output.appendSlice(", ");
                                var local_inference0 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                                try self.codegenTypename(param.ty, &local_inference0, output);
                                try output.append(' ');
                                try output.appendSlice("case_");
                                const _id = try std.fmt.allocPrint(self.alloc, "{}_{} /* ", .{ case_offset, param_idx });
                                try output.appendSlice(_id);
                                try output.appendSlice(param.name);
                                try output.appendSlice(" */ ;\n");
                            }
                        },
                        .simple => {},
                    }

                    try output.appendSlice(") {\n");
                    var local_inference0 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(idx, &local_inference0, output);
                    try output.appendSlice("* tmp = (");
                    var local_inference1 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                    try self.codegenTypename(idx, &local_inference1, output);
                    try output.appendSlice("*)allocate(allocator, sizeof(struct enum_");
                    const _id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                    try output.appendSlice(_id);
                    try output.appendSlice("), allocation_id);\n");

                    try output.appendSlice("tmp->arm_id = \n");
                    const case_off0 = try std.fmt.allocPrint(self.alloc, "{d}", .{case_offset});
                    try output.appendSlice(case_off0);
                    try output.appendSlice(";\n");

                    switch (case) {
                        .single => |single| {
                            try output.appendSlice("tmp->case_");
                            const case_off_str = try std.fmt.allocPrint(self.alloc, "{d}", .{case_offset});
                            try output.appendSlice(case_off_str);
                            try output.appendSlice(" = ");
                            try output.appendSlice("arg; /* ");
                            try output.appendSlice(single.name);
                            try output.appendSlice(" */\n");
                        },
                        .@"struct" => |s| {
                            for (s.params.items, 0..) |param, param_idx| {
                                const tmp = try std.fmt.allocPrint(self.alloc, "tmp->case_{}_{}", .{ case_offset, param_idx });
                                try output.appendSlice(tmp);
                                try output.appendSlice(" = ");
                                const c = try std.fmt.allocPrint(self.alloc, "case_{}_{}; /*", .{ case_offset, param_idx });
                                try output.appendSlice(c);
                                try output.appendSlice(param.name);
                                try output.appendSlice(" */\n");
                            }
                        },
                        .simple => {},
                    }

                    try output.appendSlice("return tmp;\n");

                    try output.appendSlice("}\n");
                }
            },
            .raw_buffer => |inner_type_id| {
                switch (self.compiler.getType(inner_type_id)) {
                    .fun_local_type_val => continue,
                    else => {},
                }

                var local_inference = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                try self.codegenTypename(inner_type_id, &local_inference, output);
                try output.appendSlice("* create_buffer_");
                const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                try output.appendSlice(idx_str);
                try output.appendSlice("(int level, int count, ...)\n{\n");
                var local_inference1 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                try self.codegenTypename(idx, &local_inference1, output);
                try output.appendSlice(" output = allocate_resizeable_page_on_allocator_level(allocator, level, sizeof(");
                var local_inference2 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                try self.codegenTypename(inner_type_id, &local_inference2, output);
                try output.appendSlice(") * count);\n");
                try output.appendSlice("va_list args;\n");
                try output.appendSlice("va_start(args, count);\n");
                try output.appendSlice("for (int i = 0; i < count; i++) {\n");
                try output.appendSlice("*(output + i) = va_arg(args, ");
                var local_inference3 = std.ArrayList(Typechecker.TypeId).init(self.alloc);
                try self.codegenTypename(inner_type_id, &local_inference3, output);
                try output.appendSlice(");\n");
                try output.appendSlice("}\n");
                try output.appendSlice("va_end(args);\n");
                try output.appendSlice("return output;\n}\n");
            },
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

pub fn codegenVtableDecl(self: *Codegen, type_id: usize, virtual_methods: *std.ArrayList(Typechecker.FuncId), output: *std.ArrayList(u8)) !void {
    try output.appendSlice("struct vtable_");
    const id = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
    try output.appendSlice(id);
    try output.appendSlice("{\n");

    for (virtual_methods.items) |method| {
        const fun = self.compiler.functions.items[method];
        try self.codegenTypename(fun.return_type, @constCast(&fun.inference_vars), output);
        try output.appendSlice(" (*");
        try output.appendSlice(self.compiler.getSource(fun.name));
        try output.appendSlice(")(");
        try output.appendSlice("long allocation_id");

        for (fun.params.items) |param| {
            try output.appendSlice(", ");

            const variable_ty = self.compiler.getVariable(param.var_id).ty;
            try self.codegenTypename(variable_ty, @constCast(&fun.inference_vars), output);
            try output.append(' ');
            try output.appendSlice("variable_");
            const param_id = try std.fmt.allocPrint(self.alloc, "{d}", .{param.var_id});
            try output.appendSlice(param_id);
        }
        try output.appendSlice(");\n");
    }

    const type_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
    try output.appendSlice("};\n");
    try output.appendSlice("typedef struct vtable_");
    try output.appendSlice(type_id_str);
    try output.appendSlice(" vtable_");
    try output.appendSlice(type_id_str);
    try output.appendSlice(";\n");
}

pub fn codegenVirtFunctionTypedefs(self: *Codegen, virtual_methods: *std.ArrayList(Typechecker.FuncId), output: *std.ArrayList(u8)) !void {
    for (virtual_methods.items) |method| {
        const fun = self.compiler.functions.items[method];
        try output.appendSlice("typedef ");
        try self.codegenTypename(fun.return_type, @constCast(&fun.inference_vars), output);
        try output.appendSlice(" (*virt_fun_ty_");
        const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{method});
        try output.appendSlice(idx_str);
        try output.appendSlice(")(");
        try output.appendSlice("long allocation_id");

        for (fun.params.items) |param| {
            try output.appendSlice(", ");

            const variable_ty = self.compiler.getVariable(param.var_id).ty;
            try self.codegenTypename(variable_ty, @constCast(&fun.inference_vars), output);
            try output.append(' ');
            try output.appendSlice("variable_");
            const param_id = try std.fmt.allocPrint(self.alloc, "{d}", .{param.var_id});
            try output.appendSlice(param_id);
        }
        try output.appendSlice(");\n");
    }
}

pub fn codegenVtableInstantiation(self: *Codegen, base_classes: *std.ArrayList(Typechecker.TypeId), type_id: usize, methods: *std.ArrayList(Typechecker.FuncId), output: *std.ArrayList(u8)) !void {
    var i: i32 = @intCast(base_classes.items.len - 1);
    bases: while (i >= 0) : (i -= 1) {
        const base_class = base_classes.items[@intCast(i)];
        // TODO satisfy virtual methods
        const vtable_fully_satisfied = false;

        if (!vtable_fully_satisfied) {
            continue :bases;
        }

        const virtual_methods = self.compiler.virtualMethodsOnType(base_class);
        _ = virtual_methods;
        // TODO convert to hashmap (method_name -> method_id)

        const base_id = try std.fmt.allocPrint(self.alloc, "{d}", .{base_class});
        const type_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
        try output.appendSlice("static const vtable_");
        try output.appendSlice(base_id);
        try output.appendSlice(" vtable_struct_");
        try output.appendSlice(type_id_str);
        try output.appendSlice(" = {\n");

        for (methods.items) |method| {
            const fun = self.compiler.functions.items[method];
            const fun_name = self.compiler.getSource(fun.name);

            const virt_fun_id: Typechecker.FuncId = undefined;
            // get from virtual_methods hashmap

            const method_id = try std.fmt.allocPrint(self.alloc, "{d}", .{method});
            const virt_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{virt_fun_id});
            try output.appendSlice("    .");
            try output.appendSlice(fun_name);
            try output.appendSlice(" = (virt_fun_ty_");
            try output.appendSlice(virt_id_str);
            try output.appendSlice(")function_");
            try output.appendSlice(method_id);
            try output.appendSlice(",\n");
        }

        try output.appendSlice("};");
    }
}

pub fn codegenVtableMethodPredecls(self: *Codegen, base_classes: *std.ArrayList(Typechecker.TypeId), methods: *std.ArrayList(Typechecker.FuncId), output: *std.ArrayList(u8)) !void {
    var base_virtual_method_names = std.StringHashMap(usize).init(self.alloc);
    for (base_classes.items) |ty| {
        const base_virtual_method = self.compiler.virtualMethodsOnType(ty);
        for (base_virtual_method.items) |id| {
            const node_id = self.compiler.functions.items[id].name;
            const name = self.compiler.getSource(node_id);
            try base_virtual_method_names.put(name, id);
        }
    }

    for (methods.items) |method| {
        const fun = self.compiler.functions.items[method];
        const method_name = self.compiler.getSource(fun.name);
        if (base_virtual_method_names.contains(method_name)) {
            const method_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{method});
            try output.appendSlice("void /* ");
            try output.appendSlice(method_name);
            try output.appendSlice(" */ function_");
            try output.appendSlice(method_id_str);
            try output.appendSlice("(long allocation_id");

            for (fun.params.items) |param| {
                try output.appendSlice(", ");
                const variable_ty = self.compiler.getVariable(param.var_id).ty;
                try self.codegenTypename(variable_ty, @constCast(fun.inference_vars), output);
                try output.append(' ');
                try output.appendSlice("variable_");
                const param_id = try std.fmt.allocPrint(self.alloc, "{d}", .{param.var_id});
                try output.appendSlice(param_id);
            }
            try output.appendSlice(");");
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
    switch (self.compiler.getNodeLifetime(node_id)) {
        .@"return" => try output.appendSlice("allocation_id"),
        .param => |param| {
            const free_str = try std.fmt.allocPrint(self.alloc, "variable_{}->__allocation_id__", .{param.var_id});
            try output.appendSlice(free_str);
        },
        .scope => |scope| {
            const free_str = try std.fmt.allocPrint(self.alloc, "allocation_id + {}", .{scope.level});
            try output.appendSlice(free_str);
        },
        .unknown => try output.appendSlice("/* UNKNOWN, */ "),
    }
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
        .let => |let_stmt| {
            const var_id = self.compiler.var_resolution.get(let_stmt.variable_name).?;
            const ty = self.compiler.getVariable(var_id).ty;

            try self.codegenTypename(ty, local_inferences, output);

            try output.appendSlice(" /*");
            try output.appendSlice(self.compiler.getSource(self.compiler.getVariable(var_id).name));
            try output.appendSlice(" */");

            try output.appendSlice(" variable_");
            const id = try std.fmt.allocPrint(self.alloc, "{d}", .{var_id});
            try output.appendSlice(id);

            try output.appendSlice(" = ");
            try self.codegenNode(let_stmt.initializer, local_inferences, output);
        },
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
        .bitwise_and => try output.append('&'),
        .bitwise_or => try output.append('|'),
        .shift_left => try output.appendSlice("<<"),
        .shift_right => try output.appendSlice(">>"),
        .greater_than_or_equal => try output.appendSlice(">="),
        .add_assignment => try output.appendSlice("+="),
        .subtract_assignment => try output.appendSlice("-="),
        .multiply_assignment => try output.appendSlice("*="),
        .divide_assignment => try output.appendSlice("/="),
        .binary_op => |bin_op| {
            const lhs = bin_op.left;
            const op = bin_op.op;
            const rhs = bin_op.right;

            const node = self.compiler.getNode(op);
            switch (node) {
                Parser.AstNode.as => {
                    try output.appendSlice("((");
                    const rhs_type_id = self.compiler.getNodeType(rhs);
                    try self.codegenNode(rhs_type_id, local_inferences, output);
                    try output.append(')');
                    try self.codegenNode(lhs, local_inferences, output);
                    try output.append(')');
                },
                else => {
                    try output.append('(');
                    try self.codegenNode(lhs, local_inferences, output);
                    try output.append(')');

                    try self.codegenNode(op, local_inferences, output);

                    try output.append('(');
                    try self.codegenNode(rhs, local_inferences, output);
                    try output.append(')');
                },
            }
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

                    // for passing self argument -> box.create()
                    const check_member = self.compiler.getNode(head);
                    if (std.mem.eql(u8, @tagName(check_member), "member_access")) {
                        // A bit of a codegen workaround for now. Because we aren't updating the AST during typecheck,
                        // we haven't moved the target of the method call to be the first arg. To get around that,
                        // we'll manually push it in now.
                        if (!first) {
                            try output.appendSlice(", ");
                        }
                        try self.codegenNode(check_member.member_access.target, local_inferences, output);
                        first = false;
                    }

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
                .enum_constructor => |ec| {
                    try output.appendSlice("enum_case_");
                    const tar_id = try std.fmt.allocPrint(self.alloc, "{}", .{ec.type_id});
                    try output.appendSlice(tar_id);
                    try output.append('_');
                    const offset_id = try std.fmt.allocPrint(self.alloc, "{}", .{ec.type_id});
                    try output.appendSlice(offset_id);
                    try output.append('(');

                    try self.codegenAnnotation(node_id, output);

                    for (args.items) |arg| {
                        try output.appendSlice(", ");
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
        .new => |_new| {
            const type_id = self.compiler.getNodeType(node_id);

            const ptr_type = self.compiler.getType(type_id);
            switch (ptr_type) {
                .pointer => {},
                else => {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "internal error: 'new' creating non-pointer type: {any}", .{self.compiler.getType(type_id)});
                    @panic(error_msg);
                },
            }

            try output.appendSlice("allocator_");
            const type_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{ptr_type.pointer.target});
            try output.appendSlice(type_id_str);
            try output.append('(');

            try self.codegenAnnotation(node_id, output);

            switch (self.compiler.getNode(_new.allocated)) {
                .call => |c| {
                    for (c.args.items) |arg| {
                        try output.appendSlice(", ");
                        try self.codegenNode(arg, local_inferences, output);
                    }

                    try output.append(')');
                },
                else => {
                    @panic("internal error: expected allocation call during allocation");
                },
            }
        },
        .namespaced_lookup => |namespace| {
            const item = namespace.item;
            switch (self.compiler.getNode(item)) {
                .call => |c| {
                    const head = c.head;
                    const args = c.args;
                    const call_target = self.compiler.call_resolution.get(head).?;

                    switch (call_target) {
                        .function => |fun_id| {
                            try output.appendSlice("/* ");
                            try output.appendSlice(self.compiler.getSource(self.compiler.functions.items[fun_id].name));
                            try output.appendSlice(" */");

                            try output.appendSlice("function_");
                            const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{fun_id});
                            try output.appendSlice(f_id);
                            try output.append('(');

                            try self.codegenAnnotation(node_id, output);

                            for (args.items) |arg| {
                                try output.appendSlice(", ");
                                try self.codegenNode(arg, local_inferences, output);
                            }
                            try output.appendSlice(")");
                        },
                        .enum_constructor => |enum_cons| {
                            const target = enum_cons.type_id;
                            const offset = enum_cons.case_offset;

                            try output.appendSlice("enum_case_");
                            const target_id = try std.fmt.allocPrint(self.alloc, "{d}", .{target});
                            try output.appendSlice(target_id);
                            try output.append('_');
                            const offset_id = try std.fmt.allocPrint(self.alloc, "{d}", .{offset});
                            try output.appendSlice(offset_id);
                            try output.append('(');

                            try self.codegenAnnotation(node_id, output);

                            for (args.items) |arg| {
                                try output.appendSlice(", ");
                                try self.codegenNode(arg, local_inferences, output);
                            }
                            try output.appendSlice(")");
                        },
                        else => {
                            @panic("namedspaced lookup of non-namedspaced value");
                        },
                    }
                },
                .name => {
                    const call_target = self.compiler.call_resolution.get(item).?;

                    switch (call_target) {
                        .function => |fun_id| {
                            try output.appendSlice("/* ");
                            try output.appendSlice(self.compiler.getSource(self.compiler.functions.items[fun_id].name));
                            try output.appendSlice(" */");

                            try output.appendSlice("function_");
                            const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{fun_id});
                            try output.appendSlice(f_id);
                            try output.append('(');

                            try self.codegenAnnotation(node_id, output);

                            try output.append(')');
                        },
                        .enum_constructor => |enum_cons| {
                            const target = enum_cons.type_id;
                            const offset = enum_cons.case_offset;

                            try output.appendSlice("enum_case_");
                            const target_id = try std.fmt.allocPrint(self.alloc, "{d}", .{target});
                            try output.appendSlice(target_id);
                            try output.append('_');
                            const offset_id = try std.fmt.allocPrint(self.alloc, "{d}", .{offset});
                            try output.appendSlice(offset_id);
                            try output.append('(');

                            try self.codegenAnnotation(node_id, output);

                            try output.append(')');
                        },
                        .node_id => |target| {
                            try output.append('(');
                            try self.codegenNode(target, local_inferences, output);
                            try output.append(')');

                            try output.append('(');

                            try self.codegenAnnotation(node_id, output);

                            try output.append(')');
                        },
                    }
                },
                else => {
                    @panic("unsupported namespace lookup");
                },
            }
        },
        .named_value => |named_value| {
            // FIXME: this should probably be handled cleanly via typecheck+codegen
            // rather than ignoring the name
            try self.codegenNode(named_value.value, local_inferences, output);
        },
        .@"break" => {
            if (self.compiler.exiting_blocks.get(node_id)) |exiting_blocks| {
                var i: i32 = @intCast(exiting_blocks.items.len - 1);
                while (i >= 0) : (i -= 1) {
                    const exiting_block = exiting_blocks.items[@intCast(i)];
                    if (self.compiler.blocks.items[exiting_block].may_locally_allocate) |scope_level| {
                        const free_str = try std.fmt.allocPrint(self.alloc, "free_allocator_level(allocator, allocation_id + {});\n", .{scope_level});
                        try output.appendSlice(free_str);
                    }
                }
            }
            try output.appendSlice("break;\n");
        },
        .member_access => |member_access| {
            try self.codegenNode(member_access.target, local_inferences, output);
            try output.appendSlice("->");

            const field_name = self.compiler.getSource(member_access.field);

            var type_id = self.compiler.getNodeType(member_access.target);
            type_id = self.compiler.resolveType(type_id, local_inferences);
            type_id = self.compiler.getUnderlyingTypeId(type_id);

            // FIXME: we can do this because the fields are unique, but we probably want
            // the resolution to tell us which one to use
            switch (self.compiler.getType(type_id)) {
                .@"struct" => |s| {
                    var found = false;
                    for (s.fields.items, 0..) |type_field, idx| {
                        if (std.mem.eql(u8, type_field.name, field_name)) {
                            try output.appendSlice("field_");
                            const f_id = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
                            try output.appendSlice(f_id);
                            try output.appendSlice(" /*");
                            try output.appendSlice(type_field.name);
                            try output.appendSlice(" */");
                            found = true;
                        }
                    }

                    if (!found) {
                        @panic("internal error: field could not be codegen'd");
                    }
                },
                else => {
                    @panic("internal error: field access on non-struct");
                },
            }
        },
        .raw_buffer => |raw_buffer| {
            const type_id = self.compiler.resolveNodeType(node_id, local_inferences);
            try output.appendSlice("create_buffer_");
            const type_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{type_id});
            try output.appendSlice(type_id_str);
            try output.append('(');
            try self.codegenAnnotation(node_id, output);
            try output.appendSlice(", ");
            const len_str = try std.fmt.allocPrint(self.alloc, "{d}", .{raw_buffer.items.len});
            try output.appendSlice(len_str);
            for (raw_buffer.items) |item| {
                try output.appendSlice(", ");
                try self.codegenNode(item, local_inferences, output);
            }
            try output.append(')');
        },
        .index => |index| {
            try output.appendSlice("(*((");
            try self.codegenNode(index.target, local_inferences, output);
            try output.appendSlice(") + (");
            try self.codegenNode(index.index, local_inferences, output);
            try output.appendSlice(")))");
        },
        .statement => |stmt| {
            try self.codegenNode(stmt, local_inferences, output);
            try output.appendSlice(";\n");
        },
        .@"if" => |if_expr| {
            const condition = if_expr.condition;
            const then_block = if_expr.then_block;
            const else_expression = if_expr.else_expression;

            try output.appendSlice("if (");
            try self.codegenNode(condition, local_inferences, output);
            try output.appendSlice(") {\n");
            try self.codegenNode(then_block, local_inferences, output);

            if (else_expression) |else_expr| {
                try output.appendSlice("} else {\n");
                try self.codegenNode(else_expr, local_inferences, output);
            }

            try output.appendSlice("}\n");
        },
        .@"while" => |while_expr| {
            try output.appendSlice("while (");
            try self.codegenNode(while_expr.condition, local_inferences, output);
            try output.appendSlice(") {\n");
            try self.codegenNode(while_expr.block, local_inferences, output);
            try output.appendSlice("}\n");
        },
        .@"for" => |for_expr| {
            try output.appendSlice("for (");

            var range_type: Parser.AstNode = undefined;
            switch (self.compiler.getNode(for_expr.range)) {
                .range => {
                    range_type = self.compiler.getNode(for_expr.range);
                },
                else => {
                    @panic("internal error: range not found for 'for'");
                },
            }

            const var_id = self.compiler.var_resolution.get(for_expr.variable).?;
            const var_type = self.compiler.getVariable(var_id).ty;

            try self.codegenTypename(var_type, local_inferences, output);

            try output.appendSlice(" variable_");
            const var_id_str = try std.fmt.allocPrint(self.alloc, "{d}", .{var_id});
            try output.appendSlice(var_id_str);

            try output.appendSlice(" = ");
            try self.codegenNode(range_type.range.lhs, local_inferences, output);

            try output.appendSlice("; variable_");
            try output.appendSlice(var_id_str);
            try output.appendSlice(" <= ");
            try self.codegenNode(range_type.range.rhs, local_inferences, output);

            try output.appendSlice("; ++variable_");
            try output.appendSlice(var_id_str);
            try output.appendSlice(") {");
            try self.codegenNode(for_expr.block, local_inferences, output);
            try output.appendSlice("}\n");
        },
        .@"defer" => |defer_expr| {
            try output.appendSlice("add_resource_cleanup(allocator, ");
            try self.codegenAnnotation(defer_expr.pointer, output);
            try output.appendSlice(", ");
            try self.codegenNode(defer_expr.pointer, local_inferences, output);
            try output.appendSlice(", (void (*)(long, void *))");
            try self.codegenNode(defer_expr.callback, local_inferences, output);
            try output.appendSlice(");\n");
        },
        .resize_buffer => |buffer| {
            try self.codegenNode(buffer.pointer, local_inferences, output);
            try output.appendSlice(" = resize_page_on_allocator_level(allocator, ");
            try self.codegenAnnotation(buffer.pointer, output);
            try output.appendSlice(", ");
            try self.codegenNode(buffer.pointer, local_inferences, output);
            try output.appendSlice(", sizeof(");

            const pointer_type_id = self.compiler.resolveNodeType(buffer.pointer, local_inferences);

            switch (self.compiler.getType(pointer_type_id)) {
                .raw_buffer => |inner_type_id| {
                    try self.codegenTypename(inner_type_id, local_inferences, output);
                },
                else => {
                    @panic("internal error: resize of non-buffer type");
                },
            }

            try output.appendSlice(") * (");
            try self.codegenNode(buffer.new_size, local_inferences, output);
            try output.appendSlice("));\n");
        },
        .match => |match| {
            const target = match.target;
            const match_arms = match.match_arms;

            try output.appendSlice("{\n");
            const type_id = self.compiler.getNodeType(target);
            try self.codegenTypename(type_id, local_inferences, output);
            try output.append(' ');
            try output.appendSlice("match_var_");

            const target_id = try std.fmt.allocPrint(self.alloc, "{}", .{target});
            try output.appendSlice(target_id);

            try output.appendSlice(" = ");
            try self.codegenNode(target, local_inferences, output);

            try output.appendSlice(";\n");

            var first = true;
            for (match_arms.items) |arm| {
                const match_arm = arm[0];
                const match_result = arm[1];
                if (!first) {
                    try output.appendSlice("else ");
                } else {
                    first = false;
                }

                switch (self.compiler.getNode(match_arm)) {
                    .name => {
                        try output.appendSlice("if (true) {\n");

                        const var_id = self.compiler.var_resolution.get(match_arm).?;
                        const var_type = self.compiler.getVariable(var_id).ty;
                        try self.codegenTypename(var_type, local_inferences, output);
                        try output.appendSlice(" variable_");
                        const var_id_str = try std.fmt.allocPrint(self.alloc, "{}", .{var_id});
                        try output.appendSlice(var_id_str);

                        try output.appendSlice(" = ");
                        try output.appendSlice("match_var_");
                        const tar_id = try std.fmt.allocPrint(self.alloc, "{}", .{target});
                        try output.appendSlice(tar_id);
                        try output.appendSlice(";\n");

                        try self.codegenNode(match_result, local_inferences, output);

                        try output.appendSlice("}\n");
                    },
                    .namespaced_lookup => |namespaced_look| {
                        const item = namespaced_look.item;
                        switch (self.compiler.getNode(item)) {
                            .name => {
                                const resolution = self.compiler.call_resolution.get(match_arm).?;

                                switch (resolution) {
                                    .enum_constructor => |ec| {
                                        const case_offset = ec.case_offset;
                                        try output.appendSlice("if (");
                                        try output.appendSlice("match_var_");
                                        const tar_id0 = try std.fmt.allocPrint(self.alloc, "{}", .{target});
                                        try output.appendSlice(tar_id0);
                                        try output.appendSlice("->arm_id == ");
                                        const case_off = try std.fmt.allocPrint(self.alloc, "{}", .{case_offset});
                                        try output.appendSlice(case_off);
                                        try output.appendSlice(") {");
                                        try self.codegenNode(match_result, local_inferences, output);
                                        try output.appendSlice("}\n");
                                    },
                                    else => {
                                        @panic("target not supported in enum codegen");
                                    },
                                }
                            },
                            .call => |c| {
                                const args = c.args;
                                const resolution = self.compiler.call_resolution.get(match_arm).?;

                                switch (resolution) {
                                    .enum_constructor => |ec| {
                                        const case_offset = ec.case_offset;
                                        try output.appendSlice("if (");
                                        try output.appendSlice("match_var_");
                                        const tar_id0 = try std.fmt.allocPrint(self.alloc, "{}", .{target});
                                        try output.appendSlice(tar_id0);
                                        try output.appendSlice("->arm_id == ");
                                        const case_off = try std.fmt.allocPrint(self.alloc, "{}", .{case_offset});
                                        try output.appendSlice(case_off);

                                        var variable_assignments = std.ArrayList(u8).init(self.alloc);

                                        for (args.items, 0..) |arg, arg_idx| {
                                            switch (self.compiler.getNode(arg)) {
                                                .name => {
                                                    const var_id = self.compiler.var_resolution.get(arg).?;
                                                    const var_type = self.compiler.getVariable(var_id).ty;

                                                    try self.codegenTypename(var_type, local_inferences, &variable_assignments);
                                                    try variable_assignments.appendSlice(" variable_");
                                                    const var_id_str = try std.fmt.allocPrint(self.alloc, "{}", .{var_id});
                                                    try variable_assignments.appendSlice(var_id_str);

                                                    try variable_assignments.appendSlice(" = ");
                                                    const match_var = try std.fmt.allocPrint(self.alloc, "match_var_{}", .{target});
                                                    try variable_assignments.appendSlice(match_var);
                                                    try variable_assignments.appendSlice("->");

                                                    switch (self.compiler.getType(ec.type_id)) {
                                                        .@"enum" => |e| {
                                                            switch (e.variants.items[case_offset]) {
                                                                .single => {
                                                                    const case_var = try std.fmt.allocPrint(self.alloc, "case_{}", .{case_offset});
                                                                    try variable_assignments.appendSlice(case_var);
                                                                },
                                                                .@"struct" => {
                                                                    const case_var = try std.fmt.allocPrint(self.alloc, "case_{}_{}", .{ case_offset, arg_idx });
                                                                    try variable_assignments.appendSlice(case_var);
                                                                },
                                                                else => @panic("unsupported enum variant"),
                                                            }
                                                        },
                                                        else => {
                                                            @panic("internal error: enum match on non-enum variant ast node");
                                                        },
                                                    }

                                                    try variable_assignments.appendSlice(";\n");
                                                },
                                                else => {
                                                    @panic("not yet supported");
                                                },
                                            }
                                        }

                                        try output.appendSlice(") {\n");
                                        try output.appendSlice(try variable_assignments.toOwnedSlice());

                                        try self.codegenNode(match_result, local_inferences, output);
                                        try output.appendSlice("}\n");
                                    },
                                    else => {
                                        @panic("target not supported in enum codegen");
                                    },
                                }
                            },
                            else => {
                                @panic("node not supported in enum codegen");
                            },
                        }
                    },
                    else => {
                        @panic("node not supported in enum codegen");
                    },
                }
            }

            try output.appendSlice("}\n");
        },
        .block => {
            try self.codegenBlock(node_id, local_inferences, output);
        },
        .unsafe_block => |bl| try self.codegenBlock(bl, local_inferences, output),
        .true => try output.appendSlice("true"),
        .false => try output.appendSlice("false"),
        .fun, .@"struct", .@"enum", .extern_type => {
            // ignore this, as we handle it elsewhere
        },
        .type_coercion => |type_coercion| {
            const ty = self.compiler.getNodeType(type_coercion.target_type);
            try self.codegenTypename(ty, local_inferences, output);
            try output.appendSlice(")");
            try output.appendSlice("(");
            try self.codegenNode(type_coercion.source_node, local_inferences, output);
            try output.appendSlice(")");
        },
        else => {
            std.debug.print("node_type: {any}\n", .{self.compiler.getNode(node_id)});
            @panic("unsupported node");
        },
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

                        if (self.compiler.exiting_blocks.get(node_id)) |exiting_blocks| {
                            var i: i32 = @intCast(exiting_blocks.items.len - 1);
                            while (i >= 0) : (i -= 1) {
                                const exiting_block = exiting_blocks.items[@intCast(i)];
                                if (self.compiler.blocks.items[exiting_block].may_locally_allocate) |scope_level| {
                                    const free_str = try std.fmt.allocPrint(self.alloc, "free_allocator_level(allocator, allocation_id + {});\n", .{scope_level});
                                    try output.appendSlice(free_str);
                                }
                            }
                        }

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

                if (self.compiler.blocks.items[block_id].may_locally_allocate) |scope_level| {
                    const free_str = try std.fmt.allocPrint(self.alloc, "free_allocator_level(allocator, allocation_id + {});\n", .{scope_level});
                    try output.appendSlice(free_str);
                }
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
        if (idx == 0) continue;
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
