const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Errors = @import("Errors.zig");

alloc: std.mem.Allocator,
compiler: Compiler,
scope: std.ArrayList(Scope),

const Typechecker = @This();

pub const TypeId = usize;

pub const VarId = usize;

pub const FuncId = usize;

pub const ScopeId = usize;

pub const ModuleId = usize;

pub const TypeField = struct {
    member_access: Parser.MemberAccess,
    name: []const u8,
    ty: TypeId,
    where_defined: Parser.NodeId,
};

// pub const Void = usize;
pub const Void = enum {
    void,
};

pub const Type = union(enum) {
    unknown: Void,
    void: Void,
    i64: Void,
    f64: Void,
    bool: Void,
    range: TypeId,
    raw_buffer: TypeId,
    fun: struct {
        params: std.ArrayList(Param),
        ret: TypeId,
    },

    c_int: Void,
    c_size_t: Void,
    c_void_ptr: Void,
    c_char: Void,
    c_string: Void,
    c_external_type: Parser.NodeId,

    @"struct": struct {
        generic_params: std.ArrayList(TypeId),
        fields: std.ArrayList(TypeField),
        is_allocator: bool,
    },
    @"enum": struct {
        generic_params: std.ArrayList(TypeId),
        variants: std.ArrayList(EnumVariant),
    },
    pointer: struct {
        pointer_type: Parser.PointerType,
        optional: bool,
        target: TypeId,
    },
    type_variable: Parser.NodeId,
    fun_local_type_val: struct {
        offset: usize,
    },
};

pub const EnumStructVariant = struct {
    name: []const u8,
    ty: TypeId,
};

pub const EnumVariant = union(enum) {
    simple: struct {
        name: []const u8,
    },
    single: struct {
        name: []const u8,
        param: TypeId,
    },
    @"struct": struct {
        name: []const u8,
        params: std.ArrayList(EnumStructVariant),
    },
};

pub const Variable = struct {
    name: Parser.NodeId,
    ty: TypeId,
    is_mutable: bool,
    where_defined: Parser.NodeId,
};

pub const Param = struct {
    name: []const u8,
    var_id: VarId,

    pub fn init(name: []const u8, var_id: VarId) Param {
        return Param{ .name = name, .var_id = var_id };
    }
};

pub const TypeParam = struct {
    name: []const u8,
    type_id: TypeId,

    pub fn init(name: []const u8, type_id: TypeId) TypeParam {
        return TypeParam{ .name = name, .type_id = type_id };
    }
};

pub const Lifetime = union(enum) {
    variable: VarId,
    @"return": Void,
};

pub const LifetimeAnnotation = union(enum) {
    equality: struct {
        left: Lifetime,
        right: Lifetime,
    },
};

pub const Function = struct {
    name: Parser.NodeId,
    params: std.ArrayList(Param),
    lifetime_annotations: std.ArrayList(LifetimeAnnotation),
    type_params: std.ArrayList(TypeParam),
    inference_vars: std.ArrayList(TypeId),
    return_node: ?Parser.NodeId,
    return_type: TypeId,
    initial_node_id: ?Parser.NodeId,
    body: ?Parser.NodeId,
    is_extern: bool,
};

pub const Scope = struct {
    modules: std.StringHashMap(ModuleId),
    variables: std.StringHashMap(VarId),
    functions: std.StringHashMap(FuncId),
    types: std.StringHashMap(TypeId),
    expected_return_type: ?TypeId,
    move_owned_values: std.AutoHashMap(VarId, Parser.NodeId),
    allow_unsafe: bool,

    pub fn init(alloc: std.mem.Allocator) Scope {
        return Scope{
            .modules = std.StringHashMap(ModuleId).init(alloc),
            .variables = std.StringHashMap(VarId).init(alloc),
            .functions = std.StringHashMap(FuncId).init(alloc),
            .types = std.StringHashMap(TypeId).init(alloc),
            .expected_return_type = null,
            .move_owned_values = std.AutoHashMap(VarId, Parser.NodeId).init(alloc),
            .allow_unsafe = false,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.modules.deinit();
        self.functions.deinit();
        self.types.deinit();
        self.move_owned_values.deinit();
    }

    pub fn setUnsafe(self: *Scope) void {
        self.allow_unsafe = true;
    }

    pub fn findType(self: *Scope, name: []const u8) ?TypeId {
        return self.types.get(name);
    }

    pub fn findFunction(self: *Scope, name: []const u8) ?FuncId {
        return self.functions.get(name);
    }

    pub fn findName(self: *Scope, name: []const u8) ?VarOrFuncId {
        if (self.variables.get(name)) |var_id| {
            return .{ .var_id = var_id };
        } else if (self.functions.get(name)) |fun_id| {
            return .{ .fun_id = fun_id };
        } else {
            return null;
        }
    }
};

pub const Module = struct {
    scope: Scope,
};

pub const VarOrFuncId = union(enum) {
    var_id: VarId,
    fun_id: FuncId,
};

pub const UNKNOWN_TYPE_ID: TypeId = 0;
pub const VOID_TYPE_ID: TypeId = 1;
pub const I64_TYPE_ID: TypeId = 2;
pub const F64_TYPE_ID: TypeId = 3;
pub const BOOL_TYPE_ID: TypeId = 4;
pub const RANGE_I64_TYPE_ID: TypeId = 5;
pub const C_INT_TYPE_ID: TypeId = 6;
pub const C_SIZE_T_TYPE_ID: TypeId = 7;
pub const C_VOID_PTR_TYPE_ID: TypeId = 8;
pub const C_CHAR_TYPE_ID: TypeId = 9;
pub const C_STRING_TYPE_ID: TypeId = 10;

pub fn init(alloc: std.mem.Allocator, compiler: Compiler) !Typechecker {
    var typechecker = Typechecker{
        .alloc = alloc,
        .compiler = compiler,
        .scope = std.ArrayList(Scope).init(alloc),
    };

    // temporarily - let's add `println` for now, to get examples to typecheck
    try typechecker.compiler.variables.append(Variable{
        .name = 0,
        .ty = UNKNOWN_TYPE_ID,
        .is_mutable = false,
        .where_defined = 0,
    });

    try typechecker.compiler.functions.append(Function{
        .name = 0,
        .params = std.ArrayList(Param).init(alloc),
        .lifetime_annotations = std.ArrayList(LifetimeAnnotation).init(alloc),
        .type_params = std.ArrayList(TypeParam).init(alloc),
        .inference_vars = std.ArrayList(TypeId).init(alloc),
        .return_node = null,
        .return_type = VOID_TYPE_ID,
        .initial_node_id = null,
        .body = null,
        .is_extern = true,
    });

    try typechecker.compiler.functions.items[0].params.append(Param{
        .name = "input",
        .var_id = 0,
    });

    // hardwire in the core types before the user-defined types
    try typechecker.compiler.types.append(Type{ .unknown = Void.void });
    try typechecker.compiler.types.append(Type{ .void = Void.void });
    try typechecker.compiler.types.append(Type{ .i64 = Void.void });
    try typechecker.compiler.types.append(Type{ .f64 = Void.void });
    try typechecker.compiler.types.append(Type{ .bool = Void.void });
    try typechecker.compiler.types.append(Type{ .range = I64_TYPE_ID });
    try typechecker.compiler.types.append(Type{ .c_int = Void.void });
    try typechecker.compiler.types.append(Type{ .c_size_t = Void.void });
    try typechecker.compiler.types.append(Type{ .c_void_ptr = Void.void });
    try typechecker.compiler.types.append(Type{ .c_char = Void.void });
    try typechecker.compiler.types.append(Type{ .c_string = Void.void });

    var scope = std.ArrayList(Scope).init(alloc);
    try scope.append(Scope.init(alloc));

    const last = scope.items.len - 1;
    try scope.items[last].functions.put("println", 0);

    typechecker.scope = scope;

    return typechecker;
}

pub fn deinit(self: *Typechecker) void {
    for (self.scope.items) |*scope| {
        scope.deinit();
    }
}

pub fn unsafeAllowed(self: *Typechecker) bool {
    for (self.scope.items) |scope| {
        if (scope.allow_unsafe) return true;
    }
    return false;
}

pub fn setUnsafe(self: *Typechecker) void {
    const last = self.scope.items.len - 1;
    self.scope.items[last].setUnsafe();
}

pub fn unifyTypes(self: *Typechecker, lhs: TypeId, rhs: TypeId, local_inferences: *std.ArrayList(TypeId)) bool {
    const lhs_ty = self.compiler.getType(lhs);
    const rhs_ty = self.compiler.getType(rhs);

    if (std.mem.eql(u8, @tagName(lhs_ty), "fun_local_type_val")) {
        if (local_inferences.items[lhs_ty.fun_local_type_val.offset] == UNKNOWN_TYPE_ID) {
            local_inferences.*.items[lhs_ty.fun_local_type_val.offset] = rhs;
            return true;
        } else {
            return self.unifyTypes(local_inferences.items[lhs_ty.fun_local_type_val.offset], rhs, local_inferences);
        }
    } else if (std.mem.eql(u8, @tagName(rhs_ty), "fun_local_type_val")) {
        if (local_inferences.items[rhs_ty.fun_local_type_val.offset] == UNKNOWN_TYPE_ID) {
            local_inferences.*.items[rhs_ty.fun_local_type_val.offset] = lhs;
            return true;
        } else {
            return self.unifyTypes(lhs, local_inferences.items[rhs_ty.fun_local_type_val.offset], local_inferences);
        }
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "pointer") and std.mem.eql(u8, @tagName(rhs_ty), "pointer")) {
        // We allow for unknown pointer types to assign in from the other types,
        // which allows us to not have to guess how `self` will be used
        // Also, if an owned pointer is assigned to a shared pointer, then we'll
        // allow the move into a shared pointer, effectively removing the owned-ness.
        // We can do this because the ownership will move.
        // zig fmt: off
        return (lhs_ty.pointer.pointer_type == Parser.PointerType.Unknown 
        or lhs_ty.pointer.pointer_type == rhs_ty.pointer.pointer_type 
        or (lhs_ty.pointer.pointer_type == Parser.PointerType.Shared
        and rhs_ty.pointer.pointer_type == Parser.PointerType.Owned))
        // TODO generics not working properly if we use logical and here
        or lhs_ty.pointer.target == rhs_ty.pointer.target
        and (lhs_ty.pointer.optional or lhs_ty.pointer.optional == rhs_ty.pointer.optional);
        // zig fmt: on
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "raw_buffer") and std.mem.eql(u8, @tagName(rhs_ty), "raw_buffer")) {
        const lhs_inner = lhs_ty.raw_buffer;
        const rhs_inner = rhs_ty.raw_buffer;

        if (self.unifyTypes(lhs_inner, rhs_inner, local_inferences)) {
            const lhs_resolved = self.compiler.resolveType(lhs_inner, local_inferences);
            const rhs_resolved = self.compiler.resolveType(rhs_inner, local_inferences);

            // Make sure we have concrete versions of both types for later stages in the compiler
            _ = self.compiler.findOrCreateType(.{ .raw_buffer = lhs_resolved }) catch unreachable;
            _ = self.compiler.findOrCreateType(.{ .raw_buffer = rhs_resolved }) catch unreachable;

            return true;
        } else {
            return false;
        }
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "fun") and std.mem.eql(u8, @tagName(rhs_ty), "fun")) {
        const lhs_ret = lhs_ty.fun.ret;
        const rhs_ret = rhs_ty.fun.ret;

        const lhs_fun_params = lhs_ty.fun.params;
        const rhs_fun_params = rhs_ty.fun.params;

        if (lhs_fun_params.items.len != rhs_fun_params.items.len) {
            return false;
        }

        for (lhs_fun_params.items, 0..) |x, idx| {
            const y = rhs_fun_params.items[idx];
            const x_ty = self.compiler.getVariable(x.var_id).ty;
            const y_ty = self.compiler.getVariable(y.var_id).ty;

            if (!self.unifyTypes(x_ty, y_ty, local_inferences)) {
                return false;
            }
        }

        return self.unifyTypes(lhs_ret, rhs_ret, local_inferences);
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "c_int") and std.mem.eql(u8, @tagName(rhs_ty), "i64")) {
        return true;
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "i64") and std.mem.eql(u8, @tagName(rhs_ty), "c_int")) {
        return true;
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "i64") and std.mem.eql(u8, @tagName(rhs_ty), "c_size_t")) {
        return true;
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "c_size_t") and std.mem.eql(u8, @tagName(rhs_ty), "i64")) {
        return true;
    } else {
        return std.meta.eql(lhs_ty, rhs_ty);
    }
}

pub fn checkIsSubtypeOf(self: *Typechecker, expected_type: TypeId, actual_type: TypeId, local_inferences: *std.ArrayList(TypeId)) bool {
    var expected_ty = self.compiler.resolveType(expected_type, local_inferences);
    expected_ty = self.compiler.getUnderlyingTypeId(expected_ty);
    var actual_ty = self.compiler.resolveType(actual_type, local_inferences);
    actual_ty = self.compiler.getUnderlyingTypeId(actual_ty);

    var base_classes = std.ArrayList(TypeId).init(self.alloc);
    if (self.compiler.base_classes.get(actual_ty)) |classes| {
        base_classes = classes;
    } else {
        return false;
    }

    return containsClass(base_classes, expected_ty);
}

pub fn typecheckTypename(self: *Typechecker, node_id: Parser.NodeId) !TypeId {
    switch (self.compiler.getNode(node_id)) {
        .type => |ty| {
            const name_node_id = ty.name;
            const optional = ty.optional;
            const pointer_type = ty.pointer_type;

            const name = self.compiler.getSource(name_node_id);

            if (std.mem.eql(u8, name, "i64")) {
                return I64_TYPE_ID;
            } else if (std.mem.eql(u8, name, "f64")) {
                return F64_TYPE_ID;
            } else if (std.mem.eql(u8, name, "c_string")) {
                return C_STRING_TYPE_ID;
            } else if (std.mem.eql(u8, name, "c_voidptr")) {
                return C_VOID_PTR_TYPE_ID;
            } else if (std.mem.eql(u8, name, "c_char")) {
                return C_CHAR_TYPE_ID;
            } else if (std.mem.eql(u8, name, "c_int")) {
                return C_INT_TYPE_ID;
            } else if (std.mem.eql(u8, name, "bool")) {
                return BOOL_TYPE_ID;
            } else if (std.mem.eql(u8, name, "void")) {
                return VOID_TYPE_ID;
            } else {
                const type_id = self.findTypeInScope(name_node_id);
                if (type_id) |id| {
                    if (self.compiler.isTypeVariable(id)) {
                        return id;
                    } else {
                        // Assume custom types are pointers
                        return try self.compiler.findOrCreateType(Type{ .pointer = .{
                            .pointer_type = pointer_type,
                            .optional = optional,
                            .target = id,
                        } });
                    }
                } else {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "unknown type: {s}", .{name});

                    try self.@"error"(error_msg, node_id);
                    return UNKNOWN_TYPE_ID;
                }
            }
        },
        .fun_type => |fun_type| {
            const params = fun_type.params;
            const ret = fun_type.ret;

            var typed_params = std.ArrayList(Param).init(self.alloc);

            for (params.items) |param| {
                const param_ty = try self.typecheckTypename(param);
                const var_id = try self.defineVariable(param, param_ty, false, param);

                try typed_params.append(Param{ .name = "", .var_id = var_id });
            }

            const typed_ret = try self.typecheckTypename(ret);

            return try self.compiler.findOrCreateType(.{
                .fun = .{ .params = typed_params, .ret = typed_ret },
            });
        },
        .raw_buffer_type => |raw_buffer_type| {
            const inner = raw_buffer_type.inner;
            const inner_ty = try self.typecheckTypename(inner);
            return try self.compiler.findOrCreateType(.{ .raw_buffer = inner_ty });
        },
        else => {
            const error_msg = try std.fmt.allocPrint(self.alloc, "expected type name {s}", .{self.compiler.getSource(node_id)});

            try self.@"error"(error_msg, node_id);
            return VOID_TYPE_ID;
        },
    }
}

pub fn typecheckCallWithNodeId(self: *Typechecker, name: Parser.NodeId, node_id: Parser.NodeId, args: *std.ArrayList(Parser.NodeId), local_inferences: *std.ArrayList(TypeId)) !TypeId {
    var type_id = self.compiler.getNodeType(name);
    type_id = self.compiler.resolveType(type_id, local_inferences);

    const call_type = self.compiler.getType(type_id);
    switch (call_type) {
        .fun => |f| {
            const params = f.params;
            const ret = f.ret;

            try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .node_id = node_id });

            var type_var_replacements = std.AutoHashMap(TypeId, TypeId).init(self.alloc);

            try self.typecheckCallHelper(args, params, null, local_inferences, &type_var_replacements);

            if (self.compiler.isTypeVariable(ret)) {
                if (type_var_replacements.get(ret)) |ret_ty| {
                    return ret_ty;
                } else {
                    try self.@"error"("unknown type variable in return", name);
                    return UNKNOWN_TYPE_ID;
                }
            } else {
                return ret;
            }
        },
        else => {
            try self.@"error"("attempt to call a non-function", name);
            return UNKNOWN_TYPE_ID;
        },
    }
}

pub fn typecheckCallWithFunId(self: *Typechecker, name: Parser.NodeId, fun_id: FuncId, args: *std.ArrayList(Parser.NodeId), method_target: ?Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    const fun = self.compiler.functions.items[fun_id];

    const params = fun.params;
    const return_type = fun.return_type;

    var fun_args: std.ArrayList(Parser.NodeId) = undefined;
    if (method_target) |method| {
        var output = std.ArrayList(Parser.NodeId).init(self.alloc);
        try output.append(method);
        try output.appendSlice(args.*.items);
        fun_args = output;
    } else {
        fun_args = args.*;
    }

    if (fun_id == 0) {
        // Just for now, special-case println
        try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .function = fun_id });

        for (fun_args.items) |arg| {
            // TODO add name-checking
            _ = try self.typecheckNode(arg, local_inferences);
        }

        return VOID_TYPE_ID;
    }

    if (fun_args.items.len != params.items.len) {
        const error_msg = try std.fmt.allocPrint(self.alloc, "expected {d} args, found {d}", .{ params.items.len, fun_args.items.len });

        try self.@"error"(error_msg, name);

        return return_type;
    }

    if (self.compiler.functions.items[fun_id].is_extern and !self.unsafeAllowed()) {
        // This is an external function. For now, assume an `extern "C"` function
        // These must be called in an unsafe block
        try self.@"error"("call to extern \"C\" functions requires 'unsafe' block", name);
    }

    if (method_target) |_| {
        try self.enterScope();
    }

    var type_var_replacements = std.AutoHashMap(TypeId, TypeId).init(self.alloc);
    try self.typecheckCallHelper(&fun_args, params, method_target, local_inferences, &type_var_replacements);

    var func_id = fun_id;
    if (!(type_var_replacements.count() == 0)) {
        func_id = try self.instantiateGenericFun(fun_id, &type_var_replacements);
    }

    // TODO do we want to wait until all params are checked
    // before we mark this as resolved?
    try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .function = func_id });

    if (method_target) |_| {
        self.exitScope();
    }

    if (self.compiler.isTypeVariable(return_type)) {
        if (type_var_replacements.get(return_type)) |ret_ty| {
            return ret_ty;
        } else {
            try self.@"error"("unknown type variable in return", name);
            return UNKNOWN_TYPE_ID;
        }
    } else {
        return return_type;
    }
}

pub fn typecheckCallHelper(self: *Typechecker, args: *std.ArrayList(Parser.NodeId), params: std.ArrayList(Param), method_target: ?Parser.NodeId, local_inferences: *std.ArrayList(TypeId), type_var_replacements: *std.AutoHashMap(TypeId, TypeId)) !void {
    for (args.items, 0..) |arg, idx| {
        const arg_node = self.compiler.getNode(arg);
        switch (arg_node) {
            .named_value => |named_value| {
                const name_slice = named_value.name;
                const name_value = named_value.value;

                // Set up expected type for inference. Note: if we find concrete values
                // this inference type will be replaced by the concrete type.
                self.compiler.setNodeType(name_value, self.compiler.getVariable(params.items[idx].var_id).ty);

                // If this is a method, we've already checked the first argument (aka
                // the target of the method)
                var arg_ty = try self.typecheckNode(name_value, local_inferences);
                if (idx == 0 and method_target != null) {
                    arg_ty = self.compiler.getNodeType(name_value);
                }

                try self.maybeMoveVariable(name_value, local_inferences);

                if (self.compiler.getVariable(params.items[idx].var_id).is_mutable and !self.isBindingMutable(name_value)) {
                    try self.@"error"("argument to function needs to be mutable", name_value);
                    try self.note("parameter defined here", self.compiler.getVariable(params.items[idx].var_id).where_defined);
                }

                const variable = self.compiler.getVariable(params.items[idx].var_id);
                const variable_ty = variable.ty;
                const variable_method = variable.name;
                if (self.unifyTypes(arg_ty, variable_ty, local_inferences)) {} else if (self.checkIsSubtypeOf(variable_ty, arg_ty, local_inferences)) {
                    _ = try self.compiler.replaceNode(arg, variable_method);
                } else {
                    const expected_type = self.compiler.resolveType(arg_ty, local_inferences);
                    const found_type = self.compiler.resolveType(variable_ty, local_inferences);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ try self.compiler.prettyType(expected_type), try self.compiler.prettyType(found_type) });

                    try self.@"error"(error_msg, name_value);
                    try self.note("parameter defined here", variable.where_defined);
                }

                const arg_name = self.compiler.getSource(name_slice);
                if (!std.mem.eql(u8, arg_name, params.items[idx].name)) {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "expected name {s}", .{params.items[idx].name});

                    try self.@"error"(error_msg, name_slice);
                    try self.note("parameter defined here", variable.where_defined);
                }
            },
            else => {
                var arg_type: TypeId = undefined;
                if (idx == 0 and method_target != null) {
                    arg_type = self.compiler.getNodeType(arg);
                } else {
                    arg_type = try self.typecheckNode(arg, local_inferences);
                }

                try self.maybeMoveVariable(arg, local_inferences);
                const param = params.items[idx];
                const variable = self.compiler.getVariable(param.var_id);

                const variable_ty = variable.ty;
                const variable_node_id = variable.name;

                if (self.compiler.isTypeVariable(variable_ty)) {
                    if (type_var_replacements.get(variable_ty)) |replacement| {
                        if (self.unifyTypes(replacement, arg_type, local_inferences)) {} else {
                            const expected_type = self.compiler.resolveType(replacement, local_inferences);
                            const found_type = self.compiler.resolveType(arg_type, local_inferences);
                            const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ try self.compiler.prettyType(expected_type), try self.compiler.prettyType(found_type) });

                            try self.@"error"(error_msg, arg);
                            try self.note("parameter defined here", variable.where_defined);
                        }
                    } else {
                        try type_var_replacements.put(self.compiler.getUnderlyingTypeId(variable_ty), self.compiler.getUnderlyingTypeId(arg_type));
                    }
                } else if (self.unifyTypes(variable_ty, arg_type, local_inferences)) {} else if (self.checkIsSubtypeOf(variable_ty, arg_type, local_inferences)) {
                    _ = try self.compiler.replaceNode(arg, variable_node_id);
                } else {
                    const expected_type = self.compiler.resolveType(variable_ty, local_inferences);
                    const found_type = self.compiler.resolveType(arg_type, local_inferences);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ try self.compiler.prettyType(expected_type), try self.compiler.prettyType(found_type) });

                    try self.@"error"(error_msg, arg);
                    try self.note("parameter defined here", variable.where_defined);
                }

                if (self.compiler.getVariable(param.var_id).is_mutable and !self.isBindingMutable(arg)) {
                    try self.@"error"("argument to function needs to be mutable", arg);
                    try self.note("parameter defined here", variable.where_defined);
                }
            },
        }
    }
}

pub fn typecheckCall(self: *Typechecker, node_id: Parser.NodeId, head: Parser.NodeId, args: *std.ArrayList(Parser.NodeId), local_inferences: *std.ArrayList(TypeId)) !TypeId {
    const type_id = try self.typecheckNode(head, local_inferences);
    self.compiler.setNodeType(head, type_id);

    switch (self.compiler.getNode(head)) {
        .member_access => |mem_access| {
            const target = mem_access.target;

            if (self.compiler.fun_resolution.get(head)) |fun_id| {
                return try self.typecheckCallWithFunId(head, fun_id, args, target, local_inferences);
            } else {
                // We're not looking at the name of a defined function, but likely looking at a first-class function instead
                return try self.typecheckCallWithNodeId(head, node_id, args, local_inferences);
            }
        },
        else => {
            if (self.compiler.fun_resolution.get(head)) |fun_id| {
                return try self.typecheckCallWithFunId(head, fun_id, args, null, local_inferences);
            } else {
                // We're not looking at the name of a defined function, but likely looking at a first-class function instead
                return try self.typecheckCallWithNodeId(head, node_id, args, local_inferences);
            }
        },
    }
}

pub fn typecheckFunPredecl(self: *Typechecker, name: Parser.NodeId, type_params: ?Parser.NodeId, params: Parser.NodeId, lifetime_annotations: *std.ArrayList(Parser.NodeId), return_ty: ?Parser.NodeId, initial_node_id: ?Parser.NodeId, block: ?Parser.NodeId, is_extern: bool) !FuncId {
    var fun_params = std.ArrayList(Param).init(self.alloc);
    var fun_type_params = std.ArrayList(TypeParam).init(self.alloc);

    const fun_name = self.compiler.source[self.compiler.span_start.items[name]..self.compiler.span_end.items[name]];

    try self.enterScope();

    if (type_params) |ty_params| {
        const params_list = self.compiler.getNode(ty_params);
        if (!std.mem.eql(u8, @tagName(params_list), "params")) {
            @panic("internal error: enum generic params are not proper ast node");
        }

        const new_ty_params = try params_list.params.clone();
        for (new_ty_params.items) |ty_param| {
            const new_ty_id = try self.compiler.freshTypeVariable(ty_param);
            const type_var_name = self.compiler.getSource(ty_param);

            try fun_type_params.append(TypeParam.init(type_var_name, new_ty_id));
            try self.addTypeToScope(type_var_name, new_ty_id);
        }
    }

    const params_node = self.compiler.getNode(params);
    switch (params_node) {
        .params => |pm| {
            for (pm.items) |unchecked_param| {
                const param = self.compiler.getNode(unchecked_param);
                switch (param) {
                    .param => |p| {
                        const param_name = p.name;
                        const param_name_slice = self.compiler.getSource(param_name);
                        const ty = p.ty;
                        const is_mutable = p.is_mutable;

                        const tt = try self.typecheckTypename(ty);

                        const var_id = try self.defineVariable(param_name, tt, is_mutable, param_name);
                        self.compiler.setNodeType(param_name, tt);
                        try fun_params.append(Param.init(param_name_slice, var_id));
                    },
                    else => try self.@"error"("expected function parameter", unchecked_param),
                }
            }
        },
        else => try self.@"error"("expected function parameters", params),
    }

    var checked_lifetime_annotations = std.ArrayList(LifetimeAnnotation).init(self.alloc);

    for (lifetime_annotations.items) |lifetime_annotation| {
        switch (self.compiler.getNode(lifetime_annotation)) {
            .binary_op => |bin_op| {
                var lhs: Lifetime = undefined;
                var rhs: Lifetime = undefined;

                switch (self.compiler.getNode(bin_op.left)) {
                    .name => {
                        if (self.findVariableInScope(bin_op.left)) |var_id| {
                            lhs = .{ .variable = var_id };
                        } else {
                            try self.@"error"("couldn't find parameter for lifetime", bin_op.left);
                            continue;
                        }
                    },
                    .return_lifetime => lhs = .{ .@"return" = Void.void },
                    else => @panic("internal error: non-variable and non-return lifetime"),
                }

                switch (self.compiler.getNode(bin_op.right)) {
                    .name => {
                        if (self.findVariableInScope(bin_op.right)) |var_id| {
                            rhs = .{ .variable = var_id };
                        } else {
                            try self.@"error"("couldn't find parameter for lifetime", bin_op.right);
                            continue;
                        }
                    },
                    .return_lifetime => rhs = .{ .@"return" = Void.void },
                    else => @panic("internal error: non-variable and non-return lifetime"),
                }

                try checked_lifetime_annotations.append(.{ .equality = .{ .left = lhs, .right = rhs } });
            },
            else => {
                @panic("internal error: lifetime anotation is not a binary op");
            },
        }
    }

    var ret_ty = VOID_TYPE_ID;
    if (return_ty) |ty| {
        ret_ty = try self.typecheckTypename(ty);
    }

    self.exitScope();

    try self.compiler.functions.append(Function{
        .name = name,
        .params = fun_params,
        .lifetime_annotations = checked_lifetime_annotations,
        .type_params = fun_type_params,
        .inference_vars = std.ArrayList(TypeId).init(self.alloc),
        .return_node = return_ty,
        .return_type = ret_ty,
        .initial_node_id = initial_node_id,
        .body = block,
        .is_extern = is_extern,
    });

    const fun_id = self.compiler.functions.items.len - 1;

    const last = self.scope.items.len - 1;
    try self.scope.items[last].functions.put(fun_name, fun_id);

    // Mark the name of the function as resolved, in case type inference needs to run again
    // we know we don't have to recreate this function
    try self.compiler.fun_resolution.put(name, fun_id);

    return fun_id;
}

pub fn typecheckFun(self: *Typechecker, fun_id: FuncId) !void {
    var fun = self.compiler.functions.items[fun_id];
    if (fun.body) |body| {
        fun.body = body;
    } else {
        return;
    }

    try self.enterScope();

    for (fun.type_params.items) |type_params| {
        try self.addTypeToScope(type_params.name, type_params.type_id);
    }

    try self.setExpectedReturnType(fun.return_type);

    for (fun.params.items) |param| {
        try self.addVariableToScope(param.name, param.var_id);
    }

    // Create our local inference variable list we'll use as we infer types
    var local_inference = std.ArrayList(TypeId).init(self.alloc);

    // Typecheck until we hit a fix point for our inferences
    while (true) {
        const before = local_inference.items;

        _ = try self.typecheckNode(fun.body.?, &local_inference);

        if (!(self.compiler.errors.items.len == 0) or isListEqual(before, local_inference.items)) {
            break;
        }
    }

    if (fun.return_type != VOID_TYPE_ID and !self.endsInReturn(fun.body.?)) {
        if (fun.return_node) |return_node| {
            try self.@"error"(
                "function is missing expected return at end of function",
                return_node,
            );
        } else {
            try self.@"error"("function is missing expected return at end of function", fun.name);
        }
    }

    self.exitScope();

    const infer = &self.compiler.functions.items[fun_id];
    infer.*.inference_vars = local_inference;
}

pub fn typecheckEnum(self: *Typechecker, typename: Parser.NodeId, cases: std.ArrayList(Parser.NodeId), methods: std.ArrayList(Parser.NodeId)) !TypeId {
    var type_name: Parser.NodeId = undefined;
    var params: ?Parser.NodeId = null;
    switch (self.compiler.getNode(typename)) {
        .type => |ty| {
            type_name = ty.name;
            params = ty.params;
        },
        else => {
            @panic("internal error: enum does not have type as name");
        },
    }

    var generic_params = std.ArrayList(TypeId).init(self.alloc);

    try self.enterScope();

    if (params) |list| {
        var params_list = std.ArrayList(Parser.NodeId).init(self.alloc);
        switch (self.compiler.getNode(list)) {
            .params => |p| {
                params_list = try p.clone();
            },
            else => @panic("internal error: enum does not have type as name"),
        }

        for (params_list.items) |param| {
            const type_id = try self.compiler.freshTypeVariable(param);

            try generic_params.append(type_id);

            const type_var_name = self.compiler.getSource(param);
            try self.addTypeToScope(type_var_name, type_id);
        }
    }

    const enum_name = self.compiler.getSource(type_name);

    var output_cases = std.ArrayList(EnumVariant).init(self.alloc);
    for (cases.items) |enum_case| {
        switch (self.compiler.getNode(enum_case)) {
            .enum_case => |e_case| {
                const case_name = self.compiler.getSource(e_case.name);
                if (e_case.payload) |payload| {
                    if (payload.items.len == 0) {
                        try self.@"error"("missing payload in enum case", e_case.name);
                        break;
                    }

                    switch (self.compiler.getNode(payload.items[0])) {
                        .named_value => {
                            var fields = std.ArrayList(EnumStructVariant).init(self.alloc);

                            const payload_cp = payload.items;
                            for (payload_cp) |item| {
                                switch (self.compiler.getNode(item)) {
                                    .named_value => |named_value| {
                                        const field_name = self.compiler.getSource(named_value.name);
                                        const field_ty = try self.typecheckTypename(named_value.value);

                                        try fields.append(.{ .name = field_name, .ty = field_ty });
                                    },
                                    else => try self.@"error"("expected 'name: type' for each field in enum case", item),
                                }
                            }

                            try output_cases.append(.{ .@"struct" = .{
                                .name = case_name,
                                .params = fields,
                            } });
                        },
                        .type => {
                            const type_id = try self.typecheckTypename(payload.items[0]);

                            try output_cases.append(.{ .single = .{
                                .name = case_name,
                                .param = type_id,
                            } });
                        },
                        else => {
                            try self.@"error"("unexpected node in enum cases", payload.items[0]);
                        },
                    }
                } else {
                    try output_cases.append(.{ .simple = .{ .name = case_name } });
                }
            },
            else => try self.@"error"("expect enum case inside of enum", enum_case),
        }
    }

    const type_id = try self.compiler.pushType(.{
        .@"enum" = .{
            .generic_params = generic_params,
            .variants = output_cases,
        },
    });

    _ = try self.compiler.pushType(.{
        .pointer = .{
            .pointer_type = Parser.PointerType.Owned,
            .optional = false,
            .target = type_id,
        },
    });

    try self.addTypeToScope(enum_name, type_id);

    if (!(methods.items.len == 0)) {
        try self.enterScope();

        try self.addTypeToScope("self", type_id);

        var fun_ids = std.ArrayList(FuncId).init(self.alloc);
        for (methods.items) |method| {
            switch (self.compiler.getNode(method)) {
                .fun => |f| {
                    const fun_id = try self.typecheckFunPredecl(f.name, f.type_params, f.params, @constCast(&f.lifetime_annotations), f.return_ty, f.initial_node_id, f.block, f.is_extern);
                    try fun_ids.append(fun_id);
                },
                else => {
                    try self.@"error"("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                },
            }
        }
        try self.compiler.insertMethodsOnType(type_id, fun_ids);
        for (fun_ids.items) |fun_id| {
            try self.typecheckFun(fun_id);
        }
        self.exitScope();
    }

    self.exitScope();
    try self.addTypeToScope(enum_name, type_id);
    try self.compiler.type_resolution.put(typename, type_id);

    return type_id;
}

pub fn typecheckStruct(self: *Typechecker, typename: Parser.NodeId, fields: std.ArrayList(Parser.NodeId), methods: std.ArrayList(Parser.NodeId), explicit_no_alloc: bool, base_class: ?Parser.NodeId) !TypeId {
    const node_type = self.compiler.getNode(typename);
    var type_name: Parser.NodeId = undefined;
    var params: ?Parser.NodeId = null;
    switch (node_type) {
        .type => |ty| {
            type_name = ty.name;
            params = ty.params;
        },
        else => @panic("internal error: struct does not have type as name"),
    }

    var has_pointers = false;
    var generics_params = std.ArrayList(TypeId).init(self.alloc);

    try self.enterScope();

    if (params) |list| {
        var params_list = std.ArrayList(Parser.NodeId).init(self.alloc);
        switch (self.compiler.getNode(list)) {
            .params => |p| {
                params_list = try p.clone();
            },
            else => @panic("internal error: struct generic params are not proper ast node"),
        }

        for (params_list.items) |param| {
            const type_id = try self.compiler.freshTypeVariable(param);

            // be conservative and assume generic parameters might be pointers
            has_pointers = true;
            try generics_params.append(type_id);

            const type_var_name = self.compiler.getSource(param);
            try self.addTypeToScope(type_var_name, type_id);
        }
    }

    const struct_name = self.compiler.getSource(type_name);

    const type_id = try self.compiler.pushType(.{
        .@"struct" = .{
            .generic_params = generics_params,
            .fields = std.ArrayList(TypeField).init(self.alloc),
            .is_allocator = false, // will be replaced later
        },
    });

    try self.addTypeToScope(struct_name, type_id);
    try self.addTypeToScope("Self", type_id);

    var field_name: Parser.NodeId = undefined;
    var field_type: Parser.NodeId = undefined;
    var member_access: Parser.MemberAccess = undefined;
    var output_fields = std.ArrayList(TypeField).init(self.alloc);
    for (fields.items) |field| {
        switch (self.compiler.getNode(field)) {
            .field => |f| {
                member_access = f.member_access;
                field_name = f.name;
                field_type = f.typename;
            },
            else => @panic("internal error: field expected inside of struct typechecking"),
        }

        const field_name_text = self.compiler.getSource(field_name);
        const field_type_id = try self.typecheckTypename(field_type);

        if (!self.compiler.isCopyableType(field_type_id)) {
            has_pointers = true;
        }

        try output_fields.append(TypeField{
            .member_access = member_access,
            .name = field_name_text,
            .ty = field_type_id,
            .where_defined = field_name,
        });
    }

    const mut_struct = self.compiler.getMutType(type_id);
    switch (mut_struct.*) {
        .@"struct" => {
            mut_struct.*.@"struct".fields = output_fields;
            if (explicit_no_alloc) {
                mut_struct.*.@"struct".is_allocator = false;
            } else {
                mut_struct.*.@"struct".is_allocator = has_pointers;
            }
        },
        else => @panic("internal error: previously inserted struct can't be found"),
    }

    if (!(methods.items.len == 0)) {
        try self.enterScope();
        try self.addTypeToScope("self", type_id);

        var fun_ids = std.ArrayList(FuncId).init(self.alloc);
        var virtual_fun_ids = std.ArrayList(FuncId).init(self.alloc);

        var fun_name: Parser.NodeId = undefined;
        var fun_type_params: ?Parser.NodeId = null;
        var fun_params: Parser.NodeId = undefined;
        var lifetime_annotations = std.ArrayList(Parser.NodeId).init(self.alloc);
        var return_ty: ?Parser.NodeId = null;
        var initial_node_id: ?Parser.NodeId = null;
        var fun_block: ?Parser.NodeId = null;
        var fun_is_external = false;
        for (methods.items) |method| {
            switch (self.compiler.getNode(method)) {
                .fun => |f| {
                    fun_name = f.name;
                    fun_type_params = f.type_params;
                    fun_params = f.params;
                    lifetime_annotations = try f.lifetime_annotations.clone();
                    return_ty = f.return_ty;
                    initial_node_id = f.initial_node_id;
                    fun_block = f.block;
                    fun_is_external = f.is_extern;
                },
                else => {
                    try self.@"error"("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                },
            }

            const fun_id = try self.typecheckFunPredecl(fun_name, fun_type_params, fun_params, &lifetime_annotations, return_ty, initial_node_id, fun_block, fun_is_external);

            if (fun_block == null and !fun_is_external) {
                try virtual_fun_ids.append(fun_id);
            } else {
                try fun_ids.append(fun_id);
            }
        }

        if (base_class) |base| {
            const class = self.findTypeInScope(base).?;

            var base_classes = std.ArrayList(TypeId).init(self.alloc);
            try base_classes.append(class);

            if (self.compiler.base_classes.get(class)) |base_base_class| {
                try base_classes.appendSlice(base_base_class.items);
            }

            try self.compiler.base_classes.put(type_id, base_classes);

            const ty_methods = self.compiler.methodsOnType(class);
            const virtual_methods = self.compiler.virtualMethodsOnType(class);

            // TODO: proper equality for methods, have some way to check equality between two functions to see if one is an implementation of the other
            var implemented_methods = std.ArrayList([]const u8).init(self.alloc);

            for (ty_methods.items) |method| {
                const fun = self.compiler.functions.items[method];
                const method_name = self.compiler.getSource(fun.name);
                try implemented_methods.append(method_name);
                try fun_ids.append(method);
            }

            for (virtual_methods.items) |method| {
                const fun = self.compiler.functions.items[method];
                const method_name = self.compiler.getSource(fun.name);
                if (contains(implemented_methods, method_name)) {
                    try virtual_fun_ids.append(method);
                }
            }
        }

        try self.compiler.insertMethodsOnType(type_id, fun_ids);
        try self.compiler.insertVirtualMethodsOnType(type_id, virtual_fun_ids);

        for (fun_ids.items) |fun_id| {
            try self.typecheckFun(fun_id);
        }

        self.exitScope();
    }

    self.exitScope();
    try self.addTypeToScope(struct_name, type_id);
    try self.compiler.type_resolution.put(typename, type_id);

    return type_id;
}

pub fn varWasPreviouslyMoved(self: *Typechecker, var_id: VarId) !?Parser.NodeId {
    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        const scope_frame = self.scope.items[@intCast(i)];
        if (scope_frame.move_owned_values.get(var_id)) |where_moved| {
            return where_moved;
        }
    }

    return null;
}

pub fn maybeMoveVariable(self: *Typechecker, node_id: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !void {
    switch (self.compiler.getNode(node_id)) {
        .name => {
            const var_id = self.compiler.var_resolution.get(node_id);

            // Assume the mistyped variable error has already been reported
            if (var_id) |v_id| {
                var var_type = self.compiler.getVariable(v_id).ty;
                var_type = self.compiler.resolveType(var_type, local_inferences);
                const var_ty = self.compiler.getType(var_type);

                switch (var_ty) {
                    .pointer => |ptr_ty| {
                        if (ptr_ty.pointer_type == .Owned) {
                            const last = self.scope.items.len - 1;
                            try self.scope.items[last].move_owned_values.put(v_id, node_id);
                        }
                    },
                    else => {},
                }
            }
        },
        else => {},
    }
}

pub fn typecheckBlock(self: *Typechecker, node_id: Parser.NodeId, block_id: Parser.BlockId, local_inferences: *std.ArrayList(TypeId)) anyerror!Scope {
    var funs = std.ArrayList(FuncId).init(self.alloc);

    try self.enterScope();

    const block = self.compiler.blocks.items[block_id];

    for (block.nodes.items) |id| {
        const node = self.compiler.getNode(id);
        switch (node) {
            .fun => |*f| {
                if (self.compiler.fun_resolution.get(f.name)) |fun_id| {
                    const fun_name = self.compiler.source[self.compiler.span_start.items[f.name]..self.compiler.span_end.items[f.name]];

                    const last = self.scope.items.len - 1;
                    try self.scope.items[last].functions.put(fun_name, fun_id);

                    continue;
                }
                try funs.append(try self.typecheckFunPredecl(f.name, f.type_params, f.params, @constCast(&f.lifetime_annotations), f.return_ty, f.initial_node_id, f.block, f.is_extern));
            },
            .@"struct" => |*s| {
                if (self.compiler.type_resolution.get(s.typename)) |type_id| {
                    // we've already created this. Instead of recreating it, put the previous
                    // definition into scope
                    const struct_name = self.compiler.getSource(s.typename);

                    try self.addTypeToScope(struct_name, type_id);
                    continue;
                }

                _ = try self.typecheckStruct(s.typename, s.fields, s.methods, s.explicit_no_alloc, s.base_class);
            },
            .@"enum" => |*e| {
                if (self.compiler.type_resolution.get(e.typename)) |type_id| {
                    // we've already created this. Instead of recreating it, put the previous
                    // definition into scope
                    const struct_name = self.compiler.getSource(e.typename);

                    try self.addTypeToScope(struct_name, type_id);
                    continue;
                }

                _ = try self.typecheckEnum(e.typename, e.cases, e.methods);
            },
            .extern_type => |extern_type| {
                const type_name = self.compiler.getSource(extern_type.name);
                const ty = Type{ .c_external_type = extern_type.name };
                const type_id = try self.compiler.findOrCreateType(ty);

                try self.addTypeToScope(type_name, type_id);
            },
            .use => |use_expr| {
                _ = try self.typecheckModule(use_expr.path, local_inferences);
            },
            else => {},
        }
    }

    for (funs.items) |f| {
        try self.typecheckFun(f);
    }

    for (block.nodes.items) |id| {
        _ = try self.typecheckNode(id, local_inferences);
    }

    self.compiler.setNodeType(node_id, VOID_TYPE_ID);

    return self.scope.pop().?;
}

pub fn typecheckNode(self: *Typechecker, node_id: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) anyerror!TypeId {
    var node_type: TypeId = undefined;
    switch (self.compiler.getNode(node_id)) {
        .block => |block_id| {
            _ = try self.typecheckBlock(node_id, block_id, local_inferences);
            node_type = VOID_TYPE_ID;
        },
        .int => node_type = I64_TYPE_ID,
        .float => node_type = F64_TYPE_ID,
        .true, .false => node_type = BOOL_TYPE_ID,
        .none => {
            // FIXME: check that this is an optional type
            var type_id = self.compiler.getNodeType(node_id);
            type_id = self.compiler.resolveType(type_id, local_inferences);

            switch (self.compiler.getType(type_id)) {
                .pointer => |ptr_ty| {
                    if (ptr_ty.optional) {
                        // Success, none can point to an optional pointer
                        node_type = type_id;
                    } else {
                        try self.@"error"("'none' used on required (non-optional) pointer", node_id);
                    }
                },
                else => {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "'none' requires pointer type, found: {s}", .{try self.compiler.prettyType(type_id)});

                    try self.@"error"(error_msg, node_id);
                },
            }

            node_type = type_id;
        },
        .string => {
            try self.@"error"("strings not yet supported", node_id);
            node_type = UNKNOWN_TYPE_ID;
        },
        .c_char => node_type = C_CHAR_TYPE_ID,
        .c_string => node_type = C_STRING_TYPE_ID,
        .let => |let_stmt| {
            const variable_name = let_stmt.variable_name;
            const initializer = let_stmt.initializer;
            const is_mutable = let_stmt.is_mutable;
            const ty = let_stmt.ty;

            var initializer_type = try self.typecheckNode(initializer, local_inferences);
            initializer_type = self.compiler.resolveType(initializer_type, local_inferences);

            try self.maybeMoveVariable(initializer, local_inferences);

            var var_id: VarId = undefined;
            if (ty) |let_ty| {
                const tt = try self.typecheckTypename(let_ty);
                if (!self.unifyTypes(tt, initializer_type, local_inferences)) {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "initializer and given type do not match (expected: {s}, found: {s})", .{ try self.compiler.prettyType(tt), try self.compiler.prettyType(initializer_type) });

                    try self.@"error"(error_msg, initializer);
                }

                if (self.compiler.var_resolution.get(variable_name)) |v_id| {
                    const var_name = self.compiler.getSource(variable_name);
                    try self.addVariableToScope(var_name, v_id);

                    var_id = v_id;
                } else {
                    var_id = try self.defineVariable(variable_name, tt, is_mutable, node_id);
                }
            } else {
                const node_type_id = self.compiler.getNodeType(variable_name);
                var let_ty: TypeId = undefined;
                switch (node_type_id) {
                    UNKNOWN_TYPE_ID => {
                        const tt = try self.compiler.findOrCreateType(.{
                            .fun_local_type_val = .{ .offset = local_inferences.items.len },
                        });

                        try local_inferences.*.append(UNKNOWN_TYPE_ID);
                        self.compiler.setNodeType(variable_name, tt);

                        let_ty = tt;
                    },
                    else => {
                        let_ty = node_type_id;
                    },
                }

                var tt = self.compiler.resolveType(let_ty, local_inferences);
                if (tt == UNKNOWN_TYPE_ID) {
                    tt = let_ty;
                } else {
                    tt = self.compiler.resolveType(let_ty, local_inferences);
                }

                if (!self.unifyTypes(tt, initializer_type, local_inferences)) {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "initializer and given type do not match (expected: {s}, found: {s})", .{ try self.compiler.prettyType(tt), try self.compiler.prettyType(initializer_type) });

                    try self.@"error"(error_msg, initializer);
                }

                const var_res = self.compiler.var_resolution.get(variable_name);
                if (var_res) |v_res| {
                    const var_name = self.compiler.getSource(variable_name);
                    try self.addVariableToScope(var_name, v_res);

                    var_id = v_res;
                } else {
                    var_id = try self.defineVariable(variable_name, tt, is_mutable, node_id);
                }
            }

            try self.compiler.var_resolution.put(variable_name, var_id);

            node_type = VOID_TYPE_ID;
        },
        .binary_op => |bin_op| {
            const lhs = bin_op.left;
            const op = bin_op.op;
            const rhs = bin_op.right;

            switch (self.compiler.getNode(op)) {
                .plus, .minus, .multiply, .divide, .shift_left, .shift_right, .bitwise_and, .bitwise_or => {
                    const lhs_ty = try self.typecheckNode(lhs, local_inferences);
                    const rhs_ty = try self.typecheckNode(rhs, local_inferences);

                    if (!self.unifyTypes(lhs_ty, rhs_ty, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch during binary operation. expected: {s}, found: {s}", .{ try self.compiler.prettyType(lhs_ty), try self.compiler.prettyType(rhs_ty) });

                        try self.@"error"(error_msg, op);
                    }

                    node_type = lhs_ty;
                },
                .equals, .not_equals => {
                    const lhs_ty = try self.typecheckNode(lhs, local_inferences);

                    // use a quick inference for comparison with 'none'
                    if (std.mem.eql(u8, @tagName(self.compiler.getNode(rhs)), "none")) {
                        self.compiler.setNodeType(rhs, lhs_ty);
                    }

                    const rhs_ty = try self.typecheckNode(rhs, local_inferences);
                    if (!self.unifyTypes(lhs_ty, rhs_ty, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch during operation. expected: {s}, found: {s}", .{ try self.compiler.prettyType(lhs_ty), try self.compiler.prettyType(rhs_ty) });

                        try self.@"error"(error_msg, op);
                    }

                    node_type = BOOL_TYPE_ID;
                },
                .less_than, .less_than_or_equal, .greater_than, .greater_than_or_equal => {
                    const lhs_ty = try self.typecheckNode(lhs, local_inferences);
                    const rhs_ty = try self.typecheckNode(rhs, local_inferences);

                    if (!self.unifyTypes(lhs_ty, rhs_ty, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch during operation. expected: {s}, found: {s}", .{ try self.compiler.prettyType(lhs_ty), try self.compiler.prettyType(rhs_ty) });

                        try self.@"error"(error_msg, op);
                    }

                    node_type = BOOL_TYPE_ID;
                },
                .assignment, .add_assignment, .subtract_assignment, .multiply_assignment, .divide_assignment => {
                    const lhs_ty = try self.typecheckLvalue(lhs, local_inferences);
                    const rhs_ty = try self.typecheckNode(rhs, local_inferences);
                    try self.maybeMoveVariable(rhs, local_inferences);

                    if (!self.unifyTypes(lhs_ty, rhs_ty, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch during operation. expected: {s}, found: {s}", .{ try self.compiler.prettyType(lhs_ty), try self.compiler.prettyType(rhs_ty) });

                        try self.@"error"(error_msg, op);
                    }

                    node_type = VOID_TYPE_ID;
                },
                .@"and", .@"or" => {
                    const lhs_ty = try self.typecheckNode(lhs, local_inferences);
                    const rhs_ty = try self.typecheckNode(rhs, local_inferences);

                    if (!self.unifyTypes(lhs_ty, BOOL_TYPE_ID, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch during binary operation. expected: bool, found: {s}", .{try self.compiler.prettyType(lhs_ty)});

                        try self.@"error"(error_msg, op);
                    }

                    if (!self.unifyTypes(rhs_ty, BOOL_TYPE_ID, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch during binary operation. expected: bool, found: {s}", .{try self.compiler.prettyType(rhs_ty)});

                        try self.@"error"(error_msg, op);
                    }

                    node_type = BOOL_TYPE_ID;
                },
                .as => {
                    _ = try self.typecheckNode(lhs, local_inferences);
                    const rhs_ty = try self.typecheckTypename(rhs);

                    self.compiler.setNodeType(rhs, rhs_ty);
                    self.compiler.setNodeType(node_id, rhs_ty);

                    //FIXME: Add type conversion compatibility check
                    node_type = rhs_ty;
                },
                else => @panic("unsupported operator"),
            }
        },
        .name => {
            // This looks like a variable, but may also be the name of a function
            const var_or_fun_id = self.findNameInScope(node_id);
            node_type = try self.typecheckVarOrFunction(node_id, var_or_fun_id);
        },
        .call => |c| {
            const head = c.head;
            var args = c.args;
            node_type = try self.typecheckCall(node_id, head, &args, local_inferences);
        },
        .member_access => |member| {
            // print the member
            const target = member.target;
            const field = member.field;

            var type_id = try self.typecheckNode(target, local_inferences);
            type_id = self.compiler.resolveType(type_id, local_inferences);

            const target_name = self.compiler.getSource(target);
            type_id = self.compiler.getUnderlyingTypeId(type_id);

            const field_name = self.compiler.getSource(field);
            switch (self.compiler.getType(type_id)) {
                .@"struct" => |s| {
                    for (s.fields.items) |type_field| {
                        const field_type_id = type_field.ty;

                        if (std.mem.eql(u8, field_name, type_field.name)) {
                            if (type_field.member_access == Parser.MemberAccess.Private and !std.mem.eql(u8, target_name, ".") and !std.mem.eql(u8, target_name, "self")) {
                                // We're private and not accessing 'self'
                                try self.@"error"("access of private field", field);
                            }

                            self.compiler.setNodeType(node_id, field_type_id);
                            self.compiler.setNodeType(field, field_type_id);
                            return field_type_id;
                        }
                    }

                    const methods = self.compiler.methodsOnType(type_id);
                    for (methods.items) |method| {
                        const fun = self.compiler.functions.items[method];
                        const method_name = self.compiler.getSource(fun.name);
                        if (std.mem.eql(u8, field_name, method_name)) {
                            try self.compiler.fun_resolution.put(field, method);
                            try self.compiler.fun_resolution.put(node_id, method);
                            return try self.compiler.findOrCreateType(.{ .fun = .{ .params = fun.params, .ret = fun.return_type } });
                        }
                    }

                    const virtual_methods = self.compiler.virtualMethodsOnType(type_id);
                    for (virtual_methods.items) |method| {
                        const fun = self.compiler.functions.items[method];
                        const method_name = self.compiler.getSource(fun.name);
                        if (std.mem.eql(u8, field_name, method_name)) {
                            try self.compiler.fun_resolution.put(field, method);
                            try self.compiler.fun_resolution.put(node_id, method);
                            return try self.compiler.findOrCreateType(.{ .fun = .{ .params = fun.params, .ret = fun.return_type } });
                        }
                    }

                    try self.@"error"("unknown field or method", field);
                    node_type = UNKNOWN_TYPE_ID;
                },
                .@"enum" => {
                    const methods = self.compiler.methodsOnType(type_id);
                    for (methods.items) |method| {
                        const fun = self.compiler.functions.items[method];
                        const method_name = self.compiler.getSource(fun.name);
                        if (std.mem.eql(u8, field_name, method_name)) {
                            try self.compiler.fun_resolution.put(field, method);
                            try self.compiler.fun_resolution.put(node_id, method);
                            return try self.compiler.findOrCreateType(.{ .fun = .{
                                .params = fun.params,
                                .ret = fun.return_type,
                            } });
                        }
                    }

                    try self.@"error"("unknown method", field);
                    node_type = UNKNOWN_TYPE_ID;
                },
                else => {
                    try self.@"error"("field or method access on type without fields or methods", target);
                    node_type = UNKNOWN_TYPE_ID;
                },
            }
        },
        // ignore here, since we checked this in an earlier pass
        .fun, .@"struct", .@"enum", .extern_type => node_type = VOID_TYPE_ID,
        .statement => |stmt| {
            _ = try self.typecheckNode(stmt, local_inferences);
            node_type = VOID_TYPE_ID;
        },
        .new => |new_expr| {
            const allocation_type = new_expr.pointer_type;
            const allocation_node_id = new_expr.allocated;
            node_type = try self.typecheckinit(allocation_type, allocation_node_id, local_inferences);
        },
        .@"return" => |ret| {
            const expected_type = self.findExpectedReturnType();

            if (ret) |return_expr| {
                const expr_type = try self.typecheckNode(return_expr, local_inferences);
                if (expected_type) |expected| {
                    if (!self.unifyTypes(expected, expr_type, local_inferences)) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "incompatible type at return, found: {s} expected: {s}", .{ try self.compiler.prettyType(expr_type), try self.compiler.prettyType(expected) });

                        try self.@"error"(error_msg, return_expr);
                    }
                } else {
                    try self.@"error"("return used outside of a function", return_expr);
                }
            } else if (expected_type) |_| {
                try self.@"error"("return needs value", node_id);
            } else {
                try self.@"error"("return used outside of a function", node_id);
            }
        },
        .namespaced_lookup => |namespaced_lookup| {
            node_type = try self.typecheckNamespacedLookup(namespaced_lookup.namespace, namespaced_lookup.item, local_inferences);
        },
        .use => |use_expr| {
            node_type = try self.typecheckModule(use_expr.path, local_inferences);
        },
        .match => |match| {
            node_type = try self.typecheckMatch(match.target, match.match_arms, local_inferences);
        },
        .@"if" => |if_expr| {
            _ = try self.typecheckNode(if_expr.condition, local_inferences);
            _ = try self.typecheckNode(if_expr.then_block, local_inferences);

            if (self.compiler.getNodeType(if_expr.condition) != BOOL_TYPE_ID) {
                try self.@"error"("condition not a boolean expression", if_expr.condition);
            }

            if (if_expr.else_expression) |else_expr| {
                _ = try self.typecheckNode(else_expr, local_inferences);

                // FIXME: add type compatibility
                if (self.compiler.getNodeType(if_expr.then_block) != self.compiler.getNodeType(else_expr)) {
                    try self.@"error"("return used outside of a function", else_expr);
                }
            }

            node_type = self.compiler.getNodeType(if_expr.then_block);
        },
        .@"while" => |while_expr| {
            _ = try self.typecheckNode(while_expr.condition, local_inferences);
            _ = try self.typecheckNode(while_expr.block, local_inferences);

            if (self.compiler.getNodeType(while_expr.condition) != BOOL_TYPE_ID) {
                try self.@"error"("condition not a boolean expression", while_expr.condition);
            }

            node_type = self.compiler.getNodeType(while_expr.block);
        },
        .@"for" => |for_expr| {
            const variable = for_expr.variable;
            const range = for_expr.range;
            const block = for_expr.block;

            const range_type = try self.typecheckNode(range, local_inferences);

            // TODO make sure that range type is integer value
            if (std.mem.eql(u8, @tagName(self.compiler.getType(range_type)), "range")) {
                try self.enterScope();

                const var_id = try self.defineVariable(variable, I64_TYPE_ID, true, variable);
                try self.compiler.var_resolution.put(variable, var_id);

                _ = try self.typecheckNode(block, local_inferences);

                self.exitScope();
            } else {
                try self.@"error"("expected range in for loop", range);
            }

            node_type = VOID_TYPE_ID;
        },
        .@"break" => {
            //FIXME: ensure that we're inside a loop
            node_type = VOID_TYPE_ID;
        },
        .@"defer" => |defer_expr| {
            const pointer_type_id = try self.typecheckNode(defer_expr.pointer, local_inferences);
            const callback_type_id = try self.typecheckNode(defer_expr.callback, local_inferences);

            switch (self.compiler.getType(callback_type_id)) {
                .fun => |f| {
                    if (f.ret != VOID_TYPE_ID) {
                        try self.@"error"("callback for 'defer' should not return a value", defer_expr.callback);
                    } else if (f.params.items.len == 1) {
                        const var_id = f.params.items[0].var_id;

                        if (!self.unifyTypes(self.compiler.getVariable(var_id).ty, pointer_type_id, local_inferences)) {
                            try self.@"error"("incompatible type in callback for 'defer'", defer_expr.callback);
                        }
                    } else if (f.params.items.len == 0) {
                        // we don't use the pointer, ignore it
                    } else {
                        try self.@"error"("incompatible callback for 'defer'", defer_expr.callback);
                    }
                },
                else => {
                    try self.@"error"("expected function for callback in 'defer'", defer_expr.callback);
                },
            }

            node_type = VOID_TYPE_ID;
        },
        .raw_buffer => |raw_buffer| {
            const items = raw_buffer.items;
            var ty = self.compiler.getNodeType(node_id);

            switch (self.compiler.getType(ty)) {
                .raw_buffer => |raw_ty| ty = raw_ty,
                else => {},
            }

            for (items) |item| {
                const item_type = try self.typecheckNode(item, local_inferences);
                if (ty == UNKNOWN_TYPE_ID) {
                    ty = item_type;
                } else if (ty != item_type) {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch in buffer. expected {s}, found: {s}", .{ try self.compiler.prettyType(ty), try self.compiler.prettyType(item_type) });

                    try self.@"error"(error_msg, node_id);
                }
            }

            if (ty == UNKNOWN_TYPE_ID) {
                ty = try self.compiler.findOrCreateType(.{ .fun_local_type_val = .{ .offset = local_inferences.items.len } });
                try local_inferences.append(UNKNOWN_TYPE_ID);
            }

            node_type = try self.compiler.findOrCreateType(.{ .raw_buffer = ty });
        },
        .resize_buffer => |resize_buffer| {
            const pointer = resize_buffer.pointer;
            const new_size = resize_buffer.new_size;

            var pointer_type_id = try self.typecheckNode(pointer, local_inferences);
            pointer_type_id = self.compiler.resolveType(pointer_type_id, local_inferences);

            var new_size_type_id = try self.typecheckNode(new_size, local_inferences);
            new_size_type_id = self.compiler.resolveType(new_size_type_id, local_inferences);

            pointer_type_id = self.compiler.resolveType(pointer_type_id, local_inferences);

            if (!self.isBindingMutable(pointer)) {
                try self.@"error"("variable is not mutable", pointer);
            }

            if (!std.mem.eql(u8, @tagName(self.compiler.getType(pointer_type_id)), "raw_buffer")) {
                try self.@"error"("expected raw buffer for resize", pointer);
            }

            if (!std.mem.eql(u8, @tagName(self.compiler.getType(new_size_type_id)), "i64")) {
                try self.@"error"("expected integer size for resize", pointer);
            }

            if (!self.unsafeAllowed()) {
                try self.@"error"("buffer resize requires 'unsafe' block", node_id);
            }

            node_type = VOID_TYPE_ID;
        },
        .unsafe_block => |bl| {
            const block_id = bl;
            try self.enterScope();
            self.setUnsafe();
            _ = try self.typecheckNode(block_id, local_inferences);
            self.exitScope();
            node_type = VOID_TYPE_ID;
        },
        .index => |index| {
            const target = index.target;
            const id = index.index;
            var target_type_id = try self.typecheckNode(target, local_inferences);
            target_type_id = self.compiler.resolveType(target_type_id, local_inferences);
            const index_type_id = try self.typecheckNode(id, local_inferences);

            switch (self.compiler.getType(target_type_id)) {
                .raw_buffer => |inner_type_id| {
                    if (index_type_id != I64_TYPE_ID) {
                        try self.@"error"("index with a non-integer type", id);
                        return node_type;
                    }
                    if (!self.unsafeAllowed()) {
                        try self.@"error"("index into raw buffer requires 'unsafe' block", node_id);
                        return node_type;
                    }
                    node_type = inner_type_id;
                },
                else => {
                    try self.@"error"("index on a non-buffer type", target);
                    node_type = UNKNOWN_TYPE_ID;
                },
            }
        },
        .range => |range| {
            const lhs = range.lhs;
            const rhs = range.rhs;

            var lhs_type = try self.typecheckNode(lhs, local_inferences);
            lhs_type = self.compiler.resolveType(lhs_type, local_inferences);

            var rhs_type = try self.typecheckNode(rhs, local_inferences);
            rhs_type = self.compiler.resolveType(rhs_type, local_inferences);

            if (lhs_type != I64_TYPE_ID) {
                try self.@"error"("expected i64 in range", lhs);
            }

            if (rhs_type != I64_TYPE_ID) {
                try self.@"error"("expected i64 in range", rhs);
            }

            node_type = RANGE_I64_TYPE_ID;
        },
        .type_coercion => |type_coercion| {
            node_type = self.compiler.getNodeType(type_coercion.target_type);
        },
        else => {
            std.debug.print("{any}\n", .{self.compiler.getNode(node_id)});
            unreachable;
        },
    }

    self.compiler.setNodeType(node_id, node_type);
    return node_type;
}

pub fn typecheckLvalue(self: *Typechecker, lvalue: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) anyerror!TypeId {
    switch (self.compiler.getNode(lvalue)) {
        .name => {
            _ = try self.typecheckNode(lvalue, local_inferences);

            const var_id = self.compiler.var_resolution.get(lvalue);
            if (var_id) |v_id| {
                const variable = self.compiler.getVariable(v_id);
                const ty = variable.ty;
                if (!variable.is_mutable) {
                    if (std.mem.eql(u8, self.compiler.getSource(lvalue), ".")) {
                        try self.@"error"("'self' variable is not mutable", lvalue);
                    } else {
                        try self.@"error"("variable is not mutable", lvalue);
                    }
                }
                return ty;
            } else {
                try self.@"error"("internal error: variable unresolved when checking lvalue", lvalue);
                return VOID_TYPE_ID;
            }
        },
        .index => |index| {
            const target = index.target;
            const id = index.index;
            var target_type_id = try self.typecheckLvalue(target, local_inferences);
            target_type_id = self.compiler.resolveType(target_type_id, local_inferences);
            const index_type_id = try self.typecheckNode(id, local_inferences);

            if (index_type_id != I64_TYPE_ID) {
                try self.@"error"("expected integer type for indexing", id);
            }
            if (!self.unsafeAllowed()) {
                try self.@"error"("index into raw buffer requires 'unsafe' block", lvalue);
            }

            switch (self.compiler.getType(target_type_id)) {
                .raw_buffer => |inner_type_id| {
                    return inner_type_id;
                },
                else => {
                    try self.@"error"("expected buffer when indexing", target);
                    return VOID_TYPE_ID;
                },
            }
        },
        .member_access => |member| {
            const target = member.target;
            const field = member.field;

            var head_type_id = try self.typecheckLvalue(target, local_inferences);
            head_type_id = self.compiler.resolveType(head_type_id, local_inferences);

            const field_name = self.compiler.getSource(field);
            const target_name = self.compiler.getSource(target);

            const target_type_id = self.compiler.getUnderlyingTypeId(head_type_id);

            switch (self.compiler.getType(target_type_id)) {
                .@"struct" => |s| {
                    for (s.fields.items) |type_field| {
                        const mem_access = type_field.member_access;
                        const type_name = type_field.name;
                        const ty = type_field.ty;

                        if (std.mem.eql(u8, type_name, field_name)) {
                            if (mem_access == Parser.MemberAccess.Private and !std.mem.eql(u8, target_name, ".") and !std.mem.eql(u8, target_name, "self")) {
                                // Private and not accessing 'self'
                                try self.@"error"("modifying private member field", field);
                            }

                            self.compiler.setNodeType(lvalue, ty);
                            self.compiler.setNodeType(field, ty);
                            return ty;
                        }
                    }
                },
                else => {
                    try self.@"error"("field access only supported on structs", field);
                    return VOID_TYPE_ID;
                },
            }
        },
        else => {
            std.debug.print("{any}\n", .{self.compiler.getType(lvalue)});
            try self.@"error"("unsupported lvalue, needs variable or field,", lvalue);
            return VOID_TYPE_ID;
        },
    }
    return VOID_TYPE_ID;
}

pub fn typeIsOwned(self: *Typechecker, type_id: TypeId) !bool {
    switch (self.compiler.getType(type_id)) {
        .bool, .f64, .i64, .void => return true,
        .@"struct" => |s| {
            const generic_params = s.generic_params;
            const fields = s.fields;

            for (generic_params.items) |generic_param| {
                if (!try self.typeIsOwned(generic_param)) {
                    return false;
                }
            }

            for (fields.items) |field| {
                if (field.member_access == Parser.MemberAccess.Public and !try self.typeIsOwned(field.ty)) {
                    try self.note("public field is a shared pointer", field.where_defined);
                    return false;
                }
            }

            const methods = self.compiler.methodsOnType(type_id);
            for (methods.items) |method| {
                const fun = self.compiler.functions.items[method];
                const return_node = fun.return_node;
                var self_is_mutable = false;

                const params = fun.params;

                for (params.items) |param| {
                    if (std.mem.eql(u8, param.name, "self")) {
                        const var_id = param.var_id;

                        if (self.compiler.getVariable(var_id).is_mutable) {
                            self_is_mutable = true;
                        }
                    }
                }

                const return_type = fun.return_type;
                if (self_is_mutable) {
                    for (params.items) |param| {
                        const var_id = param.var_id;
                        const variable = self.compiler.getVariable(var_id);

                        const var_type_id = variable.ty;
                        const where_defined = variable.where_defined;

                        if (!try self.typeIsOwned(var_type_id) and !std.mem.eql(u8, param.name, "self")) {
                            try self.note("param is a shared pointer, and self is mutable", where_defined);
                            return false;
                        }
                    }
                }

                if (!try self.typeIsOwned(return_type)) {
                    if (return_node) |ret_node| {
                        try self.note("return type is a shared pointer", ret_node);
                    }
                    return false;
                }
            }

            return true;
        },
        .pointer => |ptr| {
            return ptr.pointer_type == .Owned;
        },
        .@"enum" => |e| {
            const generic_params = e.generic_params;
            const variants = e.variants;

            for (generic_params.items) |generic_param| {
                if (!try self.typeIsOwned(generic_param)) {
                    return false;
                }
            }

            for (variants.items) |variant| {
                switch (variant) {
                    .single => |single| {
                        if (!try self.typeIsOwned(single.param)) {
                            return false;
                        }
                    },
                    .@"struct" => |s| {
                        for (s.params.items) |param_type| {
                            if (!try self.typeIsOwned(param_type.ty)) {
                                return false;
                            }
                        }
                    },
                    else => {},
                }
            }
            return true;
        },
        else => return true,
    }
}

pub fn typecheckinit(self: *Typechecker, pointer_type: Parser.PointerType, node_id: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    switch (self.compiler.getNode(node_id)) {
        .call => |call| {
            const head = call.head;
            const args = call.args;

            var type_id = self.findTypeInScope(head);
            if (type_id == null) {
                try self.@"error"("unknown type in allocation", head);
                return UNKNOWN_TYPE_ID;
            }

            const output_type = try self.compiler.findOrCreateType(.{
                .pointer = .{
                    .pointer_type = pointer_type,
                    .optional = false,
                    .target = type_id.?,
                },
            });

            // FIXME: remember the reason why something isn't safe to be owned
            // so we can give a better error
            if (pointer_type == .Owned and !try self.typeIsOwned(type_id.?)) {
                try self.@"error"("tried to create owned pointer on type that shares its pointers", node_id);
            }

            type_id = self.compiler.getUnderlyingTypeId(type_id.?);

            var replacements = std.AutoHashMap(TypeId, TypeId).init(self.alloc);

            switch (self.compiler.getType(type_id.?)) {
                .@"struct" => |s| {
                    var fields: std.ArrayList(TypeField) = s.fields;
                    if (self.compiler.base_classes.get(type_id.?)) |base_classes| {
                        for (base_classes.items) |base| {
                            switch (self.compiler.getType(base)) {
                                .@"struct" => |sn| {
                                    try fields.appendSlice(sn.fields.items);
                                },
                                else => {},
                            }
                        }
                    }

                    if (args.items.len != fields.items.len) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "mismatch in number of arguments. expected {d}, found {d}", .{ fields.items.len, args.items.len });

                        try self.@"error"(error_msg, head);
                    }

                    arg_label: for (args.items) |arg| {
                        switch (self.compiler.getNode(arg)) {
                            .named_value => |nval| {
                                const name_slice = nval.name;
                                const name_value = nval.value;

                                const field_name = self.compiler.getSource(name_slice);

                                for (fields.items) |type_field| {
                                    if (std.mem.eql(u8, field_name, type_field.name)) {
                                        const member_access = type_field.member_access;

                                        if (member_access == Parser.MemberAccess.Private) {
                                            const result = self.findTypeInScopeByName("Self");

                                            if (result) |scope_type_id| {
                                                if (scope_type_id != type_id.?) {
                                                    // FIXME: add a hint to say you need to create your own constructor
                                                    try self.@"error"("'new' used on private member field from outside struct or class", name_slice);
                                                }
                                            } else {
                                                try self.@"error"("'new' used on private member field from outside struct or class", name_slice);
                                            }
                                        }

                                        const known_field_type = type_field.ty;

                                        if (self.compiler.isTypeVariable(known_field_type)) {
                                            const value_type = try self.typecheckNode(name_value, local_inferences);

                                            try replacements.put(
                                                self.compiler.getUnderlyingTypeId(known_field_type),
                                                self.compiler.getUnderlyingTypeId(value_type),
                                            );

                                            self.compiler.setNodeType(arg, value_type);

                                            // Set up expected type for inference. Note: if we find concrete values
                                            // this inference type will be replaced by the concrete type.
                                            self.compiler.setNodeType(name_value, value_type);
                                        } else {
                                            self.compiler.setNodeType(arg, known_field_type);

                                            // Set up expected type for inference. Note: if we find concrete values
                                            // this inference type will be replaced by the concrete type.
                                            self.compiler.setNodeType(name_value, known_field_type);

                                            const value_type = try self.typecheckNode(name_value, local_inferences);

                                            if (!self.unifyTypes(known_field_type, value_type, local_inferences)) {
                                                const error_msg = try std.fmt.allocPrint(self.alloc, "incompatible type for argument, expected: {s}, found: {s}", .{ try self.compiler.prettyType(value_type), try self.compiler.prettyType(known_field_type) });

                                                try self.@"error"(error_msg, name_value);
                                            }
                                        }
                                        continue :arg_label;
                                    }
                                }
                                try self.@"error"("unknown field", name_slice);
                                return UNKNOWN_TYPE_ID;
                            },
                            else => {
                                try self.@"error"("unexpected argument in allocation", arg);
                                return UNKNOWN_TYPE_ID;
                            },
                        }
                    }
                },
                else => {
                    try self.@"error"("internal error: allocation of non-struct type", node_id);
                    return UNKNOWN_TYPE_ID;
                },
            }

            if (!(replacements.count() == 0)) {
                const generic_ty_id = try self.instantiateGenericType(type_id.?, &replacements);

                return try self.compiler.findOrCreateType(.{ .pointer = .{
                    .pointer_type = pointer_type,
                    .optional = false,
                    .target = generic_ty_id,
                } });
            } else {
                return output_type;
            }
        },
        else => {
            try self.@"error"("expected an allocation call", node_id);
            return UNKNOWN_TYPE_ID;
        },
    }
    unreachable;
}

pub fn typecheckNamespacedLookup(self: *Typechecker, namespace: Parser.NodeId, item: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    if (self.findTypeInScope(namespace)) |type_id| {
        return try self.typecheckNamespacedTypeLookup(namespace, @constCast(&type_id), item, local_inferences);
    } else if (self.findModuleInScope(namespace)) |module_id| {
        return try self.typecheckNamespacedItemLookup(namespace, module_id, item, local_inferences);
    } else {
        try self.@"error"("could not find namespace", namespace);
        return VOID_TYPE_ID;
    }
}

// enter the module's scope then recurse and call typecheck_namespaced_lookup?
pub fn typecheckNamespacedItemLookup(self: *Typechecker, namespace: Parser.NodeId, module: ModuleId, item: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    const mods = self.compiler.modules.items[module];
    const node = self.compiler.getNode(item);

    switch (node) {
        .call => |call_expr| {
            const call_name = self.compiler.getSource(call_expr.head);
            const fun_id = @constCast(&mods).scope.findFunction(call_name);
            if (fun_id) |f_id| {
                return try self.typecheckCallWithFunId(call_expr.head, f_id, @constCast(&call_expr.args), null, local_inferences);
            } else {
                try self.@"error"("could not find item in namespace", namespace);
                return VOID_TYPE_ID;
            }
        },
        else => {
            try self.@"error"("could not find item in namespace", namespace);
            return VOID_TYPE_ID;
        },
    }
}

// lookup what kind of type we're working with (struct vs enum)
// resolve matching method
// typecheck against the resolved method
pub fn typecheckNamespacedTypeLookup(self: *Typechecker, namespace: Parser.NodeId, type_id: *TypeId, item: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    switch (self.compiler.getType(type_id.*)) {
        .@"struct" => {
            switch (self.compiler.getNode(item)) {
                .call => |call| {
                    const head = call.head;
                    const args = call.args;

                    const call_name = self.compiler.getSource(head);
                    const methods = self.compiler.methodsOnType(type_id.*);
                    for (methods.items) |method| {
                        const method_name = self.compiler.getSource(self.compiler.functions.items[method].name);
                        if (std.mem.eql(u8, call_name, method_name)) {
                            return try self.typecheckCallWithFunId(head, method, @constCast(&args), null, local_inferences);
                        }
                    }
                },
                else => {
                    try self.@"error"("expected static method call on struct", item);
                    return VOID_TYPE_ID;
                },
            }
        },
        .@"enum" => |e| {
            const cases = e.variants;

            switch (self.compiler.getNode(item)) {
                .call => |call| {
                    const head = call.head;
                    const args = call.args;
                    const case_name = self.compiler.getSource(head);

                    for (cases.items, 0..) |case, case_offset| {
                        switch (case) {
                            .single => |single| {
                                if (std.mem.eql(u8, single.name, case_name)) {
                                    const param = single.param;
                                    if (args.items.len == 1) {
                                        const arg_type_id = try self.typecheckNode(args.items[0], local_inferences);

                                        if (self.compiler.isTypeVariable(param)) {
                                            var replacements = std.AutoHashMap(TypeId, TypeId).init(self.alloc);
                                            try replacements.put(
                                                self.compiler.getUnderlyingTypeId(param),
                                                self.compiler.getUnderlyingTypeId(arg_type_id),
                                            );

                                            type_id.* = try self.instantiateGenericType(type_id.*, &replacements);
                                        } else if (!self.unifyTypes(param, arg_type_id, local_inferences)) {
                                            try self.@"error"("incompatible type for enum case", args.items[0]);
                                            return VOID_TYPE_ID;
                                        }

                                        try self.compiler.call_resolution.put(head, Compiler.CallTarget{ .enum_constructor = .{
                                            .type_id = type_id.*,
                                            .case_offset = case_offset,
                                        } });

                                        return self.compiler.findOrCreateType(.{
                                            .pointer = .{
                                                .pointer_type = .Shared,
                                                .optional = false,
                                                .target = type_id.*,
                                            },
                                        });
                                    } else {
                                        const error_msg = try std.fmt.allocPrint(self.alloc, "enum case has {} values, but should have 1", .{args.items.len});

                                        try self.@"error"(error_msg, item);
                                        return VOID_TYPE_ID;
                                    }
                                }
                            },
                            .@"struct" => |s| {
                                if (std.mem.eql(u8, s.name, case_name)) {
                                    if (args.items.len == s.params.items.len) {
                                        var replacements = std.AutoHashMap(TypeId, TypeId).init(self.alloc);

                                        for (args.items, 0..) |arg, idx| {
                                            const param_name = s.params.items[idx].name;
                                            const param_type_id = s.params.items[idx].ty;

                                            switch (self.compiler.getNode(arg)) {
                                                .named_value => |named_value| {
                                                    const named_value_slice = named_value.name;
                                                    const value = named_value.value;

                                                    const name_content = self.compiler.getSource(named_value_slice);

                                                    if (!std.mem.eql(u8, name_content, param_name)) {
                                                        try self.@"error"("name mismatch in enum case", named_value_slice);
                                                        return VOID_TYPE_ID;
                                                    }

                                                    const arg_type_id = try self.typecheckNode(value, local_inferences);

                                                    if (self.compiler.isTypeVariable(param_type_id)) {
                                                        try replacements.put(
                                                            param_type_id,
                                                            arg_type_id,
                                                        );
                                                    } else if (!self.unifyTypes(param_type_id, arg_type_id, local_inferences)) {
                                                        try self.@"error"("incompatible type for enum case", arg);
                                                        return VOID_TYPE_ID;
                                                    }
                                                },
                                                else => {},
                                            }
                                        }

                                        // instantiate, if we have replacements available
                                        if (!(replacements.count() == 0)) {
                                            type_id.* = try self.instantiateGenericType(type_id.*, &replacements);
                                        }

                                        try self.compiler.call_resolution.put(head, Compiler.CallTarget{ .enum_constructor = .{
                                            .type_id = type_id.*,
                                            .case_offset = case_offset,
                                        } });

                                        return self.compiler.findOrCreateType(.{
                                            .pointer = .{
                                                .pointer_type = .Shared,
                                                .optional = false,
                                                .target = type_id.*,
                                            },
                                        });
                                    } else {
                                        const error_msg = try std.fmt.allocPrint(self.alloc, "enum case has {} values, but should have 1", .{args.items.len});

                                        try self.@"error"(error_msg, item);
                                        return VOID_TYPE_ID;
                                    }
                                }
                            },
                            else => {},
                        }
                    }

                    const call_name = self.compiler.getSource(head);

                    const methods = self.compiler.methodsOnType(type_id.*);
                    for (methods.items) |method| {
                        const method_name = self.compiler.getSource(self.compiler.functions.items[method].name);
                        if (std.mem.eql(u8, call_name, method_name)) {
                            return try self.typecheckCallWithFunId(head, method, @constCast(&args), null, local_inferences);
                        }
                    }

                    try self.@"error"("could not find enum case when created enum value", item);
                },
                .name => {
                    const case_name = self.compiler.getSource(item);

                    for (cases.items, 0..) |case, case_offset| {
                        switch (case) {
                            .simple => |simple| {
                                if (std.mem.eql(u8, simple.name, case_name)) {
                                    try self.compiler.call_resolution.put(item, Compiler.CallTarget{ .enum_constructor = .{
                                        .type_id = type_id.*,
                                        .case_offset = case_offset,
                                    } });

                                    return self.compiler.findOrCreateType(.{
                                        .pointer = .{
                                            .pointer_type = .Shared,
                                            .optional = false,
                                            .target = type_id.*,
                                        },
                                    });
                                }
                            },
                            else => {},
                        }
                    }

                    try self.@"error"("can't find match enum case", item);
                },
                else => try self.@"error"("expected enum case when created enum value", item),
            }
        },
        else => try self.@"error"("expected struct or enum", namespace),
    }

    return VOID_TYPE_ID;
}

pub fn typecheckMatch(self: *Typechecker, target: Parser.NodeId, match_arms: std.ArrayList([2]Parser.NodeId), local_inferences: *std.ArrayList(TypeId)) !TypeId {
    var target_type_id = try self.typecheckNode(target, local_inferences);
    try self.maybeMoveVariable(target, local_inferences);

    target_type_id = self.compiler.resolveType(target_type_id, local_inferences);

    var inner_type_id: TypeId = target_type_id;
    switch (self.compiler.getType(target_type_id)) {
        .pointer => |ptr| {
            inner_type_id = ptr.target;
        },
        else => {},
    }

    switch (self.compiler.getType(inner_type_id)) {
        .@"enum" => |e| {
            const variants = e.variants;
            var seen_variants = std.ArrayList(bool).init(self.alloc);
            for (variants.items) |_| {
                try seen_variants.append(false);
            }

            arm_label: for (match_arms.items) |arm| {
                try self.enterScope();
                switch (self.compiler.getNode(arm[0])) {
                    .name => {
                        const var_id = try self.defineVariable(arm[0], target_type_id, false, arm[0]);
                        const variable_name = self.compiler.getSource(arm[0]);

                        try self.addVariableToScope(variable_name, var_id);
                        try self.compiler.var_resolution.put(arm[0], var_id);

                        _ = try self.typecheckNode(arm[1], local_inferences);
                        for (seen_variants.items) |*seen| {
                            seen.* = true;
                        }
                    },
                    .namespaced_lookup => |namespaced_lookup| {
                        const namespace = namespaced_lookup.namespace;
                        const item = namespaced_lookup.item;

                        // For now, let's keep things simple. The namespace has to be the enum name
                        // and the item has to be the case/arm to match

                        // FIXME/TODO: Confirm that the namespace given is a valid namespace
                        // for the type being matched
                        // let namespace_type_id = self.find_type_in_scope(namespace);

                        const namespace_typ_id = inner_type_id;

                        if (namespace_typ_id != inner_type_id) {
                            try self.@"error"("expected match case to be the same type as matched value", namespace);
                        } else {
                            switch (self.compiler.getNode(item)) {
                                .name => {
                                    const arm_name = self.compiler.getSource(item);

                                    for (variants.items, 0..) |variant, idx| {
                                        switch (variant) {
                                            .simple => |simple| {
                                                if (std.mem.eql(u8, simple.name, arm_name)) {
                                                    try self.compiler.call_resolution.put(arm[0], Compiler.CallTarget{
                                                        .enum_constructor = .{
                                                            .type_id = inner_type_id,
                                                            .case_offset = idx,
                                                        },
                                                    });

                                                    _ = try self.typecheckNode(arm[1], local_inferences);
                                                    seen_variants.items[idx] = true;
                                                    self.exitScope();
                                                    continue :arm_label;
                                                }
                                            },
                                            else => {},
                                        }
                                    }
                                    try self.@"error"("could not find match enum case", item);
                                },
                                .call => |call| {
                                    const head = call.head;
                                    const args = call.args;

                                    const arm_name = self.compiler.getSource(head);

                                    for (variants.items, 0..) |variant, idx| {
                                        switch (variant) {
                                            .single => |single| {
                                                const single_slice = single.name;
                                                const param = single.param;

                                                if (std.mem.eql(u8, single_slice, arm_name)) {
                                                    try self.compiler.call_resolution.put(arm[0], Compiler.CallTarget{
                                                        .enum_constructor = .{
                                                            .type_id = inner_type_id,
                                                            .case_offset = idx,
                                                        },
                                                    });

                                                    if (std.mem.eql(u8, @tagName(self.compiler.getNode(args.items[0])), "name")) {
                                                        const var_id = try self.defineVariable(args.items[0], param, false, args.items[0]);
                                                        try self.compiler.var_resolution.put(args.items[0], var_id);
                                                    }

                                                    _ = try self.typecheckNode(arm[1], local_inferences);
                                                    seen_variants.items[idx] = true;
                                                    self.exitScope();
                                                    continue :arm_label;
                                                }
                                            },
                                            .@"struct" => |s| {
                                                const variant_name = s.name;
                                                const params = s.params;

                                                if (std.mem.eql(u8, variant_name, arm_name)) {
                                                    try self.compiler.call_resolution.put(arm[0], Compiler.CallTarget{
                                                        .enum_constructor = .{
                                                            .type_id = inner_type_id,
                                                            .case_offset = idx,
                                                        },
                                                    });

                                                    seen_variants.items[idx] = true;
                                                    var id: usize = 0;
                                                    while (id < params.items.len) {
                                                        const param = params.items[id];
                                                        const arg = args.items[id];

                                                        if (std.mem.eql(u8, @tagName(self.compiler.getNode(args.items[0])), "name")) {
                                                            const var_id = try self.defineVariable(arg, param.ty, false, arg);
                                                            try self.compiler.var_resolution.put(args.items[id], var_id);
                                                        }
                                                        id += 1;
                                                    }

                                                    _ = try self.typecheckNode(arm[1], local_inferences);
                                                    self.exitScope();
                                                    continue :arm_label;
                                                }
                                            },
                                            else => {},
                                        }
                                    }

                                    try self.@"error"("could not find match enum case", item);
                                },
                                else => {
                                    @panic("not yet supported");
                                },
                            }
                        }
                    },
                    else => try self.@"error"("unexpected kind of match case in match", arm[0]),
                }
                self.exitScope();
            }

            for (seen_variants.items, 0..) |seen, variant| {
                if (!seen) {
                    switch (variants.items[variant]) {
                        .simple => |simple| {
                            const variant_name = simple.name;
                            const error_msg = try std.fmt.allocPrint(self.alloc, "missing pattern match for {s}", .{variant_name});

                            try self.@"error"(error_msg, target);
                        },
                        .single => |single| {
                            const variant_name = single.name;
                            const error_msg = try std.fmt.allocPrint(self.alloc, "missing pattern match for {s}(..)", .{variant_name});

                            try self.@"error"(error_msg, target);
                        },
                        .@"struct" => |s| {
                            const variant_name = s.name;
                            const error_msg = try std.fmt.allocPrint(self.alloc, "missing pattern match for {s}(..)", .{variant_name});

                            try self.@"error"(error_msg, target);
                        },
                    }
                }
            }

            return VOID_TYPE_ID;
        },
        else => {
            try self.@"error"("currently only enums are supported in matches", target);
            return VOID_TYPE_ID;
        },
    }
}

pub fn instantiateGenericFun(self: *Typechecker, fun_id: FuncId, replacements: *std.AutoHashMap(TypeId, TypeId)) anyerror!TypeId {
    const fun = self.compiler.functions.items[fun_id];

    const fun_name = fun.name;
    const params = try fun.params.clone();
    const new_params = try params.clone();
    const lifetime_annotations = try fun.lifetime_annotations.clone();
    const type_params = std.ArrayList(TypeParam).init(self.alloc);
    const inference_vars = std.ArrayList(TypeId).init(self.alloc);
    const return_node = fun.return_node;
    var return_type = fun.return_type;
    var initial_node_id = fun.initial_node_id;
    var fun_body = fun.body;
    const is_extern = fun.is_extern;

    for (new_params.items) |*new_param| {
        var new_var = self.compiler.getVariable(new_param.*.var_id);
        new_var.ty = try self.instantiateGenericType(new_var.ty, replacements);
        try self.compiler.variables.append(new_var);
        new_param.*.var_id = self.compiler.variables.items.len - 1;
    }

    for (lifetime_annotations.items) |*lifetime_annotation| {
        switch (lifetime_annotation.*) {
            .equality => |*equality| {
                switch (equality.*.left) {
                    .variable => |*v_id| {
                        for (params.items, 0..) |param, idx| {
                            const new_param = new_params.items[idx];
                            if (v_id.* == param.var_id) {
                                v_id.* = new_param.var_id;
                            }
                        }
                    },
                    else => {},
                }
                switch (equality.*.right) {
                    .variable => |*v_id| {
                        for (params.items, 0..) |param, idx| {
                            const new_param = new_params.items[idx];
                            if (v_id.* == param.var_id) {
                                v_id.* = new_param.var_id;
                            }
                        }
                    },
                    else => {},
                }
            },
        }
    }

    return_type = try self.instantiateGenericType(return_type, replacements);

    if (initial_node_id) |inner_initial_node_id| {
        if (fun_body) |inner_body| {
            const offset = self.compiler.numAstNodes() - inner_initial_node_id;

            try self.compiler.resizeNodeTypes(self.compiler.numAstNodes() + (inner_body - inner_initial_node_id + 1), UNKNOWN_TYPE_ID);

            for (inner_initial_node_id..inner_body + 1) |raw_node_id| {
                var ast_node = self.compiler.getNode(raw_node_id);
                switch (ast_node) {
                    .binary_op => |*bin_op| {
                        bin_op.*.left = bin_op.*.left + offset;
                        bin_op.*.op = bin_op.*.op + offset;
                        bin_op.*.right = bin_op.*.right + offset;
                    },
                    .block => |*block_id| {
                        const may_allocate = self.compiler.blocks.items[block_id.*].may_locally_allocate;
                        const node_list = try self.compiler.blocks.items[block_id.*].nodes.clone();
                        const block0 = Parser.Block{ .nodes = node_list, .may_locally_allocate = may_allocate };
                        for (block0.nodes.items) |*node| {
                            node.* = node.* + offset;
                        }
                        try self.compiler.blocks.append(block0);
                        block_id.* = self.compiler.blocks.items.len - 1;
                    },
                    .call => |*call| {
                        call.*.head = call.*.head + offset;
                        for (call.*.args.items) |*arg| {
                            arg.* = arg.* + offset;
                        }
                    },
                    .@"defer" => |*defer_expr| {
                        defer_expr.*.pointer = defer_expr.*.pointer + offset;
                        defer_expr.*.callback = defer_expr.*.callback + offset;
                    },
                    .@"enum" => |*e| {
                        e.*.typename = e.*.typename + offset;
                        for (e.*.cases.items) |*c| {
                            c.* = c.* + offset;
                        }

                        for (e.*.methods.items) |*method| {
                            method.* = method.* + offset;
                        }
                    },
                    .enum_case => |*enum_case| {
                        enum_case.*.name = enum_case.*.name + offset;
                        if (enum_case.*.payload) |*pay| {
                            for (pay.*.items) |*p| {
                                p.* = p.* + offset;
                            }
                        }
                    },
                    .extern_type => |*extern_type| {
                        extern_type.*.name = extern_type.*.name + offset;
                    },
                    .field => |*field| {
                        field.*.name = field.*.name + offset;
                        field.*.typename = field.*.typename + offset;
                    },
                    .@"for" => |*for_expr| {
                        for_expr.*.variable = for_expr.*.variable + offset;
                        for_expr.*.range = for_expr.*.range + offset;
                        for_expr.*.block = for_expr.*.block + offset;
                    },
                    .fun_type => |*fun_type| {
                        for (fun_type.*.params.items) |*param| {
                            param.* = param.* + offset;
                        }
                        fun_type.*.ret = fun_type.*.ret + offset;
                    },
                    .@"if" => |*if_expr| {
                        if_expr.*.condition = if_expr.*.condition + offset;
                        if_expr.*.then_block = if_expr.*.then_block + offset;
                        if (if_expr.*.else_expression) |*else_expr| {
                            else_expr.* = else_expr.* + offset;
                        }
                    },
                    .index => |*index| {
                        index.*.target = index.*.target + offset;
                        index.*.index = index.*.index + offset;
                    },
                    .let => |*let_stmt| {
                        let_stmt.*.variable_name = let_stmt.*.variable_name + offset;
                        if (let_stmt.*.ty) |*ty| {
                            let_stmt.*.ty = ty.* + offset;
                        }
                        let_stmt.*.initializer = let_stmt.*.initializer + offset;
                    },
                    .match => |*match| {
                        match.*.target = match.*.target + offset;
                        for (match.*.match_arms.items) |*m| {
                            m.*[0] = m.*[0] + offset;
                            m.*[1] = m.*[1] + offset;
                        }
                    },
                    .member_access => |*member_access| {
                        member_access.*.target = member_access.*.target + offset;
                        member_access.*.field = member_access.*.field + offset;
                    },
                    .named_value => |*named_value| {
                        named_value.*.name = named_value.*.name + offset;
                        named_value.*.value = named_value.*.value + offset;
                    },
                    .namespaced_lookup => |*namespaced_lookup| {
                        namespaced_lookup.*.namespace = namespaced_lookup.*.namespace + offset;
                        namespaced_lookup.*.item = namespaced_lookup.*.item + offset;
                    },
                    .new => |*new_expr| {
                        new_expr.*.allocated = new_expr.*.allocated + offset;
                    },
                    .param => |*param| {
                        param.*.name = param.*.name + offset;
                        param.*.ty = param.*.ty + offset;
                    },
                    .range => |*range| {
                        range.*.lhs = range.*.lhs + offset;
                        range.*.rhs = range.*.rhs + offset;
                    },
                    .raw_buffer => |*raw_buffer| {
                        for (raw_buffer.*.items) |*item| {
                            item.* = item.* + offset;
                        }
                    },
                    .raw_buffer_type => |*inner| {
                        inner.*.inner = inner.*.inner + offset;
                    },
                    .resize_buffer => |*buffer| {
                        buffer.*.pointer = buffer.*.pointer + offset;
                        buffer.*.new_size = buffer.*.new_size + offset;
                    },
                    .@"return" => |*ret| {
                        if (ret.*) |_| {
                            ret.* = ret.*.? + offset;
                        }
                    },
                    .statement => |*stmt| {
                        stmt.* = stmt.* + offset;
                    },
                    .type => |*ty| {
                        ty.*.name = ty.*.name + offset;
                        if (ty.*.params) |_| {
                            ty.*.params = ty.*.params.? + offset;
                        }
                    },
                    .unsafe_block => |*ub| {
                        ub.* = ub.* + offset;
                    },
                    .use => |*use| {
                        use.*.path = use.*.path + offset;
                    },
                    .@"while" => |*while_expr| {
                        while_expr.*.condition = while_expr.*.condition + offset;
                        while_expr.*.block = while_expr.*.block + offset;
                    },
                    else => {},
                }

                _ = try self.compiler.pushNode(ast_node);
                try self.compiler.span_start.append(self.compiler.span_start.items[raw_node_id]);
                try self.compiler.span_end.append(self.compiler.span_end.items[raw_node_id]);
            }
            initial_node_id = inner_initial_node_id + offset;
            fun_body = inner_body + offset;
        }
    }

    try self.compiler.functions.append(.{
        .name = fun_name,
        .params = new_params,
        .lifetime_annotations = lifetime_annotations,
        .type_params = type_params,
        .inference_vars = inference_vars,
        .return_node = return_node,
        .return_type = return_type,
        .initial_node_id = initial_node_id,
        .body = fun_body,
        .is_extern = is_extern,
    });

    const f_id = self.compiler.functions.items.len - 1;
    try self.typecheckFun(f_id);

    return f_id;
}

pub fn instantiateGenericType(self: *Typechecker, type_id: TypeId, replacements: *std.AutoHashMap(TypeId, TypeId)) !TypeId {
    var replacements_copy = try replacements.clone();

    switch (self.compiler.getType(type_id)) {
        .@"enum" => |e| {
            var new_variants = std.ArrayList(EnumVariant).init(self.alloc);
            const methods = self.compiler.methodsOnType(type_id);
            var new_methods = std.ArrayList(FuncId).init(self.alloc);

            variant_label: for (e.variants.items) |variant| {
                switch (variant) {
                    .simple => try new_variants.append(variant),
                    .single => |single| {
                        if (replacements_copy.get(single.param)) |replacement| {
                            try new_variants.append(.{ .single = .{
                                .name = single.name,
                                .param = replacement,
                            } });
                            continue :variant_label;
                        }

                        try new_variants.append(.{ .single = .{
                            .name = single.name,
                            .param = single.param,
                        } });
                    },
                    .@"struct" => |s| {
                        var new_params = std.ArrayList(EnumStructVariant).init(self.alloc);
                        for (s.params.items) |param| {
                            if (replacements_copy.get(param.ty)) |replacement| {
                                try new_params.append(.{
                                    .name = param.name,
                                    .ty = replacement,
                                });
                            }
                        }
                        try new_variants.append(.{ .@"struct" = .{
                            .name = s.name,
                            .params = new_params,
                        } });
                    },
                }
            }

            const new_type_id = try self.compiler.findOrCreateType(Type{
                .@"enum" = .{
                    // check type of params
                    .generic_params = std.ArrayList(TypeId).init(self.alloc), // we're now fully instantiated
                    .variants = new_variants,
                },
            });

            try replacements_copy.put(self.compiler.getUnderlyingTypeId(type_id), self.compiler.getUnderlyingTypeId(new_type_id));

            for (methods.items) |method| {
                try new_methods.append(try self.instantiateGenericFun(method, &replacements_copy));
            }

            try self.compiler.insertMethodsOnType(new_type_id, new_methods);
            return new_type_id;
        },
        .@"struct" => |s| {
            var new_fields = std.ArrayList(TypeField).init(self.alloc);
            var new_methods = std.ArrayList(FuncId).init(self.alloc);
            const methods = self.compiler.methodsOnType(type_id);
            const is_allocator = s.is_allocator;

            for (s.fields.items) |type_field| {
                if (replacements_copy.get(type_field.ty)) |replacement| {
                    try new_fields.append(.{
                        .member_access = type_field.member_access,
                        .name = type_field.name,
                        .ty = replacement,
                        .where_defined = type_field.where_defined,
                    });
                    break;
                }
            }

            const new_type_id = try self.compiler.findOrCreateType(Type{
                .@"struct" = .{
                    // check type of params
                    .generic_params = std.ArrayList(TypeId).init(self.alloc),
                    .fields = new_fields,
                    .is_allocator = is_allocator,
                },
            });

            try replacements_copy.put(self.compiler.getUnderlyingTypeId(type_id), self.compiler.getUnderlyingTypeId(new_type_id));

            for (methods.items) |method| {
                try new_methods.append(try self.instantiateGenericFun(method, &replacements_copy));
            }

            try self.compiler.insertMethodsOnType(new_type_id, new_methods);
            return new_type_id;
        },
        .pointer => |ptr| {
            if (replacements_copy.get(ptr.target)) |replacement| {
                return self.compiler.findOrCreateType(.{ .pointer = .{
                    .pointer_type = ptr.pointer_type,
                    .optional = ptr.optional,
                    .target = replacement,
                } });
            }
            return type_id;
        },
        else => {
            // Check to see if we have a replacement for this exact TypeId. If so, return the replacement.
            // Otherwise return the original type_id
            if (replacements_copy.get(type_id)) |replacement| {
                return replacement;
            }
            return type_id;
        },
    }
}

pub fn typecheckVarOrFunction(self: *Typechecker, node_id: Parser.NodeId, var_or_fun_id: ?VarOrFuncId) !TypeId {
    if (var_or_fun_id) |var_or_fun| {
        switch (var_or_fun) {
            .var_id => |var_id| {
                if (try self.varWasPreviouslyMoved(var_id)) |where_moved| {
                    try self.@"error"("moved variable accessed after move", node_id);
                    try self.note("location of variable move", where_moved);
                }
                try self.compiler.var_resolution.put(node_id, var_id);

                const variable = self.compiler.getVariable(var_id);
                return variable.ty;
            },
            .fun_id => |fun_id| {
                const fun = self.compiler.functions.items[fun_id];

                try self.compiler.fun_resolution.put(node_id, fun_id);

                if (fun_id != 0) {
                    return try self.compiler.findOrCreateType(.{ .fun = .{ .params = fun.params, .ret = fun.return_type } });
                } else {
                    return UNKNOWN_TYPE_ID;
                }
            },
        }
    } else {
        const name = self.compiler.getSource(node_id);

        // Reserved name for synthetic self variables
        if (std.mem.eql(u8, name, ".")) {
            try self.@"error"("can't find 'self' variable", node_id);
        } else {
            try self.@"error"("can't find variable", node_id);
        }
        return UNKNOWN_TYPE_ID;
    }
}

pub fn typecheckModule(self: *Typechecker, path: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    var module_block: Parser.NodeId = undefined;
    if (self.compiler.module_lookup_use.get(path)) |b| {
        module_block = b;
    } else {
        @panic("all paths should have valid module blocks associated with them");
    }

    if (self.compiler.module_resolution.contains(module_block)) {
        return VOID_TYPE_ID;
    } else {
        const block_id = self.compiler.getNode(module_block);
        switch (block_id) {
            .block => |b_id| {
                const scope = try self.typecheckBlock(module_block, b_id, local_inferences);
                const module = Module{ .scope = scope };
                const module_id = try self.compiler.addModule(module_block, module);
                // for now it's just a simple identifer(ex: use utils)
                const file_path = self.compiler.getSourcePath(module_block);
                try self.addModuleToScope(file_path, module_id);
                return VOID_TYPE_ID;
            },
            else => {
                @panic("module block node ids should always refer to a valid Block in the ast");
            },
        }
    }

    unreachable;
}

pub fn typecheck(self: *Typechecker) !Compiler {
    const num_nodes = self.compiler.ast_node.items.len;
    try self.compiler.resizeNodeTypes(num_nodes, UNKNOWN_TYPE_ID);

    const top_level: Parser.NodeId = num_nodes - 1;
    // Top-level local inferences
    var local_inference = std.ArrayList(TypeId).init(self.alloc);
    local_inference.deinit();

    // Typecheck until we hit a fix point for our inferences
    while (true) {
        const before = local_inference.items;

        _ = try self.typecheckNode(top_level, &local_inference);

        if (!(self.compiler.errors.items.len == 0) or isListEqual(before, local_inference.items)) {
            break;
        }
    }

    const top_level_type = self.compiler.getNodeType(top_level);

    // If we haven't seen a main, create one from the top-level node
    if (!self.compiler.hasMain()) {
        // Synthesis of a fake 'main' node
        const new_source = try std.fmt.allocPrint(self.alloc, "{s}main", .{self.compiler.source});
        self.compiler.source = new_source;
        const main_node = try self.compiler.pushNode(Parser.AstNode{ .name = Parser.Void.void });
        try self.compiler.span_start.append(self.compiler.source.len - 4);
        try self.compiler.span_end.append(self.compiler.source.len);
        // re-resize to make sure we have enough nodes
        try self.compiler.resizeNodeTypes(main_node + 1, UNKNOWN_TYPE_ID);

        try self.compiler.functions.append(Function{
            .name = main_node,
            .params = std.ArrayList(Param).init(self.alloc),
            .type_params = std.ArrayList(TypeParam).init(self.alloc),
            .lifetime_annotations = std.ArrayList(LifetimeAnnotation).init(self.alloc),
            .inference_vars = local_inference,
            .return_type = top_level_type,
            .return_node = null,
            .initial_node_id = num_nodes,
            .body = top_level,
            .is_extern = false,
        });
    }

    return self.compiler;
}

pub fn addVariableToScope(self: *Typechecker, name: []const u8, var_id: VarId) !void {
    const last = self.scope.items.len - 1;
    try self.scope.items[last].variables.put(name, var_id);
}

pub fn addTypeToScope(self: *Typechecker, name: []const u8, type_id: TypeId) !void {
    const last = self.scope.items.len - 1;
    try self.scope.items[last].types.put(name, type_id);
}

pub fn addModuleToScope(self: *Typechecker, path: []const u8, module_id: ModuleId) !void {
    const last = self.scope.items.len - 1;
    try self.scope.items[last].modules.put(path, module_id);
}

pub fn defineVariable(self: *Typechecker, name: Parser.NodeId, ty: TypeId, is_mutable: bool, where_defined: Parser.NodeId) !VarId {
    const variable_name = self.compiler.getSource(name);
    try self.compiler.variables.append(.{
        .name = name,
        .ty = ty,
        .is_mutable = is_mutable,
        .where_defined = where_defined,
    });

    const var_id = self.compiler.variables.items.len - 1;

    try self.addVariableToScope(variable_name, var_id);

    return var_id;
}

pub fn findNameInScope(self: *Typechecker, name: Parser.NodeId) ?VarOrFuncId {
    var name_str = self.compiler.getSource(name);

    // Expand the shorthand, and look for 'self' instead
    if (std.mem.eql(u8, name_str, ".")) {
        name_str = "self";
    }

    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[@intCast(i)].findName(name_str)) |v| {
            return v;
        }
    }

    return null;
}

pub fn findVariableInScope(self: *Typechecker, var_name: Parser.NodeId) ?VarId {
    var name = self.compiler.source[self.compiler.span_start.items[var_name]..self.compiler.span_end.items[var_name]];

    // Expand the shorthand, and look for 'self' instead
    if (std.mem.eql(u8, name, ".")) {
        name = "self";
    }

    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[@intCast(i)].variables.get(name)) |var_id| {
            return var_id;
        }
    }

    return null;
}

pub fn findTypeInScope(self: *Typechecker, type_name: Parser.NodeId) ?TypeId {
    const name = self.compiler.getSource(type_name);

    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[@intCast(i)].findType(name)) |type_id| {
            return type_id;
        }
    }

    return null;
}

pub fn findTypeInScopeByName(self: *Typechecker, name: []const u8) ?TypeId {
    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[@intCast(i)].types.get(name)) |type_id| {
            return type_id;
        }
    }

    return null;
}

pub fn findExpectedReturnType(self: *Typechecker) ?TypeId {
    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[@intCast(i)].expected_return_type) |ret_type| {
            return ret_type;
        }
    }

    return null;
}

pub fn setExpectedReturnType(self: *Typechecker, expected_type: TypeId) !void {
    self.scope.items[self.scope.items.len - 1].expected_return_type = expected_type;
}

pub fn isBindingMutable(self: *Typechecker, node_id: Parser.NodeId) bool {
    switch (self.compiler.getNode(node_id)) {
        .name => {
            if (self.compiler.var_resolution.get(node_id)) |var_id| {
                return self.compiler.getVariable(var_id).is_mutable;
            } else {
                return false;
            }
        },
        .member_access => |member| {
            return self.isBindingMutable(member.target);
        },
        else => return false,
    }
}

pub fn endsInReturn(self: *Typechecker, node_id: Parser.NodeId) bool {
    switch (self.compiler.getNode(node_id)) {
        .@"return" => return true,
        .block => |stmt| {
            const bl = self.compiler.blocks.items[stmt];
            if (bl.nodes.items.len == 0) return false;
            const id = bl.nodes.items[bl.nodes.items.len - 1];
            return self.endsInReturn(id);
        },
        .unsafe_block => |bl| {
            return self.endsInReturn(bl);
        },
        .@"if" => |if_expr| {
            const then_block_ends_in_return = self.endsInReturn(if_expr.then_block);

            if (if_expr.else_expression) |else_expression| {
                const else_ends_in_return = self.endsInReturn(else_expression);
                return then_block_ends_in_return and else_ends_in_return;
            } else {
                return then_block_ends_in_return;
            }
        },
        else => return false,
    }
}

pub fn findModuleInScope(self: *Typechecker, namespace: Parser.NodeId) ?ModuleId {
    const name = self.compiler.getSource(namespace);

    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        var module_iter = self.scope.items[@intCast(i)].modules.iterator();
        while (module_iter.next()) |module_entry| {
            // definitely incorrect, but we currently only have one path segment
            // this needs to somehow be able to resolve the path against all the segments of the path
            const simple_path = module_entry.key_ptr.*[0 .. module_entry.key_ptr.len - 5];
            var path_copy: []const u8 = std.fmt.allocPrint(self.alloc, "{s}", .{simple_path}) catch unreachable;
            std.mem.reverse(u8, @constCast(path_copy));
            var split_item = std.mem.splitSequence(u8, path_copy, "/");
            path_copy = split_item.first();
            std.mem.reverse(u8, @constCast(path_copy));
            if (std.mem.eql(u8, path_copy, name)) {
                return module_entry.value_ptr.*;
            }
        }
    }

    return null;
}

pub fn @"error"(self: *Typechecker, message: []const u8, node_id: Parser.NodeId) !void {
    try self.compiler.errors.append(Errors.SourceError{ .message = message, .node_id = node_id, .severity = Errors.Severity.Error });
}

pub fn note(self: *Typechecker, message: []const u8, node_id: Parser.NodeId) !void {
    try self.compiler.errors.append(Errors.SourceError{ .message = message, .node_id = node_id, .severity = Errors.Severity.Note });
}

pub fn enterScope(self: *Typechecker) !void {
    try self.scope.append(Scope.init(self.alloc));
}

pub fn exitScope(self: *Typechecker) void {
    _ = self.scope.pop();
}

pub fn isListEqual(a: []TypeId, b: []TypeId) bool {
    if (a.len != b.len) return false;

    for (a, 0..) |a_item, i| {
        if (a_item != b[i]) return false;
    }

    return true;
}

pub fn contains(a: std.ArrayList([]const u8), b: []const u8) bool {
    for (a.items) |item| {
        if (std.mem.eql(u8, item, b)) {
            return true;
        }
    }

    return false;
}

pub fn containsClass(a: std.ArrayList(TypeId), id: TypeId) bool {
    for (a.items) |item| {
        if (item == id) {
            return true;
        }
    }

    return false;
}
