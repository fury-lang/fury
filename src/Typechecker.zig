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

    pub fn new(name: []const u8, var_id: VarId) Param {
        return Param{ .name = name, .var_id = var_id };
    }
};

pub const TypeParam = struct {
    name: []const u8,
    type_id: TypeId,

    pub fn new(name: []const u8, type_id: TypeId) TypeParam {
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

    pub fn new(alloc: std.mem.Allocator) Scope {
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

pub fn new(alloc: std.mem.Allocator, compiler: Compiler) !Typechecker {
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
    try scope.append(Scope.new(alloc));

    const last = scope.items.len - 1;
    try scope.items[last].functions.put("println", 0);

    typechecker.scope = scope;

    return typechecker;
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
        return lhs_ty.pointer.pointer_type == Parser.PointerType.Unknown or (lhs_ty.pointer.pointer_type == rhs_ty.pointer.pointer_type) or (lhs_ty.pointer.pointer_type == Parser.PointerType.Shared and
            rhs_ty.pointer.pointer_type == Parser.PointerType.Owned) and lhs_ty.pointer.target == rhs_ty.pointer.target and (lhs_ty.pointer.optional == rhs_ty.pointer.optional or lhs_ty.pointer.optional);
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "raw_buffer") and std.mem.eql(u8, @tagName(rhs_ty), "raw_buffer")) {
        unreachable;
    } else if (std.mem.eql(u8, @tagName(lhs_ty), "fun") and std.mem.eql(u8, @tagName(rhs_ty), "fun")) {
        unreachable;
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
        .raw_buffer => unreachable,
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

    var _args: std.ArrayList(Parser.NodeId) = undefined;
    if (method_target) |method| {
        var output = std.ArrayList(Parser.NodeId).init(self.alloc);
        try output.append(method);
        try output.appendSlice(args.*.items);
        _args = output;
    } else {
        _args = args.*;
    }

    if (fun_id == 0) {
        // Just for now, special-case println
        try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .function = fun_id });

        for (_args.items) |arg| {
            // TODO add name-checking
            _ = try self.typecheckNode(arg, local_inferences);
        }

        return VOID_TYPE_ID;
    }

    if (_args.items.len != params.items.len) {
        const error_msg = try std.fmt.allocPrint(self.alloc, "expected {d} args, found {d}", .{ params.items.len, _args.items.len });
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
    try self.typecheckCallHelper(&_args, params, method_target, local_inferences, &type_var_replacements);

    const _fun_id = fun_id;
    if (!(type_var_replacements.count() == 0)) {
        // TODO generic fun
    }

    // TODO do we want to wait until all params are checked
    // before we mark this as resolved?
    try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .function = _fun_id });

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
            .named_value => {
                // TODO
            },
            else => {
                var arg_type: TypeId = undefined;
                if (idx == 0 and method_target != null) {
                    arg_type = self.compiler.getNodeType(arg);
                } else {
                    arg_type = try self.typecheckNode(arg, local_inferences);
                }

                // TODO check for variables moves
                const param = params.items[idx];
                const variable = self.compiler.getVariable(param.var_id);

                const variable_ty = variable.ty;
                const variable_node_id = variable.name;
                _ = variable_node_id;

                if (self.compiler.isTypeVariable(variable_ty)) {
                    if (type_var_replacements.get(variable_ty)) |replacement| {
                        if (self.unifyTypes(replacement, arg_type, local_inferences)) {} else {
                            const expected_type = self.compiler.resolveType(replacement, local_inferences);
                            const found_type = self.compiler.resolveType(arg_type, local_inferences);
                            const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ try self.compiler.prettyType(expected_type), try self.compiler.prettyType(found_type) });
                            try self.@"error"(error_msg, arg);

                            // TODO add a note about where params are defined
                        }
                    } else {
                        try type_var_replacements.put(self.compiler.getUnderlyingTypeId(variable_ty), self.compiler.getUnderlyingTypeId(arg_type));
                    }
                } else if (self.unifyTypes(variable_ty, arg_type, local_inferences)) {}
                // TODO check for subtype
                else {
                    const expected_type = self.compiler.resolveType(variable_ty, local_inferences);
                    const found_type = self.compiler.resolveType(arg_type, local_inferences);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ try self.compiler.prettyType(expected_type), try self.compiler.prettyType(found_type) });
                    try self.@"error"(error_msg, arg);

                    // TODO add a note about where params are defined
                }

                if (self.compiler.getVariable(param.var_id).is_mutable and !self.isBindingMutable(arg)) {
                    try self.@"error"("argument to function needs to be mutable", arg);
                    // TODO add a note about where params are defined
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
    _ = lifetime_annotations;
    var fun_params = std.ArrayList(Param).init(self.alloc);
    var fun_type_params = std.ArrayList(TypeParam).init(self.alloc);
    _ = &fun_type_params;

    const fun_name = self.compiler.source[self.compiler.span_start.items[name]..self.compiler.span_end.items[name]];

    try self.enterScope();

    // TODO generic type params
    if (type_params) |_| {}

    const _params = self.compiler.getNode(params);
    switch (_params) {
        .params => |pm| {
            for (pm.items) |unchecked_param| {
                const _param = self.compiler.getNode(unchecked_param);
                switch (_param) {
                    .param => |p| {
                        const _name = p.name;
                        const _param_name = self.compiler.getSource(_name);
                        const ty = p.ty;
                        const is_mutable = p.is_mutable;

                        const tt = try self.typecheckTypename(ty);

                        const var_id = try self.defineVariable(_name, tt, is_mutable, name);
                        self.compiler.setNodeType(_name, ty);
                        try fun_params.append(Param.new(_param_name, var_id));
                    },
                    else => try self.@"error"("expected function parameter", unchecked_param),
                }
            }
        },
        else => try self.@"error"("expected function parameters", params),
    }

    const checked_lifetime_annotations = std.ArrayList(LifetimeAnnotation).init(self.alloc);

    // TODO typecheck lifetime annotations

    var _return_ty = VOID_TYPE_ID;
    if (return_ty) |ret_ty| {
        _return_ty = try self.typecheckTypename(ret_ty);
    }

    self.exitScope();

    try self.compiler.functions.append(Function{
        .name = name,
        .params = fun_params,
        .lifetime_annotations = checked_lifetime_annotations,
        .type_params = fun_type_params,
        .inference_vars = std.ArrayList(TypeId).init(self.alloc),
        .return_node = return_ty,
        .return_type = _return_ty,
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

pub fn typecheckStruct(self: *Typechecker, typename: Parser.NodeId, fields: std.ArrayList(Parser.NodeId), methods: std.ArrayList(Parser.NodeId), explicit_no_alloc: bool, base_class: ?Parser.NodeId) !TypeId {
    const node_type = self.compiler.getNode(typename);
    var _name: Parser.NodeId = undefined;
    var params: ?Parser.NodeId = null;
    switch (node_type) {
        .type => |ty| {
            _name = ty.name;
            params = ty.params;
        },
        else => @panic("internal error: struct does not have type as name"),
    }

    var has_pointers = false;
    var generics_params = std.ArrayList(TypeId).init(self.alloc);

    try self.enterScope();

    if (params) |p| {
        var _params = std.ArrayList(Parser.NodeId).init(self.alloc);
        switch (self.compiler.getNode(p)) {
            .params => |pr| {
                _params = pr;
            },
            else => @panic("internal error: struct generic params are not proper ast node"),
        }

        for (_params.items) |param| {
            const type_id = try self.compiler.freshTypeVariable(param);

            // be conservative and assume generic parameters might be pointers
            has_pointers = true;
            try generics_params.append(type_id);

            const type_var_name = self.compiler.getSource(param);
            try self.addTypeToScope(type_var_name, type_id);
        }
    }

    const struct_name = self.compiler.getSource(_name);

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

        if (!self.compiler.isCopyableType(field_type)) {
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

        var _name0: Parser.NodeId = undefined;
        var _type_params: ?Parser.NodeId = null;
        var _params: Parser.NodeId = undefined;
        var lifetime_annotations = std.ArrayList(Parser.NodeId).init(self.alloc);
        var return_ty: ?Parser.NodeId = null;
        var initial_node_id: ?Parser.NodeId = null;
        var _block: ?Parser.NodeId = null;
        var _is_external = false;
        for (methods.items) |method| {
            switch (self.compiler.getNode(method)) {
                .fun => |f| {
                    _name0 = f.name;
                    _type_params = f.type_params;
                    _params = f.params;
                    lifetime_annotations = f.lifetime_annotations;
                    return_ty = f.return_ty;
                    initial_node_id = f.initial_node_id;
                    _block = f.block;
                    _is_external = f.is_extern;
                },
                else => {
                    try self.@"error"("internal error: can't find method definition during typecheck", method);
                    return VOID_TYPE_ID;
                },
            }

            const fun_id = try self.typecheckFunPredecl(_name0, _type_params, _params, &lifetime_annotations, return_ty, initial_node_id, _block, _is_external);

            if (_block == null and !_is_external) {
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

            const _methods = self.compiler.methodsOnType(class);
            const virtual_methods = self.compiler.virtualMethodsOnType(class);

            // TODO: proper equality for methods, have some way to check equality between two functions to see if one is an implementation of the other
            var implemented_methods = std.ArrayList([]const u8).init(self.alloc);

            for (_methods.items) |method| {
                const fun = self.compiler.functions.items[method];
                const method_name = self.compiler.getSource(fun.name);
                try implemented_methods.append(method_name);
                try fun_ids.append(method);
            }

            for (virtual_methods.items) |method| {
                const fun = self.compiler.functions.items[method];
                const method_name = self.compiler.getSource(fun.name);
                if (!contains(implemented_methods, method_name)) {
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
                }
            }
        },
    }
}

pub fn typecheckBlock(self: *Typechecker, node_id: Parser.NodeId, block_id: Parser.BlockId, local_inferences: *std.ArrayList(TypeId)) !Scope {
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

    return self.scope.pop();
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
        .none => unreachable,
        .string => @panic("strings not yet supported"),
        .c_char => node_type = C_CHAR_TYPE_ID,
        .c_string => node_type = C_STRING_TYPE_ID,
        .let => |let_stmt| {
            const variable_name = let_stmt.variable_name;
            const initializer = let_stmt.initializer;
            const is_mutable = let_stmt.is_mutable;
            const ty = let_stmt.ty;

            var initializer_type = try self.typecheckNode(initializer, local_inferences);
            initializer_type = self.compiler.resolveType(initializer_type, local_inferences);

            // TODO check for variable moves

            var var_id: VarId = undefined;
            if (ty) |_ty| {
                const tt = try self.typecheckTypename(_ty);
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
                var _ty: TypeId = undefined;
                switch (node_type_id) {
                    UNKNOWN_TYPE_ID => {
                        const tt = try self.compiler.findOrCreateType(.{
                            .fun_local_type_val = .{ .offset = local_inferences.items.len },
                        });

                        try local_inferences.*.append(UNKNOWN_TYPE_ID);
                        self.compiler.setNodeType(variable_name, tt);

                        _ty = tt;
                    },
                    else => {
                        _ty = node_type_id;
                    },
                }

                var tt = self.compiler.resolveType(_ty, local_inferences);
                if (tt == UNKNOWN_TYPE_ID) {
                    tt = _ty;
                } else {
                    tt = self.compiler.resolveType(_ty, local_inferences);
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
                    if (@TypeOf(self.compiler.getNode(rhs)) == @TypeOf(Parser.AstNode.none)) {
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
                    const rhs_ty = try self.typecheckNode(rhs, local_inferences);

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
                        const _type_id = type_field.ty;

                        if (std.mem.eql(u8, field_name, type_field.name)) {
                            if (type_field.member_access == Parser.MemberAccess.Private and !std.mem.eql(u8, target_name, ".") and !std.mem.eql(u8, target_name, "self")) {
                                // We're private and not accessing 'self'
                                try self.@"error"("access of private field", field);
                            }

                            self.compiler.setNodeType(node_id, _type_id);
                            self.compiler.setNodeType(field, _type_id);
                            return _type_id;
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
                .@"enum" => unreachable,
                else => {
                    try self.@"error"("field or method access on type without fields or methods", target);
                    node_type = UNKNOWN_TYPE_ID;
                },
            }
        },
        // ignore here, since we checked this in an earlier pass
        .fun, .@"struct", .@"enum", .expern_type => node_type = VOID_TYPE_ID,
        .statement => |stmt| {
            _ = try self.typecheckNode(stmt, local_inferences);
            node_type = VOID_TYPE_ID;
        },
        .new => |_new| {
            const allocation_type = _new.pointer_type;
            const allocation_node_id = _new.allocated;
            node_type = try self.typecheckNew(allocation_type, allocation_node_id, local_inferences);
        },
        else => unreachable,
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
                        const _name = type_field.name;
                        const ty = type_field.ty;

                        if (std.mem.eql(u8, _name, field_name)) {
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
            try self.@"error"("unsupported lvalue, needs variable or field,", lvalue);
            return VOID_TYPE_ID;
        },
    }
    return VOID_TYPE_ID;
}

pub fn typeIsOwned(self: *Typechecker, type_id: TypeId) bool {
    switch (self.compiler.getType(type_id)) {
        .bool, .f64, .i64, .void => return true,
        .@"struct" => |s| {
            const generic_params = s.generic_params;
            const fields = s.fields;

            for (generic_params.items) |generic_param| {
                if (!self.typeIsOwned(generic_param)) {
                    return false;
                }
            }

            for (fields.items) |field| {
                if (field.member_access == Parser.MemberAccess.Private and !self.typeIsOwned(field.ty)) {
                    // TODO add a note
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

                        if (!self.typeIsOwned(var_type_id) and !std.mem.eql(u8, param.name, "self")) {
                            // TODO add a note
                            _ = where_defined;
                            return false;
                        }
                    }
                }

                if (!self.typeIsOwned(return_type)) {
                    if (return_node) |_| {
                        // TODO add a not
                    }
                    return false;
                }
            }

            return true;
        },
        .pointer => |ptr| {
            return ptr.pointer_type == .Owned;
        },
        .@"enum" => unreachable,
        else => return true,
    }
}

pub fn typecheckNew(self: *Typechecker, pointer_type: Parser.PointerType, node_id: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
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
            if (pointer_type == .Owned and !self.typeIsOwned(type_id.?)) {
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
                                const _name = nval.name;
                                const _value = nval.value;

                                const field_name = self.compiler.getSource(_name);

                                for (fields.items) |type_field| {
                                    if (std.mem.eql(u8, field_name, type_field.name)) {
                                        const member_access = type_field.member_access;

                                        if (member_access == Parser.MemberAccess.Private) {
                                            const result = self.findTypeInScopeByName("Self");

                                            if (result) |scope_type_id| {
                                                if (scope_type_id != type_id.?) {
                                                    // FIXME: add a hint to say you need to create your own constructor
                                                    try self.@"error"("'new' used on private member field from outside struct or class", _name);
                                                }
                                            } else {
                                                try self.@"error"("'new' used on private member field from outside struct or class", _name);
                                            }
                                        }

                                        const known_field_type = type_field.ty;

                                        if (self.compiler.isTypeVariable(known_field_type)) {
                                            const value_type = try self.typecheckNode(_value, local_inferences);

                                            try replacements.put(
                                                self.compiler.getUnderlyingTypeId(known_field_type),
                                                self.compiler.getUnderlyingTypeId(value_type),
                                            );

                                            self.compiler.setNodeType(arg, value_type);

                                            // Set up expected type for inference. Note: if we find concrete values
                                            // this inference type will be replaced by the concrete type.
                                            self.compiler.setNodeType(_value, value_type);
                                        } else {
                                            self.compiler.setNodeType(arg, known_field_type);

                                            // Set up expected type for inference. Note: if we find concrete values
                                            // this inference type will be replaced by the concrete type.
                                            self.compiler.setNodeType(_value, known_field_type);

                                            const value_type = try self.typecheckNode(_value, local_inferences);

                                            if (!self.unifyTypes(known_field_type, value_type, local_inferences)) {
                                                const error_msg = try std.fmt.allocPrint(self.alloc, "incompatible type for argument, expected: {s}, found: {s}", .{ try self.compiler.prettyType(value_type), try self.compiler.prettyType(known_field_type) });
                                                try self.@"error"(error_msg, _value);
                                            }
                                        }
                                        continue :arg_label;
                                    }
                                }
                                try self.@"error"("unknown field", _name);
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
                // TODO initiate generic types
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

pub fn typecheckVarOrFunction(self: *Typechecker, node_id: Parser.NodeId, var_or_fun_id: ?VarOrFuncId) !TypeId {
    if (var_or_fun_id) |var_or_fun| {
        switch (var_or_fun) {
            .var_id => |var_id| {
                // TODO check for variable move
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

pub fn typecheck(self: *Typechecker) !Compiler {
    const num_nodes = self.compiler.ast_node.items.len;
    try self.compiler.resizeNodeTypes(num_nodes, UNKNOWN_TYPE_ID);

    const top_level: Parser.NodeId = num_nodes - 1;
    // Top-level local inferences
    var local_inference = std.ArrayList(TypeId).init(self.alloc);

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
    var name = self.compiler.source[self.compiler.span_start[var_name]..self.compiler.span_end[var_name]];

    // Expand the shorthand, and look for 'self' instead
    if (std.mem.eql(u8, name, ".")) {
        name = "self";
    }

    var i: i32 = @intCast(self.scope.items.len - 1);
    while (i >= 0) : (i -= 1) {
        if (self.scope.variables.get(name)) |var_id| {
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
    const frame = &self.scope.getLast();
    @constCast(frame).*.expected_return_type = expected_type;
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
        // TODO
        else => return false,
    }
}

pub fn @"error"(self: *Typechecker, message: []const u8, node_id: Parser.NodeId) !void {
    try self.compiler.errors.append(Errors.SourceError{ .message = message, .node_id = node_id, .severity = Errors.Severity.Error });
}

pub fn enterScope(self: *Typechecker) !void {
    try self.scope.append(Scope.new(self.alloc));
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
