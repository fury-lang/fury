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
    member_access: []const u8,
    name: []const u8,
    ty: TypeId,
    where_defined: Parser.NodeId,
};

// pub const Void = usize;
pub const Void = enum {
    Void,
};

pub const Type = union(enum) {
    unknown: Void,
    void: Void,
    i64: Void,
    f64: Void,
    bool: Void,
    range: TypeId,
    raw_buffer: TypeId,
    fun: struct { params: std.ArrayList(Param), ret: TypeId },

    c_int: Void,
    c_size_t: Void,
    c_void_ptr: Void,
    c_char: Void,
    c_string: Void,
    c_external_type: Void,

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
    name: []const u8,
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

pub const Module = struct { scope: Scope };

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
    try typechecker.compiler.types.append(Type{ .unknown = Void });
    try typechecker.compiler.types.append(Type{ .void = Void });
    try typechecker.compiler.types.append(Type{ .i64 = Void });
    try typechecker.compiler.types.append(Type{ .f64 = Void });
    try typechecker.compiler.types.append(Type{ .bool = Void });
    try typechecker.compiler.types.append(Type{ .range = I64_TYPE_ID });
    try typechecker.compiler.types.append(Type{ .c_int = Void });
    try typechecker.compiler.types.append(Type{ .c_size_t = Void });
    try typechecker.compiler.types.append(Type{ .c_void_ptr = Void });
    try typechecker.compiler.types.append(Type{ .c_char = Void });
    try typechecker.compiler.types.append(Type{ .c_string = Void });

    var scope = std.ArrayList(Scope).init(alloc);
    try scope.append(Scope.new(alloc));

    const last = scope.items.len - 1;
    try scope.items[last].functions.put("println", 0);

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

pub fn unifyTypes(self: *Typechecker, lhs: TypeId, rhs: TypeId, local_inferences: *std.ArrayList(TypeId)) !bool {
    _ = local_inferences;
    const lhs_ty = self.compiler.getType(lhs);
    const rhs_ty = self.compiler.getType(rhs);

    if (lhs_ty == @TypeOf(Type.fun_local_type_val)) {
        unreachable;
    } else if (rhs_ty == @TypeOf(Type.fun_local_type_val)) {
        unreachable;
    } else if (lhs_ty == @TypeOf(Type.pointer) and rhs_ty == @TypeOf(Type.pointer)) {
        unreachable;
    } else if (lhs_ty == @TypeOf(Type.raw_buffer) and rhs_ty == @TypeOf(Type.raw_buffer)) {
        unreachable;
    } else if (lhs_ty == @TypeOf(Type.fun) and rhs_ty == @TypeOf(Type.fun)) {
        unreachable;
    } else if (lhs_ty == @TypeOf(Type.c_int) and rhs_ty == @TypeOf(Type.i64)) {
        return true;
    } else if (lhs_ty == @TypeOf(Type.i64) and rhs_ty == @TypeOf(Type.c_int)) {
        return true;
    } else if (lhs_ty == @TypeOf(Type.i64) and rhs_ty == @TypeOf(Type.c_size_t)) {
        return true;
    } else if (lhs_ty == @TypeOf(Type.c_size_t) and rhs_ty == @TypeOf(Type.i64)) {
        return true;
    } else {
        return lhs_ty == rhs_ty;
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
                        try self.compiler.findOrCreateType(Type{ .pointer = .{
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

            try self.compiler.findOrCreateType(Type.fun{
                .params = typed_params,
                .ret = typed_ret,
            });
        },
        .raw_buffer => unreachable,
        else => {
            const error_msg = try std.fmt.allocPrint(self.alloc, "expected type name {s}", .{self.compiler.getNode(node_id)});
            try self.@"error"(error_msg, node_id);
            return VOID_TYPE_ID;
        },
    }
}

pub fn typecheckCallWithNodeId(self: *Typechecker, name: Parser.NodeId, node_id: Parser.NodeId, args: *std.ArrayList(Parser.NodeId), local_inferences: *std.ArrayList(TypeId)) !TypeId {
    var type_id = self.compiler.getNodeType(node_id);
    type_id = self.compiler.resolveType(type_id, local_inferences);

    const call_type = self.compiler.getType(type_id);
    switch (call_type) {
        .fun => |f| {
            const params = f.params;
            const ret = f.ret;

            try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .node_id = node_id });

            var type_var_replacements = std.AutoHashMap(TypeId, TypeId);

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

    var _args = undefined;
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
            try self.typecheckNode(arg, local_inferences);
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

    var type_var_replacements = std.AutoHashMap(TypeId, TypeId);
    try self.typecheckCallHelper(_args, params, method_target, local_inferences, &type_var_replacements);

    const _fun_id = fun_id;
    if (!type_var_replacements.Size == 0) {
        // TODO generic fun
    }

    // TODO do we want to wait until all params are checked
    // before we mark this as resolved?
    try self.compiler.call_resolution.put(name, Compiler.CallTarget{ .function = _fun_id });

    if (method_target) |_| {
        try self.exitScope();
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

pub fn typecheckCallHelper(self: *Typechecker, args: *std.ArrayList(Parser.NodeId), params: std.ArrayList(Param), method_target: ?Parser.NodeId, local_inferences: *std.ArrayList(TypeId), type_var_replacements: *std.AutoArrayHashMap(TypeId, TypeId)) !TypeId {
    for (args.items, 0..) |arg, idx| {
        const arg_node = self.compiler.getNode(arg);
        switch (arg_node) {
            .named_value => unreachable,
            _ => {
                var arg_type: TypeId = undefined;
                if (idx == 0) {
                    if (method_target) |_| {
                        arg_type = try self.compiler.getNodeType(arg);
                    }
                } else {
                    arg_type = try self.typecheckNode(arg, local_inferences);
                }

                // TODO check for variables moves
                const param = params.items[idx];
                const variable = self.compiler.getVariable(param.var_id);

                const variable_ty = variable.ty;
                const variable_node_id = variable.name;

                if (self.compiler.isTypeVariable(variable_ty)) {
                    if (type_var_replacements.get(variable_ty)) |replacement| {
                        if (self.unifyTypes(replacement, arg_type, local_inferences)) {} else {
                            const expected_type = self.compiler.resolveType(replacement, local_inferences);
                            const fount_type = self.compiler.resolveType(arg_type, local_inferences);
                            const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ expected_type, fount_type });
                            try self.@"error"(error_msg, arg);

                            // TODO add a note about where params are defined
                        }
                    } else {
                        try type_var_replacements.put(variable_ty, arg_type);
                    }
                } else if (self.unifyTypes(variable_ty, arg_type, local_inferences)) {}
                // TODO check for subtype
                else {
                    const expected_type = self.compiler.resolveType(variable_ty, local_inferences);
                    const found_type = self.compiler.resolveType(arg_type, local_inferences);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "type mismatch for arg. expected {s}, found {s}", .{ expected_type, found_type });
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
        .member_access => unreachable,
        _ => {
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
    _ = &fun_type_params;

    const fun_name = self.compiler.source[self.compiler.span_start.items[name]..self.compiler.span_end.items[name]];

    try self.enterScope();

    // TODO generic type params

    const _params = self.compiler.getNode(params);
    switch (_params) {
        .params => {
            for (_params.items) |unchecked_param| {
                const _param = self.compiler.getNode(unchecked_param);
                switch (_param) {
                    .param => |p| {
                        const _name = p.name;
                        const _param_name = self.compiler.getSource(_name);
                        const ty = p.ty;
                        const is_mutable = p.is_mutable;

                        const tt = try self.typecheckTypename(ty);

                        const var_id = try self.defineVariable(_name, tt, is_mutable, name);
                        // self.compiler.setNodeType(_name, ty);
                        try fun_params.append(Param.new(_param_name, var_id));
                    },
                    else => try self.@"error"("expected function parameter", unchecked_param),
                }
            }
        },
        else => try self.@"error"("expected function parameters", _params),
    }

    try self.exitScope();

    self.compiler.functions.append(Function{
        .name = name,
        .params = fun_params,
        .lifetime_annotations = lifetime_annotations.*,
        .type_params = fun_type_params,
        .inference_vars = std.ArrayList(TypeId).init(self.alloc),
        .return_type = return_ty,
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
    var local_inference = try std.ArrayList(TypeId).init(self.alloc);

    // Typecheck until we hit a fix point for our inferences
    while (true) {
        const before = local_inference.items;

        try self.typecheckNode(fun.body.?, &local_inference);

        if (!self.compiler.errors.items.len == 0 or isListEqual(before, local_inference.items)) {
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

    try self.exitScope();

    const infer = &self.compiler.functions.items[fun_id];
    infer.*.inference_vars = local_inference;
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
                try funs.append(try self.typecheckFunPredecl(f.name, f.type_params, f.params, &f.lifetime_annotations, f.return_ty, f.initial_node_id, f.block, f.is_extern));
            },
            else => unreachable,
        }
    }

    for (funs.items) |f| {
        try self.typecheckFun(f);
    }

    for (block.nodes.items) |id| {
        try self.typecheckNode(id, local_inferences);
    }

    self.compiler.setNodeType(node_id, VOID_TYPE_ID);

    return self.scope.pop();
}

pub fn typecheckNode(self: *Typechecker, node_id: Parser.NodeId, local_inferences: *std.ArrayList(TypeId)) !TypeId {
    var id: TypeId = undefined;
    switch (self.compiler.getNode(node_id)) {
        .block => |block_id| {
            _ = try self.typecheckBlock(node_id, block_id, local_inferences);
            id = VOID_TYPE_ID;
        },
        .int => id = I64_TYPE_ID,
        .float => id = F64_TYPE_ID,
        .true, .false => id = BOOL_TYPE_ID,
        .none => {},
        .string => @panic("strings not yet supported"),
        .c_char => id = C_CHAR_TYPE_ID,
        .c_string => id = C_STRING_TYPE_ID,
        .name => {
            // This looks like a variable, but may also be the name of a function
        },
        .call => {},
        // ignore here, since we checked this in an earlier pass
        .fun, .@"struct", .@"enum", .expern_type => id = VOID_TYPE_ID,
        else => unreachable,
    }

    return id;
}

pub fn typecheck(self: *Typechecker) !Compiler {
    const num_nodes = self.compiler.ast_node.items.len;
    try self.compiler.resizeNodeTypes(num_nodes, UNKNOWN_TYPE_ID);

    const top_level: Parser.NodeId = num_nodes - 1;
    // Top-level local inferences
    var local_inference = try std.ArrayList(TypeId).init(self.alloc);

    // Typecheck until we hit a fix point for our inferences
    while (true) {
        const before = local_inference.items;

        try self.typecheckNode(top_level, &local_inference);

        if (!self.compiler.errors.items.len == 0 or isListEqual(before, local_inference.items)) {
            break;
        }
    }
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
        .name = variable_name,
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

    var i: usize = self.scope.items.len - 1;
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[i].findName(name_str)) |v| {
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

    var i: usize = self.scope.items.len - 1;
    while (i >= 0) : (i -= 1) {
        if (self.scope.variables.get(name)) |var_id| {
            return var_id;
        }
    }

    return null;
}

pub fn findTypeInScope(self: *Typechecker, type_name: Parser.NodeId) ?TypeId {
    const name = self.compiler.getSource(type_name);

    var i: usize = self.scope.items.len - 1;
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[i].findType(name)) |type_id| {
            return type_id;
        }
    }

    return null;
}

pub fn findTypeInScopeByName(self: *Typechecker, name: []const u8) ?TypeId {
    var i: usize = self.scope.items.len - 1;
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[i].types.get(name)) |type_id| {
            return type_id;
        }
    }

    return null;
}

pub fn findExpectedReturnType(self: *Typechecker) ?TypeId {
    var i: usize = self.scope.items.len - 1;
    while (i >= 0) : (i -= 1) {
        if (self.scope.items[i].expected_return_type) |ret_type| {
            return ret_type;
        }
    }

    return null;
}

pub fn setExpectedReturnType(self: *Typechecker, expected_type: TypeId) !void {
    const frame = &self.scope.getLast();
    frame.*.expected_return_type = expected_type;
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
    self.scope.pop();
}

pub fn isListEqual(a: []TypeId, b: []TypeId) bool {
    if (a.len != b.len) return false;

    for (a, 0..) |*a_item, i| {
        if (a_item != b[i]) return false;
    }

    return true;
}
