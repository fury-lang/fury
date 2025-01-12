const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");

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
    name: []const u8,
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
