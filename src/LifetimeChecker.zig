const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Errors = @import("Errors.zig");
const Typechecker = @import("Typechecker.zig");

const LifetimeChecker = @This();

alloc: std.mem.Allocator,
compiler: Compiler,
current_blocks: std.ArrayList(Parser.BlockId),
possible_allocation_sites: std.ArrayList(PossibleAllocationSiteStruct),
num_lifetime_inferences: std.AutoHashMap(Parser.BlockId, usize),

pub const PossibleAllocationSiteStruct = struct {
    blocks: std.ArrayList(Parser.BlockId),
    scope_level: usize,
    node_id: Parser.NodeId,
};

pub const AllocationLifetime = union(enum) {
    @"return": Parser.Void,
    param: struct {
        var_id: Typechecker.VarId,
    },
    scope: struct {
        level: usize,
    },
    unknown: Parser.Void,
};

pub fn new(alloc: std.mem.Allocator, compiler: Compiler) !LifetimeChecker {
    return .{
        .alloc = alloc,
        .compiler = compiler,
        .current_blocks = std.ArrayList(Parser.BlockId).init(alloc),
        .possible_allocation_sites = std.ArrayList(PossibleAllocationSiteStruct).init(alloc),
        .num_lifetime_inferences = std.AutoHashMap(Parser.BlockId, usize).init(alloc),
    };
}

pub fn @"error"(self: *LifetimeChecker, message: []const u8, node_id: Parser.NodeId) !void {
    try self.compiler.errors.append(Errors.SourceError{
        .message = message,
        .node_id = node_id,
        .severity = Errors.Severity.Error,
    });
}

pub fn checkBlockLifetime(self: *LifetimeChecker, block_id: Parser.BlockId, scope_level: usize) !void {
    try self.current_blocks.append(block_id);
    const block = self.compiler.blocks.items[block_id];

    // Run lifetime inference to fixpoint or error
    while (true) {
        var num_lifetime_inferences_before: usize = 0;
        if (self.num_lifetime_inferences.get(block_id)) |n| {
            num_lifetime_inferences_before = n;
        }

        const num_errors_before = self.compiler.errors.items.len;

        var idx: i32 = @intCast(block.nodes.items.len);
        idx -= 1;
        while (idx >= 0) : (idx -= 1) {
            const node_id = block.nodes.items[@intCast(idx)];
            try self.checkNodeLifetime(node_id, scope_level);
        }

        var num_lifetime_inferences_after: usize = 0;
        if (self.num_lifetime_inferences.get(block_id)) |n| {
            num_lifetime_inferences_after = n;
        }

        const num_errors_after = self.compiler.errors.items.len;

        if (num_lifetime_inferences_after == num_lifetime_inferences_before or num_errors_before != num_errors_after) {
            break;
        }
    }

    _ = self.current_blocks.popOrNull();
}

pub fn currentBlockMayAllocate(self: *LifetimeChecker, scope_level: usize, node_id: Parser.NodeId) !void {
    switch (self.compiler.getNodeLifetime(node_id)) {
        .scope => |scope| {
            if (scope.level > scope_level) {
                const error_msg = try std.fmt.allocPrint(self.alloc, "current_block_may_allocate saw an impossible level/scope_level: {} vs {}", .{ scope.level, scope_level });
                @panic(error_msg);
            }
        },
        else => {},
    }

    try self.possible_allocation_sites.append(.{
        .blocks = try self.current_blocks.clone(),
        .scope_level = scope_level,
        .node_id = node_id,
    });
}

pub fn incrementLifetimeInferences(self: *LifetimeChecker) !void {
    const current_block_id = self.current_blocks.getLast();
    if (self.num_lifetime_inferences.get(current_block_id)) |entry| {
        try self.num_lifetime_inferences.put(current_block_id, entry + 1);
    } else {
        try self.num_lifetime_inferences.put(current_block_id, 1);
    }
}

pub fn expandLifetimeWithNode(self: *LifetimeChecker, node_id: Parser.NodeId, lifetime_from_node: Parser.NodeId) !void {
    const lifetime = self.compiler.getNodeLifetime(lifetime_from_node);
    try self.expandLifetime(node_id, lifetime_from_node, lifetime);
}

pub fn expandLifetime(self: *LifetimeChecker, node_id: Parser.NodeId, lifetime_from_node: Parser.NodeId, lifetime: AllocationLifetime) !void {
    const target_type_id = self.compiler.getNodeType(node_id);
    const source_type_id = self.compiler.getNodeType(lifetime_from_node);

    if (self.compiler.isCopyableType(target_type_id) or self.compiler.isCopyableType(source_type_id)) {
        // this is a trivially copyable type, so don't calculate the lifetime
        return;
    }

    if (std.mem.eql(u8, @tagName(lifetime), "unknown")) {
        // you can't expand to unknown
        return;
    }

    const current_lifetime = self.compiler.getNodeLifetime(node_id);
    switch (current_lifetime) {
        .unknown => {
            self.compiler.setNodeLifetime(node_id, lifetime);
            try self.incrementLifetimeInferences();
        },
        .param => |p| {
            const var_id = p.var_id;
            switch (lifetime) {
                .param => |pdep| {
                    const incoming_var_id = pdep.var_id;
                    const param_name1 = self.compiler.getVariableName(var_id);
                    const param_name2 = self.compiler.getVariableName(incoming_var_id);

                    if (incoming_var_id != var_id) {
                        const error_msg = try std.fmt.allocPrint(self.alloc, "can't find compatible lifetime between param '{s}' and param '{s}'", .{ param_name1, param_name2 });
                        try self.@"error"(error_msg, node_id);
                        // TODO add a note
                    }
                },
                .scope => {
                    // Params outlive all scopes
                },
                .@"return" => {
                    const param_name1 = self.compiler.getVariableName(var_id);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "can't find compatible lifetime between param '{s}' and return", .{param_name1});
                    try self.@"error"(error_msg, node_id);
                    // TODO add a note
                },
                else => {
                    const param_name1 = self.compiler.getVariableName(var_id);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "can't find compatible lifetime for param '{s}'", .{param_name1});
                    try self.@"error"(error_msg, node_id);
                },
            }
        },
        .@"return" => {
            // TODO add fix to check for raw and custom lifetimes

            switch (lifetime) {
                .param => |p| {
                    const param_name = self.compiler.getVariableName(p.var_id);
                    const error_msg = try std.fmt.allocPrint(self.alloc, "can't find compatible lifetime for param '{s}'", .{param_name});
                    try self.@"error"(error_msg, node_id);
                },
                .scope => {
                    // Return is larger than all scopes, so ignore
                },
                .@"return" => {
                    // Already return
                },
                else => {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "can't find compatible lifetime for return, found '{s}'", .{@tagName(lifetime)});
                    try self.@"error"(error_msg, node_id);
                },
            }
        },
        .scope => |s| {
            const current_level = s.level;
            switch (lifetime) {
                .scope => |sdep| {
                    const new_level = sdep.level;
                    if (new_level < current_level) {
                        self.compiler.setNodeLifetime(node_id, lifetime);
                        try self.incrementLifetimeInferences();
                    }
                },
                .param => |p| {
                    const var_id = p.var_id;
                    const var_type = self.compiler.getVariable(var_id).ty;

                    if (!self.compiler.isAllocatorType(var_type)) {
                        const param_name = self.compiler.getVariableName(var_id);
                        const error_msg = try std.fmt.allocPrint(self.alloc, "param '{s}' is not an allocator, so we can't infer a safe lifetime", .{param_name});
                        try self.@"error"(error_msg, node_id);
                    } else {
                        self.compiler.setNodeLifetime(node_id, lifetime);
                        try self.incrementLifetimeInferences();
                    }
                },
                .@"return" => {
                    self.compiler.setNodeLifetime(node_id, lifetime);
                    try self.incrementLifetimeInferences();
                },
                .unknown => {
                    const error_msg = try std.fmt.allocPrint(self.alloc, "can't find compatible lifetime for return, found '{s}'", .{@tagName(lifetime)});
                    try self.@"error"(error_msg, node_id);
                },
            }
        },
    }
}

pub fn lifetimeName(self: *LifetimeChecker, lifetime: AllocationLifetime) []const u8 {
    const name = @tagName(lifetime);
    if (std.mem.eql(u8, name, "unknown")) {
        @panic("Can't create a name for an unknown lifetime");
    } else if (std.mem.eql(u8, name, "param")) {
        const var_id = self.compiler.getVariableName(lifetime.param.var_id);
        const var_name = std.fmt.allocPrint(self.alloc, "param '{s}'", .{var_id}) catch unreachable;
        return var_name;
    }

    return name;
}

pub fn checkLvalueLifetime(self: *LifetimeChecker, lvalue: Parser.NodeId) anyerror!void {
    switch (self.compiler.getNode(lvalue)) {
        .name => {
            const var_id = self.compiler.var_resolution.get(lvalue);
            if (var_id) |v_id| {
                const definition_node_id = self.compiler.getVariable(v_id).where_defined;
                try self.expandLifetimeWithNode(lvalue, definition_node_id);
            } else {
                try self.@"error"(
                    "internal error: variable unresolved when checking lifetimes",
                    lvalue,
                );
            }
        },
        .index => |index| {
            const target = index.target;
            try self.checkLvalueLifetime(target);
            try self.expandLifetimeWithNode(lvalue, target);
        },
        .member_access => |member_access| {
            const target = member_access.target;
            try self.checkLvalueLifetime(target);
            try self.expandLifetimeWithNode(lvalue, target);
        },
        else => {
            try self.@"error"("unsupported lvalue, needs variable or field", lvalue);
        },
    }
}

pub fn checkNodeLifetime(self: *LifetimeChecker, node_id: Parser.NodeId, scope_level: usize) anyerror!void {
    switch (self.compiler.getNode(node_id)) {
        .block => |block_id| {
            try self.checkBlockLifetime(block_id, scope_level + 1);
        },
        .unsafe_block => |bl| try self.checkNodeLifetime(bl, scope_level),
        .int, .float, .true, .false, .string, .c_string, .c_char, .none => {},
        .let => |let_stmt| {
            // Push lifetime requirement from let into the variable and initializer
            const initializer = let_stmt.initializer;

            try self.expandLifetime(node_id, node_id, .{ .scope = .{ .level = scope_level } });

            try self.expandLifetimeWithNode(initializer, node_id);
            try self.checkNodeLifetime(initializer, scope_level);

            // If the assignment is under-constrained, we'll see if we can get our lifetime
            // from the initializer
            try self.expandLifetimeWithNode(node_id, initializer);
        },
        .name => {
            // We're seeing a use of a variable at this point, so make sure the variable
            // lives long enough to get here

            if (self.compiler.var_resolution.get(node_id)) |var_id| {
                const definition_node_id = self.compiler.getVariable(var_id).where_defined;
                try self.expandLifetimeWithNode(definition_node_id, node_id);
                try self.expandLifetimeWithNode(node_id, definition_node_id);
            } else if (self.compiler.fun_resolution.contains(node_id)) {
                // TODO
            } else {
                try self.@"error"("unresolved variable found during lifetime checking", node_id);
            }
        },
        .member_access => |member_access| {
            // Check the type of the access. If it isn't something that can
            // affect lifetimes, we don't need to push the lifetime
            // requirement deeper
            const target = member_access.target;
            const field_type = self.compiler.getNodeType(node_id);
            if (!self.compiler.isCopyableType(field_type)) {
                try self.expandLifetimeWithNode(target, node_id);
                try self.expandLifetimeWithNode(node_id, target);
            }
            try self.checkNodeLifetime(target, scope_level);
        },
        .binary_op => |bin_op| {
            const lhs = bin_op.left;
            const op = bin_op.op;
            const rhs = bin_op.right;

            if (std.mem.eql(u8, @tagName(self.compiler.getNode(op)), "assignment")) {
                try self.checkLvalueLifetime(lhs);

                if (std.mem.eql(u8, @tagName(self.compiler.getNodeLifetime(lhs)), "unknown")) {
                    try self.expandLifetime(lhs, lhs, .{ .scope = .{ .level = scope_level } });
                }

                try self.checkNodeLifetime(rhs, scope_level);

                const num_errors_before = self.compiler.errors.items.len;
                try self.expandLifetimeWithNode(rhs, lhs);
                const num_errors_after = self.compiler.errors.items.len;

                // a tiny bit hackish, but we don't need to check
                // both directions if we've already errored
                if (num_errors_before == num_errors_after) {
                    try self.expandLifetimeWithNode(lhs, rhs);
                }

                // Make sure any new lifetimes get back to the variable declaration
                try self.checkNodeLifetime(rhs, scope_level);
            } else {
                try self.expandLifetimeWithNode(lhs, node_id);
                try self.expandLifetimeWithNode(rhs, node_id);

                try self.checkNodeLifetime(lhs, scope_level);
                try self.checkNodeLifetime(rhs, scope_level);
            }
        },
        .@"if" => |if_expr| {
            const condition = if_expr.condition;
            const then_block = if_expr.then_block;
            const else_expression = if_expr.else_expression;

            try self.expandLifetimeWithNode(condition, node_id);
            try self.expandLifetimeWithNode(then_block, node_id);

            try self.checkNodeLifetime(condition, scope_level);
            try self.checkNodeLifetime(then_block, scope_level);

            if (else_expression) |else_expr| {
                try self.expandLifetimeWithNode(else_expr, node_id);
                try self.checkNodeLifetime(else_expr, scope_level);
            }
        },
        .@"while" => |while_expr| {
            const condition = while_expr.condition;
            const block = while_expr.block;

            try self.expandLifetimeWithNode(condition, node_id);
            try self.expandLifetimeWithNode(block, node_id);

            try self.checkNodeLifetime(condition, scope_level);
            try self.checkNodeLifetime(block, scope_level);
        },
        .@"for" => |for_expr| {
            const block = for_expr.block;
            try self.expandLifetimeWithNode(block, node_id);
            try self.checkNodeLifetime(block, scope_level);
        },
        .@"defer" => |defer_expr| {
            const pointer = defer_expr.pointer;

            try self.checkNodeLifetime(pointer, scope_level);

            const expected_lifetime = self.compiler.getNodeLifetime(pointer);
            try self.expandLifetimeWithNode(node_id, pointer);

            switch (expected_lifetime) {
                .@"return", .param, .unknown => {},
                .scope => {
                    // is this right?
                    try self.currentBlockMayAllocate(scope_level, node_id);
                },
            }
        },
        .resize_buffer => |buffer| {
            const pointer = buffer.pointer;

            try self.checkNodeLifetime(pointer, scope_level);

            const expected_lifetime = self.compiler.getNodeLifetime(pointer);
            try self.expandLifetimeWithNode(node_id, pointer);

            switch (expected_lifetime) {
                .@"return", .param, .unknown => {},
                .scope => {
                    // is this right?
                    try self.currentBlockMayAllocate(scope_level, node_id);
                },
            }
        },
        .call => |call| {
            const head = call.head;
            const args = call.args;

            // If the call is not constrained, use the local scope level
            if (std.mem.eql(u8, @tagName(self.compiler.getNodeLifetime(node_id)), "unknown")) {
                self.compiler.setNodeLifetime(node_id, .{ .scope = .{ .level = scope_level } });
            }

            // note: fun_id 0 is currently the built-in print
            if (self.compiler.call_resolution.get(head)) |call_target| {
                switch (call_target) {
                    .function => |fun_id| {
                        if (fun_id != 0) {
                            // TODO double check this will be enough
                            try self.checkNodeLifetime(head, scope_level);

                            const params = self.compiler.functions.items[fun_id].params;
                            if (self.compiler.functions.items[fun_id].body == null) {
                                // External calls handle their own lifetimes?
                                return;
                            }

                            for (params.items, 0..) |param, idx| {
                                if (idx >= args.items.len) break;
                                const arg = args.items[idx];
                                const param_node_id = self.compiler.getVariable(param.var_id).where_defined;

                                const expected_lifetime = self.compiler.getNodeLifetime(param_node_id);
                                switch (expected_lifetime) {
                                    .@"return" => {
                                        // This is actually the lifetime of the call itself, which is where
                                        // we are tracking the lifetime of the return value of the call
                                        try self.expandLifetimeWithNode(arg, node_id);
                                        try self.checkNodeLifetime(arg, scope_level);
                                    },
                                    .param => |p| {
                                        // figure out which arg corresponds to this var_id
                                        const var_id = p.var_id;
                                        for (params.items, 0..) |param2, _idx| {
                                            if (_idx >= args.items.len) break;
                                            const arg2 = args.items[_idx];
                                            if (param2.var_id == var_id and arg != arg2) {
                                                try self.expandLifetimeWithNode(arg, arg2);
                                                break;
                                            }
                                        }

                                        // self.expand_lifetime(arg, param_node_id, expected_lifetime);
                                        try self.checkNodeLifetime(arg, scope_level);
                                    },
                                    else => {
                                        try self.checkNodeLifetime(arg, scope_level);
                                    },
                                }
                            }

                            try self.currentBlockMayAllocate(scope_level, node_id);
                        } else if (fun_id == 0) {
                            try self.checkNodeLifetime(head, scope_level);
                            for (args.items) |arg| {
                                try self.checkNodeLifetime(arg, scope_level);
                            }
                        }
                    },
                    else => {
                        try self.checkNodeLifetime(head, scope_level);
                        for (args.items) |arg| {
                            try self.checkNodeLifetime(arg, scope_level);
                        }

                        try self.currentBlockMayAllocate(scope_level, node_id);
                    },
                }
            }
        },
        .new => |new_expr| {
            const required_lifetime = new_expr.required_lifetime;
            const allocation_node_id = new_expr.allocated;

            if (std.mem.eql(u8, @tagName(self.compiler.getNodeLifetime(node_id)), "unknown")) {
                // If we don't have enough constraints, then allocate at the current local scope level
                try self.expandLifetime(allocation_node_id, node_id, .{ .scope = .{ .level = scope_level } });
                try self.expandLifetime(node_id, node_id, .{ .scope = .{ .level = scope_level } });
            } else {
                try self.expandLifetimeWithNode(allocation_node_id, node_id);
            }

            switch (self.compiler.getNode(allocation_node_id)) {
                .call => |call| {
                    const args = call.args;
                    for (args.items) |arg| {
                        try self.checkNodeLifetime(arg, scope_level);

                        try self.expandLifetimeWithNode(arg, node_id);

                        try self.expandLifetimeWithNode(allocation_node_id, arg);
                        try self.expandLifetimeWithNode(node_id, arg);
                    }
                },
                else => {
                    try self.@"error"("expected call as part of allocation", allocation_node_id);
                },
            }

            // Before we leave, make sure we haven't expanded the lifetime beyond what is allowed
            switch (required_lifetime) {
                .Local => {
                    if (!std.mem.eql(u8, @tagName(self.compiler.getNodeLifetime(node_id)), "scope")) {
                        // We're not one of the local scopes, we're escaping but this is required to be a local allocation
                        const error_msg = try std.fmt.allocPrint(self.alloc, "allocation is not local, lifetime inferred to be {s}", .{self.lifetimeName(self.compiler.getNodeLifetime(node_id))});
                        try self.@"error"(error_msg, node_id);
                    }
                },
                else => {
                    // No requirement, so go ahead and ignore
                },
            }

            try self.currentBlockMayAllocate(scope_level, node_id);
        },
        .raw_buffer => |items| {
            try self.expandLifetime(node_id, node_id, .{ .scope = .{ .level = scope_level } });

            for (items.items) |item| {
                try self.expandLifetimeWithNode(item, node_id);
                try self.checkNodeLifetime(item, scope_level);
            }

            try self.currentBlockMayAllocate(scope_level, node_id);
        },
        .index => |index| {
            const target = index.target;
            try self.expandLifetimeWithNode(target, node_id);
            try self.checkNodeLifetime(target, scope_level);
        },
        .@"return" => |return_expr| {
            const current_blocks1 = try self.current_blocks.clone();
            try self.compiler.exiting_blocks.put(node_id, current_blocks1);
            if (return_expr) |ret_expr| {
                try self.expandLifetime(ret_expr, ret_expr, .{ .@"return" = Parser.Void.void });
                try self.checkNodeLifetime(ret_expr, scope_level);
            }
        },
        .named_value => |named_value| {
            const value = named_value.value;
            try self.expandLifetimeWithNode(value, node_id);
            try self.checkNodeLifetime(value, scope_level);
            try self.expandLifetimeWithNode(node_id, value);
        },
        .namespaced_lookup => |namespaced| {
            const item = namespaced.item;
            if (std.mem.eql(u8, @tagName(self.compiler.getNodeLifetime(node_id)), "unknown")) {
                self.compiler.setNodeLifetime(node_id, .{ .scope = .{ .level = scope_level } });
            }

            switch (self.compiler.getNode(item)) {
                .name => try self.expandLifetimeWithNode(item, node_id),
                .call => |call| {
                    const args = call.args;
                    const node_type_id = self.compiler.getNodeType(node_id);
                    const node_type = self.compiler.getType(self.compiler.getUnderlyingTypeId(node_type_id));

                    switch (node_type) {
                        .@"enum" => {
                            try self.expandLifetimeWithNode(item, node_id);
                            for (args.items) |arg| {
                                try self.expandLifetimeWithNode(arg, item);
                                try self.checkNodeLifetime(arg, scope_level);
                            }
                        },
                        else => {
                            try self.expandLifetimeWithNode(item, node_id);
                            try self.currentBlockMayAllocate(scope_level, node_id);
                        },
                    }
                },
                else => {},
            }
        },
        .match => |match| {
            const target = match.target;
            const match_arms = match.match_arms;

            try self.expandLifetimeWithNode(target, node_id);
            for (match_arms.items) |match0| {
                try self.checkNodeLifetime(match0[1], scope_level);
            }
        },
        .fun, .@"struct", .@"enum", .extern_type => {},
        .@"break" => {
            var list = std.ArrayList(Parser.BlockId).init(self.alloc);
            try list.append(self.current_blocks.getLast());
            try self.compiler.exiting_blocks.put(node_id, list);
        },
        .statement => |stmt| {
            try self.checkNodeLifetime(stmt, scope_level);
        },
        .type => {
            // ignore types (eg "foo as bar")
        },
        .type_coercion => {
            // ignore
        },
        else => {
            @panic("unsupported node");
        },
    }
}

pub fn checkLifetimes(self: *LifetimeChecker) !Compiler {
    const num_nodes = self.compiler.numAstNodes();
    try self.compiler.resizeNodeLifetimes(num_nodes);

    // Set up param lifetimes, skipping over our built-in print
    for (self.compiler.functions.items, 0..) |_, fun_id| {
        if (fun_id == 0) continue;
        const fun = self.compiler.functions.items[fun_id];
        param_label: for (fun.params.items) |param| {
            const param_node_id = self.compiler.getVariable(param.var_id).where_defined;

            for (fun.lifetime_annotations.items) |lifetime_annotation| {
                switch (lifetime_annotation) {
                    .equality => |equality| {
                        if (std.mem.eql(u8, @tagName(equality.left), "return") and std.mem.eql(u8, @tagName(equality.right), "variable")) {
                            if (equality.right.variable == param.var_id) {
                                self.compiler.setNodeLifetime(param_node_id, .{ .@"return" = Parser.Void.void });
                                continue :param_label;
                            }
                        } else if (std.mem.eql(u8, @tagName(equality.left), "variable") and std.mem.eql(u8, @tagName(equality.right), "return")) {
                            if (equality.left.variable == param.var_id) {
                                self.compiler.setNodeLifetime(param_node_id, .{ .@"return" = Parser.Void.void });
                                continue :param_label;
                            }
                        } else if (std.mem.eql(u8, @tagName(equality.left), "variable") and std.mem.eql(u8, @tagName(equality.right), "variable")) {
                            if (equality.left.variable == param.var_id) {
                                self.compiler.setNodeLifetime(param_node_id, .{ .param = .{ .var_id = equality.right.variable } });
                                continue :param_label;
                            }
                        }
                    },
                }
            }

            self.compiler.setNodeLifetime(param_node_id, .{
                .param = .{ .var_id = param.var_id },
            });
        }
    }

    // Check function bodies, skipping over our built-in print
    for (self.compiler.functions.items, 0..) |_, fun_id| {
        if (fun_id == 0) continue;
        if (self.compiler.functions.items[fun_id].body) |body| {
            try self.checkNodeLifetime(body, 0);
        }
    }

    // Before we leave, go through our possible allocation sites and see
    // which local scopes allocate for themselves. If they do, mark their
    // blocks so we can properly deallocate these resources
    for (self.possible_allocation_sites.items) |site| {
        switch (self.compiler.getNodeLifetime(site.node_id)) {
            .scope => |scope| {
                if (site.scope_level >= scope.level) {
                    const idx: i32 = @intCast(site.blocks.items.len - 1 - (site.scope_level - scope.level));
                    if (idx >= 0 and idx < site.blocks.items.len) {
                        const block_id = site.blocks.items[@intCast(idx)];
                        self.compiler.blocks.items[block_id].may_locally_allocate = scope.level;
                    }
                }
            },
            else => {},
        }
    }

    return self.compiler;
}
