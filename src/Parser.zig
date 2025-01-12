const std = @import("std");
const Compiler = @import("Compiler.zig");
const Errors = @import("Errors.zig");

// How parser works?

const Parser = @This();

alloc: std.mem.Allocator,
compiler: Compiler,
current_file: FileCursor,
content_length: usize,

const FileCursor = struct {
    span_offset: usize,
    /// index into `Compiler.file_offsets` list
    file_index: usize,
};

pub const PointerType = enum {
    Shared,
    Owned,
    Unknown,
};

pub const MemberAccess = enum {
    Public,
    Private,
};

pub const RequiredLifetime = enum {
    Local,
    Unknown,
};

pub const ASSIGNMENT_PRECEDENCE: usize = 10;

const Void = enum {
    void,
};

pub const AstNode = union(enum) {
    int: Void,
    float: Void,
    string: Void,
    c_string: Void,
    c_char: Void,
    name: Void,
    type: struct {
        name: NodeId,
        params: ?NodeId,
        optional: bool,
        pointer_type: PointerType,
    },
    fun_type: struct {
        params: std.ArrayList(NodeId),
        ret: NodeId,
    },
    raw_buffer_type: struct {
        inner: NodeId,
    },
    type_coercion: struct {
        /// the node of the value being coerced
        source_node: NodeId,
        /// the nodeid of the definition of the type that the value should be coerced into
        target_type: NodeId,
    },

    // Booleans
    true: Void,
    false: Void,

    // Special lifetimes
    return_lifetime: Void,

    // Empty optional values
    none: Void,

    // Operators
    equals: Void,
    not_equals: Void,
    less_than: Void,
    greater_than: Void,
    less_than_or_equal: Void,
    greater_than_or_equal: Void,
    plus: Void,
    minus: Void,
    append: Void,
    multiply: Void,
    divide: Void,
    @"and": Void,
    @"or": Void,
    pow: Void,

    // Bitwise operators
    bitwise_and: Void,
    bitwise_or: Void,
    shift_left: Void,
    shift_right: Void,

    // Special operator
    as: Void,

    // Assignment
    assignment: Void,
    add_assignment: Void,
    subtract_assignment: Void,
    multiply_assignment: Void,
    divide_assignment: Void,

    // Statements
    let: struct {
        variable_name: NodeId,
        ty: ?NodeId,
        initializer: NodeId,
        is_mutable: bool,
    },
    @"while": struct {
        condition: NodeId,
        block: NodeId,
    },
    @"for": struct {
        variable: NodeId,
        range: NodeId,
        block: NodeId,
    },
    @"return": ?NodeId,
    @"break": Void,

    namespaced_lookup: struct {
        namespace: NodeId,
        item: NodeId,
    },
    use: struct {
        path: NodeId,
    },

    // Definitions
    fun: struct {
        name: NodeId,
        type_params: ?NodeId,
        params: NodeId,
        lifetime_annotations: std.ArrayList(NodeId),
        return_ty: ?NodeId,
        initial_node_id: ?NodeId,
        block: ?NodeId,
        is_extern: bool,
    },
    params: std.ArrayList(NodeId),
    param: struct {
        name: NodeId,
        ty: NodeId,
        is_mutable: bool,
    },
    @"struct": struct {
        typename: NodeId,
        fields: std.ArrayList(NodeId),
        methods: std.ArrayList(NodeId),
        explicit_no_alloc: bool,
        base_class: ?NodeId,
    },
    field: struct {
        member_access: MemberAccess,
        name: NodeId,
        typename: NodeId,
    },
    @"enum": struct {
        typename: NodeId,
        cases: std.ArrayList(NodeId),
        methods: std.ArrayList(NodeId),
    },
    enum_case: struct {
        name: NodeId,
        payload: ?std.ArrayList(NodeId),
    },
    expern_type: struct {
        name: NodeId,
    },

    // closure: struct {
    //     params: NodeId,
    //     block: NodeId,
    // },

    // Expressions
    call: struct {
        // TODO replace with proper documentation answering the question
        // this is the method name right? so if it's a method self ends up in args?
        head: NodeId,
        args: std.ArrayList(NodeId),
    },
    named_value: struct {
        name: NodeId,
        value: NodeId,
    },
    binary_op: struct {
        left: NodeId,
        op: NodeId,
        right: NodeId,
    },
    range: struct {
        lhs: NodeId,
        rhs: NodeId,
    },
    member_access: struct {
        target: NodeId,
        field: NodeId,
    },
    index: struct {
        target: NodeId,
        index: NodeId,
    },
    block: BlockId,
    @"if": struct {
        condition: NodeId,
        then_block: NodeId,
        else_expression: ?NodeId,
    },
    match: struct {
        target: NodeId,
        match_arms: std.ArrayList([2]NodeId),
    },
    new: struct {
        pointer_type: PointerType,
        required_lifetime: RequiredLifetime,
        name: NodeId,
    },
    @"defer": struct {
        pointer: NodeId,
        callback: NodeId,
    },
    resize_buffer: struct {
        pointer: NodeId,
        new_size: NodeId,
    },
    raw_buffer: std.ArrayList(NodeId),
    unsage_block: NodeId,
    statement: NodeId,
    garbage: Void,

    pub fn precedence(self: AstNode) usize {
        return switch (self) {
            .as => 200,
            .pow => 100,
            .multiply, .divide => 95,
            //.modulo => 95,
            .plus, .minus => 90,
            .shift_left, .shift_right => 88,
            .bitwise_and => 85,
            .bitwise_or => 83,
            .less_than, .less_than_or_equal, .greater_than, .greater_than_or_equal, .equals, .not_equals => 80,
            .@"and" => 50,
            .@"or" => 40,
            .assignment, .add_assignment, .subtract_assignment, .multiply_assignment, .divide_assignment => ASSIGNMENT_PRECEDENCE,
            else => 0,
        };
    }
};

pub const NodeId = usize;

pub const BlockId = usize;

pub const TokenType = enum {
    Int,
    Float,
    Comma,
    CString,
    CChar,
    String,
    True,
    False,
    Dot,
    DotDot,
    Name,
    Pipe,
    PipePipe,
    Colon,
    ColonColon,
    Semicolon,
    Plus,
    PlusPlus,
    PlusEquals,
    Minus,
    Dash,
    Multiply,
    Divide,
    And,
    Or,
    Pow,
    DashEquals,
    Exclamation,
    Asterisk,
    AsteriskAsterisk,
    AsteriskEquals,
    ForwardSlash,
    ForwardSlashForwardSlash,
    ForwardSlashEquals,
    Equals,
    NotEqual,
    EqualsEquals,
    EqualsTilde,
    ExclamationTilde,
    ExclamationEquals,
    LParen,
    LSquare,
    LCurly,
    LessThan,
    LessThanEqual,
    LessThanLessThan,
    RParen,
    RSquare,
    RCurly,
    GreaterThan,
    GreaterThanEqual,
    GreaterThanGreaterThan,
    Ampersand,
    AmpersandAmpersand,
    QuestionMark,
    ThinArrow,
    ThickArrow,
    Newline,
    ReturnLifetime,
};

pub const Token = struct {
    token_type: TokenType,
    span_start: usize,
    span_end: usize,
};

pub const Block = struct {
    nodes: std.ArrayList(NodeId),
    may_locally_allocate: ?usize,

    pub fn new(nodes: std.ArrayList(NodeId)) Block {
        return Block{
            .nodes = nodes,
            .may_locally_allocate = null,
        };
    }
};

fn isSymbol(b: u8) bool {
    return b == '+' or b == '-' or b == '*' or b == '/' or b == '.' or b == ',' or b == '(' or b == ')' or b == '[' or b == ']' or b == '{' or b == '}' or b == '<' or b == '>' or b == ':' or b == ';' or b == '=' or b == '$' or b == '|' or b == '!' or b == '~' or b == '&' or b == '\'' or b == '"' or b == '?';
}

pub fn new(alloc: std.mem.Allocator, compiler: Compiler, span_offset: usize) Parser {
    const content_length = compiler.source.len - span_offset;
    const current_file = FileCursor{
        .span_offset = span_offset,
        .file_index = 0,
    };
    return Parser{
        .alloc = alloc,
        .compiler = compiler,
        .current_file = current_file,
        .content_length = content_length,
    };
}

fn position(self: *Parser) usize {
    if (self.peek()) |token| {
        return token.span_start;
    } else {
        return self.currentFileEnd();
    }
}

fn getSpanEnd(self: *Parser, node_id: NodeId) usize {
    return self.compiler.span_end.items[node_id];
}

pub fn hasTokens(self: *Parser) bool {
    if (self.peek()) |_| {
        return true;
    }
    return false;
}

pub fn isOperator(self: *Parser) bool {
    if (self.peek()) |token| {
        return switch (token.token_type) {
            TokenType.Asterisk,
            TokenType.AsteriskAsterisk,
            TokenType.Dash,
            TokenType.EqualsEquals,
            TokenType.ExclamationEquals,
            TokenType.ForwardSlash,
            TokenType.LessThan,
            TokenType.LessThanEqual,
            TokenType.Plus,
            TokenType.GreaterThan,
            TokenType.GreaterThanEqual,
            TokenType.AmpersandAmpersand,
            TokenType.Ampersand,
            TokenType.PipePipe,
            TokenType.Pipe,
            TokenType.LessThanLessThan,
            TokenType.GreaterThanGreaterThan,
            TokenType.Equals,
            TokenType.PlusEquals,
            TokenType.DashEquals,
            TokenType.AsteriskEquals,
            TokenType.ForwardSlashEquals,
            => true,
            else => {
                if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "as")) {
                    return true;
                } else {
                    return false;
                }
            },
        };
    }
    return false;
}

pub fn isExpectedToken(self: *Parser, expected: TokenType) bool {
    if (self.peek()) |token| {
        if (expected == token.token_type) {
            return true;
        }
    }
    return false;
}

pub fn isKeyword(self: *Parser, keyword: []const u8) bool {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Name) {
            return std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], keyword);
        } else {
            return false;
        }
    }
    return false;
}

pub fn _keyword(self: *Parser, keyword: []const u8) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Name) {
            if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], keyword)) {
                _ = self.next();
            } else {
                _ = try self.@"error"("expected: keyword");
            }
        }
    } else {
        _ = try self.@"error"("expected: keyword");
    }
}

pub fn isExpression(self: *Parser) bool {
    return self.isSimpleExpression() or self.isKeyword("if");
}

pub fn isSimpleExpression(self: *Parser) bool {
    if (self.peek()) |token| {
        switch (token.token_type) {
            TokenType.Int,
            TokenType.Float,
            TokenType.Dash,
            TokenType.String,
            TokenType.CString,
            TokenType.CChar,
            TokenType.LCurly,
            TokenType.LSquare,
            TokenType.LParen,
            TokenType.Dot,
            TokenType.Name,
            => return true,
            else => {
                if (token.token_type == TokenType.Name) {
                    // zig fmt: off
                    if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "true") 
                    or std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "false") 
                    or std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "new") 
                    or std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "local") 
                    or std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "raw") 
                    or std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "unsafe")) 
                    {
                        return true;
                    } else {
                        return false;
                    }
                    // zig fmt: on
                } else {
                    return false;
                }
            },
        }
    }
    return false;
}

pub fn errorOnNode(self: *Parser, message: []const u8, node_id: NodeId) !void {
    try self.compiler.errors.append(
        Errors.SourceError{
            .message = message,
            .node_id = node_id,
            .severity = Errors.Severity.Error,
        },
    );
}

pub fn @"error"(self: *Parser, message: []const u8) !NodeId {
    if (self.next()) |token| {
        const garbage = AstNode{ .garbage = Void.void };
        const node_id = try self.createNode(garbage, token.span_start, token.span_end);
        try self.compiler.errors.append(
            Errors.SourceError{
                .message = message,
                .node_id = node_id,
                .severity = Errors.Severity.Error,
            },
        );

        return node_id;
    } else {
        const garbage = AstNode{ .garbage = Void.void };
        const node_id = try self.createNode(garbage, self.content_length, self.content_length);
        try self.compiler.errors.append(
            Errors.SourceError{
                .message = message,
                .node_id = node_id,
                .severity = Errors.Severity.Error,
            },
        );

        return node_id;
    }
}

pub fn createNode(self: *Parser, ast_node: AstNode, span_start: usize, span_end: usize) !NodeId {
    return try self.compiler.createNode(ast_node, span_start, span_end);
}

pub fn parse(self: *Parser) !Compiler {
    _ = try self.program();
    return self.compiler;
}

pub fn program(self: *Parser) !NodeId {
    return try self.block(false);
}

pub fn block(self: *Parser, expect_curly_braces: bool) !NodeId {
    const span_start = self.position();
    var span_end = self.position();

    var curr_body = std.ArrayList(NodeId).init(self.alloc);
    if (expect_curly_braces) {
        try self.lcurly();
    }

    while (self.position() < self.currentFileEnd()) {
        if (self.isExpectedToken(TokenType.RCurly) and expect_curly_braces) {
            span_end = self.position() + 1;
            try self.rcurly();
            break;
        } else if (self.isExpectedToken(TokenType.Semicolon) or self.isExpectedToken(TokenType.Newline)) {
            _ = self.next();
            continue;
        } else if (self.isKeyword("fun")) {
            try curr_body.append(try self.funDefinition());
        } else if (self.isKeyword("extern")) {
            unreachable;
        } else if (self.isKeyword("struct")) {
            unreachable;
        } else if (self.isKeyword("class")) {
            unreachable;
        } else if (self.isKeyword("enum")) {
            unreachable;
        } else if (self.isKeyword("use")) {
            unreachable;
        } else if (self.isKeyword("let")) {
            try curr_body.append(try self.letStatement());
        } else if (self.isKeyword("mut")) {
            unreachable;
        } else if (self.isKeyword("while")) {
            unreachable;
        } else if (self.isKeyword("for")) {
            unreachable;
        } else if (self.isKeyword("return")) {
            unreachable;
        } else if (self.isKeyword("break")) {
            unreachable;
        } else if (self.isKeyword("defer")) {
            unreachable;
        } else if (self.isKeyword("resize")) {
            unreachable;
        } else if (self.isKeyword("unsafe")) {
            unreachable;
        } else {
            const _span_start = self.position();
            const expr = try self.expressionOrAssignment();
            const _span_end = self.getSpanEnd(expr);

            if (self.isExpectedToken(TokenType.Semicolon)) {
                // This is a statement, not an expression
                _ = self.next();
                try curr_body.append(
                    try self.createNode(
                        .{ .statement = expr },
                        _span_start,
                        _span_end,
                    ),
                );
            } else {
                try curr_body.append(expr);
            }
        }
    }

    try self.compiler.blocks.append(Block.new(curr_body));

    return try self.createNode(
        .{ .block = self.compiler.blocks.items.len - 1 },
        span_start,
        span_end,
    );
}

pub fn funDefinition(self: *Parser) anyerror!NodeId {
    const span_start = self.position();
    try self._keyword("fun");

    const _name = try self.name();

    var type_params: ?NodeId = null;
    if (self.isExpectedToken(TokenType.LessThan)) {
        type_params = try self.typeParams();
    }

    const _params = try self.params();

    var lifetime_annotations = std.ArrayList(NodeId).init(self.alloc);

    if (self.isExpectedToken(TokenType.LSquare)) {
        // we have lifetime constraints/annotations
        try self.lsquare();

        while (true) {
            if (self.isExpectedToken(TokenType.RSquare)) {
                try self.rsquare();
                break;
            } else if (self.isExpectedToken(TokenType.Newline)) {
                _ = self.newLine();
            } else if (self.isExpectedToken(TokenType.Comma)) {
                _ = self.next();
            } else if (self.isExpectedToken(TokenType.Name)) {
                // TODO
                _ = &lifetime_annotations;
            } else {
                _ = try self.@"error"("expected: lifetime annotation");
                break;
            }
        }
    }

    var return_ty: ?NodeId = null;
    if (self.isExpectedToken(TokenType.ThinArrow)) {
        _ = self.next();
        return_ty = try self.typeName();
    }

    const initial_node_id: ?NodeId = self.compiler.numAstNodes();

    var _block: ?NodeId = null;
    var span_end: usize = undefined;
    if (self.isExpectedToken(TokenType.LCurly)) {
        _block = try self.block(true);
        span_end = self.getSpanEnd(_block.?);
    } else {
        span_end = self.position();
    }

    return try self.createNode(
        .{
            .fun = .{
                .name = _name,
                .type_params = type_params,
                .params = _params,
                .lifetime_annotations = lifetime_annotations,
                .return_ty = return_ty,
                .initial_node_id = initial_node_id,
                .block = _block,
                .is_extern = false,
            },
        },
        span_start,
        span_end,
    );
}

pub fn typeParams(self: *Parser) !NodeId {
    const span_start = self.position();
    var span_end: usize = undefined;

    var params_list = std.ArrayList(NodeId).init(self.alloc);
    try self.lessThan();

    while (self.hasTokens()) {
        if (self.isExpectedToken(TokenType.GreaterThan)) {
            break;
        }
        try params_list.append(try self.name());
        if (self.isExpectedToken(TokenType.Comma)) {
            _ = self.next();
        }
    }

    span_end = self.position() + 1;
    try self.greaterThan();

    return try self.createNode(
        .{ .params = params_list },
        span_start,
        span_end,
    );
}

pub fn params(self: *Parser) !NodeId {
    const span_start = self.position();
    var span_end: usize = undefined;

    var param_list = std.ArrayList(NodeId).init(self.alloc);
    try self.lparen();
    param_list = try self.paramList();
    span_end = self.position() + 1;
    try self.rparen();

    return try self.createNode(
        .{ .params = param_list },
        span_start,
        span_end,
    );
}

pub fn paramList(self: *Parser) !std.ArrayList(NodeId) {
    var _params = std.ArrayList(NodeId).init(self.alloc);
    while (self.hasTokens()) {
        if (self.isExpectedToken(TokenType.RParen) or self.isExpectedToken(TokenType.RSquare) or self.isExpectedToken(TokenType.Pipe)) {
            break;
        }

        if (self.isExpectedToken(TokenType.Comma)) {
            _ = self.next();
            continue;
        }

        // Parse param
        const span_start = self.position();

        var is_mutable = false;
        if (self.isKeyword("mut")) {
            is_mutable = true;
            _ = self.next();
        }

        const _name = try self.name();
        if (self.isExpectedToken(TokenType.Colon)) {
            try self.colon();

            const ty = try self.typeName();

            const span_end = self.getSpanEnd(ty);

            try _params.append(
                try self.createNode(
                    .{ .param = .{ .name = _name, .ty = ty, .is_mutable = is_mutable } },
                    span_start,
                    span_end,
                ),
            );
        } else {
            const name_contents = self.compiler.getSource(_name);

            if (std.mem.eql(u8, name_contents, "self")) {
                const span_end = self.getSpanEnd(_name);

                const ty = try self.createNode(
                    .{ .type = .{
                        .name = _name,
                        .params = null,
                        .optional = false,
                        .pointer_type = PointerType.Unknown,
                    } },
                    span_start,
                    span_end,
                );

                try _params.append(
                    try self.createNode(
                        .{ .param = .{ .name = _name, .ty = ty, .is_mutable = is_mutable } },
                        span_start,
                        span_end,
                    ),
                );
            } else {
                try _params.append(try self.@"error"("parameter missing type"));
            }
        }
    }

    return _params;
}

pub fn typeName(self: *Parser) !NodeId {
    if (self.isKeyword("raw")) {
        // TODO
        unreachable;
    }

    var pointer_type = PointerType.Unknown;
    if (self.isKeyword("owned")) {
        _ = self.next();
        pointer_type = PointerType.Owned;
    }

    if (self.peek()) |token| {
        if (token.token_type == TokenType.Name) {
            if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "fun")) {
                // TODO
                unreachable;
            }

            const _name = try self.name();
            var _params: ?NodeId = null;
            if (self.isExpectedToken(TokenType.LessThan)) {
                // We have generics
                _params = try self.typeParams();
            }

            var optional = false;
            if (self.isExpectedToken(TokenType.QuestionMark)) {
                // We have an optional type
                _ = self.next();
                optional = true;
            }

            return try self.createNode(
                .{ .type = .{ .name = _name, .params = _params, .optional = optional, .pointer_type = pointer_type } },
                token.span_start,
                token.span_end,
            );
        } else {
            return try self.@"error"("expected: type name");
        }
    } else {
        return try self.@"error"("expect name");
    }
}

pub fn expressionOrAssignment(self: *Parser) !NodeId {
    return try self.mathExpression(true);
}

pub fn expression(self: *Parser) !NodeId {
    return try self.mathExpression(false);
}

pub fn mathExpression(self: *Parser, allow_assignment: bool) anyerror!NodeId {
    var expr_stack = std.ArrayList(NodeId).init(self.alloc);
    var last_prec: usize = 1000000;
    const span_start = self.position();

    // Check for special forms
    if (self.isKeyword("if")) {
        unreachable;
    } else if (self.isKeyword("new") or self.isKeyword("local")) {
        unreachable;
    } else if (self.isKeyword("match")) {
        unreachable;
    }

    // Otherwise assume a math expression
    var lhs: NodeId = undefined;
    if (self.isSimpleExpression()) {
        lhs = try self.simpleExpression();
    } else {
        return try self.@"error"("incomplete math expression");
    }

    if (self.peek()) |token| {
        if (token.token_type == TokenType.Equals) {
            if (!allow_assignment) {
                _ = try self.@"error"("assignment found in expression");
            }
            const op = try self.operator();
            const rhs = try self.expression();

            const span_end = self.getSpanEnd(rhs);

            return try self.createNode(
                .{ .binary_op = .{ .left = lhs, .op = op, .right = rhs } },
                span_start,
                span_end,
            );
        }
    }

    try expr_stack.append(lhs);

    while (self.hasTokens()) {
        if (self.isOperator()) {
            const op = try self.operator();
            const op_prec = self.operatorPrecedence(op);

            if (op_prec == ASSIGNMENT_PRECEDENCE and !allow_assignment) {
                try self.errorOnNode("assignment found in expression", op);
            }

            var rhs: NodeId = undefined;
            // TODO check for AstNode.As
            if (self.isSimpleExpression()) {
                rhs = try self.simpleExpression();
            } else {
                return try self.@"error"("incomplete math expression");
            }

            while (op_prec <= last_prec and expr_stack.items.len > 1) {
                // TODO hadle error cases here on stack
                const _rhs = expr_stack.pop();
                const _op = expr_stack.pop();

                last_prec = self.operatorPrecedence(_op);

                if (last_prec < op_prec) {
                    try expr_stack.append(_op);
                    try expr_stack.append(_rhs);
                    break;
                }

                const _lhs = expr_stack.pop();
                const _span_start = self.compiler.span_start.items[_lhs];
                const span_end = self.compiler.span_end.items[_rhs];

                try expr_stack.append(
                    try self.createNode(
                        .{ .binary_op = .{ .left = _lhs, .op = _op, .right = _rhs } },
                        _span_start,
                        span_end,
                    ),
                );
            }

            try expr_stack.append(op);
            try expr_stack.append(rhs);

            last_prec = op_prec;
        } else {
            break;
        }
    }

    while (expr_stack.items.len > 1) {
        // TODO handle error cases here on stack
        const _rhs = expr_stack.pop();
        const _op = expr_stack.pop();
        const _lhs = expr_stack.pop();

        const _span_start = self.compiler.span_start.items[lhs];
        const span_end = self.compiler.span_end.items[_rhs];

        try expr_stack.append(
            try self.createNode(
                .{ .binary_op = .{ .left = _lhs, .op = _op, .right = _rhs } },
                _span_start,
                span_end,
            ),
        );
    }

    if (expr_stack.items.len == 0) {
        @panic("internal error: expression stack empty");
    }

    return expr_stack.items[0];
}

pub fn simpleExpression(self: *Parser) !NodeId {
    const span_start = self.position();
    _ = span_start;

    var expr: NodeId = undefined;
    if (self.isExpectedToken(TokenType.LCurly)) {
        expr = try self.block(true);
    } else if (self.isExpectedToken(TokenType.LParen)) {
        try self.lparen();
        const output = try self.expression();
        try self.rparen();
        expr = output;
    } else if (self.isKeyword("raw")) {
        unreachable;
    } else if (self.isKeyword("true") or self.isKeyword("false")) {
        expr = try self.boolean();
    } else if (self.isKeyword("none")) {
        expr = try self.none();
    } else if (self.isKeyword("new") or self.isKeyword("local")) {
        unreachable;
    } else if (self.isExpectedToken(TokenType.String)) {
        expr = try self.string();
    } else if (self.isExpectedToken(TokenType.CString)) {
        expr = try self.cString();
    } else if (self.isExpectedToken(TokenType.CChar)) {
        expr = try self.cChar();
    } else if (self.isExpectedToken(TokenType.Int) or self.isExpectedToken(TokenType.Float) or self.isExpectedToken(TokenType.Minus)) {
        expr = try self.number();
    } else if (self.isExpectedToken(TokenType.Name)) {
        expr = try self.variableOrCall();
    } else if (self.isExpectedToken(TokenType.Dot)) {
        unreachable;
    } else {
        return try self.@"error"("incomplete expression");
    }

    while (true) {
        if (self.isExpectedToken(TokenType.DotDot)) {
            // Range
            unreachable;
        } else if (self.isExpectedToken(TokenType.Dot)) {
            // Member access
            unreachable;
        } else if (self.isExpectedToken(TokenType.LSquare)) {
            // Indexing operation
            unreachable;
        } else if (self.isExpectedToken(TokenType.ColonColon)) {
            unreachable;
        } else {
            return expr;
        }
    }
}

pub fn letStatement(self: *Parser) !NodeId {
    const is_mutable = false;
    const span_start = self.position();

    try self._keyword("let");

    const variable_name = try self.variable();

    var ty: ?NodeId = null;
    if (self.isExpectedToken(TokenType.Colon)) {
        try self.colon();
        ty = try self.typeName();
    }

    try self.equals();

    const initializer = try self.expression();

    const span_end = self.getSpanEnd(initializer);

    return try self.createNode(
        .{ .let = .{ .variable_name = variable_name, .ty = ty, .initializer = initializer, .is_mutable = is_mutable } },
        span_start,
        span_end,
    );
}

pub fn variable(self: *Parser) !NodeId {
    if (self.isExpectedToken(TokenType.Name)) {
        const _name = self.next().?;
        const name_start = _name.span_start;
        const name_end = _name.span_end;
        return try self.createNode(AstNode{ .name = Void.void }, name_start, name_end);
    } else {
        return try self.@"error"("expected: variable");
    }
}

pub fn variableOrCall(self: *Parser) !NodeId {
    if (self.isExpectedToken(TokenType.Name)) {
        const span_start = self.position();

        const _name = self.next().?;
        const name_start = _name.span_start;
        const name_end = _name.span_end;

        if (self.isExpectedToken(TokenType.LParen)) {
            var head = try self.createNode(AstNode{ .name = Void.void }, name_start, name_end);
            if (self.isExpectedToken(TokenType.LParen)) {
                // We're a call
                try self.lparen();
                var args = std.ArrayList(NodeId).init(self.alloc);
                while (true) {
                    if (self.isExpression()) {
                        const value_start = self.position();
                        const val = try self.expression();

                        if (self.isExpectedToken(TokenType.Comma)) {
                            try args.append(val);
                            try self.comma();
                            continue;
                        } else if (self.isExpectedToken(TokenType.RParen)) {
                            try args.append(val);
                            break;
                        } else if (self.isExpectedToken(TokenType.Colon)) {
                            // we have a name value
                            // TODO
                            _ = value_start;
                        } else {
                            try args.append(val);
                            try args.append(try self.@"error"("unexpected value in call arguments"));
                        }
                    } else {
                        break;
                    }
                }

                const span_end = self.position() + 1;
                try self.rparen();
                head = try self.createNode(.{ .call = .{ .head = head, .args = args } }, span_start, span_end);
            }

            return head;
        } else {
            // We're a variable
            return try self.createNode(AstNode{ .name = Void.void }, name_start, name_end);
        }
    } else {
        return try self.@"error"("expected: variable or call");
    }
}

pub fn number(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        switch (token.token_type) {
            TokenType.Int => {
                _ = self.next();
                return try self.createNode(AstNode{ .int = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Float => {
                _ = self.next();
                return try self.createNode(AstNode{ .float = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Minus => {
                _ = self.next();
                const remaining = try self.number();
                const span_end = self.getSpanEnd(remaining);
                const contents = self.compiler.source[token.span_start..token.span_end];

                if (std.mem.containsAtLeast(u8, contents, 1, ".")) {
                    return try self.createNode(AstNode{ .minus = Void.void }, token.span_start, span_end);
                } else {
                    return try self.createNode(AstNode{ .int = Void.void }, token.span_start, span_end);
                }
            },
            else => return try self.@"error"("expected: number"),
        }
    } else {
        return try self.@"error"("expected: number");
    }
}

pub fn boolean(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Name) {
            if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "true")) {
                _ = self.next();
                return try self.createNode(AstNode{ .true = Void.void }, token.span_start, token.span_end);
            } else if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "false")) {
                _ = self.next();
                return try self.createNode(AstNode{ .false = Void.void }, token.span_start, token.span_end);
            } else {
                return try self.@"error"("expected: boolean");
            }
        } else {
            return try self.@"error"("expected: boolean");
        }
    } else {
        return try self.@"error"("expected: boolean");
    }
}

pub fn none(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Name) {
            if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "none")) {
                _ = self.next();
                return try self.createNode(AstNode{ .none = Void.void }, token.span_start, token.span_end);
            } else {
                return try self.@"error"("expected: none");
            }
        } else {
            return try self.@"error"("expected: none");
        }
    } else {
        return try self.@"error"("expected: none");
    }
}

pub fn string(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.String) {
            _ = self.next();
            return try self.createNode(AstNode{ .string = Void.void }, token.span_start, token.span_end);
        } else {
            return try self.@"error"("expected: string");
        }
    } else {
        return try self.@"error"("expected: string");
    }
}

pub fn cString(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.CString) {
            _ = self.next();
            return try self.createNode(AstNode{ .c_string = Void.void }, token.span_start, token.span_end);
        } else {
            return try self.@"error"("expected: C-based string");
        }
    } else {
        return try self.@"error"("expected: C-based string");
    }
}

pub fn cChar(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.CChar) {
            _ = self.next();
            return try self.createNode(AstNode{ .c_char = Void.void }, token.span_start, token.span_end);
        } else {
            return try self.@"error"("expected: C-based char");
        }
    } else {
        return try self.@"error"("expected: C-based char");
    }
}

pub fn operator(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        switch (token.token_type) {
            TokenType.Plus => {
                _ = self.next();
                return try self.createNode(AstNode{ .plus = Void.void }, token.span_start, token.span_end);
            },
            TokenType.PlusPlus => {
                _ = self.next();
                return try self.createNode(AstNode{ .append = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Dash => {
                _ = self.next();
                return try self.createNode(AstNode{ .minus = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Asterisk => {
                _ = self.next();
                return try self.createNode(AstNode{ .multiply = Void.void }, token.span_start, token.span_end);
            },
            TokenType.ForwardSlash => {
                _ = self.next();
                return try self.createNode(AstNode{ .divide = Void.void }, token.span_start, token.span_end);
            },
            TokenType.LessThan => {
                _ = self.next();
                return try self.createNode(AstNode{ .less_than = Void.void }, token.span_start, token.span_end);
            },
            TokenType.LessThanEqual => {
                _ = self.next();
                return try self.createNode(AstNode{ .less_than_or_equal = Void.void }, token.span_start, token.span_end);
            },
            TokenType.GreaterThan => {
                _ = self.next();
                return try self.createNode(AstNode{ .greater_than = Void.void }, token.span_start, token.span_end);
            },
            TokenType.GreaterThanEqual => {
                _ = self.next();
                return try self.createNode(AstNode{ .greater_than_or_equal = Void.void }, token.span_start, token.span_end);
            },
            TokenType.EqualsEquals => {
                _ = self.next();
                return try self.createNode(AstNode{ .equals = Void.void }, token.span_start, token.span_end);
            },
            TokenType.ExclamationEquals => {
                _ = self.next();
                return try self.createNode(AstNode{ .not_equals = Void.void }, token.span_start, token.span_end);
            },
            TokenType.AsteriskAsterisk => {
                _ = self.next();
                return try self.createNode(AstNode{ .pow = Void.void }, token.span_start, token.span_end);
            },
            TokenType.AmpersandAmpersand => {
                _ = self.next();
                return try self.createNode(AstNode{ .@"and" = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Ampersand => {
                _ = self.next();
                return try self.createNode(AstNode{ .bitwise_and = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Pipe => {
                _ = self.next();
                return try self.createNode(AstNode{ .bitwise_or = Void.void }, token.span_start, token.span_end);
            },
            TokenType.PipePipe => {
                _ = self.next();
                return try self.createNode(AstNode{ .@"or" = Void.void }, token.span_start, token.span_end);
            },
            TokenType.LessThanLessThan => {
                _ = self.next();
                return try self.createNode(AstNode{ .shift_left = Void.void }, token.span_start, token.span_end);
            },
            TokenType.GreaterThanGreaterThan => {
                _ = self.next();
                return try self.createNode(AstNode{ .shift_right = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Equals => {
                _ = self.next();
                return try self.createNode(AstNode{ .assignment = Void.void }, token.span_start, token.span_end);
            },
            TokenType.PlusEquals => {
                _ = self.next();
                return try self.createNode(AstNode{ .add_assignment = Void.void }, token.span_start, token.span_end);
            },
            TokenType.DashEquals => {
                _ = self.next();
                return try self.createNode(AstNode{ .subtract_assignment = Void.void }, token.span_start, token.span_end);
            },
            TokenType.AsteriskEquals => {
                _ = self.next();
                return try self.createNode(AstNode{ .multiply_assignment = Void.void }, token.span_start, token.span_end);
            },
            TokenType.ForwardSlashEquals => {
                _ = self.next();
                return try self.createNode(AstNode{ .divide_assignment = Void.void }, token.span_start, token.span_end);
            },
            TokenType.Name => {
                _ = self.next();
                if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end], "as")) {
                    return try self.createNode(AstNode{ .as = Void.void }, token.span_start, token.span_end);
                } else {
                    return try self.@"error"("expected: operator");
                }
            },
            else => return try self.@"error"("expected: operator"),
        }
    } else {
        return try self.@"error"("expected: operator");
    }
}

pub fn operatorPrecedence(self: *Parser, op: NodeId) usize {
    const node = self.compiler.getNode(op);
    return node.precedence();
}

pub fn name(self: *Parser) !NodeId {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Name) {
            _ = self.next();
            return try self.createNode(AstNode{ .name = Void.void }, token.span_start, token.span_end);
        } else {
            return try self.@"error"("expected: name");
        }
    } else {
        return try self.@"error"("expected: name");
    }
}

pub fn lcurly(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.LCurly) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: left curly bracket '{'");
        }
    } else {
        _ = try self.@"error"("expected: left curly bracket '{'");
    }
}

pub fn rcurly(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.RCurly) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: right bracket '}'");
        }
    } else {
        _ = try self.@"error"("expected: right bracket '}'");
    }
}

pub fn lsquare(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.LSquare) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: left square bracket '['");
        }
    } else {
        _ = try self.@"error"("expected: left square bracket '['");
    }
}

pub fn rsquare(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.RSquare) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: right square bracket ']'");
        }
    } else {
        _ = try self.@"error"("expected: right square bracket ']'");
    }
}

pub fn lparen(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.LParen) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: left parenthesis '('");
        }
    } else {
        _ = try self.@"error"("expected: left parenthesis '('");
    }
}

pub fn rparen(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.RParen) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: right parenthesis ')'");
        }
    } else {
        _ = try self.@"error"("expected: right parenthesis ')'");
    }
}

pub fn lessThan(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.LessThan) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: less than '<'");
        }
    } else {
        _ = try self.@"error"("expected: less than '<'");
    }
}

pub fn greaterThan(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.GreaterThan) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: greater than '>'");
        }
    } else {
        _ = try self.@"error"("expected: greater than '>'");
    }
}

pub fn equals(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Equals) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: equals '='");
        }
    } else {
        _ = try self.@"error"("expected: equals '='");
    }
}

pub fn thinArrow(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.ThinArrow) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: thin arrow '->'");
        }
    } else {
        _ = try self.@"error"("expected: thin arrow '->'");
    }
}

pub fn colon(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Colon) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: colon ':'");
        }
    } else {
        _ = try self.@"error"("expected: colon ':'");
    }
}

pub fn comma(self: *Parser) !void {
    if (self.peek()) |token| {
        if (token.token_type == TokenType.Comma) {
            _ = self.next();
        } else {
            _ = try self.@"error"("expected: comma ','");
        }
    } else {
        _ = try self.@"error"("expected: comma ','");
    }
}

pub fn lexQuotedString(self: *Parser) ?Token {
    const span_start = self.current_file.span_offset;
    var span_position = span_start + 1;
    var is_excaped = false;
    while (span_position < self.currentFileEnd()) {
        if (is_excaped) {
            is_excaped = false;
        } else if (self.compiler.source[span_position] == '\\') {
            is_excaped = true;
        } else if (self.compiler.source[span_position] == '"') {
            span_position += 1;
            break;
        }
        span_position += 1;
    }

    self.current_file.span_offset = span_position;

    return Token{
        .token_type = TokenType.String,
        .span_start = span_start,
        .span_end = self.current_file.span_offset,
    };
}

pub fn lexQuotedCString(self: *Parser) ?Token {
    const span_start = self.current_file.span_offset;
    var span_position = span_start + 1;
    var is_excaped = false;
    while (span_position < self.currentFileEnd()) {
        if (is_excaped) {
            is_excaped = false;
        } else if (self.compiler.source[span_position] == '\\') {
            is_excaped = true;
        } else if (self.compiler.source[span_position] == '"') {
            span_position += 1;
            break;
        }
        span_position += 1;
    }

    self.current_file.span_offset = span_position;

    return Token{
        .token_type = TokenType.CString,
        .span_start = span_start,
        .span_end = self.current_file.span_offset,
    };
}

pub fn lexQuotedCChar(self: *Parser) ?Token {
    const span_start = self.current_file.span_offset;
    var span_position = span_start + 1;
    var is_excaped = false;
    while (span_position < self.compiler.source.len) {
        if (is_excaped) {
            is_excaped = false;
        } else if (self.compiler.source[span_position] == '\\') {
            is_excaped = true;
        } else if (self.compiler.source[span_position] == '\'') {
            span_position += 1;
            break;
        }
        span_position += 1;
    }

    self.current_file.span_offset = span_position;

    return Token{
        .token_type = TokenType.CChar,
        .span_start = span_start,
        .span_end = self.current_file.span_offset,
    };
}

pub fn lexNumber(self: *Parser) ?Token {
    const span_start = self.current_file.span_offset;
    var span_position = span_start;
    while (span_position < self.currentFileEnd()) {
        if (!isAsciiDigit(self.compiler.source[span_position])) {
            break;
        }
        span_position += 1;
    }

    // skip hex, octal, binary, float for now

    self.current_file.span_offset = span_position;

    return Token{
        .token_type = TokenType.Int,
        .span_start = span_start,
        .span_end = self.current_file.span_offset,
    };
}

pub fn skipSpace(self: *Parser) void {
    var span_position = self.current_file.span_offset;
    while (span_position < self.currentFileEnd()) {
        const c = self.compiler.source[span_position];
        if (c == ' ' or c == '\t') {
            span_position += 1;
        } else {
            break;
        }
    }
    self.current_file.span_offset = span_position;
}

pub fn newLine(self: *Parser) ?Token {
    var span_position = self.current_file.span_offset;
    while (span_position < self.currentFileEnd()) {
        const c = self.compiler.source[span_position];
        if (c == '\n' or c == '\r') {
            span_position += 1;
        } else {
            break;
        }
    }

    if (self.current_file.span_offset == span_position) {
        return null;
    } else {
        const output = Token{
            .token_type = TokenType.Newline,
            .span_start = self.current_file.span_offset,
            .span_end = span_position,
        };
        self.current_file.span_offset = span_position;
        return output;
    }
}

pub fn skipComment(self: *Parser) void {
    var span_position = self.current_file.span_offset;
    while (span_position < self.currentFileEnd() and self.compiler.source[span_position] != '\n') {
        span_position += 1;
    }
    self.current_file.span_offset = span_position;
}

pub fn lexName(self: *Parser) ?Token {
    const span_start = self.current_file.span_offset;
    var span_position = span_start;
    while (span_position < self.currentFileEnd() and (!isAsciiWhiteSpace(self.compiler.source[span_position]) and !isAsciiPunctuation(self.compiler.source[span_position]) or self.compiler.source[span_position] == '_')) {
        span_position += 1;
    }
    self.current_file.span_offset = span_position;

    return Token{
        .token_type = TokenType.Name,
        .span_start = span_start,
        .span_end = self.current_file.span_offset,
    };
}

pub fn lexSymbol(self: *Parser) ?Token {
    const span_start = self.current_file.span_offset;
    const result = switch (self.compiler.source[span_start]) {
        '(' => Token{
            .token_type = TokenType.LParen,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '[' => Token{
            .token_type = TokenType.LSquare,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '{' => Token{
            .token_type = TokenType.LCurly,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        // skip <=, << for now
        '<' => Token{
            .token_type = TokenType.LessThan,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        ')' => Token{
            .token_type = TokenType.RParen,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        ']' => Token{
            .token_type = TokenType.RSquare,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '}' => Token{
            .token_type = TokenType.RCurly,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        // skip >=, >> for now
        '>' => Token{
            .token_type = TokenType.GreaterThan,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        // skip ++, +=, --, -=, **, *=, //, /=, ==, !=, :: for now
        '+' => Token{
            .token_type = TokenType.Plus,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '-' => Token{
            .token_type = TokenType.Minus,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '*' => Token{
            .token_type = TokenType.Multiply,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '/' => Token{
            .token_type = TokenType.Divide,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '=' => Token{
            .token_type = TokenType.Equals,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        ':' => Token{
            .token_type = TokenType.Colon,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        ';' => Token{
            .token_type = TokenType.Semicolon,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        // skip .. for now
        '.' => Token{
            .token_type = TokenType.Dot,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '!' => Token{
            .token_type = TokenType.Exclamation,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '|' => Token{
            .token_type = TokenType.Pipe,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '&' => Token{
            .token_type = TokenType.Ampersand,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        ',' => Token{
            .token_type = TokenType.Comma,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        '?' => Token{
            .token_type = TokenType.QuestionMark,
            .span_start = span_start,
            .span_end = span_start + 1,
        },
        else => @panic("Internal compiler error: symbol character mismatched in lexer"),
    };

    self.current_file.span_offset = result.span_end;

    return result;
}

pub fn peek(self: *Parser) ?Token {
    const prev_offset = self.current_file.span_offset;
    const output = self.next();
    self.current_file.span_offset = prev_offset;

    return output;
}

pub fn next(self: *Parser) ?Token {
    while (true) {
        if (self.current_file.span_offset >= self.currentFileEnd()) {
            return null;
        }

        const c = self.compiler.source[self.current_file.span_offset];

        if (isAsciiDigit(c)) {
            return self.lexNumber();
        } else if (c == '"') {
            return self.lexQuotedString();
        } else if (self.current_file.span_offset < (self.currentFileEnd() - 1) and c == 'c' and self.compiler.source[self.current_file.span_offset + 1] == '"') {
            return self.lexQuotedCString();
        } else if (self.current_file.span_offset < (self.currentFileEnd() - 1) and c == 'c' and self.compiler.source[self.current_file.span_offset + 1] == '\'') {
            return self.lexQuotedCChar();
        } else if (self.current_file.span_offset < (self.currentFileEnd() - 1) and c == '/' and self.compiler.source[self.current_file.span_offset + 1] == '/') {
            self.skipComment();
        } else if (isSymbol(c)) {
            return self.lexSymbol();
        } else if (c == ' ' or c == '\t') {
            self.skipSpace();
        } else if (c == '\n' or c == '\r') {
            return self.newLine();
        } else {
            return self.lexName();
        }
    }
}

fn currentFileEnd(self: *Parser) usize {
    return self.compiler.file_offsets.items[self.current_file.file_index].end;
}

fn isAsciiWhiteSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

fn isAsciiPunctuation(c: u8) bool {
    return c == '.' or c == ',' or c == ';' or c == ':' or c == '(' or c == ')' or c == '[' or c == ']' or c == '{' or c == '}' or c == '<' or c == '>' or c == '=' or c == '$' or c == '|' or c == '!' or c == '~' or c == '&' or c == '\'' or c == '"';
}

fn isAsciiDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAsciiHexDigit(c: u8) bool {
    return isAsciiDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}
