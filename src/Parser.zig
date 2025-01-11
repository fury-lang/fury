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

pub const AstNode = union(enum) {
    int: TokenType.Int,
    float: TokenType.Float,
    string: TokenType.String,
    c_string: TokenType.CString,
    c_char: TokenType.CChar,
    name: TokenType.Name,
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
    true: TokenType.True,
    false: TokenType.False,

    // Special lifetimes
    return_lifetime: TokenType.ReturnLifetime,

    // Empty optional values
    node: null,

    // Operators
    equals: TokenType.Equals,
    not_equals: TokenType.NotEqual,
    less_than: TokenType.LessThan,
    greater_than: TokenType.GreaterThan,
    less_than_equal: TokenType.LessThanEqual,
    greater_than_equal: TokenType.GreaterThanEqual,
    plus: TokenType.Plus,
    minus: TokenType.Minus,
    append: TokenType.PlusPlus,
    multiply: TokenType.Multiply,
    divide: TokenType.Divide,
    @"and": TokenType.And,
    @"or": TokenType.Or,
    pow: TokenType.Pow,

    // Bitwise operators
    bitwise_and: TokenType.Ampersand,
    bitwise_or: TokenType.Pipe,
    shift_left: TokenType.LessThanLessThan,
    shift_right: TokenType.GreaterThanGreaterThan,

    // Special operator
    as: TokenType.Name,

    // Assignment
    assignment: TokenType.Equals,
    add_assignment: TokenType.PlusEquals,
    subtract_assignment: TokenType.DashEquals,
    multiply_assignment: TokenType.AsteriskEquals,
    divide_assignment: TokenType.ForwardSlashEquals,

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
    @"return": struct {
        value: ?NodeId,
    },
    @"break": null,
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
        is_mutablw: bool,
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
        op: TokenType,
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
    garbage: null,

    pub fn precedence(self: *AstNode) usize {
        switch (self) {
            AstNode.as => 200,
            AstNode.pow => 100,
            AstNode.multiply, AstNode.divide => 95,
            //AstNode.modulo => 95,
            AstNode.plus, AstNode.minus => 90,
            AstNode.shift_left, AstNode.shift_right => 88,
            AstNode.bitwise_and => 85,
            AstNode.bitwise_or => 83,
            AstNode.less_than, AstNode.less_than_or_equal, AstNode.greater_than, AstNode.greater_than_or_equal, AstNode.equals, AstNode.not_equals => 80,
            AstNode.@"and" => 50,
            AstNode.@"or" => 40,
            AstNode.assigment, AstNode.add_assignment, AstNode.subtract_assignment, AstNode.multiply_assignment, AstNode.divide_assignment => ASSIGNMENT_PRECEDENCE,
            else => 0,
        }
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

fn is_symbol(b: u8) bool {
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
            return self.compiler.source[token.span_start..token.span_end] == keyword;
        }
    }
    return false;
}

pub fn isExpression(self: *Parser) bool {
    self.isSimpleExpression() or self.isKeyword("if");
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
            => true,
            else => false,
            // else => {
            //     if (token.token_type == TokenType.Name) {
            //         if (std.mem.eql(u8, self.compiler.source[token.span_start..token.span_end],"true")) {
            //             return true;
            //         } else {
            //             return false;
            //         }
            //     }
            // },
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
        const garbage = AstNode{ .garbage = null };
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
        const garbage = AstNode{ .garbage = null };
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
    try self.compiler.createNode(ast_node, span_start, span_end);
}

pub fn parse(self: *Parser) Compiler {
    self.program();
    return self.compiler;
}

pub fn program(self: *Parser) NodeId {
    return self.block(false);
}

pub fn block(self: *Parser, expect_curly_braces: bool) NodeId {
    const span_start = self.position();
    var span_end = self.position();

    var curr_body = std.ArrayList(NodeId).init(self.alloc);
    if (expect_curly_braces) {
        self.isExpectedToken(TokenType.LCurly);
    }

    while (self.position() < self.currentFileEnd()) {}
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
        if (!self.compiler.source[span_position]) {
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
        } else if (is_symbol(c)) {
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
