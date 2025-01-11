const std = @import("std");
const Compiler = @import("Compiler.zig");

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
            AstNode.multiply | AstNode.divide => 95,
            //AstNode.modulo => 95,
            AstNode.plus | AstNode.minus => 90,
            AstNode.shift_left | AstNode.shift_right => 88,
            AstNode.bitwise_and => 85,
            AstNode.bitwise_or => 83,
            AstNode.less_than | AstNode.less_than_or_equal | AstNode.greater_than | AstNode.greater_than_or_equal | AstNode.equals | AstNode.not_equals => 80,
            AstNode.@"and" => 50,
            AstNode.@"or" => 40,
            AstNode.assigment | AstNode.add_assignment | AstNode.subtract_assignment | AstNode.multiply_assignment | AstNode.divide_assignment => ASSIGNMENT_PRECEDENCE,
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
