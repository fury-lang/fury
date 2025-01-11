const NodeId = @import("Parser.zig").NodeId;

pub const Severity = enum {
    Error,
    Note,
};

pub const SourceError = struct {
    message: []const u8,
    node_id: NodeId,
    severity: Severity,
};
