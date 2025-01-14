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

pub fn codegen(self: *Codegen) ![]const u8 {
    var output: std.ArrayList(u8) = std.ArrayList(u8).init(self.alloc);

    const allocator = try std.fs.cwd().openFile("allocator/allocator.c", .{});
    defer allocator.close();

    const file_size = try allocator.getEndPos();
    const allocator_src = try self.alloc.alloc(u8, file_size);
    _ = try allocator.read(allocator_src);

    try output.appendSlice(allocator_src);

    try output.appendSlice("struct Allocator *allocator;\n");

    // try self.codegenUserPredecls(&output);
    // try self.codegenUserTypes(&output);
    // try self.codegenFunDecls(&output);

    for (self.compiler.functions.items, 0..) |fun, idx| {
        const name = self.compiler.getSource(fun.name);

        if (std.mem.eql(u8, name, "main")) {
            try output.appendSlice("int main() {\n");
            try output.appendSlice("allocator = create_allocator(100)\n");
            try output.appendSlice("function_");
            const idx_str = try std.fmt.allocPrint(self.alloc, "{d}", .{idx});
            try output.appendSlice(idx_str);
            try output.appendSlice("(0);\n}\n");
        }
    }

    return try output.toOwnedSlice();
}
