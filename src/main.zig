const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    var args = std.process.args();
    _ = args.next();

    var file_name: []const u8 = "";
    if (args.next()) |f| {
        file_name = f;
    } else {
        @panic("not receive any file name");
    }

    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const source = try alloc.alloc(u8, file_size);
    _ = try file.read(source);

    var compiler = Compiler.new(alloc);
    const span_offset = compiler.spanOffset();
    try compiler.addFile(file_name);

    var parser = Parser.new(alloc, compiler, span_offset);
    var c = try parser.parse();
    if (c.errors.items.len == 0) c.print();

    for (c.errors.items) |*err| {
        try c.printErrors(err);
    }
}
