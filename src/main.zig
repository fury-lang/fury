const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Typechecker = @import("Typechecker.zig");
const Codegen = @import("Codegen.zig");

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
        // std.debug.print("err: {s}\n", .{err.*.message});
    }

    var typechecker = try Typechecker.new(alloc, c);
    var c_new = try typechecker.typecheck();
    // if (c_new.errors.items.len == 0) c_new.print();

    for (c_new.errors.items) |*err| {
        try c_new.printErrors(err);
    }

    var codegen = try Codegen.new(alloc, c_new);
    const output = try codegen.codegen();

    var output_file = try std.fs.cwd().createFile("output.c", .{});
    defer output_file.close();
    _ = try output_file.write(output);
}
