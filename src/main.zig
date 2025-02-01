const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Typechecker = @import("Typechecker.zig");
const Codegen = @import("Codegen.zig");
const LifetimeChecker = @import("LifetimeChecker.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

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

    var compiler = Compiler.init(alloc);
    const span_offset = compiler.spanOffset();
    try compiler.addFile(file_name);

    var parser = Parser.init(alloc, compiler, span_offset);
    compiler = try parser.parse();

    // if (c.errors.items.len == 0) c.print();

    for (compiler.errors.items) |*err| {
        try compiler.printErrors(err);
        // std.debug.print("err: {s}\n", .{err.*.message});
    }

    if (compiler.errors.items.len != 0) return;

    var typechecker = try Typechecker.init(alloc, compiler);
    defer typechecker.deinit();
    compiler = try typechecker.typecheck();
    // if (compiler.errors.items.len == 0) compiler.print();

    for (compiler.errors.items) |*err| {
        try compiler.printErrors(err);
    }

    if (compiler.errors.items.len != 0) return;

    var lifetime_checker = try LifetimeChecker.init(alloc, compiler);
    defer lifetime_checker.deinit();
    compiler = try lifetime_checker.checkLifetimes();

    // if (compiler.errors.items.len == 0) compiler.print();

    for (compiler.errors.items) |*err| {
        try compiler.printErrors(err);
    }

    if (compiler.errors.items.len != 0) return;

    var codegen = try Codegen.init(alloc, compiler);
    defer compiler.deinit();
    const output = try codegen.codegen();
    // defer alloc.free(output);

    if (compiler.errors.items.len == 0) compiler.print();

    for (compiler.errors.items) |*err| {
        try compiler.printErrors(err);
    }

    if (compiler.errors.items.len != 0) return;

    var output_file = try std.fs.cwd().createFile("output.c", .{});
    defer output_file.close();
    _ = try output_file.write(output);
}
