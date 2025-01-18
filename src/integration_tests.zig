const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Typechecker = @import("Typechecker.zig");

test "Tests coverage" {
    const alloc = std.heap.page_allocator;

    var dir = try std.fs.cwd().openDir("tests", .{});
    defer dir.close();
    var walker = try dir.walk(alloc);
    var total: i32 = 0;
    var pass: i32 = 0;
    var fail: i32 = 0;

    std.debug.print("Parsing coverage:\n", .{});
    while (try walker.next()) |entry| {
        total += 1;
        const file_path = try std.fmt.allocPrint(alloc, "tests/{s}", .{entry.path});
        const path_len = file_path.len;
        if (!std.mem.eql(u8, file_path[(path_len - 2)..path_len], "pn")) {
            pass += 1;
            continue;
        }

        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();
        const file_size = try file.getEndPos();
        const source = try alloc.alloc(u8, file_size);
        _ = try file.read(source);

        var compiler = Compiler.new(alloc);
        const span_offset = compiler.spanOffset();
        try compiler.addFile(file_path);

        var parser = Parser.new(alloc, compiler, span_offset);
        const c = try parser.parse();

        if (c.errors.items.len > 0) {
            std.debug.print("{s}: ❌Fail\n", .{file_path});
            fail += 1;
            continue;
        }

        pass += 1;
        std.debug.print("{s}: ✅Pass\n", .{file_path});
    }

    std.debug.print("Total: {d}, Pass: {d}, Fail: {d}\n", .{ total, pass, fail });
}
