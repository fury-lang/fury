const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Typechecker = @import("Typechecker.zig");
const LifetimeChecker = @import("LifetimeChecker.zig");
const Codegen = @import("Codegen.zig");

fn invokeGcc(alloc: std.mem.Allocator) !void {
    const argv = [_][]const u8{
        "gcc",
        "-o",
        "test",
        "test.c",
    };
    var child = std.process.Child.init(&argv, alloc);
    try child.spawn();
    _ = try child.wait();
}

fn execOutput(alloc: std.mem.Allocator) !void {
    const argv = [_][]const u8{
        "./test",
    };
    var child = std.process.Child.init(&argv, alloc);
    try child.spawn();
    _ = try child.wait();
}

test "Tests coverage" {
    const alloc = std.heap.page_allocator;

    var dir = try std.fs.cwd().openDir("tests", .{});
    defer dir.close();
    var walker = try dir.walk(alloc);
    var total: i32 = 0;
    var pass: i32 = 0;
    var skip: i32 = 0;
    var fail: i32 = 0;

    std.debug.print("Parsing coverage:\n", .{});
    while (try walker.next()) |entry| {
        total += 1;
        const file_path = try std.fmt.allocPrint(alloc, "tests/{s}", .{entry.path});
        const path_len = file_path.len;

        // generics and imports are not supported yet
        // zig fmt: off
        if (std.mem.startsWith(u8, entry.path, "integration/generics") or 
        std.mem.startsWith(u8, entry.path, "integration/modules") 
        or std.mem.startsWith(u8, entry.path, "integration/jason")) {
            std.debug.print("{s}: 🚧Skip\n", .{file_path});
            skip += 1;
            continue;
        }
        // zig fmt: on

        if (!std.mem.eql(u8, file_path[(path_len - 2)..path_len], "pn")) {
            pass += 1;
            continue;
        }

        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();
        const file_size = try file.getEndPos();
        const source = try alloc.alloc(u8, file_size);
        _ = try file.read(source);

        var split_source = std.mem.split(u8, source, "\n");
        const expected_result = split_source.first();
        if (std.mem.startsWith(u8, "// output: ", expected_result) or std.mem.startsWith(u8, "// error: ", expected_result)) {
            @panic("test should have an \"output: \" or \"error: \" test configuration comment");
        }

        var compiler = Compiler.new(alloc);
        const span_offset = compiler.spanOffset();
        try compiler.addFile(file_path);

        var parser = Parser.new(alloc, compiler, span_offset);
        var c = try parser.parse();

        if (c.errors.items.len > 0) {
            if (!std.mem.startsWith(u8, source, "// error: ")) {
                const error_msg = try std.fmt.allocPrint(alloc, "{s}\nexpected error but got output: {s}\n", .{ file_path, expected_result });
                @panic(error_msg);
            }

            std.debug.print("{s}: ❌Fail\n", .{file_path});
            fail += 1;
            continue;
        }

        var typechecker = try Typechecker.new(alloc, c);
        c = try typechecker.typecheck();

        if (c.errors.items.len > 0) {
            if (!std.mem.startsWith(u8, source, "// error: ")) {
                const error_msg = try std.fmt.allocPrint(alloc, "{s}\nexpected error but got output: {s}\n", .{ file_path, expected_result });
                @panic(error_msg);
            }

            std.debug.print("{s}: ❌Fail\n", .{file_path});
            fail += 1;
            continue;
        }

        var lifetime_checker = try LifetimeChecker.new(alloc, c);
        c = try lifetime_checker.checkLifetimes();

        if (c.errors.items.len > 0) {
            if (!std.mem.startsWith(u8, source, "// error: ")) {
                const error_msg = try std.fmt.allocPrint(alloc, "{s}\nexpected error but got output: {s}\n", .{ file_path, expected_result });
                @panic(error_msg);
            }

            std.debug.print("{s}: ❌Fail\n", .{file_path});
            fail += 1;
            continue;
        }

        var codegen = try Codegen.new(alloc, c);
        const output = try codegen.codegen();

        if (c.errors.items.len > 0) {
            if (!std.mem.startsWith(u8, source, "// error: ")) {
                const error_msg = try std.fmt.allocPrint(alloc, "{s}\nexpected error but got output: {s}\n", .{ file_path, expected_result });
                @panic(error_msg);
            }

            std.debug.print("{s}: ❌Fail\n", .{file_path});
            fail += 1;
            continue;
        }

        var output_file = try std.fs.cwd().createFile("test.c", .{});
        defer output_file.close();
        _ = try output_file.write(output);

        try invokeGcc(alloc);
        // try execOutput(alloc);

        pass += 1;
        std.debug.print("{s}: ✅Pass\n", .{file_path});
    }

    std.debug.print("Total: {d}, Pass: {d}, skip: {d}, Fail: {d}\n", .{ total, pass, skip, fail });
}
