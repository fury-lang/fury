const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "fury",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const invoke_gcc = b.step("gcc", "Compile with gcc");
    invoke_gcc.makeFn = invokeGcc;

    const exec_output = b.step("exec", "Execute output");
    exec_output.makeFn = execOutput;

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const integration_tests = b.addTest(.{
        .root_source_file = b.path("src/integration_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const run_integation_tests = b.addRunArtifact(integration_tests);

    const test_step = b.step("test", "Run unit tests");
    const coverage_step = b.step("coverage", "Tests coverage");
    test_step.dependOn(&run_exe_unit_tests.step);
    coverage_step.dependOn(&run_integation_tests.step);
}

fn invokeGcc(step: *std.Build.Step, prog_node: std.Progress.Node) !void {
    _ = step;
    _ = prog_node;
    const alloc = std.heap.page_allocator;
    const argv = [_][]const u8{
        "gcc",
        "-o",
        "output",
        "output.c",
    };
    var child = std.process.Child.init(&argv, alloc);
    try child.spawn();
    _ = try child.wait();
}

fn execOutput(step: *std.Build.Step, prog_node: std.Progress.Node) !void {
    _ = step;
    _ = prog_node;
    const alloc = std.heap.page_allocator;
    const argv = [_][]const u8{
        "./output",
    };
    var child = std.process.Child.init(&argv, alloc);
    try child.spawn();
    _ = try child.wait();
}
