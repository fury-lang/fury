const std = @import("std");
const Compiler = @import("Compiler.zig");
const Parser = @import("Parser.zig");
const Errors = @import("Errors.zig");
const Typechecker = @import("Typechecker.zig");

const LifetimeChecker = @This();
