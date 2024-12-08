const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const root_source_file = b.path("src/main.zig");
    const version = std.SemanticVersion{ .major = 0, .minor = 1, .patch = 0 };

    const httpz_dep = b.dependency("httpz", .{
        .target = target,
        .optimize = optimize,
    });
    const httpz_mod = httpz_dep.module("httpz");

    const exe = b.addExecutable(.{
        .name = "httpz",
        .target = target,
        .version = version,
        .optimize = optimize,
        .root_source_file = root_source_file,
    });
    exe.root_module.addImport("httpz", httpz_mod);
    b.installArtifact(exe);
}
