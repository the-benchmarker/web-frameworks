const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const root_source_file = b.path("src/main.zig");

    const httpz_dep = b.dependency("httpz", .{
        .target = target,

        .optimize = optimize,
    });
    const httpz_mod = httpz_dep.module("httpz");

    const root_mod = b.addModule("root_mod", .{
        .root_source_file = root_source_file,
        .target = target,
        .optimize = optimize,
    });

    root_mod.addImport("httpz", httpz_mod);
    const exe = b.addExecutable(.{
        .name = "httpz",
        .root_module = root_mod,
    });

    b.installArtifact(exe);
}
