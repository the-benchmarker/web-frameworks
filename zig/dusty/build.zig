const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dusty_dep = b.dependency("dusty", .{
        .target = target,
        .optimize = optimize,
    });
    const zio_dep = b.dependency("zio", .{
        .target = target,
        .optimize = optimize,
    });
    const dusty_mod = dusty_dep.module("dusty");
    const zio_mod = zio_dep.module("zio");
    dusty_mod.addImport("zio", zio_mod);

    const root_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    root_mod.addImport("dusty", dusty_mod);
    root_mod.addImport("zio", zio_mod);

    const exe = b.addExecutable(.{
        .name = "server",
        .root_module = root_mod,
    });

    b.installArtifact(exe);
}
