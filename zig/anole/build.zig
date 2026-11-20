const std = @import("std");

pub fn build(b: *std.Build) void {
    // HTTP/1.1 only: the preset without TLS, HTTP/2, HTTP/3 or databases.
    _ = @import("anole").addApp(b, .{
        .name = "server",
        .root_source_file = b.path("src/main.zig"),
        .profile = .minimal,
    });
}
