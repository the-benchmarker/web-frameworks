const std = @import("std");
const build_mode = @import("builtin").mode;
const httpz = @import("httpz");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    defer {
        if (build_mode == .Debug) _ = gpa.deinit();
    }

    const allocator = if (build_mode == .Debug) gpa.allocator() else std.heap.smp_allocator;
    const cpu_count = try std.Thread.getCpuCount();

    var server = try httpz.Server(void).init(allocator, .{ .address = "0.0.0.0", .port = 3000, .workers = .{
        .count = @truncate(cpu_count),
    }, .thread_pool = .{
        .count = @truncate(cpu_count),
    }, .request = .{
        .max_param_count = 1,
        .max_query_count = 0,
        .max_form_count = 0,
        .max_multiform_count = 0,
    } }, {});

    var router = try server.router(.{});

    router.get("/", empty_handler, .{});
    router.post("/user/", empty_handler, .{});
    router.get("/user/:id", user_id_handler, .{});

    try server.listen();
}

pub fn user_id_handler(req: *httpz.Request, res: *httpz.Response) !void {
    res.body = req.param("id") orelse "";
}

pub fn empty_handler(_: *httpz.Request, res: *httpz.Response) !void {
    res.body = "";
}
