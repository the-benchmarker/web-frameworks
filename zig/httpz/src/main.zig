const std = @import("std");
const httpz = @import("httpz");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    const cpu_count = try std.Thread.getCpuCount();

    var server = try httpz.Server().init(allocator, .{ .address = "0.0.0.0", .port = 3000, .workers = .{
        .count = @truncate(cpu_count / 2),
        .max_conn = 8192,
        .retain_allocated_bytes = 0,
    }, .thread_pool = .{
        .count = @truncate(cpu_count / 2),
        .buffer_size = 1024,
    }, .request = .{
        .max_param_count = 1,
        .max_query_count = 0,
        .max_form_count = 0,
        .max_multiform_count = 0,
    } });

    var router = server.router();

    router.get("/", empty_handler);
    router.post("/user", empty_handler);
    router.get("/user/:id", user_id_handler);
    try server.listen();
}

pub fn user_id_handler(req: *httpz.Request, res: *httpz.Response) !void {
    res.body = req.param("id") orelse "";
}

pub fn empty_handler(_: *httpz.Request, res: *httpz.Response) !void {
    res.body = "";
}
