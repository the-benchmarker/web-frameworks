const std = @import("std");
const zio = @import("zio");
const http = @import("dusty");

pub fn main(init: std.process.Init) !void {
    var rt = try zio.Runtime.init(init.gpa, .{ .executors = .auto });
    defer rt.deinit();

    var server = http.Server(void).init(init.gpa, rt.io(), .{
        .listeners = &.{
            .{ .address = .{ .ip = try std.Io.net.IpAddress.parse("0.0.0.0", 3000) } },
        },
    }, {});
    defer server.deinit();

    server.router.get("/", empty);
    server.router.get("/user/:id", userId);
    server.router.post("/user", empty);

    try server.run();
}

fn empty(_: *http.Request, res: *http.Response) !void {
    res.body = "";
}

fn userId(req: *http.Request, res: *http.Response) !void {
    res.body = req.params.get("id") orelse "";
}
