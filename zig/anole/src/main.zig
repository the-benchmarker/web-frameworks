const std = @import("std");
const anole = @import("anole");

pub const std_options = anole.std_options;

fn empty(_: *anole.Request, res: *anole.Response) !void {
    res.body = "";
}

fn userId(req: *anole.Request, res: *anole.Response) !void {
    res.body = req.params.get("id") orelse "";
}

pub fn main(init: std.process.Init) !void {
    var app = try anole.App.init(init.gpa, .{});
    defer app.deinit();

    app.get("/", empty);
    app.get("/user/:id", userId);
    app.post("/user", empty);

    try app.listen(":3000");
}
