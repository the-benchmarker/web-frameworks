const std = @import("std");
const log = std.log.scoped(.@"examples/basic");

pub const std_options: std.Options = .{ .log_level = .err };

const zzz = @import("zzz");
const http = zzz.http;
const Secsock = zzz.Secsock;

const tardy = zzz.tardy;
const Tardy = tardy.Tardy(.epoll);
const Runtime = tardy.Runtime;

const Server = http.Server;
const Router = http.Router;
const Context = http.Context;
const Route = http.Router.Route;
const Respond = http.Respond;

fn signal_handler(sig: std.os.linux.SIG) callconv(.c) void {
    _ = sig;
    //var buf: [4096]u8 = undefined;
    //const stdout = std.Io.File.stdout().writer(&buf);
    //stdout.interface.print("Got signal!\n", .{}) catch @panic("Unexpected failure");
    //stdout.interface.flush();
    @panic("");
}

fn base_handler(ctx: *const Context, _: void) !Respond {
    return ctx.response.apply(.{
        .status = .OK,
        .mime = http.Mime.HTML,
        .body = "",
    });
}

fn user_id(ctx: *const Context, _: void) !Respond {
    return ctx.response.apply(.{ .status = .OK, .mime = http.Mime.HTML, .body = ctx.captures[0].string });
}

fn user(ctx: *const Context, _: void) !Respond {
    return ctx.response.apply(.{
        .status = .OK,
        .mime = http.Mime.HTML,
        .body = "",
    });
}

const MyContext = struct { rt: *Runtime };

pub fn main(init: std.process.Init) !void {
    const host: []const u8 = "0.0.0.0";
    const port: u16 = 3000;

    const act = std.posix.Sigaction{ .handler = .{ .handler = signal_handler }, .mask = std.posix.sigemptyset(), .flags = std.posix.SA.RESTART };

    std.posix.sigaction(std.posix.SIG.INT, &act, null);
    std.posix.sigaction(std.posix.SIG.TERM, &act, null);

    var t = try Tardy.init(init.gpa, init.io, .{ .threading = .auto });
    defer t.deinit();

    var router = try Router.init(init.gpa, &.{ Route.init("/").get({}, base_handler).layer(), Route.init("/user/%s").get({}, user_id).layer(), Route.init("/user").post({}, user).layer() }, .{});
    defer router.deinit(init.gpa);

    const transport: Secsock.Unsecured = .empty;
    const socket = try transport.tcp(init.gpa, .{ .host = host, .port = port, .backlog = 4096 });
    defer socket.deinit(init.gpa);

    const EntryParams = struct {
        router: *const Router,
        socket: *const Secsock,
    };



    const stdout = std.Io.File.stdout();
    try stdout.writeStreamingAll(init.io, "Ready to serve!\n");

    try t.entry(
        EntryParams{ .router = &router, .socket = &socket },
        struct {
            fn entry(rt: *Runtime, p: EntryParams) !void {
                var server = Server.init(.{
                    .stack_size = .@"4MiB",
                    .socket_buffer_size = .@"2KiB",
                    .max_keepalive_count = null,
                    .max_connection_count = 512,
                });
                defer server.deinit();
                server.serve(rt, p.router, p.socket) catch |err| {
                    if (err == error.FileDescriptorClosed or err == error.OperationAborted) {
                        return; // Exit gracefully
                    }
                    return err;
                };
            }
        }.entry,
    );
}
