import lighttp;

void main(string[] args) {
    Server server = new Server();
    server.host("127.0.0.1", 8080);
    server.router.add(new Router());
    server.run();
}

final class Router
{
    // GET /
    @Get("") get(ServerResponse response) {
        response.headers["Content-Type"] = "text/plain";
        response.body = "";
    }

    // GET /user/100 => id = "100"
    @Get("user", "([0-9]+)") getUser(ServerResponse response, string userId) {
        response.headers["Content-Type"] = "text/plain";
        response.body = userId;
    }

    // POST /user
    @Post("user") post(ServerResponse response) {
        response.headers["Content-Type"] = "text/plain";
        response.body = "";
    }
}
