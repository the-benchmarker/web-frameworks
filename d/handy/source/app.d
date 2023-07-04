import handy_httpd;
import std.algorithm: startsWith;

void main() {
    ServerConfig cfg = ServerConfig.defaultValues();
    cfg.workerPoolSize = 5;
    cfg.port = 3000;
    new HttpServer((ref ctx) {
        if (ctx.request.url == "/") {
            ctx.response.writeBodyString("");
        } else if (ctx.request.url == "/user" && ctx.request.method == Method.POST) {
            ctx.response.writeBodyString("");
        } else if (startsWith(ctx.request.url, "/user/") && ctx.request.method == Method.GET) {
            ctx.response.writeBodyString(ctx.request.url[6..$]);
        } else {
            ctx.response.setStatus(HttpStatus.NOT_FOUND);
        }
    }, cfg).start();
}
