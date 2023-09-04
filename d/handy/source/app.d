import handy_httpd;
import std.parallelism: totalCPUs;
import std.algorithm: startsWith;

void main() {
    ServerConfig cfg = ServerConfig.defaultValues();
    cfg.workerPoolSize = 2*totalCPUs;
    cfg.hostname = "0.0.0.0";
    cfg.port = 3000;
    new HttpServer((ref ctx) {
        if (ctx.request.url == "") {
            ctx.response.setStatus(HttpStatus.OK);
            ctx.response.writeBodyString("");
        } else if (ctx.request.url == "/user" && ctx.request.method == Method.POST) {
            ctx.response.setStatus(HttpStatus.OK);
            ctx.response.writeBodyString("");
        } else if (startsWith(ctx.request.url, "/user/") && ctx.request.method == Method.GET) {
            ctx.response.setStatus(HttpStatus.OK);
            ctx.response.writeBodyString(ctx.request.url[6..$]);
        }
    }, cfg).start();
}
