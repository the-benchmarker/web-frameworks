import handy_httpd;
import handy_httpd.handlers.path_handler : PathHandler;

void main() {
    ServerConfig cfg = ServerConfig.defaultValues();
    cfg.workerPoolSize = 50;
    cfg.requestQueueSize = 300;
    cfg.hostname = "0.0.0.0";
    cfg.port = 3000;
    cfg.enableWebSockets = false;
    auto pathHandler = new PathHandler()
        .addMapping(Method.GET, "/", (ref ctx) {ctx.response.okResponse();})
        .addMapping(Method.POST, "/user", (ref ctx) {ctx.response.okResponse();})
        .addMapping(Method.GET, "/user/:userId", (ref ctx) {ctx.response.okResponse(ctx.request.pathParams["userId"]);});

    new HttpServer(pathHandler).start();
}
