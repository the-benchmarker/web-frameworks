import handy_httpd;
import handy_httpd.handlers.path_handler;

void main() {
    import slf4d;
    import slf4d.default_provider;

    auto provider = new DefaultProvider(true, Levels.ERROR);
    configureLoggingProvider(provider);

    ServerConfig cfg = ServerConfig.init;
    cfg.hostname = "0.0.0.0";
    cfg.port = 3000;

    auto pathHandler = new PathHandler()
        .addMapping(Method.GET, "/", (ref ctx) {ctx.response.okResponse();})
        .addMapping(Method.POST, "/user", (ref ctx) {ctx.response.okResponse();})
        .addMapping(Method.GET, "/user/:userId", (ref ctx) {ctx.response.okResponse(ctx.request.pathParams["userId"]);});

    new HttpServer(pathHandler, cfg).start();
}
