import hunt.http;

void main()
{
    HttpServerOptions serverOptions = new HttpServerOptions();
	serverOptions.setSecureConnectionEnabled(false);
	serverOptions.setFlowControlStrategy("simple");
	serverOptions.setHost("0.0.0.0");
	serverOptions.setPort(3000);

    auto server = HttpServer.builder(serverOptions)
        .addRoute("/", (RoutingContext context) {
            context.responseHeader(HttpHeader.CONTENT_TYPE, MimeType.TEXT_PLAIN_VALUE);
            context.end();
        })
        .onPost("/user", (RoutingContext context) {
            context.responseHeader(HttpHeader.CONTENT_TYPE, MimeType.TEXT_PLAIN_VALUE);
            context.end();
        })
        .onGet("/user/:id", (RoutingContext ctx) {
            string id = ctx.getRouterParameter("id");
            ctx.responseHeader(HttpHeader.CONTENT_TYPE, MimeType.TEXT_PLAIN_VALUE);
            ctx.end(id);
        })
        .build();

    server.start();
}
