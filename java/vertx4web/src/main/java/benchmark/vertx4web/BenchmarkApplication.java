package benchmark.vertx4web;

import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.ext.web.Router;

public class BenchmarkApplication {
    public static void main( String [] args )
    {
        Vertx vertx = Vertx.vertx();

        Router router = Router.router(vertx);

        router.get("/").respond( ctx -> ctx.response().setStatusCode(200).end());
        router.post("/user").respond( ctx -> ctx.response().setStatusCode(200).end());
        router.get("/user/:userId").respond( ctx -> ctx.response().setStatusCode(200).end(ctx.pathParam("userId")));

        vertx.createHttpServer(new HttpServerOptions().setSsl(false))
                .requestHandler(router)
                .listen(3000);
    }
}