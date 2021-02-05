package benchmark.vertx4web;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;

public class BenchmarkApplication extends AbstractVerticle {

    @Override
    public void start(Promise<Void> onReady) {
        Router router = Router.router(vertx);

        router.get("/").handler(RoutingContext::end);
        router.post("/user").handler(RoutingContext::end);
        router.get("/user/:userId").handler(ctx -> ctx.end(ctx.pathParam("userId")));

        vertx
                .createHttpServer()
                .requestHandler(router)
                .listen(3000)
                .onFailure(onReady::fail)
                .onSuccess(server -> onReady.complete());
    }
}