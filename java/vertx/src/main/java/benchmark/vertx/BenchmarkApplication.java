package benchmark.vertx;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.VertxOptions;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;

import static io.vertx.core.http.HttpMethod.GET;

public class BenchmarkApplication extends AbstractVerticle {
    @Override
    public void start(Promise<Void> onReady) {

//        System.out.println("Event Loop Size: " + VertxOptions.DEFAULT_EVENT_LOOP_POOL_SIZE);
//        System.out.println("Native transport : " + vertx.isNativeTransportEnabled());

        vertx
                .createHttpServer()
                .requestHandler(this::handleRequests)
                .listen(3000)
                .onFailure(onReady::fail)
                .onSuccess(server -> onReady.complete());
    }

    private void handleRequests(HttpServerRequest request) {
        // GET requests:
        if (request.method() == GET) {
            if (request.path().equals("/")) {
                request
                        .response()
                        .end();
            } else if (request.path().startsWith("/user/")) {
                request
                        .response()
                        .end(request.path().substring(6));
            } else {
                errorResponse(request.response(), "Invalid path: " + request.path());
            }
        } else if (request.method() == HttpMethod.POST) {
            if (request.path().equals("/user")) {
                request
                        .response()
                        .end();
            } else {
                errorResponse(request.response(), "Invalid path: " + request.path());
            }
        } else {
            errorResponse(request.response(), "Invalid method " + request.method());
        }
    }

    private static void errorResponse(HttpServerResponse response, String msg) {
        response.setStatusCode(500).end("Incorrect HTTP call: " + msg);
    }
}