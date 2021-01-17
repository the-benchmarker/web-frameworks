package benchmark.vertx4web;

import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;

public class BenchmarkApplication {
    public static void main( String [] args )
    {
        Vertx vertx = Vertx.vertx();

        vertx.createHttpServer(new HttpServerOptions().setSsl(false))
                .requestHandler(BenchmarkApplication::handleRequests)
                .listen();
    }

    private static void handleRequests(HttpServerRequest request) {
        // GET requests:
        if( request.method() == HttpMethod.GET ) {
            if (request.path().equals("/"))
                request.response().setStatusCode(200).end();
            else if(request.path().startsWith("/user/"))
                request.response().setStatusCode(200).end(request.path().replaceFirst("/user/", ""));
            else
                errorResponse(request.response(), "Invalid path: " + request.path());
        }
        else if(request.method() == HttpMethod.POST )
        {
            if(request.path().equals("/user"))
                request.response().setStatusCode(200).end();
            else
                errorResponse(request.response(), "Invalid path: " + request.path());
        }
        else
        {
            errorResponse(request.response(), "Invalid method " + request.method() );
        }
    }

    private static void errorResponse(HttpServerResponse response, String msg)
    {
        response.setStatusCode(500).end("Incorrect HTTP call: " + msg);
    }
}