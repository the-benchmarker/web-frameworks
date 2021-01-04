package the.benchmarker;

import io.undertow.Undertow;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.RoutingHandler;
import io.undertow.util.Headers;

import java.util.Deque;
import java.util.Optional;

public class Main {

    public static void main(final String[] args) {
        Undertow server = Undertow.builder()
                .addHttpListener(3000, "0.0.0.0")
                .setHandler(new RoutingHandler()
                        .get("/", new HttpHandler() {
                            @Override
                            public void handleRequest(HttpServerExchange exchange) throws Exception {
                                exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
                                exchange.getResponseSender().send("");
                            }
                        })
                        .post("/user", new HttpHandler() {
                            @Override
                            public void handleRequest(HttpServerExchange exchange) throws Exception {
                                exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
                                exchange.getResponseSender().send("");
                            }
                        })
                        .get("/user/{name}", new HttpHandler() {
                            @Override
                            public void handleRequest(HttpServerExchange exchange) throws Exception {
                                exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
                                var x = Optional.ofNullable(exchange.getQueryParameters().get("name"))
                                        .map(Deque::getFirst);
                                exchange.getResponseSender().send(x.get());
                            }
                        })  
                ).build();
        server.start();
    }
}
