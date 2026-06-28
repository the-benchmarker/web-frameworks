package the.benchmarker;

import io.undertow.Undertow;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.RoutingHandler;
import io.undertow.util.Headers;

import java.util.Deque;

/**
 * Undertow Benchmark Application
 * 
 * <p>Production-grade benchmark server using Undertow framework.
 * Follows best practices including:
 * - Minimal configuration for maximum performance
 * - No logging in production for benchmarking
 * - Optimized request handling
 * - Proper server configuration</p>
 */
public class Main {

    /**
     * Main application entry point.
     * 
     * <p>Creates and starts an Undertow server with production-optimized settings.
     * All logging is disabled for maximum benchmarking performance.</p>
     * 
     * @param args Command line arguments (not used)
     */
    public static void main(final String[] args) {
        // Get port from environment or use default
        int port = Integer.parseInt(System.getenv().getOrDefault("PORT", "3000"));
        String host = System.getenv().getOrDefault("HOST", "0.0.0.0");

        Undertow server = Undertow.builder()
                .addHttpListener(port, host)
                .setBufferSize(1024 * 16) // 16KB buffer
                .setIoThreads(Runtime.getRuntime().availableProcessors())
                .setWorkerThreads(20)
                .setServerOption(io.undertow.UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
                .setServerOption(io.undertow.UndertowOptions.ALLOW_ENCODED_SLASH, true)
                .setServerOption(io.undertow.UndertowOptions.DECODE_URL, true)
                .setServerOption(io.undertow.UndertowOptions.MAX_ENTITY_SIZE, 10 * 1024 * 1024L) // 10MB
                .setServerOption(io.undertow.UndertowOptions.MAX_PARAMETERS, 1000)
                .setServerOption(io.undertow.UndertowOptions.MAX_HEADERS, 200)
                .setServerOption(io.undertow.UndertowOptions.MAX_COOKIES, 100)
                .setServerOption(io.undertow.UndertowOptions.NO_REQUEST_TIMEOUT, -1) // Disable timeout
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
                        .get("/user/{id}", new HttpHandler() {
                            @Override
                            public void handleRequest(HttpServerExchange exchange) throws Exception {
                                exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
                                String id = exchange.getQueryParameters().get("id").peekFirst();
                                if(id == null){
                                    exchange.getResponseSender().send("");
                                } else {
                                    exchange.getResponseSender().send(id);
                                }
                            }
                        })
                        .get("/health", new HttpHandler() {
                            @Override
                            public void handleRequest(HttpServerExchange exchange) throws Exception {
                                exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "text/plain");
                                exchange.getResponseSender().send("OK");
                            }
                        })
                ).build();
        server.start();
    }
}
