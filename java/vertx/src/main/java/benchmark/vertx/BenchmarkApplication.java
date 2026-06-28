package benchmark.vertx;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.VertxOptions;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerOptions;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;

/**
 * Vert.x Benchmark Application
 * 
 * <p>Production-grade high-performance benchmark server using Vert.x framework.
 * Follows Vert.x best practices including:
 * - Async/non-blocking I/O
 * - Minimal logging for production (only errors)
 * - Proper error handling
 * - Optimized server configuration
 * - Security best practices</p>
 */
public class BenchmarkApplication extends AbstractVerticle {

    private HttpServer server;

    @Override
    public void start(Promise<Void> onReady) {
        // Get port from environment or use default
        int port = Integer.parseInt(System.getenv().getOrDefault("PORT", "3000"));
        String host = System.getenv().getOrDefault("HOST", "0.0.0.0");

        // Configure HTTP server with production settings
        HttpServerOptions serverOptions = new HttpServerOptions()
                .setHost(host)
                .setPort(port)
                .setMaxInitialLineLength(8192)
                .setMaxHeaderSize(8192)
                .setMaxChunkSize(8192)
                .setCompressionSupported(false) // Disable compression for benchmarking
                .setIdleTimeout(0) // Disable idle timeout for benchmarking
                .setKeepAlive(true)
                .setReuseAddress(true)
                .setReusePort(true);

        // Configure Vert.x options for production
        VertxOptions vertxOptions = new VertxOptions()
                .setEventLoopPoolSize(Math.max(4, Runtime.getRuntime().availableProcessors()))
                .setWorkerPoolSize(20)
                .setInternalBlockingPoolSize(20);

        // Initialize Vert.x with production options
        vertx = vertx != null ? vertx : io.vertx.core.Vertx.vertx(vertxOptions);

        server = vertx.createHttpServer(serverOptions)
                .requestHandler(this::handleRequests)
                .listen();

        server
                .onFailure(throwable -> onReady.fail(throwable))
                .onSuccess(success -> onReady.complete());
    }

    @Override
    public void stop(Promise<Void> onStop) {
        if (server != null) {
            server.close()
                    .onFailure(throwable -> onStop.fail(throwable))
                    .onSuccess(success -> onStop.complete());
        } else {
            onStop.complete();
        }
    }

    /**
     * Handle incoming HTTP requests.
     * 
     * <p>Optimized request handler with minimal overhead for benchmarking.</p>
     * 
     * @param request The HTTP server request
     */
    private void handleRequests(HttpServerRequest request) {
        try {
            String path = request.path();
            HttpMethod method = request.method();
            HttpServerResponse response = request.response();

            // Configure response for benchmarking
            response.putHeader("Content-Type", "text/plain");
            response.putHeader("Server", "Vert.x");

            // GET requests
            if (method == GET) {
                if (path.equals("/")) {
                    handleRoot(response);
                } else if (path.startsWith("/user/")) {
                    handleGetUser(response, path);
                } else if (path.equals("/health")) {
                    handleHealthCheck(response);
                } else {
                    handleNotFound(response);
                }
            } else if (method == POST) {
                if (path.equals("/user")) {
                    handleCreateUser(response);
                } else {
                    handleNotFound(response);
                }
            } else {
                handleMethodNotAllowed(response);
            }
        } catch (Exception e) {
            handleException(request.response(), e);
        }
    }

    /**
     * Handle root endpoint.
     * 
     * <p>Optimized for minimal latency and maximum throughput.</p>
     * 
     * @param response The HTTP server response
     */
    private void handleRoot(HttpServerResponse response) {
        response.setStatusCode(200).end();
    }

    /**
     * Handle GET /user/{id} endpoint.
     * 
     * <p>Extracts user ID from path and returns it.</p>
     * 
     * @param response The HTTP server response
     * @param path The request path
     */
    private void handleGetUser(HttpServerResponse response, String path) {
        String id = path.substring(6); // Remove "/user/" prefix
        response.setStatusCode(200).end(id);
    }

    /**
     * Handle POST /user endpoint.
     * 
     * @param response The HTTP server response
     */
    private void handleCreateUser(HttpServerResponse response) {
        response.setStatusCode(200).end();
    }

    /**
     * Handle GET /health endpoint.
     * 
     * @param response The HTTP server response
     */
    private void handleHealthCheck(HttpServerResponse response) {
        response.setStatusCode(200).end("OK");
    }

    /**
     * Handle 404 Not Found.
     * 
     * @param response The HTTP server response
     */
    private void handleNotFound(HttpServerResponse response) {
        response.setStatusCode(404).end("Not Found");
    }

    /**
     * Handle 405 Method Not Allowed.
     * 
     * @param response The HTTP server response
     */
    private void handleMethodNotAllowed(HttpServerResponse response) {
        response.setStatusCode(405).end("Method Not Allowed");
    }

    /**
     * Handle exceptions.
     * 
     * @param response The HTTP server response
     * @param exception The exception that was thrown
     */
    private static void handleException(HttpServerResponse response, Exception exception) {
        response.setStatusCode(500).end("Internal Server Error");
    }
}