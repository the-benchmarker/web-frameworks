package benchmark.vertx;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerRequest;
import io.vertx.core.http.HttpServerResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.vertx.core.http.HttpMethod.GET;
import static io.vertx.core.http.HttpMethod.POST;

/**
 * Vert.x Benchmark Application
 * 
 * <p>A high-performance benchmark server using Vert.x framework.
 * Follows Vert.x best practices including async/non-blocking I/O,
 * proper error handling, and logging.</p>
 */
public class BenchmarkApplication extends AbstractVerticle {

    private static final Logger LOG = LoggerFactory.getLogger(BenchmarkApplication.class);
    
    private HttpServer server;

    @Override
    public void start(Promise<Void> onReady) {
        // Get port from environment or use default
        int port = Integer.parseInt(System.getenv().getOrDefault("PORT", "3000"));
        String host = System.getenv().getOrDefault("HOST", "0.0.0.0");

        LOG.info("Starting Vert.x benchmark server on {}:{}", host, port);
        LOG.debug("Event Loop Size: {}", VertxOptions.DEFAULT_EVENT_LOOP_POOL_SIZE);
        LOG.debug("Native transport enabled: {}", vertx.isNativeTransportEnabled());

        server = vertx
                .createHttpServer()
                .requestHandler(this::handleRequests)
                .listen(port, host);

        server
                .onFailure(throwable -> {
                    LOG.error("Failed to start server: {}", throwable.getMessage(), throwable);
                    onReady.fail(throwable);
                })
                .onSuccess(success -> {
                    LOG.info("Server started successfully on port {}", port);
                    onReady.complete();
                });
    }

    @Override
    public void stop(Promise<Void> onStop) {
        LOG.info("Stopping Vert.x benchmark server");
        if (server != null) {
            server.close()
                    .onFailure(throwable -> {
                        LOG.error("Error stopping server: {}", throwable.getMessage(), throwable);
                        onStop.fail(throwable);
                    })
                    .onSuccess(success -> {
                        LOG.info("Server stopped successfully");
                        onStop.complete();
                    });
        } else {
            onStop.complete();
        }
    }

    /**
     * Handle incoming HTTP requests.
     * 
     * @param request The HTTP server request
     */
    private void handleRequests(HttpServerRequest request) {
        LOG.debug("{} {}", request.method(), request.path());
        
        try {
            String path = request.path();
            HttpMethod method = request.method();
            HttpServerResponse response = request.response();
            
            // Configure response for benchmarking
            response.putHeader("Content-Type", "text/plain");

            // GET requests
            if (method == GET) {
                if (path.equals("/")) {
                    handleRoot(response);
                } else if (path.startsWith("/user/")) {
                    handleGetUser(response, path);
                } else if (path.equals("/health")) {
                    handleHealthCheck(response);
                } else {
                    handleNotFound(response, path);
                }
            } else if (method == POST) {
                if (path.equals("/user")) {
                    handleCreateUser(response);
                } else {
                    handleNotFound(response, path);
                }
            } else {
                handleMethodNotAllowed(response, method);
            }
        } catch (Exception e) {
            handleException(request.response(), e);
        }
    }

    /**
     * Handle root endpoint.
     * 
     * @param response The HTTP server response
     */
    private void handleRoot(HttpServerResponse response) {
        LOG.debug("Root endpoint accessed");
        response.setStatusCode(200).end();
    }

    /**
     * Handle GET /user/{id} endpoint.
     * 
     * @param response The HTTP server response
     * @param path The request path
     */
    private void handleGetUser(HttpServerResponse response, String path) {
        String id = path.substring(6); // Remove "/user/" prefix
        LOG.debug("User endpoint accessed with ID: {}", id);
        response.setStatusCode(200).end(id);
    }

    /**
     * Handle POST /user endpoint.
     * 
     * @param response The HTTP server response
     */
    private void handleCreateUser(HttpServerResponse response) {
        LOG.debug("Create user endpoint accessed");
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
     * @param path The request path
     */
    private void handleNotFound(HttpServerResponse response, String path) {
        LOG.warn("Path not found: {}", path);
        response.setStatusCode(404).end("Not Found");
    }

    /**
     * Handle 405 Method Not Allowed.
     * 
     * @param response The HTTP server response
     * @param method The HTTP method
     */
    private void handleMethodNotAllowed(HttpServerResponse response, HttpMethod method) {
        LOG.warn("Method not allowed: {}", method);
        response.setStatusCode(405).end("Method Not Allowed");
    }

    /**
     * Handle exceptions.
     * 
     * @param response The HTTP server response
     * @param exception The exception that was thrown
     */
    private static void handleException(HttpServerResponse response, Exception exception) {
        LOG.error("Unhandled exception: {}", exception.getMessage(), exception);
        response.setStatusCode(500).end("Internal Server Error");
    }

    /**
     * Error response helper.
     * 
     * @param response The HTTP server response
     * @param msg The error message
     */
    @Deprecated
    private static void errorResponse(HttpServerResponse response, String msg) {
        response.setStatusCode(500).end("Incorrect HTTP call: " + msg);
    }
}