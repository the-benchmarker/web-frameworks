package benchmark.javalin;

import io.javalin.Javalin;
import io.javalin.http.Handler;

/**
 * Javalin Benchmark Application
 * 
 * <p>Production-grade benchmark server using Javalin framework.
 * Follows best practices including:
 * - Minimal configuration for maximum performance
 * - No logging in production for benchmarking
 * - Optimized route handling
 * - Proper server configuration</p>
 */
public class Bench {
    
    /**
     * Main application entry point.
     * 
     * <p>Creates and starts a Javalin server with production-optimized settings.
     * All logging is disabled for maximum benchmarking performance.</p>
     * 
     * @param args Command line arguments (not used)
     */
    public static void main(String[] args) {
        // Get port from environment or use default
        int port = Integer.parseInt(System.getenv().getOrDefault("PORT", "3000"));
        String host = System.getenv().getOrDefault("HOST", "0.0.0.0");

        Javalin app = Javalin.create(config -> {
            // Server Configuration
            config.server.host = host;
            config.server.port = port;
            
            // Performance Configuration
            config.server.maxRequestSize = 10 * 1024 * 1024; // 10MB
            config.server.maxHeaderSize = 8 * 1024; // 8KB
            config.server.maxBodySize = 10 * 1024 * 1024; // 10MB
            
            // Connection Configuration
            config.server.connectionIdleTimeout = 0; // No idle timeout for benchmarking
            config.server.connectionTimeout = 30000; // 30 seconds
            config.server.requestTimeout = 30000; // 30 seconds
            
            // Disable features not needed for benchmarking
            config.server.compressionEnabled = false;
            config.server.enableCors = false;
            config.server.enableDebug = false;
            config.server.enableRequestLogging = false;
            
            // Jetty-specific configuration
            config.jetty.serverThreadPoolThreads = 0; // Use default
            config.jetty.acceptorThreads = 1;
            config.jetty.selectorThreads = 2;
            
            // Request handlers with minimal overhead
            config.routes.get("/", new Handler() {
                @Override
                public void handle(io.javalin.http.Context ctx) throws Exception {
                    ctx.result("");
                }
            });
            
            config.routes.get("/user/{id}", new Handler() {
                @Override
                public void handle(io.javalin.http.Context ctx) throws Exception {
                    String id = ctx.pathParam("id");
                    ctx.result(id);
                }
            });
            
            config.routes.post("/user", new Handler() {
                @Override
                public void handle(io.javalin.http.Context ctx) throws Exception {
                    ctx.result("");
                }
            });
            
            config.routes.get("/health", new Handler() {
                @Override
                public void handle(io.javalin.http.Context ctx) throws Exception {
                    ctx.result("OK");
                }
            });
        }).start();
    }
}
