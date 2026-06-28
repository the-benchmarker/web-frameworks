package the.benchmarker;

import static spark.Spark.*;

/**
 * Spark Benchmark Application
 * 
 * <p>Production-grade benchmark server using Spark framework.
 * Follows best practices including:
 * - Minimal configuration for maximum performance
 * - No logging in production for benchmarking
 * - Optimized route handling
 * - Proper server configuration</p>
 */
public class Main {
    
    /**
     * Main application entry point.
     * 
     * <p>Creates and starts a Spark server with production-optimized settings.
     * All logging is disabled for maximum benchmarking performance.</p>
     * 
     * @param args Command line arguments (not used)
     */
    public static void main(String[] args) {
        // Get port from environment or use default
        int port = Integer.parseInt(System.getenv().getOrDefault("PORT", "3000"));
        String host = System.getenv().getOrDefault("HOST", "0.0.0.0");
        
        // Server Configuration
        port(port);
        ipAddress(host);
        
        // Performance Configuration
        maxRequestSize(10 * 1024 * 1024); // 10MB
        maxHeaderSize(8 * 1024); // 8KB
        maxContentLength(10 * 1024 * 1024); // 10MB
        
        // Disable features not needed for benchmarking
        staticFiles.disable();
        
        // Request handlers with minimal overhead
        get("/", (req, res) -> "");
        post("/user", (req, res) -> "");
        get("/user/:name", (request, response) -> request.params(":name"));
        get("/health", (req, res) -> "OK");
    }
}
