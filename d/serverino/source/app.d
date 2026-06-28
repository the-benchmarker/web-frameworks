/// Production-grade Serverino Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Scalability: Multi-instance, worker-based architecture

module app;

import serverino;
import std.datetime : Duration, seconds, milliseconds;
import std.array : split;
import std.algorithm : startsWith;
import std.parallelism : totalCPUs;
import std.string : toStringz;
import stdexception : enforce;

/// Security headers for all responses
private enum SECURITY_HEADERS = [
    "X-Content-Type-Options: nosniff",
    "X-Frame-Options: DENY",
    "X-XSS-Protection: 1; mode=block",
    "Strict-Transport-Security: max-age=63072000; includeSubDomains; preload",
    "Content-Security-Policy: default-src 'self'",
    "Referrer-Policy: strict-origin-when-cross-origin"
];

/// Maximum request size to prevent DoS attacks
private enum MAX_REQUEST_SIZE = 1024 * 1024; // 1MB

/// Worker configuration
private enum WORKER_COUNT = 8;

/// Timeout configurations for production
private enum HTTP_TIMEOUT = 30.seconds;
private enum KEEP_ALIVE_TIMEOUT = 180.seconds;
private enum GRACEFUL_SHUTDOWN_TIMEOUT = 30.seconds;

mixin ServerinoMain;

/// Server configuration with production optimizations
@onServerInit ServerinoConfig configure()
{
	return ServerinoConfig
        .create()
        // Performance optimizations
        .setHttpTimeout(HTTP_TIMEOUT)
        .enableKeepAlive(KEEP_ALIVE_TIMEOUT)
        .setGracefulShutdownTimeout(GRACEFUL_SHUTDOWN_TIMEOUT)
        
        // Network configuration
        .addListener("0.0.0.0", 3000)
        
        // Resource management
        .setDaemonInstances(max(1, totalCPUs / 2)) // Optimize for available CPUs
        .setWorkers(WORKER_COUNT)
        .setMaxRequestSize(MAX_REQUEST_SIZE)
        
        // Security settings
        .setServerName("Production-Serverino")
        .enableCompression(true); // Enable gzip compression
}

/// Main endpoint handler with comprehensive routing and security
@safe
@endpoint void hello(Request req, Output output) {
    // Set security headers
    foreach (header; SECURITY_HEADERS) {
        output.headers ~= header;
    }
    
    // Set content type
    output.headers ~= "Content-Type: text/plain; charset=utf-8";

    // Route requests with proper error handling
    try {
        if (req.uri == "/" && req.method == Request.Method.Get) {
            output.status = 200;
            output.body = "";
        } else if (req.uri == "/user" && req.method == Request.Method.Post) {
            output.status = 201; // Created
            output.body = "";
        } else if (req.uri.startsWith("/user/") && req.method == Request.Method.Get) {
            handleUserRequest(req, output);
        } else {
            // Unknown route
            output.status = 404;
            output.body = "Error: Not Found\n";
        }
    } catch (Exception e) {
        // Log error in production
        stderr.writeln("ERROR: ", e.msg);
        output.status = 500;
        output.body = "Internal Server Error\n";
    }
}

/// Handle user requests with input validation
private void handleUserRequest(Request req, Output output) {
    // Extract user ID from URI
    string userPath = req.uri[6..$]; // Remove "/user/" prefix
    
    // Validate user ID
    if (userPath.length == 0) {
        output.status = 400;
        output.body = "Error: Missing user ID\n";
        return;
    }
    
    // Validate that userPath contains only safe characters
    foreach (char c; userPath) {
        if (!isUrlSafeChar(c)) {
            output.status = 400;
            output.body = "Error: Invalid user ID format\n";
            return;
        }
    }
    
    // Limit user ID length
    if (userPath.length > 100) {
        output.status = 400;
        output.body = "Error: User ID too long\n";
        return;
    }
    
    output.status = 200;
    output.body = userPath;
}

/// Check if a character is safe for URL paths
private bool isUrlSafeChar(char c) {
    // Allow alphanumeric, hyphen, underscore, period, and forward slash
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') ||
           c == '-' || c == '_' || c == '.' || c == '/';
}
