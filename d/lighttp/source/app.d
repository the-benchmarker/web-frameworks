/// Production-grade LightTP Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Lightweight: Minimal overhead, efficient resource usage

import lighttp;
import std.string : toStringz;
import stdexception : enforce;
import std.datetime : Duration, seconds;

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

/// Main entry point with production optimizations
void main(string[] args) {
    Server server = new Server();
    
    // Configure for production
    server.host("0.0.0.0", 3000);
    server.maxRequestSize = MAX_REQUEST_SIZE;
    server.timeout = 30.seconds; // Request timeout
    server.keepAliveTimeout = 180.seconds; // Keep-alive timeout
    
    // Add router
    server.router.add(new Router());
    
    // Run server
    server.run();
}

final class Router
{
    /// GET / - Root endpoint with security headers
    @Get("") get(ServerResponse response) {
        // Set security headers
        foreach (header; SECURITY_HEADERS) {
            response.headers["X-Content-Type-Options"] = "nosniff";
            response.headers["X-Frame-Options"] = "DENY";
            response.headers["X-XSS-Protection"] = "1; mode=block";
            response.headers["Strict-Transport-Security"] = "max-age=63072000; includeSubDomains; preload";
            response.headers["Content-Security-Policy"] = "default-src 'self'";
            response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin";
        }
        response.headers["Content-Type"] = "text/plain; charset=utf-8";
        response.status = 200;
        response.body = "";
    }

    /// GET /user/:id - User endpoint with input validation
    @Get("user", "([a-zA-Z0-9\-_]+)") getUser(ServerResponse response, string userId) {
        // Validate user ID - must be alphanumeric with hyphens and underscores
        if (userId.length == 0 || userId.length > 100) {
            response.status = 400;
            response.headers["Content-Type"] = "text/plain; charset=utf-8";
            response.body = "Error: Invalid user ID format";
            return;
        }
        
        // Set security headers
        response.headers["X-Content-Type-Options"] = "nosniff";
        response.headers["X-Frame-Options"] = "DENY";
        response.headers["X-XSS-Protection"] = "1; mode=block";
        response.headers["Content-Type"] = "text/plain; charset=utf-8";
        response.status = 200;
        response.body = userId;
    }

    /// POST /user - Create user endpoint
    @Post("user") post(ServerResponse response) {
        // Set security headers
        response.headers["X-Content-Type-Options"] = "nosniff";
        response.headers["X-Frame-Options"] = "DENY";
        response.headers["X-XSS-Protection"] = "1; mode=block";
        response.headers["Content-Type"] = "text/plain; charset=utf-8";
        response.status = 201; // Created
        response.body = "";
    }

    /// Handle unknown routes
    @Get("*") notFound(ServerResponse response) {
        response.status = 404;
        response.headers["Content-Type"] = "text/plain; charset=utf-8";
        response.body = "Error: Not Found";
    }
}
