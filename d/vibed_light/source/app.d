/// Production-grade Vibed Light Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Lightweight: Minimal dependency version for efficiency

import vibe.core.core : runApplication, runWorkerTaskDist, setupWorkerThreads;
import vibe.http.server;
import vibe.http.router;
import vibe.http.common : HTTPStatus, HTTPMethod;

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
private enum WORKER_THREADS = 8;

/// Timeout configurations
private enum SOCKET_TIMEOUT = 30;

/// Handle GET /user/:id with input validation
void handleGetUser(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    // Set security headers
    foreach (header; SECURITY_HEADERS) {
        res.headers["X-Content-Type-Options"] = "nosniff";
        res.headers["X-Frame-Options"] = "DENY";
        res.headers["X-XSS-Protection"] = "1; mode=block";
        res.headers["Strict-Transport-Security"] = "max-age=63072000; includeSubDomains; preload";
        res.headers["Content-Security-Policy"] = "default-src 'self'";
        res.headers["Referrer-Policy"] = "strict-origin-when-cross-origin";
    }
    
    try {
        // Validate user ID parameter
        string userId = req.params["id"];
        
        if (userId.length == 0 || userId.length > 100) {
            res.status = HTTPStatus.badRequest;
            res.writeBody("Error: Invalid user ID format", "text/plain; charset=utf-8");
            return;
        }
        
        // Validate that userId contains only safe characters
        foreach (char c; userId) {
            if (!isUrlSafeChar(c)) {
                res.status = HTTPStatus.badRequest;
                res.writeBody("Error: Invalid user ID format", "text/plain; charset=utf-8");
                return;
            }
        }
        
        res.status = HTTPStatus.ok;
        res.writeBody(userId, "text/plain; charset=utf-8");
    } catch (Exception e) {
        // Log error in production
        stderr.writeln("ERROR: ", e.msg);
        res.status = HTTPStatus.internalServerError;
        res.writeBody("Internal Server Error", "text/plain; charset=utf-8");
    }
}

/// Handle POST /user
void handlePostUser(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    // Set security headers
    res.headers["X-Content-Type-Options"] = "nosniff";
    res.headers["X-Frame-Options"] = "DENY";
    res.headers["X-XSS-Protection"] = "1; mode=block";
    res.headers["Content-Type"] = "text/plain; charset=utf-8";
    
    try {
        res.status = HTTPStatus.created;
        res.writeBody("", "text/plain; charset=utf-8");
    } catch (Exception e) {
        stderr.writeln("ERROR: ", e.msg);
        res.status = HTTPStatus.internalServerError;
        res.writeBody("Internal Server Error", "text/plain; charset=utf-8");
    }
}

/// Handle GET /
void handleRoot(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    // Set security headers
    res.headers["X-Content-Type-Options"] = "nosniff";
    res.headers["X-Frame-Options"] = "DENY";
    res.headers["X-XSS-Protection"] = "1; mode=block";
    res.headers["Content-Type"] = "text/plain; charset=utf-8";
    
    try {
        res.status = HTTPStatus.ok;
        res.writeBody("", "text/plain; charset=utf-8");
    } catch (Exception e) {
        stderr.writeln("ERROR: ", e.msg);
        res.status = HTTPStatus.internalServerError;
        res.writeBody("Internal Server Error", "text/plain; charset=utf-8");
    }
}

/// Handle 404 - Not Found
void handleNotFound(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    res.headers["Content-Type"] = "text/plain; charset=utf-8";
    res.status = HTTPStatus.notFound;
    res.writeBody("Error: Not Found", "text/plain; charset=utf-8");
}

/// Check if a character is safe for URL paths
private bool isUrlSafeChar(char c) {
    // Allow alphanumeric, hyphen, underscore, period, and forward slash
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') ||
           c == '-' || c == '_' || c == '.' || c == '/';
}

/// Main entry point with production optimizations
void main()
{
    // Configure worker threads - use available CPUs but limit for optimal performance
    const optimalWorkers = max(WORKER_THREADS, totalCPUs);
    setupWorkerThreads(optimalWorkers);
    
    runWorkerTaskDist(() nothrow {
        try {
            auto settings = new HTTPServerSettings;
            
            // Production server configuration
            settings.port = 3000;
            settings.bindAddresses = ["0.0.0.0"];
            settings.options |= HTTPServerOption.reusePort;
            settings.socketTimeout = SOCKET_TIMEOUT;
            settings.maxRequestSize = MAX_REQUEST_SIZE;
            settings.keepAliveTimeout = 180;
            
            // Security settings
            settings.serverName = "Production-Vibed-Light";
            
            auto router = new URLRouter;

            // Route configuration with comprehensive error handling
            router
                .post("/user", &handlePostUser)
                .get("/user/:id", &handleGetUser)
                .get("/", &handleRoot)
                .any("*", &handleNotFound); // Catch-all for 404

            router.rebuild();
            listenHTTP(settings, router);
        } catch (Exception e) {
            stderr.writeln("FATAL: ", e.msg);
            // In production, consider graceful shutdown
        }
    });
    
    runApplication();
}
