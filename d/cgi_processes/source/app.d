/// Production-grade CGI Processes Server
/// 
/// This implementation demonstrates best practices for:
/// - Security: Input validation, secure headers, error handling
/// - Performance: Optimized for production with debug disabled
/// - Maintainability: Clear structure, documentation, consistent style
/// - Reliability: Graceful error handling, resource cleanup
/// - Process isolation: Each request handled in separate process

module app;

import arsd.cgi;
import std.algorithm : startsWith;
import std.string : toStringz;
import stdexception : enforce;

/// Maximum request size to prevent DoS attacks
private enum MAX_REQUEST_SIZE = 1024 * 1024; // 1MB

/// Maximum concurrent processes
private enum MAX_PROCESSES = 100;

/// Maximum process lifetime to prevent memory leaks
private enum MAX_PROCESS_LIFETIME = 60 * 60; // 1 hour in seconds

/// Security headers for all responses
private enum SECURITY_HEADERS = [
    "X-Content-Type-Options: nosniff",
    "X-Frame-Options: DENY",
    "X-XSS-Protection: 1; mode=block",
    "Strict-Transport-Security: max-age=63072000; includeSubDomains; preload",
    "Content-Security-Policy: default-src 'self'",
    "Referrer-Policy: strict-origin-when-cross-origin"
];

/// Main request handler with comprehensive error handling
void handler(Cgi cgi) {
    // Set security headers
    foreach (header; SECURITY_HEADERS) {
        cgi.setResponseHeader(header);
    }
    cgi.setResponseContentType("text/plain; charset=utf-8");

    // Validate and sanitize path
    try {
        // Limit request size
        if (cgi.contentLength > MAX_REQUEST_SIZE) {
            cgi.setResponseStatus(413, "Payload Too Large");
            cgi.write("Error: Request too large\n", true);
            return;
        }

        // Route requests
        if (startsWith(cgi.pathInfo, "/user")) {
            handleUserRequest(cgi);
        } else {
            cgi.setResponseStatus(200, "OK");
            cgi.write("", true);
        }
    } catch (Exception e) {
        // Log error in production (would be logged to stderr)
        // In production, consider using a proper logging framework
        stderr.writeln("ERROR: ", e.msg);
        
        cgi.setResponseStatus(500, "Internal Server Error");
        cgi.write("Internal Server Error\n", true);
    }
}

/// Handle /user* routes with input validation
private void handleUserRequest(Cgi cgi) {
    if (cgi.requestMethod == Cgi.RequestMethod.POST) {
        cgi.setResponseStatus(201, "Created");
        cgi.write("", true);
    } else if (cgi.requestMethod == Cgi.RequestMethod.GET) {
        // Validate user ID from path
        string userPath = cgi.pathInfo[6..$]; // Remove "/user" prefix
        
        // Basic validation - user ID should not be empty and should be URL-safe
        if (userPath.length == 0) {
            cgi.setResponseStatus(400, "Bad Request");
            cgi.write("Error: Missing user ID\n", true);
            return;
        }
        
        // Validate that userPath contains only safe characters
        foreach (char c; userPath) {
            if (!isUrlSafeChar(c)) {
                cgi.setResponseStatus(400, "Bad Request");
                cgi.write("Error: Invalid user ID format\n", true);
                return;
            }
        }
        
        cgi.setResponseStatus(200, "OK");
        cgi.write(userPath, true);
    } else {
        cgi.setResponseStatus(405, "Method Not Allowed");
        cgi.write("Error: Method not allowed\n", true);
    }
}

/// Check if a character is safe for URL paths
private bool isUrlSafeChar(char c) {
    // Allow alphanumeric, hyphen, underscore, period, and forward slash
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') ||
           c == '-' || c == '_' || c == '.' || c == '/';
}

/// Main entry point with production optimizations for process-based server
void main() {
    // In production, consider using a configuration file for these settings
    const port = 3000;
    const host = "0.0.0.0";

    // Create server with production settings for process isolation
    auto server = RequestServer(port);
    
    // Configure for production with process isolation
    server.host = host;
    server.maxProcesses = MAX_PROCESSES;
    server.processLifetime = MAX_PROCESS_LIFETIME;
    server.maxRequestSize = MAX_REQUEST_SIZE;

    // Start serving requests
    server.serve!handler;
}
