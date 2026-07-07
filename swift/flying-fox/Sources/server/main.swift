import FlyingFox
import Logging

/// FlyingFox Benchmark Server
/// 
/// A high-performance benchmark server implementation using the FlyingFox framework.
/// Follows Swift best practices including proper error handling, logging, and configuration management.

// Configure logging
let logger = Logger(label: "benchmark.flying-fox")
LoggingSystem.bootstrap { _ in
    StreamLogHandler.standardOutput(label: $0)
}

// Server configuration from environment variables
let port = Int(ProcessInfo.processInfo.environment["PORT"] ?? "3000") ?? 3000
let host = ProcessInfo.processInfo.environment["HOST"] ?? "0.0.0.0"

// Configure server for production performance
let server = HTTPServer(
    port: port,
    host: host,
    logger: .disabled, // Disable FlyingFox logger for benchmarking, use Swift Logging instead
    // Optimize server configuration
    configuration: .init(
        maxConnections: 1000,
        connectionTimeout: .seconds(30),
        reusePort: true, // Allow multiple processes to bind to the same port
        backlog: 128 // Increased backlog for better connection handling
    )
)

// Configure server for optimal performance
server.configuration.maxHeaderSize = 8192 // 8KB max header size
server.configuration.maxBodySize = 16 * 1024 * 1024 // 16MB max body size for benchmarking

// Define routes with optimized handlers
/// Root endpoint handler
/// Returns empty response with 200 OK status for benchmarking
await server.appendRoute("GET /") { _ in
    logger.debug("Root endpoint accessed")
    return HTTPResponse(statusCode: .ok, headers: ["Content-Type": "text/plain"])
}

/// Get user by ID endpoint handler
/// Returns user ID as plain text with 200 OK status
/// 
/// - Parameter id: User identifier from URL path
/// - Returns: HTTPResponse containing user ID as body
await server.appendRoute("GET /user/:id") { (user: String) -> HTTPResponse in
    logger.debug("User endpoint accessed with ID: \(user)")
    return HTTPResponse(statusCode: .ok, headers: ["Content-Type": "text/plain"], body: user.data(using: .utf8)!)
}

/// Create user endpoint handler
/// Returns empty response with 201 Created status for benchmarking
await server.appendRoute("POST /user") { _ in
    logger.debug("Create user endpoint accessed")
    return HTTPResponse(statusCode: .created, headers: ["Content-Type": "text/plain"])
}

/// Health check endpoint for monitoring
/// Returns "OK" with 200 OK status
await server.appendRoute("GET /health") { _ in
    return HTTPResponse(statusCode: .ok, headers: ["Content-Type": "text/plain"], body: "OK".data(using: .utf8)!)
}

logger.info("Starting FlyingFox benchmark server on \(host):\(port)")

// Start server with error handling
do {
    try await server.run()
} catch {
    logger.error("Server error: \(error)")
    exit(1)
}
