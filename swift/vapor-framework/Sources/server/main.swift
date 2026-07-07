import Vapor

/// Vapor Benchmark Server
/// 
/// A high-performance benchmark server implementation using the Vapor framework.
/// Follows Swift best practices including proper error handling, logging, and configuration management.

var env = Environment(name: Environment.get("VAPOR_ENV") ?? "production")
try LoggingSystem.bootstrap(from: &env)

let app = Application()
defer { app.shutdown() }
app.middleware = .init()

// Configure logging - set to debug for benchmarking, can be overridden by VAPOR_LOG
app.logger.logLevel = .debug

/// Root endpoint handler
/// Returns empty response with 200 OK status for benchmarking
app.get { _ in
    app.logger.debug("Root endpoint accessed")
    return Response(status: .ok, headers: ["Content-Type": "text/plain"])
}

/// Get user by ID endpoint handler
/// Returns user ID as plain text with 200 OK status
/// 
/// - Parameter req: Request containing path parameters
/// - Returns: User ID as String response
app.get("user", ":userID") { req -> Response in
    let userId = req.parameters.get("userID") ?? ""
    app.logger.debug("User endpoint accessed with ID: \(userId)")
    return Response(status: .ok, headers: ["Content-Type": "text/plain"], body: .string(userId))
}

/// Create user endpoint handler
/// Returns empty response with 201 Created status for benchmarking
app.post("user") { _ in
    app.logger.debug("Create user endpoint accessed")
    return Response(status: .created, headers: ["Content-Type": "text/plain"])
}

/// Health check endpoint for monitoring
/// Returns "OK" with 200 OK status
app.get("health") { _ in
    return Response(status: .ok, headers: ["Content-Type": "text/plain"], body: .string("OK"))
}

// Configure server from environment variables
app.http.server.configuration.hostname = Environment.get("SERVER_HOSTNAME") ?? Environment.get("HOST") ?? "0.0.0.0"
app.http.server.configuration.port = Int(Environment.get("SERVER_PORT") ?? Environment.get("PORT") ?? "3000") ?? 3000

// Configure for benchmarking performance
app.http.server.configuration.backlog = 128 // Increased connection backlog
app.http.server.configuration.reuseAddress = true // Allow address reuse
app.http.server.configuration.maxBodySize = 16 * 1024 * 1024 // 16MB max body size for benchmarking
app.http.server.configuration.maxHeaderSize = 8192 // 8KB max header size

app.logger.info("Starting Vapor benchmark server on \(app.http.server.configuration.hostname):\(app.http.server.configuration.port)")

try app.run()
