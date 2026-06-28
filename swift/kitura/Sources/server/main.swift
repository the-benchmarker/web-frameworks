import Kitura
import KituraNet
import LoggerAPI
import Foundation

/// Kitura Benchmark Server
/// 
/// A high-performance benchmark server implementation using the Kitura framework.
/// Follows Swift best practices including proper error handling, logging, and configuration management.

// Configure logging
Log.logger = HeliumLogger()

// Server configuration from environment variables
let port = Int(ProcessInfo.processInfo.environment["PORT"] ?? "3000") ?? 3000
let host = ProcessInfo.processInfo.environment["HOST"] ?? "0.0.0.0"

let router = Router()

// Configure for production performance - disable body parser for benchmarking
// router.all(middleware: BodyParser())

/// Root endpoint handler
/// Returns empty response with 200 OK status for benchmarking
router.get("/") { _, res, next in
    Log.debug("Root endpoint accessed")
    res.statusCode = .OK
    res.headers["Content-Type"] = "text/plain"
    try res.send("").end()
    next()
}

/// Get user by ID endpoint handler
/// Returns user ID as plain text with 200 OK status
/// 
/// - Parameter req: Request containing path parameters
/// - Parameter res: Response to send
/// - Parameter next: Next middleware function
router.get("/user/:id") { req, res, next in
    let userId = req.parameters["id"] ?? ""
    Log.debug("User endpoint accessed with ID: \(userId)")
    res.statusCode = .OK
    res.headers["Content-Type"] = "text/plain"
    try res.send(userId).end()
    next()
}

/// Create user endpoint handler
/// Returns empty response with 201 Created status for benchmarking
router.post("/user") { _, res, next in
    Log.debug("Create user endpoint accessed")
    res.statusCode = .created
    res.headers["Content-Type"] = "text/plain"
    try res.send("").end()
    next()
}

/// Health check endpoint for monitoring
/// Returns "OK" with 200 OK status
router.get("/health") { _, res, next in
    res.statusCode = .OK
    res.headers["Content-Type"] = "text/plain"
    try res.send("OK").end()
    next()
}

// Configure server for optimal performance
let server = HTTPServer.listen(
    on: port,
    with: router,
    backlog: 128, // Increased connection backlog
    reusePort: true, // Allow multiple processes to bind to the same port
    maxPendingConnections: 1000 // Maximum pending connections
)

// Configure server settings
server.maxHeaderSize = 8192 // 8KB max header size
server.maxBodySize = 16 * 1024 * 1024 // 16MB max body size for benchmarking
server.connectionTimeout = 30 // 30 second connection timeout

Log.info("Starting Kitura benchmark server on \(host):\(server.port)")

Kitura.run()
