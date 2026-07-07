import Swifter
import Dispatch
import Foundation

/// Swifter Benchmark Server
/// 
/// A high-performance benchmark server implementation using the Swifter framework.
/// Follows Swift best practices including proper error handling, logging, and configuration management.

// Configure server for production performance
let server = HttpServer()

// Configure server settings for benchmarking
server.maxChunkSize = 8192 // 8KB max chunk size
server.maxHeaderCount = 100 // Maximum header count
server.maxHeaderSize = 8192 // 8KB max header size
server.keepAlive = true // Enable keep-alive connections
server.pipeline = true // Enable pipelining
server.reusePort = true // Allow multiple processes to bind to the same port

// Server configuration from environment variables
let port = Int(ProcessInfo.processInfo.environment["PORT"] ?? "3000") ?? 3000

// Define routes
/// Root endpoint handler
/// Returns empty response with 200 OK status for benchmarking
server.GET["/"] = { _ in
    print("DEBUG: Root endpoint accessed")
    return HttpResponse.ok(.text(""))
}

/// Get user by ID endpoint handler
/// Returns user ID as plain text with 200 OK status
/// 
/// - Parameter request: HTTP request containing path parameters
/// - Returns: HTTP response with user ID as body
server.GET["/user/:id"] = { request in
    let userId = request.params[":id"] ?? ""
    print("DEBUG: User endpoint accessed with ID: \(userId)")
    return HttpResponse.ok(.text(userId))
}

/// Create user endpoint handler
/// Returns empty response with 201 Created status for benchmarking
server.POST["/user"] = { _ in
    print("DEBUG: Create user endpoint accessed")
    return HttpResponse.created(.text(""))
}

/// Health check endpoint for monitoring
/// Returns "OK" with 200 OK status
server.GET["/health"] = { _ in
    return HttpResponse.ok(.text("OK"))
}

print("Starting Swifter benchmark server on port \(port)")

let semaphore = DispatchSemaphore(value: 0)
do {
    try server.start(port, forceIPv4: true)
    print("Swifter server has started on port \(try server.port()). Ready for connections...")
    semaphore.wait()
} catch {
    print("Server start error: \(error)")
    semaphore.signal()
    exit(1)
}