import Hummingbird
import Logging

/// Hummingbird Benchmark Server
/// 
/// A high-performance benchmark server implementation using the Hummingbird framework.
/// Follows Swift best practices including proper error handling, logging, and configuration management.

@main
struct Server {
    /// Application logger
    private static let logger = Logger(label: "benchmark.hummingbird")

    static func main() async throws {
        // Configure logging
        LoggingSystem.bootstrap { _ in
            StreamLogHandler.standardOutput(label: $0)
        }

        let env = Environment()
        let serverHostName = env.get("SERVER_HOSTNAME") ?? env.get("HOST") ?? "0.0.0.0"
        let serverPort = env.get("SERVER_PORT", as: Int.self) ?? env.get("PORT", as: Int.self) ?? 3000

        logger.info("Starting Hummingbird benchmark server on \(serverHostName):\(serverPort)")

        // Configure router for production
        let router = Router()
        
        /// Root endpoint handler
        /// Returns empty response with 200 OK status for benchmarking
        router.get("/") { _, _ in
            logger.debug("Root endpoint accessed")
            return HTTPResponse(status: .ok, headers: ["Content-Type": "text/plain"])
        }

        /// Get user by ID endpoint handler
        /// Returns user ID as plain text with 200 OK status
        /// 
        /// - Parameter ctx: Request context containing path parameters
        /// - Returns: User ID as String
        router.get("user/:id") { _, ctx -> HTTPResponse in
            let userId = ctx.parameters.get("id") ?? ""
            logger.debug("User endpoint accessed with ID: \(userId)")
            return HTTPResponse(status: .ok, headers: ["Content-Type": "text/plain"], body: .string(userId))
        }

        /// Create user endpoint handler
        /// Returns empty response with 201 Created status for benchmarking
        router.post("user") { _, _ in
            logger.debug("Create user endpoint accessed")
            return HTTPResponse(status: .created, headers: ["Content-Type": "text/plain"])
        }

        /// Health check endpoint for monitoring
        /// Returns "OK" with 200 OK status
        router.get("/health") { _, _ in
            return HTTPResponse(status: .ok, headers: ["Content-Type": "text/plain"], body: .string("OK"))
        }

        // Configure application for production performance
        let configuration = Application.Configuration(
            address: .hostname(serverHostName, port: serverPort),
            serverConfiguration: .init(
                reusePort: true, // Allow multiple processes to bind to the same port
                backlog: 128, // Increased connection backlog
                maxConcurrentConnections: 1000, // Maximum concurrent connections
                connectionTimeout: .seconds(30) // Connection timeout
            ),
            routerConfiguration: .init(
                maxBodySize: 16 * 1024 * 1024, // 16MB max request body size for benchmarking
                maxHeaderSize: 8192 // 8KB max header size
            ),
            loggingConfiguration: .init(
                logLevel: .critical // Only log critical errors for benchmarking
            )
        )

        let app = Application(
            router: router,
            configuration: configuration
        )

        // Configure for optimal performance
        app.server.configuration.responseCompression = .enabled // Enable compression
        app.server.configuration.tlsConfiguration = nil // HTTP only for benchmarking

        try await app.run()
    }
}
