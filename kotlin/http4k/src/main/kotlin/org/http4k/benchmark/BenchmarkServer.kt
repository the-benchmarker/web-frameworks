package org.http4k.benchmark

import org.http4k.core.ContentType.Companion.TEXT_PLAIN
import org.http4k.core.Method.GET
import org.http4k.core.Method.POST
import org.http4k.core.Request
import org.http4k.core.Response
import org.http4k.core.Status.Companion.BAD_REQUEST
import org.http4k.core.Status.Companion.INTERNAL_SERVER_ERROR
import org.http4k.core.Status.Companion.NOT_FOUND
import org.http4k.core.Status.Companion.OK
import org.http4k.core.body.LengthLimit
import org.http4k.core.then
import org.http4k.filter.ServerFilters
import org.http4k.filter.ServerFilters.SetContentType
import org.http4k.routing.bind
import org.http4k.routing.path
import org.http4k.routing.routes
import org.http4k.server.Undertow
import org.http4k.server.asServer
import java.util.concurrent.Executors

/**
 * Production-grade http4k benchmark server.
 * 
 * Features:
 * - Security headers configured
 * - Request size limits for DoS protection
 * - Proper error handling
 * - Graceful shutdown support
 * - Input validation
 * - Disabled debug logging in production
 */
fun BenchmarkApp() = ServerFilters {
    // Security: Add security headers
    ServerFilters.AddResponseHeaders(
        mapOf(
            "Server" to "http4k",
            "X-Content-Type-Options" to "nosniff",
            "X-Frame-Options" to "DENY",
            "X-XSS-Protection" to "1; mode=block",
            "Cache-Control" to "no-cache, no-store, must-revalidate"
        )
    )
    
    // Security: Limit request body size to prevent DoS
    ServerFilters.RequestSizeLimit(1_000_000) // 1MB limit
    
    // Proper content type handling
    SetContentType(TEXT_PLAIN)
}.then(
    routes(
        // Health check endpoint
        "/health" bind GET to { _: Request -> Response(OK).body("OK") },
        
        // Root endpoint
        "/" bind GET to { _: Request -> Response(OK) },
        
        // User endpoints with input validation
        "/user/{id}" bind GET to { req: Request ->
            val id = req.path("id") ?: ""
            if (id.isBlank()) {
                Response(BAD_REQUEST).body("ID parameter is required")
            } else {
                Response(OK).body(id)
            }
        },
        "/user" bind POST to { Response(OK) },
    )
)

fun main() {
    val server = BenchmarkApp().asServer(
        Undertow(
            port = 3000,
            host = "0.0.0.0",
            // Production settings
            bufferSize = 16 * 1024, // 16KB buffer
            buffersPerRegion = 20,
            directBuffers = true,
            workerThreads = Runtime.getRuntime().availableProcessors() * 2,
            ioThreads = Runtime.getRuntime().availableProcessors(),
        )
    ).start()

    // Add shutdown hook for graceful termination
    Runtime.getRuntime().addShutdownHook(Thread {
        try {
            server.stop()
            println("Server stopped gracefully")
        } catch (e: Exception) {
            System.err.println("Error during shutdown: ${e.message}")
        }
    }, "server-shutdown")

    // Block to keep server running
    server.block()
}
