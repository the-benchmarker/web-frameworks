package benchmark

import io.jooby.Context
import io.jooby.ExecutionMode.EVENT_LOOP
import io.jooby.ServerOptions
import io.jooby.StatusCode
import io.jooby.kt.Kooby
import io.jooby.kt.runApp
import io.jooby.netty.NettyServer

/**
 * Production-grade Jooby benchmark server.
 * 
 * Features:
 * - Security headers configured
 * - Proper error handling
 * - Graceful shutdown support
 * - Input validation
 * - Disabled debug headers in production
 * - Optimized Netty configuration
 */
class App : Kooby({
    // Security: Configure default security headers
    use { ctx: Context ->
        ctx.response.header("Server", "Jooby")
        ctx.response.header("X-Content-Type-Options", "nosniff")
        ctx.response.header("X-Frame-Options", "DENY")
        ctx.response.header("X-XSS-Protection", "1; mode=block")
        ctx.response.header("Cache-Control", "no-cache, no-store, must-revalidate")
        ctx.next()
    }

    // Health check endpoint
    get("/health") { ctx.send(StatusCode.OK, "OK") }

    // Root endpoint
    get("/") { ctx.send(StatusCode.OK) }

    // User endpoints with input validation
    get("/user/{id}") { 
        val id = ctx.path("id").value()
        if (id.isNullOrBlank()) {
            ctx.send(StatusCode.BAD_REQUEST, "ID parameter is required")
        } else {
            ctx.send(StatusCode.OK, id)
        }
    }

    post("/user") { ctx.send(StatusCode.OK) }

    // Error handling
    error { ctx, cause ->
        System.err.println("Error: ${cause.message}")
        ctx.send(StatusCode.INTERNAL_SERVER_ERROR, "Internal Server Error")
    }
})

fun main(args: Array<String>) {
    val options = ServerOptions().apply {
        port = 3000
        defaultHeaders = false
        // Production settings
        ioThreads = Runtime.getRuntime().availableProcessors()
        workerThreads = Runtime.getRuntime().availableProcessors() * 2
    }
    
    runApp(args, NettyServer(options), EVENT_LOOP, ::App)
}
