package com.hexagonkt

import com.hexagonkt.core.media.TEXT_PLAIN
import com.hexagonkt.core.require
import com.hexagonkt.http.model.ContentType
import com.hexagonkt.http.server.HttpServer
import com.hexagonkt.http.server.HttpServerSettings
import com.hexagonkt.http.server.netty.epoll.NettyEpollServerAdapter
import com.hexagonkt.http.server.serve
import java.net.InetAddress

/**
 * Production-grade Hexagon Netty Epoll benchmark server.
 * 
 * Features:
 * - Epoll-based high-performance configuration for Linux
 * - Security headers configured
 * - Proper error handling
 * - Graceful shutdown support
 * - Input validation
 * - Disabled debug features for performance
 */
fun main() {
    val textPlain = ContentType(TEXT_PLAIN)
    
    val adapter = NettyEpollServerAdapter(
        executorThreads = Runtime.getRuntime().availableProcessors() * 2,
        keepAliveHandler = false,
        httpAggregatorHandler = false,
        chunkedHandler = false,
        enableWebsockets = false,
        epoll = true,
        isCompression = false,
    ).apply {
        isLogActivity = false
    }

    val settings = HttpServerSettings(
        host = InetAddress.getByName("0.0.0.0"),
        port = 3000,
        connectionTimeout = 30_000,
        idleTimeout = 60_000,
    )

    val server: HttpServer = serve(adapter, settings) {
        filter { next ->
            { request ->
                val response = next(request)
                response.apply {
                    headers["Server"] = "Hexagon-Netty-Epoll"
                    headers["X-Content-Type-Options"] = "nosniff"
                    headers["X-Frame-Options"] = "DENY"
                    headers["X-XSS-Protection"] = "1; mode=block"
                    headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
                }
            }
        }

        get("/health") {
            ok("OK", contentType = textPlain)
        }

        get {
            ok(contentType = textPlain)
        }

        path("/user") {
            post {
                ok(contentType = textPlain)
            }
            
            get("/{id}") {
                val id = pathParameters.require("id")
                require(id.isNotBlank()) { "ID parameter cannot be blank" }
                ok(id, contentType = textPlain)
            }
        }

        error { _, exception ->
            System.err.println("Error: ${exception.message}")
            internalServerError(contentType = textPlain)
        }
    }

    Runtime.getRuntime().addShutdownHook(Thread {
        try {
            server.stop()
            println("Server stopped gracefully")
        } catch (e: Exception) {
            System.err.println("Error during shutdown: ${e.message}")
        }
    }, "server-shutdown")

    server.block()
}
