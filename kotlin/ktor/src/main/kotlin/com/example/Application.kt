package com.example

import com.example.plugins.*
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*

/**
 * Production-grade Ktor benchmark server.
 * 
 * Features:
 * - Security headers configured
 * - Proper error handling
 * - Graceful shutdown support
 * - Input validation
 * - Optimized Netty configuration
 * - Disabled debug logging in production
 */
fun main() {
    embeddedServer(
        Netty,
        port = 3000,
        host = "0.0.0.0",
        module = Application::module,
        configure = {
            // Production settings
            responseWriteTimeoutSeconds = 60
            requestReadTimeoutSeconds = 60
            connectionGroupSize = Runtime.getRuntime().availableProcessors() * 2
            workerGroupSize = Runtime.getRuntime().availableProcessors() * 2
            callGroupSize = Runtime.getRuntime().availableProcessors() * 2
        }
    ).start(wait = true)
}

fun Application.module() {
    // Configure security headers
    configureSecurity()
    configureRouting()
}
