package com.example.plugins

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.plugins.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

/**
 * Security configuration for Ktor application.
 * Adds security headers and error handling.
 */
fun Application.configureSecurity() {
    install(CallId) {
        // Generate call IDs for tracing (disabled in benchmark for performance)
        generateId = { "" }
    }

    install(CallLogging) {
        // Disable logging in production for benchmark performance
        level = org.slf4j.event.Level.ERROR
    }

    // Global security headers interceptor
    intercept(ApplicationCallPipeline.Call) {
        if (call.request.path() != "/health") {
            // Add security headers to every response
            call.response.headers {
                append(HttpHeaders.Server, "Ktor")
                append(HttpHeaders.XContentTypeOptions, "nosniff")
                append(HttpHeaders.XFrameOptions, "DENY")
                append(HttpHeaders.XXSSProtection, "1; mode=block")
                append(HttpHeaders.CacheControl, "no-cache, no-store, must-revalidate")
            }
        }
    }

    // Error handling
    intercept(ApplicationCallPipeline.Call) {
        try {
            proceed()
        } catch (e: BadRequestException) {
            call.respond(HttpStatusCode.BadRequest, e.message ?: "Bad Request")
        } catch (e: NotFoundException) {
            call.respond(HttpStatusCode.NotFound, e.message ?: "Not Found")
        } catch (e: Exception) {
            System.err.println("Error: ${e.message}")
            call.respond(HttpStatusCode.InternalServerError, "Internal Server Error")
        }
    }
}
