package com.example.plugins

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

/**
 * Routing configuration with input validation and health checks.
 */
fun Application.configureRouting() {
    routing {
        // Health check endpoint
        get("/health") {
            call.respondText("OK", contentType = ContentType.Text.Plain)
        }

        // Root endpoint
        get("/") {
            call.respondText("", contentType = ContentType.Text.Plain)
        }

        // User endpoints with input validation
        get("/user/{id}") {
            val id = call.parameters["id"]
            if (id.isNullOrBlank()) {
                call.respondText("ID parameter is required", status = HttpStatusCode.BadRequest)
            } else {
                call.respondText(id, contentType = ContentType.Text.Plain)
            }
        }

        post("/user") {
            call.respondText("", contentType = ContentType.Text.Plain)
        }
    }
}
