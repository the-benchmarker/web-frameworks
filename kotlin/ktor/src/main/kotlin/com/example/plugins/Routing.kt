package com.example.plugins

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

fun Application.configureRouting() {
    routing {
        get("/") {
            call.respondText("", contentType = ContentType.Text.Plain)
        }
        get("/user/{id}") {
            call.respondText(call.parameters["id"].toString(), contentType = ContentType.Text.Plain)
        }
        post("/user") {
            call.respondText("", contentType = ContentType.Text.Plain)
        }
    }
}
