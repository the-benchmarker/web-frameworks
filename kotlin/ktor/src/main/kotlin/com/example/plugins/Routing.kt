package com.example.plugins

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

fun Application.configureRouting() {
    routing {
        get("/") {
            call.respondText("")
        }
        get("/user/{id}") {
            call.respondText(call.parameters["id"].toString())
        }
        post("/user") {
            call.respondText("")
        }
    }
}
