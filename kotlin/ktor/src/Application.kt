package ktor.benchmark

import io.ktor.application.*
import io.ktor.response.*
import io.ktor.request.*
import io.ktor.routing.*
import io.ktor.http.*

fun main(args: Array<String>): Unit = io.ktor.server.netty.EngineMain.main(args)

@Suppress("unused") // Referenced in application.conf
@kotlin.jvm.JvmOverloads
fun Application.module(testing: Boolean = false) {
    routing {
        get("/") {
            call.respondText("", contentType = ContentType.Text.Plain, status = HttpStatusCode.OK)
        }
        get("/user/{id}") {
            val id = call.parameters["id"]
            call.respondText(id ?: "", contentType = ContentType.Text.Plain, status = HttpStatusCode.OK)
        }
        post("/user") {
            call.respondText("", contentType = ContentType.Text.Plain, status = HttpStatusCode.OK)
        }
    }
}

