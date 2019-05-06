package ktor.benchmark

import io.ktor.application.*
import io.ktor.response.*
import io.ktor.request.*
import io.ktor.routing.*
import io.ktor.http.*
import io.ktor.locations.*

fun main(args: Array<String>): Unit = io.ktor.server.netty.EngineMain.main(args)

@Location("/user/{id}")
data class UserId(val id: Int)


@Suppress("unused") // Referenced in application.conf
fun Application.module() {
    install(Locations)
    routing {
        get("/") {
            call.respondText("", contentType = ContentType.Text.Plain, status = HttpStatusCode.OK)
        }
        get<UserId> { userId ->
            call.respondText("${userId.id}", contentType = ContentType.Text.Plain, status = HttpStatusCode.OK)
        }
        post("/user") {
            call.respondText("", contentType = ContentType.Text.Plain, status = HttpStatusCode.OK)
        }
    }
}

