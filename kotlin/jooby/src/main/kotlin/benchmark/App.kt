package benchmark

import io.jooby.ExecutionMode.EVENT_LOOP
import io.jooby.StatusCode
import io.jooby.kt.runApp

fun main(args: Array<String>) {
    runApp(args, EVENT_LOOP) {
        serverOptions {
            port = 3000
            defaultHeaders = false
        }

        get("/") { ctx.send(StatusCode.OK) }
        get("/user/{id}") { ctx.send(ctx.path("id").value()) }
        post("/user") { ctx.send(StatusCode.OK) }
    }
}
