package benchmark

import io.jooby.ExecutionMode.EVENT_LOOP
import io.jooby.StatusCode
import io.jooby.runApp

fun main(args: Array<String>) {
  runApp(args, EVENT_LOOP) {
    serverOptions {
      port = 3000
      // Turn off Date and Server Response headers.
      defaultHeaders = false
    }

    get("/") { ctx.send(StatusCode.OK) }
    get("/user/{id}") { ctx.send(ctx.path("id").value()) }
    post("/user") { ctx.send(StatusCode.OK) }
  }
}
