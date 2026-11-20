package benchmark

import io.jooby.ExecutionMode.EVENT_LOOP
import io.jooby.ServerOptions
import io.jooby.StatusCode
import io.jooby.kt.Kooby
import io.jooby.kt.runApp
import io.jooby.netty.NettyServer

class App : Kooby({
    get("/") { ctx.send(StatusCode.OK) }
    get("/user/{id}") { ctx.send(ctx.path("id").value()) }
    post("/user") { ctx.send(StatusCode.OK) }
})

fun main(args: Array<String>) {
    val options = ServerOptions().apply {
        port = 3000
        defaultHeaders = false
    }
    runApp(args, NettyServer(options), EVENT_LOOP, ::App)
}
