package benchmark.javalin

import io.javalin.Javalin

fun main() {
    Javalin.create { config ->
        config.routes.get("/") { ctx -> ctx.result("") }
        config.routes.get("/user/{id}") { ctx -> ctx.result(ctx.pathParam("id")) }
        config.routes.post("/user") { ctx -> ctx.result("") }
    }.start(3000)
}
