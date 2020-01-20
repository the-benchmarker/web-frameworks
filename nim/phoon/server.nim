import phoon

var app = new App

app.get("/",
    proc (ctx: Context) {.async.} =
        discard
)

app.get("/user/{id}",
    proc (ctx: Context) {.async.} =
        let user_id = ctx.parameters.get("id")
        ctx.response.body(user_id)
)

app.post("/user",
    proc (ctx: Context) {.async.} =
        discard
)

app.serve(3000)
