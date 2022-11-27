import prologue

proc simpleGet*(ctx: Context) {.async.} =
  resp ""

proc userGet*(ctx: Context) {.async.} =
  resp ctx.getPathParams("id")

proc simplePost*(ctx: Context) {.async.} =
  resp ""

let settings = newSettings(port = Port(3000), debug = false)

let app = newApp(settings = settings)

app.get("/", simpleGet)
app.get("user/{id}", userGet)
app.post("/user", simplePost)
app.run()
