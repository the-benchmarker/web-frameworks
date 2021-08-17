use "http"
use "jennet"

actor Main
  new create(env: Env) =>
    let auth =
      try
        env.root as AmbientAuth
      else
        env.out.print("unable to use network.")
        return
      end

    let j =
      Jennet(auth, env.out, "3000")
        .> get("/", EmptyHandler)
        .> get("/user/:name", GetUserHandler)
        .> post("/user", EmptyHandler)

    let j' = consume val j
    try j'.serve()? else j'.dispose() end

primitive GetUserHandler is Handler
  fun apply(c: Context, req: Payload val): Context iso^ =>
    let res = Payload.response()
    let name = c.param("name")
    res.add_chunk(name)
    c.respond(req, consume res)
    consume c

primitive EmptyHandler is Handler
  fun apply(c: Context, req: Payload val): Context iso^ =>
    let res = Payload.response()
    c.respond(req, consume res)
    consume c