use "http_server"
use "jennet"
use "net"

actor Main
  new create(env: Env) =>
    let tcp_auth: TCPListenAuth = TCPListenAuth(env.root)

    let server =
      Jennet(tcp_auth, env.out)
        .> get("/", EmptyHandler)
        .> get("/user/:name", GetUserHandler)
        .> post("/user", EmptyHandler)
        .serve(ServerConfig(where host' = "0.0.0.0", port' = "3000"))

primitive GetUserHandler is RequestHandler
  fun apply(ctx: Context): Context iso^ =>
    let name = ctx.param("name")
    ctx.respond(StatusResponse(StatusOK), name.array())
    ctx

primitive EmptyHandler is RequestHandler
  fun apply(ctx: Context): Context iso^ =>
    ctx.respond(StatusResponse(StatusOK))
    ctx
