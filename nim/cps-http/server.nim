import cps/runtime
import cps/transform
import cps/eventloop
import cps/io/tcp
import cps/io/streams
import cps/http/server/dsl
import cps/http/server/http1

let handler = router:
  get "/":
    respond 200

  get "/user/{id}":
    respond 200, pathParams["id"]

  post "/user":
    respond 200

proc startAccepting(listener: TcpListener, handler: HttpHandler) =
  let config = HttpServerConfig()
  listener.acceptEach(proc(client: TcpStream) =
    discard handleHttp1Connection(client.AsyncStream, config, handler)
  )

proc main() =
  let listener = tcpListen("0.0.0.0", 3000, reusePort = true,
                           deferAcceptSeconds = 1, noDelay = true)
  startAccepting(listener, handler)
  let loop = getEventLoop()
  while true:
    loop.tick()

main()
