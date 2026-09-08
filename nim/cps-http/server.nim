import std/os
import cps/runtime
import cps/mt
import cps/http/server/dsl
import cps/http/server/server

proc startShard(shardId: int) {.gcsafe.} =
  {.cast(gcsafe).}:
    let handler = router:
      get "/":
        respond 200

      get "/user/{id}":
        respond 200, pathParams["id"]

      post "/user":
        respond 200

    let server = newHttpServer(
      handler,
      host = "0.0.0.0",
      port = 3000,
      enableHttp2 = false,
      reusePort = true,
      tcpNoDelay = true
    )
    server.bindAndListen()
    discard server.start()

proc main() =
  let runtime = newMultiThreadRuntime()
  setMainRuntime(runtime)
  setCurrentRuntime(runtime)
  runtime.startMtIoShards(startShard)
  while true:
    sleep(1000)

main()
