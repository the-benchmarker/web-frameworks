import akane


proc main {.gcsafe.} =
  let server = newServer("0.0.0.0", 3000)

  server.pages:
    "/": 
      if request.reqMethod == HttpGet:
        await request.answer("")
      else:
        await request.error("not GET :(")

    "/user":
      if request.reqMethod == HttpGet:
        await request.answer("")
      else:
        await request.error("not GET :(")

    regex(re"\A/user/id(\d+)\Z"):
      if request.reqMethod == HttpPost:
        await request.sendJson(%*{"id": url[0]})
      else:
        await request.error("not POST :(")

  server.start()

main()
