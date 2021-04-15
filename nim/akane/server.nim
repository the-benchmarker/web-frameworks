import akane


proc main {.gcsafe.} =
  let server = newServer("0.0.0.0", 3000)

  server.pages:
    "/": 
      if request.reqMethod == HttpGet:
        await request.send("")
      else:
        await request.error("not GET :(")

    "/user":
      await request.send("")

    regex(re"\A/user/(\d+)\Z"):
      if request.reqMethod == HttpGet:
        await request.send(url[0])
      else:
        await request.error("method not allowed")

    notfound:
      await request.error("not found")

  server.start()

main()
