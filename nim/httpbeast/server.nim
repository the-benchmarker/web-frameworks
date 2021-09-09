import options, asyncdispatch, strutils, httpbeast, net

proc onRequest(req: Request): Future[void] =
  if req.httpMethod == some(HttpGet):
    if req.path.get() == "/":
      req.send(Http200, "")
    elif req.path.get().startsWith("/user/"):
      let id = req.path.get()[6 .. ^1]
      req.send(Http200, id)
  elif req.httpMethod == some(HttpPost):
    if req.path.get() == "/user":
      req.send(Http200, "")

run(onRequest, initSettings(port=Port(3000)))