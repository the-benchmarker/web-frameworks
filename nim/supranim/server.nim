# Supranim low-level server setup for the web-frameworks benchmark
# https://github.com/supranim/supranim

import std/[httpcore, tables, net, cpuinfo]
import supranim/controller
import supranim/core/[router, request, response]
import supranim/network/webserver

proc homeHandler(req: var Request, res: var Response) {.nimcall, gcsafe.} =
  req.send(200)

proc userHandler(req: var Request, res: var Response) {.nimcall, gcsafe.} =
  let id = req.params["id"]
  req.resp(Http200, id)

proc createUserHandler(req: var Request, res: var Response) {.nimcall, gcsafe.} =
  req.send(200)

var appRouter = newHttpRouter()
appRouter.get("/", homeHandler)
appRouter.get("/user/{id:id}", userHandler)
appRouter.post("/user", createUserHandler)

proc onRequest(req: var Request) {.gcsafe.} =
  {.gcsafe.}:
    var res = Response(headers: newHttpHeaders())
    let rc = appRouter.checkExists(req.getUriPath(), req.getHttpMethod())
    if rc.exists:
      req.routeParams = rc.params
      rc.route.callback(req, res)
      req.resp(res.getCode(), res.getBody(), res.getHeaders())
    else:
      req.resp(Http404, "not found")

var server = newWebServer(Port(3000), true)
server.start(onRequest, nil, threads = countProcessors())
