import mofuw, ./nest, uri

var mapper = newRouter[proc(req: mofuwReq, res: mofuwRes): Future[void]]()

mapper.map(
    proc(req: mofuwReq, res: mofuwRes) {.async.} =
      mofuwOK("")
  , "get", "/")

mapper.map(
  proc(req: mofuwReq, res: mofuwRes) {.async.} =
    mofuwOK(req.params("id"))
  , "get", "/user/{id}")

mapper.map(
  proc(req: mofuwReq, res: mofuwRes) {.async.} =
    mofuwOK("")
  , "post", "/user")

mapper.compress()

proc handler(req: mofuwReq, res: mofuwRes) {.async.} =
  var headers = req.toHttpHeaders()
  let r = mapper.route(req.getMethod, parseUri(req.getPath), headers)

  if r.status == routingFailure:
    mofuwResp(HTTP404, "text/plain", "Not Found")
  else:
    req.setParam(r.arguments.pathArgs)
    req.setQuery(r.arguments.queryArgs)
    await r.handler(req, res)

handler.mofuwRun(3000)