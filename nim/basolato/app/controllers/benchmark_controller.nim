import json
# framework
import basolato/controller


proc index*(request:Request, params:Params):Future[Response] {.async.} =
  var header = newHeaders()
  header.set("Content-Type", "text/plain")
  return render("", header)

proc show*(request:Request, params:Params):Future[Response] {.async.} =
  let id = params.getInt("id")
  return render($id)

proc store*(request:Request, params:Params):Future[Response] {.async.} =
  var header = newHeaders()
  header.set("Content-Type", "text/plain")
  return render("", header)
