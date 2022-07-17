import json
# framework
import basolato/controller


proc index*(context:Context, params:Params):Future[Response] {.async.} =
  return render("")

proc show*(context:Context, params:Params):Future[Response] {.async.} =
  let id = params.getStr("id")
  return render(id)

proc store*(context:Context, params:Params):Future[Response] {.async.} =
  return render("")
