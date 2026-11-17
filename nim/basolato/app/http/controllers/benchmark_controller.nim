import std/asyncdispatch
# framework
import basolato/controller


proc index*(context:Context):Future[Response] {.async.} =
  return render("")

proc show*(context:Context):Future[Response] {.async.} =
  let id = context.params.getStr("id")
  return render(id)

proc store*(context:Context):Future[Response] {.async.} =
  return render("")
