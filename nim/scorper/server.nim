import strutils
import scorper

proc cb(req: Request) {.async.} =
  if req.meth == HttpGet:
    if req.path == "/":
      await req.resp("")
    elif req.path.startsWith("/user/"):
      let id = req.path[6 .. ^1]
      await req.resp(id)
  elif req.meth == HttpPost:
    if req.path == "/user":
      await req.resp("")

const address = "0.0.0.0:3000"
waitFor serve(address, cb)
