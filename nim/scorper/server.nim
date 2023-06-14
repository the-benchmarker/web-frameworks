import strutils
import scorper

proc cb(req: Request) {.async.} =
  case req.meth 
  of HttpGet:
    if req.path == "/":
      await req.resp("")
    else:
      let id = req.path[6 .. ^1]
      await req.resp(id)
  of HttpPost:
    await req.resp("")
  else:
    discard

const address = "0.0.0.0:3000"
waitFor serve(address, cb)
