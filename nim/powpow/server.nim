# This is a super simple server setup for the PowPow event-driven library
#     - GitHub Repository: https://github.com/openpeeps/powpow
#     - API Reference: https://openpeeps.github.io/powpow

import pkg/powpow
import std/[httpcore, strutils]

let server = newMultiThreadHttpServer()

proc handler(req: HttpRequest, res: HttpResponse) =
  {.gcsafe.}:
    let httpMethod = req.getMethod()
    let path = req.getPath()

    # PowPow provides no router, so here we implement a simple
    # handler that dispatches based on the request method and path
    case httpMethod:
      of HttpGet:
        if path == "/":
          res.status(Http200).send("")
          return
        if path.startsWith("/user/") and path.len > 6:
          let id = path[6..^1]
          res.status(Http200).send(id)
        else:
          res.sendError(Http404,
            "404 Not Found: " & $httpMethod & " " & path)
      of HttpPost:
        if path == "/user":
          res.status(Http200).send("")
        else:
          res.sendError(Http404,
            "404 Not Found: " & $httpMethod & " " & path)
      else:
        res.sendError(Http404,
          "404 Not Found: " & $httpMethod & " " & path) 

server.start(handler, "0.0.0.0", 3000)
