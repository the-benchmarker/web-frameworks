import supranim/controller
import std/httpcore

ctrl get4xx:
  ## Renders a 4xx error page
  req.resp(Http404, "404 Not Found")
