import pkg/supranim/middleware

newBaseMiddleware uriChecker:
  ## Fix the trailing slash in the URI
  let path = req.getUriPath
  if path != "/" and path[^1] == '/':
    res.addHeader("Location", path[0..^2])
    req.resp(code = HttpCode(301), "", res.getHeaders())
    return false
  result = true