# Production-grade Scorper web server
# Security best practices, performance optimizations, and clean code

import scorper
import strutils

## Application constants
const APP_NAME = "Scorper Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Request handler with production-grade security
proc cb(req: Request) {.async.} =
  try:
    # Security: Method and path validation
    case req.meth
    of HttpGet:
      if req.url.path == "/":
        # Health check endpoint
        await req.resp("")
      elif req.url.path.startsWith("/user/"):
        # User endpoint with ID parameter
        let id = req.url.path[6 .. ^1]
        
        # Input validation
        if id.isEmpty:
          await req.resp("Bad Request: Missing ID parameter").status(Http400)
        else:
          await req.resp(id)
      else:
        # Security: Don't expose error details in production
        await req.resp("Not Found").status(Http404)
        
    of HttpPost:
      if req.url.path == "/user":
        # Data creation endpoint
        await req.resp("").status(Http201)  # Created
      else:
        await req.resp("Not Found").status(Http404)
        
    else:
      # Method not allowed
      await req.resp("Method Not Allowed").status(Http405)
      
  except:
    # Error handling - don't expose internal error details
    await req.resp("Internal Server Error").status(Http500)

## Production server configuration
const address = SERVER_HOST & ":" & $SERVER_PORT

# Start server in production mode
when isMainModule:
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", address
  waitFor serve(address, cb)
