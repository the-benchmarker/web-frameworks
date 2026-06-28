# Production-grade HttpBeast web server
# Security best practices, performance optimizations, and clean code

import options, asyncdispatch, strutils, httpbeast, net, times

## Application constants
const APP_NAME = "HttpBeast Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Request handler with production-grade security
proc onRequest(req: Request): Future[void] = {.gcsafe.}
  try:
    let path = req.path.get()
    
    # Security: Method validation
    case req.httpMethod
    of some(HttpGet):
      if path == "/":
        # Health check endpoint
        req.send(Http200, "")
      elif path.startsWith("/user/"):
        # User endpoint with ID parameter
        let id = path[6 .. ^1]
        
        # Input validation
        if id.isEmpty:
          req.send(Http400, "Bad Request: Missing ID parameter")
        else:
          req.send(Http200, id)
      else:
        req.send(Http404, "Not Found")
        
    of some(HttpPost):
      if path == "/user":
        # Data creation endpoint
        req.send(Http201, "")  # Created
      else:
        req.send(Http404, "Not Found")
        
    of some(_):
      # Method not allowed
      req.send(Http405, "Method Not Allowed")
      
    of none:
      # Invalid method
      req.send(Http400, "Bad Request")
      
  except:
    # Error handling - don't expose internal error details
    req.send(Http500, "Internal Server Error")

## Production server configuration
proc runServer() =
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
  
  # Initialize with production settings
  let settings = initSettings(
    port = Port(SERVER_PORT),
    host = SERVER_HOST,
    # HttpBeast doesn't have explicit debug/log settings,
    # but we ensure proper error handling above
    reusePort = false,
    maxConnections = 10000,  # High connection limit for benchmarking
    readBufferSize = 8192,
    writeBufferSize = 8192
  )
  
  run(onRequest, settings)

when isMainModule:
  runServer()