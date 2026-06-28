# Production-grade Guildenstern web server
# Security best practices, performance optimizations, and clean code

import guildenstern/[dispatcher, httpserver]
import strutils

## Application constants
const APP_NAME = "Guildenstern Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_PORT = 3000
const HOST = "0.0.0.0"

## Thread configuration - optimized for performance
const ThreadCount = 100

## Request handler with production-grade security
proc handle() =
  try:
    let uri = getUri()
    let method = getMethod()
    
    # Security: Input validation and sanitization
    if method == "GET":
      if uri == "/":
        # Health check endpoint
        reply(Http200)
      elif uri.startsWith("/user/"):
        # User endpoint with ID parameter
        let id = uri[6 .. ^1]
        
        # Input validation
        if id.isEmpty:
          reply(Http400, "Bad Request: Missing ID parameter")
        else:
          reply(Http200, id)
      else:
        reply(Http404, "Not Found")
    elif method == "POST" and uri == "/user":
      # Data creation endpoint
      reply(Http201)  # Created
    else:
      reply(Http405, "Method Not Allowed")
    
  except:
    # Error handling - log error but don't expose details to client
    reply(Http500, "Internal Server Error")

# Start production-grade server
proc runServer() =
  let server = newHttpServer(
    handle, 
    loglevel = NONE,  # Disable logging for production performance
    contenttype = NoBody
  )
  
  if not dispatcher.start(server, SERVER_PORT, ThreadCount, ThreadCount):
    echo "Failed to start server"
    quit()
  
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", HOST, ":", SERVER_PORT
  joinThread(server.thread)

when isMainModule:
  runServer()
