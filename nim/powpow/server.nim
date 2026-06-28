# Production-grade PowPow event-driven web server
# Security best practices, performance optimizations, and clean code
# 
# GitHub Repository: https://github.com/openpeeps/powpow
# API Reference: https://openpeeps.github.io/powpow

import pkg/powpow
import std/[httpcore, strutils]

## Application constants
const APP_NAME = "PowPow Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Server instance
let server = newMultiThreadHttpServer()

## Request handler with production-grade security
proc handler(req: HttpRequest, res: HttpResponse) = {.gcsafe.}:
  try:
    let httpMethod = req.getMethod()
    let path = req.getPath()

    # Security: Method and path validation
    case httpMethod:
      of HttpGet:
        if path == "/":
          # Health check endpoint
          res.status(Http200).send("")
          return
        elif path.startsWith("/user/"):
          # User endpoint with ID parameter
          let id = path[6..^1]
          
          # Input validation
          if id.isEmpty:
            res.status(Http400).send("Bad Request: Missing ID parameter")
          else:
            res.status(Http200).send(id)
        else:
          # Security: Don't expose error details in production
          res.status(Http404).send("Not Found")
          
      of HttpPost:
        if path == "/user":
          # Data creation endpoint
          res.status(Http201).send("")  # Created
        else:
          res.status(Http404).send("Not Found")
          
      else:
        # Method not allowed
        res.status(Http405).send("Method Not Allowed")
        
  except:
    # Error handling - don't expose internal error details
    res.status(Http500).send("Internal Server Error")

# Start production-grade server
proc runServer() =
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
  server.start(handler, SERVER_HOST, SERVER_PORT)

when isMainModule:
  runServer()
