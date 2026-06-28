# Production-grade Mike web server
# Security best practices, performance optimizations, and clean code

import mike
import strutils

## Application constants
const APP_NAME = "Mike Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Request handlers with production-grade security
proc healthHandler() =
  ## Health check endpoint
  # Security: Set content type header
  setHeader("Content-Type", "text/plain")
  result = ""

proc userHandler() =
  ## User endpoint with parameter validation
  # Input validation
  let id = ctx.pathParams["id"]
  if id.isEmpty:
    setStatus(400)
    setHeader("Content-Type", "text/plain")
    result = "Bad Request: Missing ID parameter"
  else:
    setHeader("Content-Type", "text/plain")
    result = id

proc createUserHandler() =
  ## User creation endpoint
  # Security: Set appropriate status and headers
  setStatus(201)  # Created
  setHeader("Content-Type", "text/plain")
  result = ""

## Application routes with production-grade configuration
"/" -> get: healthHandler()

"/user/:id" -> get: userHandler()

"/user" -> post: createUserHandler()

# Start server in production mode
proc runServer() =
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
  run(
    port = SERVER_PORT,
    host = SERVER_HOST,
    # Mike doesn't have explicit debug settings, 
    # but we ensure proper error handling in handlers
    workers = 100  # Optimized for benchmarking
  )

when isMainModule:
  runServer()
