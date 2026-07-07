# Production-grade HappyX web server
# Security best practices, performance optimizations, and clean code

import happyx
import strutils

## Application constants
const APP_NAME = "HappyX Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Request handlers with production-grade security
proc healthHandler() =
  ## Health check endpoint
  # Security: Set content type header
  setHeader("Content-Type", "text/plain")
  result = ""

proc userHandler(id: string) =
  ## User endpoint with parameter validation
  # Input validation
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

proc notFoundHandler() =
  ## 404 Not Found handler
  setStatus(404)
  setHeader("Content-Type", "text/plain")
  result = "Not Found"

# Start production-grade server
proc runServer() =
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
  
  serve SERVER_HOST, SERVER_PORT:
    # Health check endpoint
    get "/": healthHandler()
    
    # User endpoint with parameter validation
    get "/user/$id": userHandler(id)
    
    # User creation endpoint
    post "/user": createUserHandler()
    
    # 404 handler
    notfound: notFoundHandler()

when isMainModule:
  runServer()
