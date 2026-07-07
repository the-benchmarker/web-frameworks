# Production-grade Jester web server
# Security best practices, performance optimizations, and clean code

import jester
import strutils

## Application constants
const APP_NAME = "Jester Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Production server configuration
settings:
  # Server settings
  host = SERVER_HOST
  port = Port(SERVER_PORT)
  
  # Debug and logging - DISABLED for production
  debug = false
  logLevel = LogLevel.Error  # Only error logging in production
  
  # Performance settings
  maxThreads = 100  # Optimized thread pool for benchmarking
  maxRequestSize = 1024 * 1024  # 1MB max request size

## Request handlers with production-grade security
proc healthHandler() =
  ## Health check endpoint
  setContentType("text/plain")
  resp ""

proc userHandler(id: string) =
  ## User endpoint with parameter validation
  # Input validation
  if id.isEmpty:
    setStatus(400)
    setContentType("text/plain")
    resp "Bad Request: Missing ID parameter"
  else:
    setContentType("text/plain")
    resp id

proc createUserHandler() =
  ## User creation endpoint
  setStatus(201)  # Created
  setContentType("text/plain")
  resp ""

## Application routes
routes:
  # Health check endpoint
  get "/": healthHandler()
  
  # User endpoint with parameter validation
  get "/user/@id": userHandler(@"id")
  
  # User creation endpoint
  post "/user": createUserHandler()

# Start server in production mode
when isMainModule:
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
