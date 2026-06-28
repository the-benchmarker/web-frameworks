# Production-grade Prologue web server
# Security best practices, performance optimizations, and clean code

import prologue
import strutils

## Application constants
const APP_NAME = "Prologue Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Request handlers with production-grade security
proc simpleGet*(ctx: Context) {.async.} =
  ## Health check endpoint
  # Security: Set content type header
  ctx.setHeader("Content-Type", "text/plain")
  resp ""

proc userGet*(ctx: Context) {.async.} =
  ## User endpoint with parameter validation
  # Input validation
  let id = ctx.getPathParams("id")
  if id.isEmpty:
    ctx.setStatus(400)
    ctx.setHeader("Content-Type", "text/plain")
    resp "Bad Request: Missing ID parameter"
  else:
    ctx.setHeader("Content-Type", "text/plain")
    resp id

proc simplePost*(ctx: Context) {.async.} =
  ## User creation endpoint
  # Security: Set appropriate status and headers
  ctx.setStatus(201)  # Created
  ctx.setHeader("Content-Type", "text/plain")
  resp ""

## Production server configuration
let settings = newSettings(
  host = SERVER_HOST,
  port = Port(SERVER_PORT),
  debug = false,  # Disable debug mode for production
  logLevel = LogLevel.Error,  # Only error logging in production
  
  # Performance settings
  maxThreads = 100,
  maxConnections = 10000
)

let app = newApp(settings = settings)

# Application routes
app.get("/", simpleGet)
app.get("user/{id}", userGet)
app.post("/user", simplePost)

# Start server in production mode
when isMainModule:
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
  app.run()