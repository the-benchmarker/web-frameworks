# Production-grade Basolato web application
# Security best practices, performance optimizations, and clean code

## Framework imports
import basolato

## Controller imports
import app/http/controllers/benchmark_controller

## Application constants
const APP_NAME = "Basolato Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_PORT = 3000

## Application routes
let ROUTES = @[
  # Health check endpoint
  Route.get("/", benchmark_controller.index),
  
  # User endpoints with type-safe parameters
  Route.get("/user/{id:str}", benchmark_controller.show),
  Route.get("/user/{id:int}", benchmark_controller.show),
  
  # Data creation endpoint
  Route.post("/user", benchmark_controller.store),
]

# Security headers middleware
proc securityHeaders(context: Context, next: proc ()) {.async.} =
  context.response.headers["X-Content-Type-Options"] = "nosniff"
  context.response.headers["X-Frame-Options"] = "DENY"
  context.response.headers["X-XSS-Protection"] = "1; mode=block"
  context.response.headers["Content-Security-Policy"] = "default-src 'self'"
  await next()

# Error handling middleware
proc errorHandler(context: Context, next: proc ()) {.async.} =
  try:
    await next()
  except:
    let error = getCurrentException()
    context.response.status = 500
    context.response.body = "Internal Server Error"
    context.response.headers["Content-Type"] = "text/plain"

# Start production-grade server
proc runServer() =
  # Configure server with production settings
  let settings = newServerSettings(
    host = "0.0.0.0",
    port = Port(SERVER_PORT),
    debug = false,  # Disable debug mode for production
    logLevel = LogLevel.Error  # Only log errors in production
  )
  
  # Add middleware chain
  addMiddleware(securityHeaders)
  addMiddleware(errorHandler)
  
  # Serve the application
  serve(ROUTES, settings = settings)

# Entry point
when isMainModule:
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on port ", SERVER_PORT
  runServer()
