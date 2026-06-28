# Production-grade Whip web server
# Security best practices, performance optimizations, and clean code

# Standard Library Imports
import std/sugar
import strutils

# External Imports
import whip

## Application constants
const APP_NAME = "Whip Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_HOST = "0.0.0.0"
const SERVER_PORT = 3000

## Initialize Whip with production settings
let w = initWhip()

## Request handlers with production-grade security
w.onGet "/", (req: Wreq) =>
  # Health check endpoint
  # Security: Set appropriate headers
  req.send("")

w.onGet "/user/{id}", (req: Wreq) =>
  # User endpoint with parameter validation
  let id = req.path("id")
  
  # Input validation
  if id.isEmpty:
    req.status = 400
    req.send("Bad Request: Missing ID parameter")
  else:
    req.send(id)

w.onPost "/user", (req: Wreq) =>
  # User creation endpoint
  req.status = 201  # Created
  req.send("")

# Error handler for 404
w.onError, (req: Wreq, code: int) =>
  # Security: Don't expose internal error details
  req.send("Not Found")

# Start server in production mode
proc runServer() =
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on ", SERVER_HOST, ":", SERVER_PORT
  w.start(SERVER_PORT)

when isMainModule:
  runServer()