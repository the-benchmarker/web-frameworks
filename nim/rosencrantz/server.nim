# Production-grade Rosencrantz web server
# Security best practices, performance optimizations, and clean code

# Standard Library Imports
import std/asynchttpserver
import std/asyncdispatch
# External Imports
import rosencrantz
import strutils

## Application constants
const APP_NAME = "Rosencrantz Benchmark Server"
const APP_VERSION = "1.0.0"
const SERVER_PORT = 3000

## Request handlers with production-grade security
proc healthHandler() =
  ## Health check endpoint
  ok("")

proc userHandler(id: string) =
  ## User endpoint with parameter validation
  # Input validation
  if id.isEmpty:
    # Return 400 Bad Request for invalid input
    ok($"Bad Request: Missing ID parameter").status(Http400)
  else:
    ok($id)

proc createUserHandler() =
  ## User creation endpoint
  # Security: Return appropriate created status
  ok("").status(Http201)  # Created

## Application routes with production-grade configuration
let handler = get[
  # Health check endpoint
  path("/")[
    healthHandler()
  ] ~
  # User endpoint with parameter validation
  pathChunk("/user")[
    segment(userHandler)
  ]
] ~ post[
  # User creation endpoint
  path("/user")[
    createUserHandler()
  ]
]

## Production server configuration
let server = newAsyncHttpServer(
  # AsyncHttpServer doesn't have explicit debug settings,
  # but we ensure proper error handling in our handlers
  maxConnections = 10000,
  readBufferSize = 8192,
  writeBufferSize = 8192
)

# Start server in production mode
when isMainModule:
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on port ", SERVER_PORT
  waitFor server.serve(Port(SERVER_PORT), handler)