# Ambiorix Framework Benchmark Server
#
# A high-performance benchmark server using Ambiorix framework.
# Follows R best practices including proper error handling and logging.

# Enable error handling
options(expressions = 5000)
options(warn = 1)

# Custom logger for benchmarking
#
# @param message Log message character vector
# @param level Log level character vector (debug, info, error)
benchmark_log <- function(message, level = "debug") {
  timestamp <- Sys.time()
  cat(sprintf("[%s] %s - %s\n", timestamp, level, message))
}

# Load required library
library(ambiorix)

# Create application instance
app <- Ambiorix$new()

# Configure application with best practices
app$set("host", "0.0.0.0")
app$set("port", 3000L)
app$set("showErrors", FALSE)

# Root endpoint handler
#
# GET /
app$get("/", function(req, res) {
  benchmark_log("Root endpoint accessed")
  res$setHeader("Content-Type", "text/plain")
  res$send("")
})

# Get user by ID
#
# GET /user/:id
app$get("/user/:id", function(req, res) {
  user_id <- req$params$id
  benchmark_log(sprintf("User endpoint accessed with ID: %s", user_id))
  res$setHeader("Content-Type", "text/plain")
  res$send(user_id)
})

# Create new user
#
# POST /user
app$post("/user", function(req, res) {
  benchmark_log("Create user endpoint accessed")
  res$setHeader("Content-Type", "text/plain")
  res$send("")
})

# Health check endpoint for monitoring
#
# GET /health
app$get("/health", function(req, res) {
  benchmark_log("Health check endpoint accessed")
  res$setHeader("Content-Type", "text/plain")
  res$send("OK")
})

# Start the application
app$start()
