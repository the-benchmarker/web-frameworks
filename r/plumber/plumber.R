# Plumber API Endpoints
#
# This file contains the API endpoint definitions for the Plumber framework benchmark.
# While plumber could be made ~ 50% faster, this represents the common setup.
# See https://github.com/the-benchmarker/web-frameworks/issues/3303#issuecomment-694351654

# Custom logger for benchmarking (available from app.R)
# benchmark_log <- function(message, level = "debug") {
#   timestamp <- Sys.time()
#   cat(sprintf("[%s] %s - %s\n", timestamp, level, message))
# }

#* @title Plumber Benchmark API
#* @description High-performance benchmark endpoints for Plumber framework
#* @version 1.0.0

#* Root endpoint handler
#* @serializer contentType list(type = "text/plain")
#* @get /
function() {
  benchmark_log("Root endpoint accessed")
  return("")
}

#* Get user by ID
#* @serializer contentType list(type = "text/plain")
#* @get /user/<id>
function(id) {
  benchmark_log(sprintf("User endpoint accessed with ID: %s", id))
  return(id)
}

#* Create new user
#* @serializer contentType list(type = "text/plain")
#* @post /user
function() {
  benchmark_log("Create user endpoint accessed")
  return("")
}

#* Health check endpoint for monitoring
#* @serializer contentType list(type = "text/plain")
#* @get /health
function() {
  benchmark_log("Health check endpoint accessed")
  return("OK")
}
