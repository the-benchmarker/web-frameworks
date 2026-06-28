# Plumber API Endpoints
#
# This file contains the API endpoint definitions for the Plumber framework benchmark.
# While plumber could be made ~ 50% faster, this represents the common setup.
# See https://github.com/the-benchmarker/web-frameworks/issues/3303#issuecomment-694351654

# Custom logger for benchmarking (available from app.R)
# benchmark_log <- function(message, level = "debug") {
#   if (!DEBUG_MODE && level == "debug") return(invisible(NULL))
#   timestamp <- Sys.time()
#   cat(sprintf("[%s] %s - %s\n", timestamp, level, message))
# }

#* @title Plumber Benchmark API
#* @description High-performance benchmark endpoints for Plumber framework
#* @version 1.0.0

#* Root endpoint handler
#* @serializer contentType list(type = "text/plain")
#* @get /
function(req) {
  benchmark_log("Root endpoint accessed")
  return(list(value = "", headers = add_security_headers(req)))
}

#* Get user by ID
#* @serializer contentType list(type = "text/plain")
#* @get /user/<id>
function(id, req) {
  benchmark_log(sprintf("User endpoint accessed with ID: %s", id))
  return(list(value = id, headers = add_security_headers(req)))
}

#* Create new user
#* @serializer contentType list(type = "text/plain")
#* @post /user
function(req) {
  benchmark_log("Create user endpoint accessed")
  return(list(value = "", status = 201, headers = add_security_headers(req)))
}

#* Health check endpoint for monitoring
#* @serializer contentType list(type = "text/plain")
#* @get /health
function(req) {
  benchmark_log("Health check endpoint accessed")
  return(list(value = "OK", headers = add_security_headers(req)))
}

#* Error test endpoint for verifying error handling
#* @serializer contentType list(type = "text/plain")
#* @get /error
function(req) {
  benchmark_log("Error endpoint accessed", "error")
  if (exists("DEBUG_MODE") && DEBUG_MODE) {
    return(list(value = "Internal Server Error", status = 500, headers = add_security_headers(req)))
  } else {
    return(list(value = "", status = 500, headers = add_security_headers(req)))
  }
}
