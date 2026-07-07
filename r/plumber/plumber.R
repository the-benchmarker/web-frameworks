# Plumber API Endpoints - Production-Grade Implementation
#
# This file contains the API endpoint definitions for the Plumber framework benchmark.
# Implements security best practices, proper error handling, and production optimizations.
#
# Note: Variables from app.R are available here: DEBUG_MODE, benchmark_log, add_security_headers, app_state

#* @title Plumber Benchmark API
#* @description High-performance benchmark endpoints for Plumber framework
#* @version 1.0.0

# Request ID generator for tracing
generate_request_id <- function() {
  paste0(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
}

# Add request ID to security headers
add_request_id_headers <- function(req) {
  request_id <- generate_request_id()
  headers <- add_security_headers(req)
  headers["X-Request-ID"] <- request_id
  headers
}

#* Root endpoint handler
#* @serializer contentType list(type = "text/plain")
#* @get /
function(req) {
  benchmark_log("Root endpoint accessed")
  return(list(value = "", headers = add_request_id_headers(req)))
}

#* Get user by ID
#* @serializer contentType list(type = "text/plain")
#* @get /user/<id>
function(id, req) {
  # Security: Validate input - reject empty IDs
  if (is.null(id) || id == "" || nchar(trim(id)) == 0) {
    benchmark_log("Invalid user ID: empty", "warning")
    return(list(value = ifelse(DEBUG_MODE, "Bad Request: Missing or invalid ID parameter", ""), 
                status = 400, headers = add_request_id_headers(req)))
  }
  
  benchmark_log(sprintf("User endpoint accessed with ID: %s", id))
  return(list(value = id, headers = add_request_id_headers(req)))
}

#* Create new user
#* @serializer contentType list(type = "text/plain")
#* @post /user
function(req) {
  benchmark_log("Create user endpoint accessed")
  return(list(value = "", status = 201, headers = add_request_id_headers(req)))
}

#* Liveness probe endpoint for monitoring
#* @serializer contentType list(type = "text/plain")
#* @get /health
function(req) {
  benchmark_log("Health check endpoint accessed")
  
  # Check shutdown state
  if (exists("app_state") && app_state$shutdown_requested) {
    return(list(value = "Shutting down", status = 503, headers = add_request_id_headers(req)))
  }
  
  return(list(value = "OK", headers = add_request_id_headers(req)))
}

#* Readiness probe endpoint for container orchestration
#* @serializer contentType list(type = "text/plain")
#* @get /ready
function(req) {
  benchmark_log("Readiness check endpoint accessed")
  
  # Security: Ensure proper warmup before accepting traffic
  if (exists("app_state")) {
    uptime <- as.numeric(difftime(Sys.time(), app_state$startup_time, units = "secs"))
    if (uptime < 5) {  # 5-second warmup period for security initialization
      return(list(value = "Not ready", status = 503, headers = add_request_id_headers(req)))
    }
    if (app_state$shutdown_requested) {
      return(list(value = "Shutting down", status = 503, headers = add_request_id_headers(req)))
    }
  }
  
  return(list(value = "Ready", headers = add_request_id_headers(req)))
}

#* Error test endpoint for verifying error handling
#* @serializer contentType list(type = "text/plain")
#* @get /error
function(req) {
  benchmark_log("Error endpoint accessed", "error")
  if (exists("DEBUG_MODE") && DEBUG_MODE) {
    return(list(value = "Internal Server Error", status = 500, headers = add_request_id_headers(req)))
  } else {
    return(list(value = "", status = 500, headers = add_request_id_headers(req)))
  }
}
