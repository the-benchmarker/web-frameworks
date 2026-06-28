# httpuv Framework Benchmark Server
#
# A production-grade benchmark server using httpuv framework.
# Implements security best-practices, proper error handling, and environment-based configuration.

# Enable error handling
options(expressions = 5000)
options(warn = 1)

# Configuration - Environment-based settings for production vs development
DEBUG_MODE <- Sys.getenv("DEBUG", "false") == "true"
ENVIRONMENT <- if (DEBUG_MODE) "development" else "production"
HOST <- Sys.getenv("HOST", "0.0.0.0")
PORT <- as.integer(Sys.getenv("PORT", "3000"))

# Custom logger for benchmarking - disabled in production, enabled in development
#
# @param message Log message character vector
# @param level Log level character vector (debug, info, error)
benchmark_log <- function(message, level = "debug") {
  if (!DEBUG_MODE && level == "debug") return(invisible(NULL))
  timestamp <- Sys.time()
  cat(sprintf("[%s] %s - %s\n", timestamp, level, message))
}

# Security headers configuration
SECURITY_HEADERS <- list(
  'X-Content-Type-Options' = 'nosniff',
  'X-Frame-Options' = 'DENY',
  'X-XSS-Protection' = '1; mode=block',
  'Content-Security-Policy' = "default-src 'self'",
  'Referrer-Policy' = 'strict-origin-when-cross-origin',
  'Cache-Control' = 'no-cache, no-store, must-revalidate'
)

# Request handler function with production best practices
request_handler <- function(req) {
  path <- req$PATH_INFO
  method <- req$REQUEST_METHOD
  
  # Combine security headers with content type
  response_headers <- c(SECURITY_HEADERS, list('Content-Type' = 'text/plain'))
  
  # Determine response based on path and method
  if (path == "/" || path == "") {
    if (method == "GET") {
      benchmark_log("Root endpoint accessed")
      return(list(status = 200L, headers = response_headers, body = ""))
    }
  } else if (grepl("^/user/", path)) {
    if (method == "GET") {
      # /user/id path - extract user ID using regex
      match_info <- regexec("^/user/(.*)", path)
      user_id <- regmatches(path, match_info)[[1]][2]
      benchmark_log(sprintf("User endpoint accessed with ID: %s", user_id))
      return(list(status = 200L, headers = response_headers, body = user_id))
    }
  } else if (path == "/user") {
    if (method == "POST") {
      benchmark_log("Create user endpoint accessed")
      return(list(status = 201L, headers = response_headers, body = ""))
    }
  } else if (path == "/health") {
    if (method == "GET") {
      benchmark_log("Health check endpoint accessed")
      return(list(status = 200L, headers = response_headers, body = "OK"))
    }
  } else if (path == "/error") {
    if (method == "GET") {
      benchmark_log("Error endpoint accessed", "error")
      if (DEBUG_MODE) {
        return(list(status = 500L, headers = response_headers, body = "Internal Server Error"))
      } else {
        return(list(status = 500L, headers = response_headers, body = ""))
      }
    }
  }
  
  # Default 404 response
  if (!DEBUG_MODE) {
    return(list(status = 404L, headers = response_headers, body = ""))
  } else {
    benchmark_log(sprintf("Unknown path accessed: %s", path))
    return(list(status = 404L, headers = response_headers, body = "Not Found"))
  }
}

# Startup message with configuration summary
if (DEBUG_MODE) {
  cat(sprintf("\n=== httpuv Framework Benchmark Server (Development Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Enabled (debug level)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /error\n")
  cat("==================================================================\n\n")
} else {
  cat(sprintf("\n=== httpuv Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Disabled (production mode)\n")
  cat("==================================================================\n\n")
}

# Start httpuv server
s <- httpuv::startServer(
  host = HOST,
  port = PORT,
  app = list(
    call = request_handler
  )
)

# Main service loop
while (TRUE) {
  httpuv::service()
}
