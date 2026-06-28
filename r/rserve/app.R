# Rserve Framework Benchmark Server
#
# A production-grade benchmark server using Rserve framework.
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
  "X-Content-Type-Options" = "nosniff",
  "X-Frame-Options" = "DENY",
  "X-XSS-Protection" = "1; mode=block",
  "Content-Security-Policy" = "default-src 'self'",
  "Referrer-Policy" = "strict-origin-when-cross-origin",
  "Cache-Control" = "no-cache, no-store, must-revalidate"
)

# Request handler function with production best practices
.http.request <- function(path, query, body, headers) {
  # Combine security headers with content type
  resp_headers <- SECURITY_HEADERS
  
  # Determine response based on path
  if (path == "/" || path == "") {
    resp_body <- ""
    benchmark_log("Root endpoint accessed")
    status_code <- 200L
  } else if (path == "/user") {
    if (headers["Request-Method"] == "POST" || (exists("REQUEST_METHOD", envir = .GlobalEnv) && REQUEST_METHOD == "POST")) {
      resp_body <- ""
      benchmark_log("Create user endpoint accessed")
      status_code <- 201L
    } else {
      resp_body <- ""
      benchmark_log("Root or user endpoint accessed")
      status_code <- 200L
    }
  } else if (grepl("^/user/", path)) {
    # /user/id path - extract user ID using regex
    match_info <- regexec("^/user/(.*)", path)
    resp_body <- regmatches(path, match_info)[[1]][2]
    benchmark_log(sprintf("User endpoint accessed with ID: %s", resp_body))
    status_code <- 200L
  } else if (path == "/health") {
    resp_body <- "OK"
    benchmark_log("Health check endpoint accessed")
    status_code <- 200L
  } else if (path == "/error") {
    resp_body <- if (DEBUG_MODE) "Internal Server Error" else ""
    benchmark_log("Error endpoint accessed", "error")
    status_code <- 500L
  } else {
    resp_body <- ""
    benchmark_log(sprintf("Unknown path accessed: %s", path))
    status_code <- 404L
  }

  # Set response headers and status
  content_type <- "text/plain"

  # Return response
  list(
    resp_body,
    content_type,
    resp_headers,
    status_code
  )
}

# Startup message with configuration summary
if (DEBUG_MODE) {
  cat(sprintf("\n=== Rserve Framework Benchmark Server (Development Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Enabled (debug level)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /error\n")
  cat("=================================================================\n\n")
} else {
  cat(sprintf("\n=== Rserve Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Disabled (production mode)\n")
  cat("=================================================================\n\n")
}

# Start Rserve with production best practices
Rserve::run.Rserve(http.port = PORT)
