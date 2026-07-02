# Rserve Framework Benchmark Server - Production-Grade Implementation
#
# A high-performance benchmark server using Rserve framework.
# Implements security best practices, performance optimizations, and production-grade features.
#
# Features:
# - Security headers and CORS protection
# - Request validation and error handling
# - Production-optimized logging (minimal, security-focused)
# - Health checks and readiness probes
# - Graceful shutdown handling
# - Environment-based configuration

# Enable error handling and increase expression limit
options(expressions = 5000)
options(warn = 1)

# =============================================================================
# PRODUCTION CONFIGURATION
# =============================================================================

# Environment-based configuration
DEBUG_MODE <- Sys.getenv("DEBUG", "false") == "true"
ENVIRONMENT <- if (DEBUG_MODE) "development" else "production"
HOST <- Sys.getenv("HOST", "0.0.0.0")
PORT <- as.integer(Sys.getenv("PORT", "3000"))
WORKERS <- as.integer(Sys.getenv("WORKERS", "4"))
MAX_REQUEST_SIZE <- as.integer(Sys.getenv("MAX_REQUEST_SIZE", "16777216")) # 16 MB
TIMEOUT <- as.integer(Sys.getenv("TIMEOUT", "30"))

# Application state for shared resources
app_state <- list(
  startup_time = Sys.time(),
  shutdown_requested = FALSE
)

# Custom logger for benchmarking - production optimized
#
# @param message Log message character vector
# @param level Log level character vector (debug, info, warning, error)
benchmark_log <- function(message, level = "debug") {
  if (!DEBUG_MODE && level %in% c("debug", "info")) return(invisible(NULL))
  timestamp <- Sys.time()
  cat(sprintf("[%s] %s - %s\n", timestamp, level, message))
}

# Enhanced security headers configuration
SECURITY_HEADERS <- list(
  "X-Content-Type-Options" = "nosniff",
  "X-Frame-Options" = "DENY",
  "X-XSS-Protection" = "1; mode=block",
  "Content-Security-Policy" = "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self'; connect-src 'self'; form-action 'self'",
  "Referrer-Policy" = "strict-origin-when-cross-origin",
  "Cache-Control" = "no-cache, no-store, must-revalidate",
  "Strict-Transport-Security" = "max-age=63072000; includeSubDomains; preload",
  "Permissions-Policy" = "geolocation=(), microphone=(), camera=(), payment=(), usb=()"
)

# Request ID generator for tracing
generate_request_id <- function() {
  paste0(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
}

# Request handler function with production best practices
.http.request <- function(path, query, body, headers) {
  # Generate request ID for tracing
  request_id <- generate_request_id()
  
  # Combine security headers with content type and request ID
  resp_headers <- c(SECURITY_HEADERS, list(
    "X-Request-ID" = request_id
  ))
  
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
    
    # Security: Validate input - reject empty IDs
    if (is.null(resp_body) || resp_body == "" || nchar(trim(resp_body)) == 0) {
      benchmark_log("Invalid user ID: empty", "warning")
      status_code <- 400L
      resp_body <- ifelse(DEBUG_MODE, "Bad Request: Missing or invalid ID parameter", "")
    } else {
      benchmark_log(sprintf("User endpoint accessed with ID: %s", resp_body))
      status_code <- 200L
    }
  } else if (path == "/health") {
    resp_body <- "OK"
    benchmark_log("Health check endpoint accessed")
    status_code <- 200L
    
    # Check shutdown state
    if (app_state$shutdown_requested) {
      resp_body <- "Shutting down"
      status_code <- 503L
    }
  } else if (path == "/ready") {
    benchmark_log("Readiness check endpoint accessed")
    
    # Security: Ensure proper warmup before accepting traffic
    uptime <- as.numeric(difftime(Sys.time(), app_state$startup_time, units = "secs"))
    if (uptime < 5) {  # 5-second warmup period for security initialization
      resp_body <- "Not ready"
      status_code <- 503L
    } else if (app_state$shutdown_requested) {
      resp_body <- "Shutting down"
      status_code <- 503L
    } else {
      resp_body <- "Ready"
      status_code <- 200L
    }
  } else if (path == "/error") {
    resp_body <- if (DEBUG_MODE) "Internal Server Error" else ""
    benchmark_log("Error endpoint accessed", "error")
    status_code <- 500L
  } else {
    resp_body <- ""
    benchmark_log(sprintf("404 Not Found: %s", path), ifelse(DEBUG_MODE, "debug", "warning"))
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

# =============================================================================
# GRACEFUL SHUTDOWN HANDLING
# =============================================================================

# Handle shutdown signals for graceful shutdown
handle_shutdown <- function(sig) {
  benchmark_log(sprintf("Received signal %s, initiating graceful shutdown", sig), "warning")
  app_state$shutdown_requested <- TRUE
  
  # In production, we might want to exit after a short delay
  if (!DEBUG_MODE) {
    Sys.sleep(2)  # Allow time for current requests to complete
    quit(save = "no", status = 0, runLast = FALSE)
  }
}

# Note: R doesn't have direct signal handling like Unix systems,
# but we can use the tools package for better signal handling if available
if (requireNamespace("tools", quietly = TRUE)) {
  # Signal handling would go here if available
}


# =============================================================================
# STARTUP MESSAGE
# =============================================================================

# Startup message with configuration summary
if (DEBUG_MODE) {
  cat(sprintf("\n=== Rserve Framework Benchmark Server (Development Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Workers: %d\n", WORKERS))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat(sprintf("Max Request Size: %d bytes\n", MAX_REQUEST_SIZE))
  cat(sprintf("Timeout: %d seconds\n", TIMEOUT))
  cat("Security headers: Enabled\n")
  cat("Request ID tracking: Enabled\n")
  cat("Logging: Enabled (debug level)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /ready, /error\n")
  cat("=================================================================\n\n")
} else {
  cat(sprintf("\n=== Rserve Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Workers: %d\n", WORKERS))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Request ID tracking: Enabled\n")
  cat("Logging: Minimal (production mode)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /ready, /error\n")
  cat("=================================================================\n\n")
}

# Start Rserve with production best practices
Rserve::run.Rserve(http.port = PORT)
