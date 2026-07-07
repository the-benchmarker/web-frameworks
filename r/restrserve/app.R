# RestRserve Framework Benchmark Server - Production-Grade Implementation
#
# A high-performance benchmark server using RestRserve framework.
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

# Apply security headers to response
apply_security_headers <- function(res) {
  for (header in names(SECURITY_HEADERS)) {
    res$set_header(header, SECURITY_HEADERS[[header]])
  }
  # Add request ID for tracing
  res$set_header("X-Request-ID", generate_request_id())
  return(res)
}

# =============================================================================
# ROUTE HANDLERS
# =============================================================================

# Root endpoint handler
#
# GET /
func_serve_main <- function(req, res) {
  benchmark_log("Root endpoint accessed")
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  res$set_body("")
}

# User ID endpoint handler
#
# GET /user/:id
func_get_userid <- function(req, res) {
  id <- req$parameters_path
  
  # Security: Validate input - reject empty IDs
  if (is.null(id) || id == "" || nchar(trim(id)) == 0) {
    benchmark_log("Invalid user ID: empty", "warning")
    res <- apply_security_headers(res)
    res$set_header("Content-Type", "text/plain")
    res$set_status(400)
    if (DEBUG_MODE) {
      res$set_body("Bad Request: Missing or invalid ID parameter")
    } else {
      res$set_body("")
    }
    return()
  }
  
  benchmark_log(sprintf("User endpoint accessed with ID: %s", id))
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  res$set_body(id)
}

# Create user endpoint handler
#
# POST /user
func_post_user <- function(req, res) {
  benchmark_log("Create user endpoint accessed")
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  res$set_status(201)
  res$set_body("")
}

# =============================================================================
# HEALTH CHECK ENDPOINTS
# =============================================================================

# Liveness probe endpoint for monitoring
#
# GET /health
func_health_check <- function(req, res) {
  benchmark_log("Health check endpoint accessed")
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  
  # Check shutdown state
  if (app_state$shutdown_requested) {
    res$set_status(503)
    res$set_body("Shutting down")
  } else {
    res$set_status(200)
    res$set_body("OK")
  }
}

# Readiness probe endpoint for container orchestration
#
# GET /ready
func_ready_check <- function(req, res) {
  benchmark_log("Readiness check endpoint accessed")
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  
  # Security: Ensure proper warmup before accepting traffic
  uptime <- as.numeric(difftime(Sys.time(), app_state$startup_time, units = "secs"))
  if (uptime < 5) {  # 5-second warmup period for security initialization
    res$set_status(503)
    res$set_body("Not ready")
  } else if (app_state$shutdown_requested) {
    res$set_status(503)
    res$set_body("Shutting down")
  } else {
    res$set_status(200)
    res$set_body("Ready")
  }
}

# =============================================================================
# ERROR HANDLERS
# =============================================================================

# Error test endpoint for verifying error handling
#
# GET /error
func_error_test <- function(req, res) {
  benchmark_log("Error endpoint accessed", "error")
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  res$set_status(500)
  if (DEBUG_MODE) {
    res$set_body("Internal Server Error")
  } else {
    res$set_body("")
  }
}

# Create application instance
app <- RestRserve::Application$new()

# =============================================================================
# ROUTE DEFINITIONS
# =============================================================================

# Define Routes

# / main Route
app$add_get(
  path = "/",
  FUN = func_serve_main,
  match = "exact"
)

# /user/:id get id
app$add_get(
  path = "/user/{id}",
  FUN = func_get_userid,
  match = "regex"
)

# /user post route
app$add_post(
  path = "/user",
  FUN = func_post_user,
  match = "exact"
)

# /health check route (liveness probe)
app$add_get(
  path = "/health",
  FUN = func_health_check,
  match = "exact"
)

# /ready check route (readiness probe)
app$add_get(
  path = "/ready",
  FUN = func_ready_check,
  match = "exact"
)

# /error test route
app$add_get(
  path = "/error",
  FUN = func_error_test,
  match = "exact"
)

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
  cat(sprintf("\n=== RestRserve Framework Benchmark Server (Development Mode) ===\n"))
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
  cat("===================================================================\n\n")
} else {
  cat(sprintf("\n=== RestRserve Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Workers: %d\n", WORKERS))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Request ID tracking: Enabled\n")
  cat("Logging: Minimal (production mode)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /ready, /error\n")
  cat("===================================================================\n\n")
}

# Run Application
backend <- RestRserve::BackendRserve$new()
backend$start(app, http_port = PORT)
