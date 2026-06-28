# RestRserve Framework Benchmark Server
#
# A production-grade benchmark server using RestRserve framework.
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

# Apply security headers to response
apply_security_headers <- function(res) {
  for (header in names(SECURITY_HEADERS)) {
    res$set_header(header, SECURITY_HEADERS[[header]])
  }
  return(res)
}

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

# Health check endpoint handler
#
# GET /health
func_health_check <- function(req, res) {
  benchmark_log("Health check endpoint accessed")
  res <- apply_security_headers(res)
  res$set_header("Content-Type", "text/plain")
  res$set_body("OK")
}

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

# /health check route
app$add_get(
  path = "/health",
  FUN = func_health_check,
  match = "exact"
)

# /error test route
app$add_get(
  path = "/error",
  FUN = func_error_test,
  match = "exact"
)

# Startup message with configuration summary
if (DEBUG_MODE) {
  cat(sprintf("\n=== RestRserve Framework Benchmark Server (Development Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Enabled (debug level)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /error\n")
  cat("===================================================================\n\n")
} else {
  cat(sprintf("\n=== RestRserve Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Disabled (production mode)\n")
  cat("===================================================================\n\n")
}

# Run Application
backend <- RestRserve::BackendRserve$new()
backend$start(app, http_port = PORT)
