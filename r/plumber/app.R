# Plumber Framework Benchmark Server
#
# A production-grade benchmark server using Plumber framework.
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

# Security headers middleware for Plumber
# Plumber uses filters to add headers to responses
add_security_headers <- function(req) {
  list(
    'X-Content-Type-Options' = 'nosniff',
    'X-Frame-Options' = 'DENY',
    'X-XSS-Protection' = '1; mode=block',
    'Content-Security-Policy' = "default-src 'self'",
    'Referrer-Policy' = 'strict-origin-when-cross-origin',
    'Cache-Control' = 'no-cache, no-store, must-revalidate'
  )
}

# Startup message with configuration summary
if (DEBUG_MODE) {
  cat(sprintf("\n=== Plumber Framework Benchmark Server (Development Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Enabled (debug level)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /error\n")
  cat("==================================================================\n\n")
} else {
  cat(sprintf("\n=== Plumber Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Disabled (production mode)\n")
  cat("==================================================================\n\n")
}

# Start Plumber application with production best practices
plumber::plumb("plumber.R")$run(
  host = HOST,
  port = PORT,
  swagger = DEBUG_MODE,  # Enable swagger only in development
  debug = DEBUG_MODE     # Enable debug mode only in development
)
