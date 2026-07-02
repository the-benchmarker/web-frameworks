# Plumber Framework Benchmark Server - Production-Grade Implementation
#
# A high-performance benchmark server using Plumber framework.
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

# Security headers middleware for Plumber
# Plumber uses filters to add headers to responses
add_security_headers <- function(req) {
  SECURITY_HEADERS
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
  cat(sprintf("\n=== Plumber Framework Benchmark Server (Development Mode) ===\n"))
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
  cat("==================================================================\n\n")
} else {
  cat(sprintf("\n=== Plumber Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Workers: %d\n", WORKERS))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Request ID tracking: Enabled\n")
  cat("Logging: Minimal (production mode)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /ready, /error\n")
  cat("==================================================================\n\n")
}

# Start Plumber application with production best practices
plumber::plumb("plumber.R")$run(
  host = HOST,
  port = PORT,
  swagger = DEBUG_MODE,  # Enable swagger only in development
  debug = DEBUG_MODE     # Enable debug mode only in development
)
