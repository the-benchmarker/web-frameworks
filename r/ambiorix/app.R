# Ambiorix Framework Benchmark Server
#
# A production-grade benchmark server using Ambiorix framework.
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

# Security headers middleware - adds security headers to all responses
add_security_headers <- function(res) {
  res$setHeader("X-Content-Type-Options", "nosniff")
  res$setHeader("X-Frame-Options", "DENY")
  res$setHeader("X-XSS-Protection", "1; mode=block")
  res$setHeader("Content-Security-Policy", "default-src 'self'")
  res$setHeader("Referrer-Policy", "strict-origin-when-cross-origin")
  res$setHeader("Cache-Control", "no-cache, no-store, must-revalidate")
  return(res)
}

# Load required library
library(ambiorix)

# Create application instance
app <- Ambiorix$new()

# Configure application with production best practices
app$set("host", HOST)
app$set("port", PORT)
app$set("showErrors", FALSE)

# Root endpoint handler
#
# GET /
app$get("/", function(req, res) {
  benchmark_log("Root endpoint accessed")
  res <- add_security_headers(res)
  res$setHeader("Content-Type", "text/plain")
  res$send("")
})

# Get user by ID
#
# GET /user/:id
app$get("/user/:id", function(req, res) {
  user_id <- req$params$id
  benchmark_log(sprintf("User endpoint accessed with ID: %s", user_id))
  res <- add_security_headers(res)
  res$setHeader("Content-Type", "text/plain")
  res$send(user_id)
})

# Create new user
#
# POST /user
app$post("/user", function(req, res) {
  benchmark_log("Create user endpoint accessed")
  res <- add_security_headers(res)
  res$setHeader("Content-Type", "text/plain")
  res$setStatus(201)
  res$send("")
})

# Health check endpoint for monitoring
#
# GET /health
app$get("/health", function(req, res) {
  benchmark_log("Health check endpoint accessed")
  res <- add_security_headers(res)
  res$setHeader("Content-Type", "text/plain")
  res$send("OK")
})

# Error test endpoint for verifying error handling
#
# GET /error
app$get("/error", function(req, res) {
  benchmark_log("Error endpoint accessed", "error")
  res <- add_security_headers(res)
  res$setHeader("Content-Type", "text/plain")
  res$setStatus(500)
  if (DEBUG_MODE) {
    res$send("Internal Server Error")
  } else {
    res$send("")
  }
})

# Startup message with configuration summary
if (DEBUG_MODE) {
  cat(sprintf("\n=== Ambiorix Framework Benchmark Server (Development Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Enabled (debug level)\n")
  cat("Endpoints: /, /user/:id, /user, /health, /error\n")
  cat("=====================================================================\n\n")
} else {
  cat(sprintf("\n=== Ambiorix Framework Benchmark Server (Production Mode) ===\n"))
  cat(sprintf("Environment: %s\n", ENVIRONMENT))
  cat(sprintf("Host: %s\n", HOST))
  cat(sprintf("Port: %d\n", PORT))
  cat(sprintf("Debug: %s\n", DEBUG_MODE))
  cat("Security headers: Enabled\n")
  cat("Logging: Disabled (production mode)\n")
  cat("=====================================================================\n\n")
}

# Start the application
app$start()
