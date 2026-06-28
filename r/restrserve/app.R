# RestRserve Framework Benchmark Server
#
# A high-performance benchmark server using RestRserve framework.
# Follows R best practices including proper error handling and logging.

# Enable error handling
options(expressions = 5000)
options(warn = 1)

# Custom logger for benchmarking
#
# @param message Log message character vector
# @param level Log level character vector (debug, info, error)
benchmark_log <- function(message, level = "debug") {
  timestamp <- Sys.time()
  cat(sprintf("[%s] %s - %s\n", timestamp, level, message))
}

# Root endpoint handler
#
# GET /
func_serve_main <- function(req, res) {
  benchmark_log("Root endpoint accessed")
  res$set_header("Content-Type", "text/plain")
  res$set_body("")
}

# User ID endpoint handler
#
# GET /user/:id
func_get_userid <- function(req, res) {
  id <- req$parameters_path
  benchmark_log(sprintf("User endpoint accessed with ID: %s", id))
  res$set_header("Content-Type", "text/plain")
  res$set_body(id)
}

# Create user endpoint handler
#
# POST /user
func_post_user <- function(req, res) {
  benchmark_log("Create user endpoint accessed")
  res$set_header("Content-Type", "text/plain")
  res$set_body("")
}

# Health check endpoint handler
#
# GET /health
func_health_check <- function(req, res) {
  benchmark_log("Health check endpoint accessed")
  res$set_header("Content-Type", "text/plain")
  res$set_body("OK")
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

# Run Application
backend <- RestRserve::BackendRserve$new()
backend$start(app, http_port = 3000)
