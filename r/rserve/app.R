# Rserve Framework Benchmark Server
#
# A high-performance benchmark server using Rserve framework.
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

# Request handler function with best practices
.http.request <- function(path, query, body, headers) {
  # Determine response body based on path
  if (path == "/" || path == "" || path == "/user") {
    resp_body <- ""
    benchmark_log("Root or user endpoint accessed")
  } else if (grepl("^/user/", path)) {
    # /user/id path - extract user ID using regex
    match_info <- regexec("^/user/(.*)", path)
    resp_body <- regmatches(path, match_info)[[1]][2]
    benchmark_log(sprintf("User endpoint accessed with ID: %s", resp_body))
  } else if (path == "/health") {
    resp_body <- "OK"
    benchmark_log("Health check endpoint accessed")
  } else {
    resp_body <- ""
    benchmark_log(sprintf("Unknown path accessed: %s", path))
  }

  # Set response headers and status
  status_code <- 200L
  resp_headers <- character(0)
  content_type <- "text/plain"

  # Return response
  list(
    resp_body,
    content_type,
    resp_headers,
    status_code
  )
}

# Start Rserve with best practices
Rserve::run.Rserve(http.port = 3000)
