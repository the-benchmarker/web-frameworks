# httpuv Framework Benchmark Server
#
# A high-performance benchmark server using httpuv framework.
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
request_handler <- function(req) {
  path <- req$PATH_INFO
  
  # Determine response body based on path
  if (path == "/" || path == "" || path == "/user") {
    body <- ""
    benchmark_log("Root or user endpoint accessed")
  } else if (grepl("^/user/", path)) {
    # /user/id path - extract user ID using regex
    match_info <- regexec("^/user/(.*)", path)
    user_id <- regmatches(path, match_info)[[1]][2]
    body <- user_id
    benchmark_log(sprintf("User endpoint accessed with ID: %s", user_id))
  } else if (path == "/health") {
    body <- "OK"
    benchmark_log("Health check endpoint accessed")
  } else {
    body <- ""
    benchmark_log(sprintf("Unknown path accessed: %s", path))
  }

  # Return response with proper headers
  list(
    status = 200L,
    headers = list('Content-Type' = 'text/plain'),
    body = body
  )
}

# Start httpuv server
s <- httpuv::startServer(
  host = "0.0.0.0",
  port = 3000,
  app = list(
    call = request_handler
  )
)

# Main service loop
while (TRUE) {
  httpuv::service()
}
