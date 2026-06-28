# Plumber Framework Benchmark Server
#
# A high-performance benchmark server using Plumber framework.
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

# Start Plumber application with best practices
plumber::plumb("plumber.R")$run(
  host = "0.0.0.0",
  port = 3000,
  swagger = FALSE,  # Disable swagger for production
  debug = FALSE    # Disable debug mode for production
)
