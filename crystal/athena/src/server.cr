#!/usr/bin/env crystal
# Athena Framework Production Server
# Optimized for production deployments with security best practices

require "athena"
require "log"

# Logging: Completely disable logging for production performance
Log.setup :none

# Production-ready Controller
class BenchmarkController < ATH::Controller
  # Security: Disable CSRF protection for API endpoints
  # Note: Enable this for web applications with forms
  skip_before_action :verify_authenticity_token

  # GET / - Root endpoint
  @[ARTA::Get("/")]
  def root_get : Nil
    # Empty response for benchmarking
  end

  # POST /user - User creation endpoint
  @[ARTA::Post("/user")]
  def root_post : Nil
    # Empty response with created status
    response.status = 201
  end

  # GET /user/:id - User detail endpoint
  @[ARTA::Get("/user/{id<\\d+>}")]
  def user(id : Int32) : Int32
    # Input validation - ensure ID is positive
    if id > 0
      id
    else
      response.status = 400
      -1
    end
  end

  # Health check endpoint
  @[ARTA::Get("/health")]
  def health_check : Nil
    response.status = 200
  end
end

# Production server startup with clustering support
port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
host = ENV["HOST"]? || "0.0.0.0"
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

worker_count.times do
  Process.fork do
    ATH.run(
      host: host,
      port: port,
      reuse_port: true
    )
  end
end

# Keep main process alive
sleep
