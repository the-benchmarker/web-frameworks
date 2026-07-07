#!/usr/bin/env crystal
# Orion Framework Production Server
# Optimized for production deployments with security best practices

require "orion"
require "log"

# Logging: Completely disable logging for production performance
Log.setup(:none)

# Application Router
router MyApplication do
  # Health check endpoint
  get "/health", ->(context : Context) do
    context.response.print "OK"
    context.response.status_code = 200
  end

  # Root endpoint
  get "/", ->(context : Context) do
    context.response.print ""
    context.response.status_code = 200
  end

  # User detail endpoint
  get "/user/:id", ->(context : Context) do
    user_id = context.request.path_params["id"]
    
    # Input validation
    if user_id.empty?
      context.response.print "Invalid user ID"
      context.response.status_code = 400
    else
      context.response.print user_id
      context.response.status_code = 200
    end
  end

  # User creation endpoint
  post "/user", ->(context : Context) do
    context.response.print ""
    context.response.status_code = 201
  end
end

# Production server startup with clustering support
port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
host = ENV["HOST"]? || "0.0.0.0"
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

MyApplication.start(
  workers: worker_count,
  host: host,
  port: port,
  reuse_port: true
)