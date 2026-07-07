#!/usr/bin/env crystal
# Runcobo Framework Production Server
# Optimized for production deployments with security best practices

require "runcobo"
require "log"

# Logging: Completely disable logging for production performance
ENV["SKIP_LOG"] = "true"
Log.setup :none

# Root endpoint
class Home::Index < BaseAction
  get "/"

  call do
    # Production-ready empty response
    render_plain ""
  end
end

# User creation endpoint
class Users::CreateUser < BaseAction
  post "/user"

  call do
    # Production-ready empty response with created status
    render_plain "", status: 201
  end
end

# User detail endpoint with input validation
class Users::ShowUser < BaseAction
  get "/user/:id"
  url NamedTuple(id: Int32)

  call do
    user_id = params[:id].to_s
    
    # Input validation
    if user_id.empty?
      render_plain "Invalid user ID", status: 400
    else
      render_plain user_id
    end
  end
end

# Production server startup with clustering support
port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
host = ENV["HOST"]? || "0.0.0.0"
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

worker_count.times do |_|
  Process.fork do
    Runcobo.start(
      reuse_port: true,
      host: host,
      port: port
    )
  end
end

# Keep main process alive
sleep
