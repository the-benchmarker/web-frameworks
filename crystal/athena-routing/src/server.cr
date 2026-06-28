#!/usr/bin/env crystal
# Athena-Routing Framework Production Server
# Optimized for production deployments with security best practices

require "athena-routing"
require "log"

# Logging: Completely disable logging for production performance
Log.setup :none

# Create routing handler
handler = ART::RoutingHandler.new

# Health check endpoint
handler.add "health", ART::Route.new("/health", methods: "GET") do |ctx, _|
  ctx.response.status = 200
  ctx.response << "OK"
end

# Root endpoint
handler.add "root", ART::Route.new("/", methods: "GET") do |ctx, _|
  ctx.response.status = 200
end

# User detail endpoint with input validation
handler.add "user", ART::Route.new("/user/{id<\\d+>}", methods: "GET") do |ctx, params|
  user_id = params["id"].not_nil!.to_i
  
  # Input validation
  if user_id > 0
    ctx.response.status = 200
    ctx.response << user_id
  else
    ctx.response.status = 400
    ctx.response << "Invalid user ID"
  end
end

# User creation endpoint
handler.add "new_user", ART::Route.new("/user", methods: "POST") do |ctx, _|
  ctx.response.status = 201
end

# Production server startup with clustering support
port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
host = ENV["HOST"]? || "0.0.0.0"
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

worker_count.times do
  Process.fork do
    server = HTTP::Server.new([
      handler.compile,
    ])

    server.bind_tcp host: host, port: port, reuse_port: true
    server.listen
  end
end

# Keep main process alive
sleep
