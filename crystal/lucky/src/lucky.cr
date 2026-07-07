#!/usr/bin/env crystal
# Lucky Framework Production Server
# Optimized for production deployments with security best practices

require "lucky"
require "../config/setup"
require "./actions/api_action"

# Production server startup with clustering support
port = Lucky::Server.settings.port
host = Lucky::Server.settings.host

# Create HTTP server with Lucky route handler
server = HTTP::Server.new([
  Lucky::RouteHandler.new,
])

# Worker configuration
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

# Start server with clustering
puts "Starting Lucky production server on #{host}:#{port} with #{worker_count} workers"

worker_count.times do |worker_id|
  Process.fork do
    # Each worker listens on the same port with port reuse
    server.listen host, port, reuse_port: true
  end
end

# Graceful shutdown handling
terminate = Proc(Signal, Nil).new do |signal|
  puts "Shutting down gracefully..."
  spawn { server.close }
  signal.ignore
end

Signal::INT.trap &terminate
Signal::TERM.trap &terminate

# Keep main process alive to manage workers
puts "Lucky server started with #{worker_count} worker processes"
sleep
