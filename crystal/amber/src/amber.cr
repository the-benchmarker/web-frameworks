#!/usr/bin/env crystal
# Amber Framework Production Server
# Optimized for production deployments with security best practices

require "../config/application"

# Production server startup with clustering support
if ARGV.size > 0 && ARGV[0] == "--start-amber"
  # Start single server instance
  Amber::Server.start
else
  # Cluster mode: spawn worker processes for each CPU core
  # This provides better performance and fault tolerance
  worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count
  
  worker_count.times do |worker_id|
    Process.new(PROGRAM_NAME, ["--start-amber"])
  end
end

# Keep the main process alive to manage worker processes
sleep
