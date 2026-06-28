#!/usr/bin/env crystal
# Marten Framework Production Server
# Optimized for production deployments with security best practices

require "./project"

# Production server startup with clustering support
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

worker_count.times do |worker_id|
  Process.fork do
    Marten.start
  end
end

# Keep main process alive
sleep
