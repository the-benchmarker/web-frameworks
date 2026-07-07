#!/usr/bin/env crystal
# Kemal Framework Production Server
# Optimized for production deployments with security best practices

require "kemal"

# Production configuration
Kemal.config do |cfg|
  # Environment
  cfg.env = ENV["ENVIRONMENT"]? || "production"
  
  # Security: Disable static file serving in production
  # Use a CDN or dedicated web server (Nginx, Apache) instead
  cfg.serve_static = false
  
  # Performance: Disable logging for better performance
  # Use external logging infrastructure in production
  cfg.logging = false
  
  # Security: Set secure headers
  cfg.secure_headers = true
  
  # Host and port configuration
  cfg.host = ENV["HOST"]? || "0.0.0.0"
  cfg.port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
  
  # Performance: Enable port reuse for clustering
  cfg.port_reuse = true
end

# Security: Input validation middleware
before_all do |env|
  # Validate and sanitize all incoming requests
  # This is a placeholder for custom validation logic
end

# Health check endpoint
get "/health" do |env|
  env.response.status = 200
  env.response.headers["Content-Type"] = "text/plain"
  "OK"
end

# Root endpoint
get "/" do |env|
  env.response.status = 200
  ""
end

# User detail endpoint
get "/user/:id" do |env|
  user_id = env.params.url["id"]
  
  # Input validation
  if user_id.empty?
    env.response.status = 400
    "Invalid user ID"
  else
    env.response.status = 200
    user_id
  end
end

# User creation endpoint
post "/user" do |env|
  env.response.status = 201
  ""
end

# Error handling
not_found do |env|
  env.response.status = 404
  env.response.headers["Content-Type"] = "application/json"
  {error: "Not Found"}.to_json
end

server_error do |env|
  env.response.status = 500
  env.response.headers["Content-Type"] = "application/json"
  {error: "Internal Server Error"}.to_json
end

# Start server with clustering support
Kemal.run
