# frozen_string_literal: true

require "rack/app"

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers configuration - frozen for performance
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}.freeze

# Content type constant
CONTENT_TYPE = { 'Content-Type' => 'text/plain' }.freeze

# Security headers middleware for Rack - optimized with frozen constants
class SecurityHeaders
  def initialize(app)
    @app = app
  end

  def call(env)
    status, headers, body = @app.call(env)
    headers.merge!(SECURITY_HEADERS)
    [status, headers, body]
  end
end

# Logging middleware - disabled in production - optimized inline check
class DebugLogger
  def initialize(app)
    @app = app
  end

  def call(env)
    puts "[DEBUG] #{env['REQUEST_METHOD']} - #{env['PATH_INFO']}" if DEBUG_MODE
    @app.call(env)
  end
end

# Startup message with configuration summary
puts "\n=== Rack App Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "=================================================================\n\n"

class App < Rack::App
  # Apply security headers to all responses
  use SecurityHeaders
  use DebugLogger

  get "" do
    content_type 'text/plain'
    ""
  end

  get "/user/:id" do
    content_type 'text/plain'
    params["id"]
  end

  post "/user" do
    content_type 'text/plain'
    status 201
    ""
  end

  get "/health" do
    content_type 'text/plain'
    "OK"
  end

  get "/error" do
    content_type 'text/plain'
    status 500
    DEBUG_MODE ? "Internal Server Error" : ""
  end
end

# for more check out how-to
run App
