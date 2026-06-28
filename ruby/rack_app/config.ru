require "rack/app"

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers middleware for Rack
class SecurityHeaders
  def initialize(app)
    @app = app
  end

  def call(env)
    status, headers, body = @app.call(env)
    
    # Add security headers
    security_headers = {
      'X-Content-Type-Options' => 'nosniff',
      'X-Frame-Options' => 'DENY',
      'X-XSS-Protection' => '1; mode=block',
      'Content-Security-Policy' => "default-src 'self'",
      'Referrer-Policy' => 'strict-origin-when-cross-origin',
      'Cache-Control' => 'no-cache, no-store, must-revalidate'
    }
    
    headers.merge!(security_headers)
    [status, headers, body]
  end
end

# Logging middleware - disabled in production
class DebugLogger
  def initialize(app)
    @app = app
  end

  def call(env)
    if DEBUG_MODE
      puts "[DEBUG] #{env['REQUEST_METHOD']} - #{env['PATH_INFO']}"
    end
    @app.call(env)
  end
end

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Rack App Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "=================================================================\n\n"
else
  puts "\n=== Rack App Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "=================================================================\n\n"
end

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
    if DEBUG_MODE
      "Internal Server Error"
    else
      ""
    end
  end
end

# for more check out how-to
run App
