require 'syro'

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers configuration
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Syro Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "=========================================================\n\n"
else
  puts "\n=== Syro Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "=========================================================\n\n"
end

# Custom middleware for security headers and logging
class SecurityHeaders
  def initialize(app)
    @app = app
  end

  def call(env)
    status, headers, body = @app.call(env)
    
    # Add security headers
    SECURITY_HEADERS.each do |key, value|
      headers[key] = value
    end
    headers['Content-Type'] = 'text/plain'
    
    [status, headers, body]
  end
end

# Custom middleware for debug logging
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

App = Syro.new do
  # Apply middleware
  use DebugLogger
  use SecurityHeaders
  
  get do
    if DEBUG_MODE
      puts "[DEBUG] Root endpoint accessed"
    end
    res.status = 200
    res.write ''
  end

  get 'health' do
    if DEBUG_MODE
      puts "[DEBUG] Health check endpoint accessed"
    end
    res.status = 200
    res.write 'OK'
  end

  get 'error' do
    if DEBUG_MODE
      puts "[ERROR] Error endpoint accessed"
    end
    res.status = 500
    res.write DEBUG_MODE ? 'Internal Server Error' : ''
  end

  on 'user' do
    on :id do
      get do
        if DEBUG_MODE
          puts "[DEBUG] User endpoint accessed with ID: #{inbox[:id]}"
        end
        res.status = 200
        res.write inbox[:id]
      end
    end

    post do
      if DEBUG_MODE
        puts "[DEBUG] Create user endpoint accessed"
      end
      res.status = 201
      res.write ''
    end
  end
end
