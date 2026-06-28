require 'hanami/api'

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
  'Cache-Control' => 'no-cache, no-store, must-revalidate',
  'Content-Type' => 'text/plain'
}

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Hanami API Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "==============================================================\n\n"
else
  puts "\n=== Hanami API Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "==============================================================\n\n"
end

# Custom middleware for security headers and logging
class SecurityHeadersMiddleware
  def initialize(app)
    @app = app
  end

  def call(env)
    if DEBUG_MODE
      puts "[DEBUG] #{env['REQUEST_METHOD']} - #{env['PATH_INFO']}"
    end
    
    status, headers, body = @app.call(env)
    headers.merge!(SECURITY_HEADERS)
    [status, headers, body]
  end
end

class App < Hanami::API
  # Apply middleware
  use SecurityHeadersMiddleware

  get '/', to: ->(*) { 
    if DEBUG_MODE
      puts "[DEBUG] Root endpoint accessed"
    end
    [200, SECURITY_HEADERS, ['']]
  }

  get '/health' do
    if DEBUG_MODE
      puts "[DEBUG] Health check endpoint accessed"
    end
    [200, SECURITY_HEADERS, ['OK']]
  end

  get '/error' do
    if DEBUG_MODE
      puts "[ERROR] Error endpoint accessed"
    end
    if DEBUG_MODE
      [500, SECURITY_HEADERS, ['Internal Server Error']]
    else
      [500, SECURITY_HEADERS, ['']]
    end
  end

  get '/user/:id' do
    if DEBUG_MODE
      puts "[DEBUG] User endpoint accessed with ID: #{params[:id]}"
    end
    [200, SECURITY_HEADERS, [params[:id]]]
  end

  post '/user' do
    if DEBUG_MODE
      puts "[DEBUG] Create user endpoint accessed"
    end
    [201, SECURITY_HEADERS, ['']]
  end
end
