require 'roda'

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers plugin for Roda
class SecurityHeaders < Roda
  route do |r|
    # Apply security headers to all responses
    response.headers['X-Content-Type-Options'] = 'nosniff'
    response.headers['X-Frame-Options'] = 'DENY'
    response.headers['X-XSS-Protection'] = '1; mode=block'
    response.headers['Content-Security-Policy'] = "default-src 'self'"
    response.headers['Referrer-Policy'] = 'strict-origin-when-cross-origin'
    response.headers['Cache-Control'] = 'no-cache, no-store, must-revalidate'
    response.headers['Content-Type'] = 'text/plain'
    
    # Call the main app
    r.run App.new
  end
end

# Custom logger for Roda - disabled in production
class DebugLogger
  def call(env)
    if DEBUG_MODE
      puts "[DEBUG] #{env['REQUEST_METHOD']} - #{env['PATH_INFO']}"
    end
    @app.call(env)
  end
end

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Roda Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "===========================================================\n\n"
else
  puts "\n=== Roda Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "===========================================================\n\n"
end

class App < Roda
  # Apply logging middleware
  plugin :middleware, DebugLogger
  
  route do |r|
    r.root do
      if DEBUG_MODE
        puts "[DEBUG] Root endpoint accessed"
      end
      response.headers['Content-Type'] = 'text/plain'
      ''
    end

    r.get 'health' do
      if DEBUG_MODE
        puts "[DEBUG] Health check endpoint accessed"
      end
      response.headers['Content-Type'] = 'text/plain'
      'OK'
    end

    r.get 'error' do
      if DEBUG_MODE
        puts "[ERROR] Error endpoint accessed"
      end
      response.headers['Content-Type'] = 'text/plain'
      response.status = 500
      DEBUG_MODE ? 'Internal Server Error' : ''
    end

    r.on 'user' do
      r.get String do |id|
        if DEBUG_MODE
          puts "[DEBUG] User endpoint accessed with ID: #{id}"
        end
        response.headers['Content-Type'] = 'text/plain'
        id
      end

      r.post true do
        if DEBUG_MODE
          puts "[DEBUG] Create user endpoint accessed"
        end
        response.headers['Content-Type'] = 'text/plain'
        response.status = 201
        ''
      end
    end
  end
end
