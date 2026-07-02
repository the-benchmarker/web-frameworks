# frozen_string_literal: true

require 'roda'

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
CONTENT_TYPE = 'text/plain'.freeze

# Security headers plugin for Roda - optimized with frozen constants
class SecurityHeaders < Roda
  route do |r|
    # Apply security headers to all responses
    SECURITY_HEADERS.each { |key, value| response.headers[key] = value }
    response.headers['Content-Type'] = CONTENT_TYPE
    
    # Call the main app
    r.run App.new
  end
end

# Custom logger for Roda - disabled in production
class DebugLogger
  def call(env)
    puts "[DEBUG] #{env['REQUEST_METHOD']} - #{env['PATH_INFO']}" if DEBUG_MODE
    @app.call(env)
  end
end

# Startup message with configuration summary
puts "\n=== Roda Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "===========================================================\n\n"

class App < Roda
  # Apply logging middleware
  plugin :middleware, DebugLogger
  
  route do |r|
    r.root do
      puts "[DEBUG] Root endpoint accessed" if DEBUG_MODE
      response.headers['Content-Type'] = CONTENT_TYPE
      ''
    end

    r.get 'health' do
      puts "[DEBUG] Health check endpoint accessed" if DEBUG_MODE
      response.headers['Content-Type'] = CONTENT_TYPE
      'OK'
    end

    r.get 'error' do
      puts "[ERROR] Error endpoint accessed" if DEBUG_MODE
      response.headers['Content-Type'] = CONTENT_TYPE
      response.status = 500
      DEBUG_MODE ? 'Internal Server Error' : ''
    end

    r.on 'user' do
      r.get String do |id|
        puts "[DEBUG] User endpoint accessed with ID: #{id}" if DEBUG_MODE
        response.headers['Content-Type'] = CONTENT_TYPE
        id
      end

      r.post true do
        puts "[DEBUG] Create user endpoint accessed" if DEBUG_MODE
        response.headers['Content-Type'] = CONTENT_TYPE
        response.status = 201
        ''
      end
    end
  end
end
