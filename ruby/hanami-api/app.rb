# frozen_string_literal: true

require 'hanami/api'

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
  'Cache-Control' => 'no-cache, no-store, must-revalidate',
  'Content-Type' => 'text/plain'
}.freeze

# Startup message with configuration summary
puts "\n=== Hanami API Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "==============================================================\n\n"

# Custom middleware for security headers and logging
class SecurityHeadersMiddleware
  def initialize(app)
    @app = app
  end

  def call(env)
    puts "[DEBUG] #{env['REQUEST_METHOD']} - #{env['PATH_INFO']}" if DEBUG_MODE
    status, headers, body = @app.call(env)
    headers.merge!(SECURITY_HEADERS)
    [status, headers, body]
  end
end

class App < Hanami::API
  # Apply middleware
  use SecurityHeadersMiddleware

  get '/', to: ->(*) { 
    puts "[DEBUG] Root endpoint accessed" if DEBUG_MODE
    [200, SECURITY_HEADERS, ['']]
  }

  get '/health' do
    puts "[DEBUG] Health check endpoint accessed" if DEBUG_MODE
    [200, SECURITY_HEADERS, ['OK']]
  end

  get '/error' do
    puts "[ERROR] Error endpoint accessed" if DEBUG_MODE
    [500, SECURITY_HEADERS, [DEBUG_MODE ? 'Internal Server Error' : '']]
  end

  get '/user/:id' do
    puts "[DEBUG] User endpoint accessed with ID: #{params[:id]}" if DEBUG_MODE
    [200, SECURITY_HEADERS, [params[:id]]]
  end

  post '/user' do
    puts "[DEBUG] Create user endpoint accessed" if DEBUG_MODE
    [201, SECURITY_HEADERS, ['']]
  end
end
