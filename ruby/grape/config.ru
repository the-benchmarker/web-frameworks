Bundler.require :default

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

# Custom error handlers
module Bench
  class BaseAPI < Grape::API
    # Configure base API settings
    version 'v1', using: :header, vendor: 'bench'
    format :txt
    default_format :txt
    
    # Apply security headers to all responses
    before do
      header 'X-Content-Type-Options', 'nosniff'
      header 'X-Frame-Options', 'DENY'
      header 'X-XSS-Protection', '1; mode=block'
      header 'Content-Security-Policy', "default-src 'self'"
      header 'Referrer-Policy', 'strict-origin-when-cross-origin'
      header 'Cache-Control', 'no-cache, no-store, must-revalidate'
      header 'Content-Type', 'text/plain'
    end

    # Root endpoint
    get do
      if DEBUG_MODE
        puts "[DEBUG] Root endpoint accessed"
      end
      body false
    end

    # Health check endpoint
    get '/health' do
      if DEBUG_MODE
        puts "[DEBUG] Health check endpoint accessed"
      end
      'OK'
    end

    # Error test endpoint
    get '/error' do
      if DEBUG_MODE
        puts "[ERROR] Error endpoint accessed"
      end
      error!('Internal Server Error', 500) if DEBUG_MODE
      error!('', 500)
    end
  end

  class UserAPI < Grape::API
    # Configure user API settings
    format :txt
    default_format :txt
    
    # Apply security headers
    before do
      header 'X-Content-Type-Options', 'nosniff'
      header 'X-Frame-Options', 'DENY'
      header 'X-XSS-Protection', '1; mode=block'
      header 'Content-Security-Policy', "default-src 'self'"
      header 'Referrer-Policy', 'strict-origin-when-cross-origin'
      header 'Cache-Control', 'no-cache, no-store, must-revalidate'
      header 'Content-Type', 'text/plain'
    end

    get "/user/:id" do
      if DEBUG_MODE
        puts "[DEBUG] User endpoint accessed with ID: #{params[:id]}"
      end
      params[:id]
    end
    
    post "/user" do
      if DEBUG_MODE
        puts "[DEBUG] Create user endpoint accessed"
      end
      status 201
      body false
    end
  end

  class API < Grape::API
    mount ::Bench::BaseAPI
    mount ::Bench::UserAPI
  end
end

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Grape Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "============================================================\n\n"
else
  puts "\n=== Grape Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "============================================================\n\n"
end

Bench::API.compile!
run Bench::API
