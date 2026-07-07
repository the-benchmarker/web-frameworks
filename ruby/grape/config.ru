# frozen_string_literal: true

Bundler.require :default

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

# Custom error handlers
module Bench
  class BaseAPI < Grape::API
    # Configure base API settings
    version 'v1', using: :header, vendor: 'bench'
    format :txt
    default_format :txt
    
    # Apply security headers to all responses
    before do
      SECURITY_HEADERS.each { |key, value| header key, value }
      header 'Content-Type', 'text/plain'
    end

    # Root endpoint
    get do
      puts "[DEBUG] Root endpoint accessed" if DEBUG_MODE
      body false
    end

    # Health check endpoint
    get '/health' do
      puts "[DEBUG] Health check endpoint accessed" if DEBUG_MODE
      'OK'
    end

    # Error test endpoint
    get '/error' do
      puts "[ERROR] Error endpoint accessed" if DEBUG_MODE
      error!(DEBUG_MODE ? 'Internal Server Error' : '', 500)
    end
  end

  class UserAPI < Grape::API
    # Configure user API settings
    format :txt
    default_format :txt
    
    # Apply security headers
    before do
      SECURITY_HEADERS.each { |key, value| header key, value }
      header 'Content-Type', 'text/plain'
    end

    get "/user/:id" do
      puts "[DEBUG] User endpoint accessed with ID: #{params[:id]}" if DEBUG_MODE
      params[:id]
    end
    
    post "/user" do
      puts "[DEBUG] Create user endpoint accessed" if DEBUG_MODE
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
puts "\n=== Grape Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "============================================================\n\n"

Bench::API.compile!
run Bench::API
