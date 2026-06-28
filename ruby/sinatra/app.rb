# frozen_string_literal: true

# Benchmark Sinatra Application
# 
# A production-grade benchmark server implementation using Sinatra framework.
# Implements security best-practices, proper error handling, and environment-based configuration.

require 'sinatra'
require 'sinatra/base'
require 'logger'

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
  puts "\n=== Sinatra Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "==============================================================\n\n"
else
  puts "\n=== Sinatra Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "==============================================================\n\n"
end

# Configure Sinatra application
class BenchmarkApp < Sinatra::Base
  # Configure environment-based settings
  set :environment, ENVIRONMENT.to_sym
  
  # Enable logging in development, disable in production
  if DEBUG_MODE
    set :logging, Logger::DEBUG
    logger.level = Logger::DEBUG
  else
    set :logging, false
  end
  
  # Configure logging format
  def self.logger
    @logger ||= begin
      logger = Logger.new(STDOUT)
      logger.level = DEBUG_MODE ? Logger::DEBUG : Logger::WARN
      logger.formatter = proc { |severity, datetime, progname, msg|
        if DEBUG_MODE || severity >= Logger::WARN
          "#{datetime} - #{severity} - #{msg}\n"
        else
          ""
        end
      }
      logger
    end
  end

  # Apply security headers to all responses
  before do
    SECURITY_HEADERS.each do |key, value|
      response.headers[key] = value
    end
    response.headers['Content-Type'] = 'text/plain'
  end

  # Root endpoint
  # GET /
  get '/' do
    logger.debug('Root endpoint accessed') if DEBUG_MODE
    status 200
    body ''
  end

  # Get user by ID endpoint
  # GET /user/:id
  get '/user/:id' do |id|
    logger.debug("User endpoint accessed with ID: #{id}") if DEBUG_MODE
    status 200
    body id.to_s
  end

  # Create user endpoint
  # POST /user
  post '/user' do
    logger.debug('Create user endpoint accessed') if DEBUG_MODE
    status 201
    body ''
  end

  # Health check endpoint for monitoring
  # GET /health
  get '/health' do
    logger.debug('Health check endpoint accessed') if DEBUG_MODE
    status 200
    body 'OK'
  end

  # Error test endpoint for verifying error handling
  # GET /error
  get '/error' do
    logger.error('Error endpoint accessed') if DEBUG_MODE
    status 500
    if DEBUG_MODE
      body 'Internal Server Error'
    else
      body ''
    end
  end

  # Error handling
  not_found do
    status 404
    if DEBUG_MODE
      body 'Not Found'
    else
      body ''
    end
  end

  # Internal server error handling
  error do
    status 500
    if DEBUG_MODE
      body "Internal Server Error: #{env['sinatra.error'].message}"
    else
      body ''
    end
  end

  # Custom error handler
  configure do
    set :show_exceptions, DEBUG_MODE
    set :raise_errors, DEBUG_MODE
    set :dump_errors, DEBUG_MODE
  end
end

# Create and run application
BenchmarkApp.run! do |server|
  server.set :bind, HOST
  server.set :port, PORT
  server.set :server, :puma  # Use Puma server for better performance
end
