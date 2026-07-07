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

# Security headers configuration - frozen for performance
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}.freeze

# Startup message with configuration summary
puts "\n=== Sinatra Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "==============================================================\n\n"

# Configure Sinatra application
class BenchmarkApp < Sinatra::Base
  # Configure environment-based settings
  set :environment, ENVIRONMENT.to_sym
  
  # Enable logging in development, disable in production
  configure do
    set :logging, DEBUG_MODE
    logger.level = DEBUG_MODE ? Logger::DEBUG : Logger::WARN if logger
  end
  
  # Configure logging format
  def self.logger
    @logger ||= Logger.new(STDOUT).tap do |logger|
      logger.level = DEBUG_MODE ? Logger::DEBUG : Logger::WARN
      logger.formatter = ->(severity, datetime, progname, msg) {
        (DEBUG_MODE || severity >= Logger::WARN) ? "#{datetime} - #{severity} - #{msg}\n" : ""
      }
    end
  end

  # Apply security headers to all responses
  before do
    SECURITY_HEADERS.each { |key, value| response.headers[key] = value }
    response.headers['Content-Type'] = 'text/plain'
  end

  # Root endpoint
  # GET /
  get '/' do
    logger.debug('Root endpoint accessed') if DEBUG_MODE
    [200, {}, '']
  end

  # Get user by ID endpoint
  # GET /user/:id
  get '/user/:id' do |id|
    logger.debug("User endpoint accessed with ID: #{id}") if DEBUG_MODE
    [200, {}, id.to_s]
  end

  # Create user endpoint
  # POST /user
  post '/user' do
    logger.debug('Create user endpoint accessed') if DEBUG_MODE
    [201, {}, '']
  end

  # Health check endpoint for monitoring
  # GET /health
  get '/health' do
    logger.debug('Health check endpoint accessed') if DEBUG_MODE
    [200, {}, 'OK']
  end

  # Error test endpoint for verifying error handling
  # GET /error
  get '/error' do
    logger.error('Error endpoint accessed') if DEBUG_MODE
    [500, {}, DEBUG_MODE ? 'Internal Server Error' : '']
  end

  # Error handling
  not_found do
    [404, {}, DEBUG_MODE ? 'Not Found' : '']
  end

  # Internal server error handling
  error do
    [500, {}, DEBUG_MODE ? "Internal Server Error: #{env['sinatra.error'].message}" : '']
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
