# frozen_string_literal: true

# Benchmark Sinatra Application
# 
# A benchmark server implementation using Sinatra framework.
# Follows Ruby best practices including proper error handling, logging,
# and RESTful conventions.

require 'sinatra'
require 'sinatra/base'
require 'logger'

# Configure Sinatra application
class BenchmarkApp < Sinatra::Base
  # Enable logging
  set :logging, Logger::INFO
  
  # Configure logging format
  def self.logger
    @logger ||= begin
      logger = Logger.new(STDOUT)
      logger.level = Logger::INFO
      logger.formatter = proc { |severity, datetime, progname, msg|
        "#{datetime} - #{severity} - #{msg}\n"
      }
      logger
    end
  end

  # Root endpoint
  # GET /
  get '/' do
    logger.debug('Root endpoint accessed')
    content_type :txt
    status 200
    body ''
  end

  # Get user by ID endpoint
  # GET /user/:id
  get '/user/:id' do |id|
    logger.debug("User endpoint accessed with ID: #{id}")
    content_type :txt
    status 200
    body id.to_s
  end

  # Create user endpoint
  # POST /user
  post '/user' do
    logger.debug('Create user endpoint accessed')
    content_type :txt
    status 200
    body ''
  end

  # Health check endpoint for monitoring
  # GET /health
  get '/health' do
    content_type :txt
    status 200
    body 'OK'
  end

  # Error handling
  not_found do
    content_type :txt
    status 404
    body 'Not Found'
  end

  # Internal server error handling
  error do
    content_type :txt
    status 500
    
    if env('RACK_ENV') == 'production'
      body ''
    else
      body "Internal Server Error: #{env['sinatra.error'].message}"
    end
  end

  # Custom error handler
  configure do
    set :show_exceptions, false
    set :raise_errors, false
    set :dump_errors, false
  end
end

# Create and run application
BenchmarkApp.run! do |server|
  server.set :bind, ENV.fetch('HOST', '0.0.0.0')
  server.set :port, ENV.fetch('PORT', 3000).to_i
end
