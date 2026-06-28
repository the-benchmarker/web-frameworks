# frozen_string_literal: true

# Benchmark Application Controller
# 
# A production-grade benchmark controller for Rails API application.
# Implements security best-practices, proper error handling, and environment-based configuration.
class ApplicationController < ActionController::API
  # Disable CSRF protection for API-only benchmarking
  skip_before_action :verify_authenticity_token, raise: false

  # Security headers for all responses
  before_action :set_security_headers
  
  # Configure logging for benchmark endpoints
  before_action :log_request_access

  # Error handling
  rescue_from StandardError, with: :handle_standard_error
  
  private
  
  # Security headers configuration
  SECURITY_HEADERS = {
    'X-Content-Type-Options' => 'nosniff',
    'X-Frame-Options' => 'DENY',
    'X-XSS-Protection' => '1; mode=block',
    'Content-Security-Policy' => "default-src 'self'",
    'Referrer-Policy' => 'strict-origin-when-cross-origin',
    'Cache-Control' => 'no-cache, no-store, must-revalidate'
  }.freeze
  
  def set_security_headers
    SECURITY_HEADERS.each do |key, value|
      response.headers[key] = value
    end
  end

  # Root endpoint
  # GET /
  def index
    Rails.logger.debug("Root endpoint accessed")
    head :ok, content_type: "text/plain"
  end

  # Get user by ID endpoint
  # GET /user/:id
  def user
    Rails.logger.debug("User endpoint accessed with ID: #{params[\"id\"]}")
    render plain: params["id"], content_type: "text/plain", status: :ok
  end

  # Create user endpoint
  # POST /user
  def register_user
    Rails.logger.debug("Create user endpoint accessed")
    head :ok, content_type: "text/plain"
  end

  # Health check endpoint
  # GET /health
  def health_check
    render plain: "OK", content_type: "text/plain", status: :ok
  end

  private

  # Log request access for debugging
  def log_request_access
    if DEBUG_MODE
      Rails.logger.debug("#{request.method} #{request.path}")
    end
  end

  # Handle standard errors
  def handle_standard_error(error)
    if DEBUG_MODE
      Rails.logger.error("Unhandled error: #{error.message}")
      Rails.logger.error(error.backtrace.join("\n"))
    end
    
    # For benchmarking, return empty response on error in production
    if Rails.env.production? || !DEBUG_MODE
      head :internal_server_error, content_type: "text/plain"
    else
      render plain: "Internal Server Error: #{error.message}", 
             content_type: "text/plain", 
             status: :internal_server_error
    end
  end

public
  
  # Root endpoint
  # GET /
  def index
    if DEBUG_MODE
      Rails.logger.debug("Root endpoint accessed")
    end
    head :ok, content_type: "text/plain"
  end

  # Get user by ID endpoint
  # GET /user/:id
  def user
    if DEBUG_MODE
      Rails.logger.debug("User endpoint accessed with ID: #{params[\"id\"]}")
    end
    render plain: params["id"], content_type: "text/plain", status: :ok
  end

  # Create user endpoint
  # POST /user
  def register_user
    if DEBUG_MODE
      Rails.logger.debug("Create user endpoint accessed")
    end
    head :created, content_type: "text/plain"
  end

  # Health check endpoint
  # GET /health
  def health_check
    if DEBUG_MODE
      Rails.logger.debug("Health check endpoint accessed")
    end
    render plain: "OK", content_type: "text/plain", status: :ok
  end

  # Error test endpoint
  # GET /error
  def error
    if DEBUG_MODE
      Rails.logger.error("Error endpoint accessed")
    end
    if DEBUG_MODE
      render plain: "Internal Server Error", content_type: "text/plain", status: :internal_server_error
    else
      head :internal_server_error, content_type: "text/plain"
    end
  end
end
