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
    SECURITY_HEADERS.each { |key, value| response.headers[key] = value }
  end

  # Root endpoint
  # GET /
  def index
    Rails.logger.debug("Root endpoint accessed") if DEBUG_MODE && Rails.logger
    head :ok, content_type: "text/plain"
  end

  # Get user by ID endpoint
  # GET /user/:id
  def user
    Rails.logger.debug("User endpoint accessed with ID: #{params[\"id\"]}") if DEBUG_MODE && Rails.logger
    render plain: params["id"], content_type: "text/plain", status: :ok
  end

  # Create user endpoint
  # POST /user
  def register_user
    Rails.logger.debug("Create user endpoint accessed") if DEBUG_MODE && Rails.logger
    head :created, content_type: "text/plain"
  end

  # Health check endpoint
  # GET /health
  def health_check
    Rails.logger.debug("Health check endpoint accessed") if DEBUG_MODE && Rails.logger
    render plain: "OK", content_type: "text/plain", status: :ok
  end

  private

  # Log request access for debugging
  def log_request_access
    Rails.logger.debug("#{request.method} #{request.path}") if DEBUG_MODE && Rails.logger
  end

  # Handle standard errors
  def handle_standard_error(error)
    if DEBUG_MODE && Rails.logger
      Rails.logger.error("Unhandled error: #{error.message}")
      Rails.logger.error(error.backtrace.join("\n"))
    end
    
    # For benchmarking, return empty response on error in production
    if !DEBUG_MODE
      head :internal_server_error, content_type: "text/plain"
    else
      render plain: "Internal Server Error: #{error.message}", 
             content_type: "text/plain", 
             status: :internal_server_error
    end
  end

public

  # Error test endpoint
  # GET /error
  def error
    Rails.logger.error("Error endpoint accessed") if DEBUG_MODE && Rails.logger
    if DEBUG_MODE
      render plain: "Internal Server Error", content_type: "text/plain", status: :internal_server_error
    else
      head :internal_server_error, content_type: "text/plain"
    end
  end
end
