# frozen_string_literal: true

# Benchmark Application Controller
# 
# A benchmark controller for Rails API application.
# Follows Ruby on Rails best practices including proper error handling,
# logging, and RESTful conventions.
class ApplicationController < ActionController::API
  # Disable CSRF protection for API-only benchmarking
  skip_before_action :verify_authenticity_token, raise: false

  # Configure logging for benchmark endpoints
  before_action :log_request_access

  # Error handling
  rescue_from StandardError, with: :handle_standard_error

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
    Rails.logger.debug("#{request.method} #{request.path}")
  end

  # Handle standard errors
  def handle_standard_error(error)
    Rails.logger.error("Unhandled error: #{error.message}")
    Rails.logger.error(error.backtrace.join("\n"))
    
    # For benchmarking, return empty response on error in production
    if Rails.env.production?
      head :internal_server_error, content_type: "text/plain"
    else
      render plain: "Internal Server Error: #{error.message}", 
             content_type: "text/plain", 
             status: :internal_server_error
    end
  end
end
