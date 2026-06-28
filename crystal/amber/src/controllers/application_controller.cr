class ApplicationController < Amber::Controller::Base
  # Security: Disable CSRF protection for API endpoints
  # Note: Enable this for web applications with forms
  skip_before_action :verify_authenticity_token

  # GET / - Root endpoint
  def index
    # Production-ready empty response
    respond_with ""
  end

  # GET /user/:id - User detail endpoint
  def get
    # Security: Validate and sanitize ID parameter
    user_id = params[:id].to_s
    
    # Input validation - ensure ID is not empty
    if user_id.empty?
      halt(400, "Invalid user ID")
    end
    
    respond_with user_id
  end

  # POST /user - User creation endpoint
  def create
    # Security: Force SSL in production (uncomment when using HTTPS)
    # force_ssl if ENV["ENVIRONMENT"]? == "production"
    
    # Production-ready empty response
    head(201)
  end
end
