class ApplicationController < ActionController::API
  # Category 1: Core Framework Requirements - Request Routing & Response Building
  
  # Health check endpoint
  def index
    head 200
  end
  
  # Legacy user endpoint - Category 1: HTTP Method Support & Request Parsing
  def user
    render plain: params["id"]
  end
  
  # Legacy register user endpoint - Category 1: HTTP Method Support
  def register_user
    head 200
  end
  
  # Category 6: API & Integration - JSON API Support
  def api_json
    render json: {
      message: "JSON API response",
      timestamp: Time.current.iso8601,
      framework: "Rails",
      version: Rails.version
    }
  end
  
  # Category 6: API & Integration - External API Integration (simulated)
  def external_api
    # Simulate external API call
    external_data = {
      id: 1,
      title: "External resource",
      source: "mock_external_api"
    }
    render json: external_data
  end
  
  # Category 3: Performance & Scalability - Caching
  def cached_response
    # Simple in-memory cache simulation
    @cache ||= {}
    cache_key = "cached_data"
    
    if @cache[cache_key].nil?
      @cache[cache_key] = { data: "Cached response", cached_at: Time.current.iso8601 }
    end
    
    render json: @cache[cache_key]
  end
  
  # Category 4: Security - Basic Authentication
  def secure_endpoint
    auth_header = request.headers["Authorization"]
    
    if auth_header && auth_header.start_with?("Bearer ")
      token = auth_header.split(" ").last
      render json: { authenticated: true, token: token, message: "Access granted" }
    else
      render json: { error: "Unauthorized" }, status: :unauthorized
    end
  end
  
  # Category 4: Security - Protected endpoint with simple auth
  def protected_resource
    # For testing purposes, accept any bearer token
    auth_header = request.headers["Authorization"]
    
    if auth_header && auth_header.start_with?("Bearer ")
      render json: { resource: "Protected data", access: "granted" }
    else
      head :unauthorized
    end
  end
end
