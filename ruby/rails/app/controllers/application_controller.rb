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
  
  # Category 5: Data Management - Database ORM Testing
  # Create a user in the database
  def create_user_db
    user = User.new(user_params)
    
    if user.save
      render json: {
        status: "created",
        user: {
          id: user.id,
          name: user.name,
          email: user.email,
          created_at: user.created_at.iso8601
        }
      }, status: :created
    else
      render json: { errors: user.errors.full_messages }, status: :unprocessable_entity
    end
  end
  
  # Get all users from database
  def list_users_db
    users = User.all.order(created_at: :desc)
    
    render json: {
      count: users.count,
      users: users.map { |u| { id: u.id, name: u.name, email: u.email } }
    }
  end
  
  # Get a specific user from database
  def get_user_db
    user = User.find_by(id: params[:id])
    
    if user
      render json: {
        id: user.id,
        name: user.name,
        email: user.email,
        created_at: user.created_at.iso8601
      }
    else
      render json: { error: "User not found" }, status: :not_found
    end
  end
  
  private
  
  def user_params
    params.require(:user).permit(:name, :email)
  end
end
