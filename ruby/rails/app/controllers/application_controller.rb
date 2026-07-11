class ApplicationController < ActionController::API
  include ActionController::Serialization
  
  before_action :set_default_headers
  before_action :log_request
  around_action :handle_exceptions
  
  # Main health check endpoint
  def index
    render json: {
      status: 'ok',
      timestamp: Time.current.iso8601,
      framework: 'Rails',
      version: Rails.version,
      environment: Rails.env,
      endpoints: {
        health: '/',
        users: '/api/v1/users',
        posts: '/api/v1/posts',
        external: '/api/v1/external',
        cache: '/api/v1/cache',
        jobs: '/api/v1/jobs'
      }
    }, status: :ok
  end
  
  # Legacy user endpoint for backward compatibility
  def user
    render plain: params["id"]
  end
  
  # Legacy register user endpoint
  def register_user
    head 200
  end
  
  # Handle 404 Not Found
  def not_found
    render json: { error: 'Not Found' }, status: :not_found
  end
  
  private
  
  def set_default_headers
    response.headers['X-Framework'] = 'Rails'
    response.headers['X-Version'] = Rails.version
  end
  
  def log_request
    Rails.logger.info "Request: #{request.method} #{request.path} from #{request.ip}"
  end
  
  def handle_exceptions
    yield
  rescue ActiveRecord::RecordNotFound => e
    render json: { error: e.message }, status: :not_found
  rescue ActiveRecord::RecordInvalid => e
    render json: { error: e.message }, status: :unprocessable_entity
  rescue StandardError => e
    Rails.logger.error "Unhandled exception: #{e.message}"
    render json: { error: 'Internal Server Error' }, status: :internal_server_error
  end
end
