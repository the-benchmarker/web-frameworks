class ApiController < ActionController::API
  # Security headers for all responses
  before_action :set_security_headers
  
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
  
  def debug_log(message, level = 'debug')
    return unless DEBUG_MODE
    case level
    when 'error'
      Rails.logger.error(message) if Rails.logger
    when 'info'
      Rails.logger.info(message) if Rails.logger
    else
      Rails.logger.debug(message) if Rails.logger
    end
  end

public
  
  def index
    debug_log('Root endpoint accessed')
    head 200, content_type: "text/plain"
  end

  def user
    debug_log("User endpoint accessed with ID: #{params[\"id\"]}")
    render plain: params["id"], content_type: "text/plain", status: :ok
  end

  def register_user
    debug_log('Create user endpoint accessed')
    head 201, content_type: "text/plain"
  end

  def health
    debug_log('Health check endpoint accessed')
    render plain: "OK", content_type: "text/plain", status: :ok
  end

  def error
    debug_log('Error endpoint accessed', 'error')
    if DEBUG_MODE
      render plain: "Internal Server Error", content_type: "text/plain", status: :internal_server_error
    else
      head :internal_server_error, content_type: "text/plain"
    end
  end
end
