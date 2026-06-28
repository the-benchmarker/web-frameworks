class ApplicationController < RageController::API
  # Security headers for all responses
  before_action :set_security_headers
  
  private
  
  def set_security_headers
    response.headers['X-Content-Type-Options'] = 'nosniff'
    response.headers['X-Frame-Options'] = 'DENY'
    response.headers['X-XSS-Protection'] = '1; mode=block'
    response.headers['Content-Security-Policy'] = "default-src 'self'"
    response.headers['Referrer-Policy'] = 'strict-origin-when-cross-origin'
    response.headers['Cache-Control'] = 'no-cache, no-store, must-revalidate'
  end
  
  def debug_log(message, level = 'debug')
    return unless DEBUG_MODE
    case level
    when 'error'
      Rails.logger.error(message) if defined?(Rails) && Rails.logger
    when 'info'
      Rails.logger.info(message) if defined?(Rails) && Rails.logger
    else
      Rails.logger.debug(message) if defined?(Rails) && Rails.logger
    end
  end

public
  
  def index
    debug_log('Root endpoint accessed')
    head 200
  end

  def user
    debug_log("User endpoint accessed with ID: #{params[:id]}")
    response.headers['Content-Type'] = 'text/plain'
    render plain: params[:id]
  end

  def register_user
    debug_log('Create user endpoint accessed')
    head 201
  end

  def health
    debug_log('Health check endpoint accessed')
    response.headers['Content-Type'] = 'text/plain'
    render plain: 'OK'
  end

  def error
    debug_log('Error endpoint accessed', 'error')
    response.headers['Content-Type'] = 'text/plain'
    head 500
    if DEBUG_MODE
      render plain: 'Internal Server Error'
    else
      render plain: ''
    end
  end
end
