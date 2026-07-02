class ApplicationController < RageController::API
  # Security headers for all responses - frozen for performance
  SECURITY_HEADERS = {
    'X-Content-Type-Options' => 'nosniff',
    'X-Frame-Options' => 'DENY',
    'X-XSS-Protection' => '1; mode=block',
    'Content-Security-Policy' => "default-src 'self'",
    'Referrer-Policy' => 'strict-origin-when-cross-origin',
    'Cache-Control' => 'no-cache, no-store, must-revalidate'
  }.freeze
  
  before_action :set_security_headers
  
  private
  
  def set_security_headers
    SECURITY_HEADERS.each { |key, value| response.headers[key] = value }
  end
  
  def debug_log(message, level = 'debug')
    return unless DEBUG_MODE && defined?(Rails) && Rails.logger
    case level
    when 'error' then Rails.logger.error(message)
    when 'info' then Rails.logger.info(message)
    else Rails.logger.debug(message)
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
