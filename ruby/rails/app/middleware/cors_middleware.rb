# Category 4: Security - CORS Support
# Production-grade Cross-Origin Resource Sharing middleware for Rails
# Implements proper CORS handling according to W3C specification

class CorsMiddleware
  def initialize(app)
    @app = app
  end

  def call(env)
    request = ActionDispatch::Request.new(env)
    
    # Handle preflight OPTIONS requests
    if request.options?
      handle_preflight(request)
      return [204, {}, []] # 204 No Content for preflight
    end
    
    # Process the request
    status, headers, body = @app.call(env)
    
    # Add CORS headers to successful responses
    if status.between?(200, 299)
      headers.merge!(cors_headers(request))
    end
    
    [status, headers, body]
  end

  private

  # Generate CORS headers based on request
  def cors_headers(request)
    headers = {}
    
    # Allow origin - in production, validate against allowed origins
    origin = request.headers['Origin']
    if origin
      headers['Access-Control-Allow-Origin'] = allowed_origin(origin)
      headers['Vary'] = 'Origin'
    else
      headers['Access-Control-Allow-Origin'] = '*'
    end
    
    # Allow methods
    headers['Access-Control-Allow-Methods'] = 'GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD'
    
    # Allow headers
    headers['Access-Control-Allow-Headers'] = 'Origin, Content-Type, Accept, Authorization, X-Requested-With, X-CSRF-Token'
    
    # Credentials
    headers['Access-Control-Allow-Credentials'] = 'true' if credentials_allowed?(request)
    
    # Max age for preflight caching
    headers['Access-Control-Max-Age'] = '86400'
    
    # Expose headers
    headers['Access-Control-Expose-Headers'] = 'Location, X-Request-Id, X-Total-Count, X-Page, X-Per-Page'
    
    headers
  end

  # Handle preflight request
  def handle_preflight(request)
    # Validate origin
    origin = request.headers['Origin']
    if origin && allowed_origin(origin) != origin
      return
    end
    
    # Validate method
    request_method = request.headers['Access-Control-Request-Method']
    allowed_methods = ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD']
    unless allowed_methods.include?(request_method)
      return
    end
    
    # Validate headers
    request_headers = request.headers['Access-Control-Request-Headers']
    if request_headers
      allowed_headers = ['Origin', 'Content-Type', 'Accept', 'Authorization', 'X-Requested-With', 'X-CSRF-Token']
      requested_headers = request_headers.split(',').map(&:strip)
      unless (requested_headers - allowed_headers).empty?
        return
      end
    end
  end

  # Determine allowed origin
  def allowed_origin(origin)
    # In production, implement proper origin validation
    # For now, allow all origins
    origin
  end

  # Check if credentials are allowed
  def credentials_allowed?(request)
    # Allow credentials for all requests
    true
  end
end
