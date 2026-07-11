# Category 4: Security - CORS Support
# Cross-Origin Resource Sharing middleware for Rails

class CorsMiddleware
  def initialize(app)
    @app = app
  end

  def call(env)
    request = ActionDispatch::Request.new(env)
    
    # Handle preflight OPTIONS requests
    if request.options?
      return [204, cors_headers, []]
    end
    
    status, headers, body = @app.call(env)
    
    # Add CORS headers to all responses
    headers.merge!(cors_headers)
    
    [status, headers, body]
  end

  private

  def cors_headers
    {
      'Access-Control-Allow-Origin' => '*',
      'Access-Control-Allow-Methods' => 'GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD',
      'Access-Control-Allow-Headers' => 'Origin, Content-Type, Accept, Authorization, X-Requested-With',
      'Access-Control-Max-Age' => '86400'
    }
  end
end
