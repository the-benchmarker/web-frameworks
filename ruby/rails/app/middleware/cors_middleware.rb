# CORS Middleware for Rails API
# Configuration-driven CORS support
#
# HTTP Standards Compliance:
# - RFC 7231: OPTIONS method support
# - RFC 6454: CORS protocol
# - All responses include Date header per RFC 7231 Section 7.1.1.1

require "action_dispatch"

class CorsMiddleware
  def initialize(app)
    @app = app
  end

  def call(env)
    request = ActionDispatch::Request.new(env)

    # Handle preflight OPTIONS requests per RFC 6454
    if request.options?
      return handle_preflight(request)
    end

    # Process the request
    status, headers, body = @app.call(env)

    # Add CORS headers to all responses
    headers.merge!(cors_headers)

    # Ensure Date header is present (RFC 7231 Section 7.1.1.1)
    headers["Date"] ||= Time.now.httpdate

    [status, headers, body]
  end

  private

  def cors_headers
    {
      # CORS headers per RFC 6454
      "Access-Control-Allow-Origin" => "*",
      "Access-Control-Allow-Methods" => "GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD",
      "Access-Control-Allow-Headers" => "Origin, Content-Type, Accept, Authorization, Cache-Control",
      "Access-Control-Max-Age" => "86400",
      "Access-Control-Allow-Credentials" => "true",
      # Vary header per RFC 7231 Section 7.1.4
      "Vary" => "Origin",
    }
  end

  def handle_preflight(request)
    # Preflight response per RFC 6454 Section 6
    # 204 No Content is the standard response for successful preflight
    headers = cors_headers

    # Add specific headers based on request
    if request.headers["Access-Control-Request-Method"]
      headers["Access-Control-Allow-Methods"] = request.headers["Access-Control-Request-Method"]
    end

    if request.headers["Access-Control-Request-Headers"]
      headers["Access-Control-Allow-Headers"] = request.headers["Access-Control-Request-Headers"]
    end

    # Date header is required by RFC 7231
    headers["Date"] = Time.now.httpdate

    [204, headers, []]
  end
end
