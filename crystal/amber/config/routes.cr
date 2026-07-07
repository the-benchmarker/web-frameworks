Amber::Server.configure do |app|
  # Security middleware pipeline
  pipeline :web do
    # Security headers middleware (if using amber-security shard)
    # use Amber::Security::Headers
    
    # Rate limiting (if using rate limiting shard)
    # use Amber::RateLimiting
    
    # Error handling
    use Amber::Middleware::ErrorHandler
  end

  # API routes
  routes :web do
    # Health check endpoint
    get "/health", ->(env) { [200, {"Content-Type" => "text/plain"}, ["OK"]] }
    
    # Root endpoint
    get "/", ApplicationController, :index
    
    # User endpoints
    get "/user/:id", ApplicationController, :get
    post "/user", ApplicationController, :create
  end
end
