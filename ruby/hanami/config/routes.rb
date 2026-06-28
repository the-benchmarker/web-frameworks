module Benchmark
  class Routes < Hanami::Routes
    root to: "index.empty"

    get "/user/:id", to: "user.get"
    post "/user", to: "index.empty"
    
    # Additional endpoints for production-grade
    get "/health", to: "health.get"
    get "/error", to: "error.get"
  end
end
