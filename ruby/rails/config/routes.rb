Rails.application.routes.draw do
  # Root route - API entry point
  root to: ->(env) { [200, { "Content-Type" => "application/json" }, [{ status: "ok", message: "Rails API Benchmark", api_documentation: "/api/v1/json" }.to_json]] }

  # Health check endpoint for Kubernetes/liveness probes
  get "/health", to: ->(env) { [200, { "Content-Type" => "text/plain" }, ["OK"]] }

  # API v1 routes
  namespace :api do
    namespace :v1 do
      # Category 6: API & Integration
      get "/json", to: "system#info"
      get "/external", to: "system#external"

      # Category 3: Performance & Scalability
      get "/cached", to: "system#cached"

      # Category 4: Security
      get "/secure", to: "security#secure"
      get "/protected", to: "security#protected"

      # Category 5: Data Management (Database ORM)
      resources :users, only: [:index, :show, :create, :update, :destroy], path: "/db/users"
    end
  end
end
