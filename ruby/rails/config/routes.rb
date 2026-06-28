# Benchmark Routes Configuration
# 
# Defines all routes for the benchmark Rails application.
# Follows Rails RESTful routing conventions.

Rails.application.routes.draw do
  # Benchmark endpoints
  get "/", to: "application#index", as: :root
  get "/user/:id", to: "application#user", as: :get_user
  post "/user", to: "application#register_user", as: :create_user
  
  # Health check endpoint for monitoring
  get "/health", to: "application#health_check", as: :health_check
  
  # Error test endpoint for verifying error handling
  get "/error", to: "application#error", as: :error
end
