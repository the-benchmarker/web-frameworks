Rails.application.routes.draw do
  # Category 1: Core Framework Requirements
  # HTTP Foundation - Request Routing, HTTP Method Support, Response Building
  get "/" => "application#index"
  get "/user/:id" => "application#user"
  post "/user" => "application#register_user"
  
  # Category 6: API & Integration
  # JSON API Support - JSON response endpoints
  get "/api/json" => "application#api_json"
  
  # External API Integration - External service consumption
  get "/api/external" => "application#external_api"
  
  # Category 3: Performance & Scalability
  # Caching - Response caching demonstration
  get "/api/cached" => "application#cached_response"
  
  # Category 4: Security
  # Authentication & Authorization - Bearer token authentication
  get "/api/secure" => "application#secure_endpoint"
  get "/api/protected" => "application#protected_resource"
  
  # Category 5: Data Management - Database ORM Testing
  # Full CRUD operations for database testing
  post "/api/db/users" => "application#create_user_db"
  get "/api/db/users" => "application#list_users_db"
  get "/api/db/users/:id" => "application#get_user_db"
  patch "/api/db/users/:id" => "application#update_user_db"
  delete "/api/db/users/:id" => "application#destroy_user_db"
end
