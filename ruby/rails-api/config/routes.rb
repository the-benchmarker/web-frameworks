Rails.application.routes.draw do
  get "/" => "api#index"
  get "/user/:id" => "api#user"
  post "/user" => "api#register_user"
  get "/health" => "api#health"
  get "/error" => "api#error"
end
