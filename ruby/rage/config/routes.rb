Rage.routes.draw do
  get "/", to: "application#index"
  get "/user/:id", to: "application#user"
  post "/user", to: "application#register_user"
  get "/health", to: "application#health"
  get "/error", to: "application#error"
end
