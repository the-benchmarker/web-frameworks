Rails.application.routes.draw do
  get "/", to: "health#show"
  get "/heath", to: "health#show"
  get "/user/:user", to: "users#show"
  post "/user", to: "users#create"
  get "/serialization", to: "serializations#show"
  post "/deserialization", to: "deserializations#create"
  post "/upload", to: "uploads#create"
end
