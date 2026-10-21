Rails.application.routes.draw do
  root 'root#index'
  get '/user/:id', to: 'users#show', format: false
  post '/user', to: 'users#create'
  get '/serialization', to: 'serializations#show'
  post '/deserialization', to: 'deserializations#create'
  post '/upload', to: 'uploads#create'
end
