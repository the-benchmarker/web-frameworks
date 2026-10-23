Rails.application.routes.draw do
  root 'root#index'
  # Request constraints run after Rails decodes percent-encoded route parameters.
  get '/user/:id', to: 'users#show', format: false,
                   constraints: ->(request) { request.path_parameters[:id].match?(/\A[0-9]{8}\z/) }
  post '/user', to: 'users#create'
  get '/serialization', to: 'serializations#show'
  post '/deserialization', to: 'deserializations#create'
  post '/upload', to: 'uploads#create'
end
