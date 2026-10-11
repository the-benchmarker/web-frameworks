Rails.application.routes.draw do
  get '/', to: 'health#show'
  get '/user/:user', to: 'users#show', format: false, constraints: { user: %r{[^/]+} }
  post '/user', to: 'users#create'

  # Keep the three historical routes as the default benchmark.
  if Rails.configuration.x.complete
    get '/health', to: 'health#show'
    get '/serialization', to: 'serializations#show'
    post '/deserialization', to: 'deserializations#create'
    post '/upload', to: 'uploads#create'
  end
end
