Rails.application.routes.draw do
  get  '/'         => 'api#index'
  get  '/user/:id' => 'api#user', constraints: { id: /[0-9]+/ }
  post '/user'     => 'api#register_user'
end
