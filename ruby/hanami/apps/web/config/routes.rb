# frozen_string_literal: true

get '/', to: 'home#index'
get '/user/:id', id: /\d+/, to: 'home#user'
post '/user', to: 'home#register_user'
