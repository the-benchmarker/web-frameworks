# frozen_string_literal: true

Rails.application.routes.draw do
  get '/' => 'application#index'
  get '/user/:id' => 'application#user'
  post '/user' => 'application#register_user'
end
