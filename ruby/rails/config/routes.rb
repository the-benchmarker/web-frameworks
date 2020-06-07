# frozen_string_literal: true

Rails.application.routes.draw do
  get :/ => "api#index"
  get "/user/:id": "api#user"
  post "/user": "api#register_user"
end
