defmodule MyPhoenix.Router do
  use MyPhoenix.Web, :router

  scope "/", MyPhoenix do
    get "/", PageController, :index
    get "/user/:id", PageController, :show_user
    post "/user", PageController, :create_user
  end
end
