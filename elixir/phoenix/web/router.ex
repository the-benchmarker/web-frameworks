defmodule MyPhoenix.Router do
  use MyPhoenix.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", MyPhoenix do
    pipe_through :api

    get "/", PageController, :index
    get "/user/:id", PageController, :show_user
    post "/user", PageController, :create_user
  end
end
