defmodule Server.Router do
  use Phoenix.Router

  get "/", Server.Controller, :index

  resources "/user", Server.Controller, only: [:show, :create]
end
