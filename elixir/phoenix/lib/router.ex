defmodule Server.Router do
  use Phoenix.Router

  @compile :native
  @compile {:hipe, [:o3]}

  get("/", Server.Controller, :index)

  resources("/user", Server.Controller, only: [:show, :create])
end
