defmodule Server.Endpoint do
  use Phoenix.Endpoint, otp_app: :server

  @compile :native
  @compile {:hipe, [:o3]}

  plug(Server.Router)
end
