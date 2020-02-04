defmodule Server.Endpoint do
  use Phoenix.Endpoint, otp_app: :server

  plug(Server.Router)
end
