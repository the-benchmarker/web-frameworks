defmodule Server.Endpoint do
  use Phoenix.Endpoint, otp_app: :server

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  plug Server.Router
end
