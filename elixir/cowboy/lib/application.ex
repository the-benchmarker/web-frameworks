defmodule Server.Application do
  use Application

  def start(_type, _args) do
    dispatch_config = :cowboy_router.compile([{:_, Server.routes()}])

    {:ok, _} = :cowboy.start_clear(:server, [port: 3000], %{env: %{dispatch: dispatch_config}})
  end
end
