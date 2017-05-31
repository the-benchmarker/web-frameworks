defmodule MyCowboy.Application do
  @moduledoc false

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([{:_, [{:_, MyCowboy.Handler, []}]}])
    {:ok, _} = :cowboy.start_http(:http, 100, [port: 3000], [
      env: [dispatch: dispatch],
      max_connections: 16_384
    ])
  end
end
