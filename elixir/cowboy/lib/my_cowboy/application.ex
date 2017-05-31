defmodule MyCowboy.Application do
  @moduledoc false

  def start(_type, _args) do
    opts = [max_connections: 16_384, acceptors: 100, port: 3000]
    dispatch = :cowboy_router.compile([{:_, [{:_, MyCowboy.Handler, []}]}])
    {:ok, _} = :cowboy.start_clear(:http, 100, opts, %{env: %{dispatch: dispatch}})
  end
end
