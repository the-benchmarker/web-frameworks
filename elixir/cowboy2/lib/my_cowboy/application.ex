defmodule MyCowboy.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    nb_acceptors = 100
    trans_opts = [port: 3000]
    proto_opts = %{max_connections: 16_384,
                   max_keepalive: 5_000_000,
                   stream_handlers: [MyCowboy.StreamHandler]}

    {:ok, _pid} = :cowboy.start_clear(:http, nb_acceptors, trans_opts, proto_opts)
  end
end
