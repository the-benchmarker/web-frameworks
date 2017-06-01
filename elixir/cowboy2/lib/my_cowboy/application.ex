defmodule MyCowboy.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([{:_, [{:_, MyCowboy.Handler, []}]}])
    nb_acceptors = 100
    trans_opts = [port: 3000]
    proto_opts = %{env: %{dispatch: dispatch},
                   max_connections: 16_384,
                   max_keepalive: 5_000_000}

    children = [{{:ranch_listener_sup, MyRanch},
                 {:cowboy, :start_clear, [MyCowboy, nb_acceptors, trans_opts, proto_opts]},
                  :permanent, :infinity, :supervisor, [:ranch_listener_sup]}]

    opts = [strategy: :one_for_one, name: __MODULE__.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
