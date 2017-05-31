defmodule MyCowboy.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([{:_, [{:_, MyCowboy.Handler, []}]}])

    nb_acceptors = 100
    trans_opts = [port: 3000]
    proto_opts = [env: [dispatch: dispatch], max_connections: 16_384]

    children = [
      :ranch.child_spec(MyCowboy, nb_acceptors, :ranch_tcp, trans_opts, :cowboy_protocol, proto_opts)
    ]

    opts = [strategy: :one_for_one, name: __MODULE__.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
