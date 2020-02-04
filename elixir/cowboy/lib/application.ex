defmodule Server.Application do
  use Application

  def start(_type, _args) do
    children = [
      cowboy_child_spec()
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Server.Supervisor)
  end

  def cowboy_child_spec do
    %{
      id: :server,
      start: {
        :cowboy,
        :start_clear,
        [
          :server,
          %{
            socket_opts: [port: 3000],
            max_connections: 16_384,
            keepalive: true,
            num_acceptors: 100
          },
          %{env: %{dispatch: :cowboy_router.compile([{:_, Server.routes()}])}}
        ]
      },
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor
    }
  end
end
