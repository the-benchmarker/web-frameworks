defmodule Server.Application do
  use Application

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def start(_type, _args) do
    Supervisor.start_link([cowboy_child_spec()], strategy: :one_for_one, name: Server.Supervisor)
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
            num_acceptors: 100
          },
          %{
            stream_handlers: [Server]
          }
        ]
      },
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor
    }
  end
end
