defmodule Server.Application do
  use Application

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def start(_type, _args) do
    children = [
      {Bandit, plug: Server, port: 3000, websocket_options: [enabled: false]}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Server.Supervisor)
  end
end
