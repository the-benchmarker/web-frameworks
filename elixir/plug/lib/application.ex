defmodule Server.Application do
  use Application

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def start(_type, _args) do
    children = [
      Plug.Cowboy.child_spec(scheme: :http, plug: Server, options: [port: 3000])
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Server.Supervisor)
  end
end
