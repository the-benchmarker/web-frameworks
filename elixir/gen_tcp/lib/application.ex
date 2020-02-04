defmodule Server.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Server, port: 3000}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Server.Supervisor)
  end
end
