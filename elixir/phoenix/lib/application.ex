defmodule Server.Application do
  use Application

  def start(_type, _args) do
    Supervisor.start_link([Server.Endpoint], strategy: :one_for_one, name: Server.Supervisor)
  end
end
