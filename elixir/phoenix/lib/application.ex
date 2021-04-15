defmodule Server.Application do
  use Application

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def start(_type, _args) do
    Supervisor.start_link([Server.Endpoint], strategy: :one_for_one, name: Server.Supervisor)
  end
end
