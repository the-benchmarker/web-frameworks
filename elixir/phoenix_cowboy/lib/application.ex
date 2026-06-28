defmodule Server.Application do
  @moduledoc """
  Server Application Module for Phoenix Plug Cowboy
  
  Responsible for starting and supervising the Phoenix Plug Cowboy web server.
  Implements production-grade best practices for security, performance, and reliability.
  """
  
  use Application

  @compile :native
  @compile {:hipe, [:o3]}
  
  # ===========================================================================
  # Application Lifecycle
  # ===========================================================================

  @impl true
  def start(_type, _args) do
    children = [
      endpoint_child_spec()
    ]

    Supervisor.init(children, strategy: :one_for_one, name: Server.Supervisor)
    |> Supervisor.start_link()
  end

  @impl true
  def stop(_state), do: :ok

  # ===========================================================================
  # Supervisor Configuration
  # ===========================================================================

  defp endpoint_child_spec do
    %{
      id: :endpoint,
      start: {Server.Endpoint, :start_link, []},
      restart: :permanent,
      shutdown: 5_000,  # Give connections time to drain
      type: :worker
    }
  end
end
