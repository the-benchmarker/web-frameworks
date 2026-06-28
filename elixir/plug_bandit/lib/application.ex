defmodule Server.Application do
  @moduledoc """
  Server Application Module for Plug Bandit
  
  Responsible for starting and supervising the Plug Bandit web server.
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
      http_server_child_spec()
    ]

    Supervisor.init(children, strategy: :one_for_one, name: Server.Supervisor)
    |> Supervisor.start_link()
  end

  @impl true
  def stop(_state), do: :ok

  # ===========================================================================
  # Supervisor Configuration
  # ===========================================================================

  defp http_server_child_spec do
    port = Application.get_env(:bandit, :port) || System.get_env("PORT") || 3000
    
    socket_opts = Application.get_env(:bandit, :socket_opts) || [
      port: port,
      nodelay: true,
      reuseaddr: true,
      reuseport: true,
      keepalive: true
    ]

    max_connections = Application.get_env(:bandit, :max_connections) || 16_384
    num_acceptors = Application.get_env(:bandit, :num_acceptors) || 100

    %{
      id: :http_server,
      start: {
        Bandit,
        :start_link,
        [
          plug: Server,
          port: port,
          socket_opts: socket_opts,
          max_connections: max_connections,
          num_acceptors: num_acceptors,
          # Security: Disable WebSocket support if not needed
          websocket_options: [enabled: false]
        ]
      },
      restart: :permanent,
      shutdown: 5_000,  # Give connections time to drain
      type: :worker
    }
  end
end
