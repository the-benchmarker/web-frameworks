defmodule Server.Application do
  @moduledoc """
  Server Application Module for Plug Cowboy
  
  Responsible for starting and supervising the Plug Cowboy web server.
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
    port = Application.get_env(:plug_cowboy, :port) || System.get_env("PORT") || 3000
    
    socket_opts = Application.get_env(:plug_cowboy, :socket_opts) || [
      port: port,
      nodelay: true,
      reuseaddr: true,
      reuseport: true,
      keepalive: true
    ]

    max_connections = Application.get_env(:plug_cowboy, :max_connections) || 16_384
    num_acceptors = Application.get_env(:plug_cowboy, :num_acceptors) || 100

    %{
      id: :http_server,
      start: {
        Plug.Cowboy,
        :start_link,
        [
          scheme: :http,
          plug: Server,
          options: [
            port: port,
            socket_opts: socket_opts,
            max_connections: max_connections,
            num_acceptors: num_acceptors,
            # Security: Disable compression to prevent CRIME/BREACH attacks
            enable_compression: false
          ]
        ]
      },
      restart: :permanent,
      shutdown: 5_000,  # Give connections time to drain
      type: :worker
    }
  end
end
