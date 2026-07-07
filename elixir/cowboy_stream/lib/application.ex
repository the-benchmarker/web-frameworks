defmodule Server.Application do
  @moduledoc """
  Server Application Module for Cowboy Stream
  
  Responsible for starting and supervising the Cowboy Stream web server.
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
      cowboy_child_spec()
    ]

    Supervisor.init(children, strategy: :one_for_one, name: Server.Supervisor)
    |> Supervisor.start_link()
  end

  @impl true
  def stop(_state), do: :ok

  # ===========================================================================
  # Supervisor Configuration
  # ===========================================================================

  def cowboy_child_spec do
    %{
      id: :http_stream_server,
      start: {
        :cowboy,
        :start_clear,
        [
          # Name of the application
          :server,
          
          # Transport configuration
          %{
            # Socket options - configured in config.exs for better maintainability
            socket_opts: Application.get_all_env(:cowboy)[:socket_opts] || 
                         [port: 3000, nodelay: true, reuseaddr: true],
            
            # Connection limits
            max_connections: Application.get_env(:cowboy, :max_connections) || 16_384,
            num_acceptors: Application.get_env(:cowboy, :num_acceptors) || 100,
            
            # Security: Disable compression to prevent CRIME/BREACH attacks
            enable_compression: Application.get_env(:cowboy, :enable_compression, false),
            
            # Protocol options
            protocols: [:http]
          },
          
          # Stream handler configuration
          %{
            stream_handlers: [Server],
            # Security: Disable directory listing
            directory_index: nil
          }
        ]
      },
      restart: :permanent,
      shutdown: 5_000,  # Give connections time to drain
      type: :worker
    }
  end
end
