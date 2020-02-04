defmodule Server.Application do
  use Application

  def start(_type, _args) do
    opts = %{
      num_acceptors: 100,
      max_connections: 32,
      socket_opts: [
        keepalive: true,
        port: 3000
      ]
    }

    :ranch.start_listener(:server, :ranch_tcp, opts, Server, [])
  end
end
