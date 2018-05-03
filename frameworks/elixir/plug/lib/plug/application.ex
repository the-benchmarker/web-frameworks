defmodule MyPlug.Application do
  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children = [
      # Starts a worker by calling: Plug.Worker.start_link(arg1, arg2, arg3)
      # worker(Plug.Worker, [arg1, arg2, arg3]),
      Plug.Adapters.Cowboy.child_spec(:http, MyPlug.Router, [], [
                                        port: 3000,
                                        protocol_options: [
                                          max_keepalive: 5_000_000
                                        ]
                                      ])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MyPlug.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
