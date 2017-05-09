defmodule Phoenix.PubSub.Supervisor do
  use Application
  import Supervisor.Spec, warn: false

  def start(_type, _args) do
    children = if pubsub = Application.get_env(:phoenix_pubsub, :pubsub) do
      [supervisor(Phoenix.PubSub.PG2, pubsub)]
    else
      []
    end
    opts = [strategy: :one_for_one]
    Supervisor.start_link(children, opts)
  end
end
