defmodule Phoenix.PubSub.PG2 do
  use Supervisor

  @moduledoc """
  Phoenix PubSub adapter based on PG2.

  To use it as your PubSub adapter, simply add it to your Endpoint's config:

      config :my_app, MyApp.Endpoint,
        pubsub: [name: MyApp.PubSub,
                 adapter: Phoenix.PubSub.PG2]

  ## Options

    * `:name` - The registered name and optional node name to for the PubSub
      processes, for example: `MyApp.PubSub`, `{MyApp.PubSub, :node@host}`.
      When only a server name is provided, the node name defaults to `node()`.

    * `:pool_size` - Both the size of the local pubsub server pool and subscriber
      shard size. Defaults the number of schedulers (cores). A single pool is
      often enough for most use-cases, but for high subscriber counts on a single
      topic or greater than 1M clients, a pool size equal to the number of
      schedulers (cores) is a well rounded size.

  """

  def start_link(name, opts) do
    supervisor_name = Module.concat(name, Supervisor)
    Supervisor.start_link(__MODULE__, [name, opts], name: supervisor_name)
  end

  @doc false
  def init([server, opts]) do
    scheduler_count = :erlang.system_info(:schedulers)
    pool_size = Keyword.get(opts, :pool_size, scheduler_count)
    node_name = opts[:node_name]
    dispatch_rules = [{:broadcast, Phoenix.PubSub.PG2Server, [opts[:fastlane], server, pool_size]},
                      {:direct_broadcast, Phoenix.PubSub.PG2Server, [opts[:fastlane], server, pool_size]},
                      {:node_name, __MODULE__, [node_name]}]

    children = [
      supervisor(Phoenix.PubSub.LocalSupervisor, [server, pool_size, dispatch_rules]),
      worker(Phoenix.PubSub.PG2Server, [server, pool_size]),
    ]

    supervise children, strategy: :rest_for_one
  end

  @doc false
  def node_name(nil), do: node()
  def node_name(configured_name), do: configured_name
end
