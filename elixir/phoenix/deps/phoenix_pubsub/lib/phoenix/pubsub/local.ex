defmodule Phoenix.PubSub.Local do
  @moduledoc """
  PubSub implementation for handling local-node process groups.

  This module is used by Phoenix pubsub adapters to handle
  their local node subscriptions and it is usually not accessed
  directly. See `Phoenix.PubSub.PG2` for an example integration.
  """

  use GenServer

  @doc """
  Starts the server.

    * `server_name` - The name to register the server under

  """
  def start_link(server_name, gc_name) do
    GenServer.start_link(__MODULE__, {server_name, gc_name}, name: server_name)
  end

  @doc """
  Subscribes the pid to the topic.

    * `pubsub_server` - The registered server name
    * `pool_size` - The size of the pool
    * `pid` - The subscriber pid
    * `topic` - The string topic, for example "users:123"
    * `opts` - The optional list of options. Supported options
      only include `:link` to link the subscriber to local

  ## Examples

      iex> subscribe(MyApp.PubSub, 1, self, "foo")
      :ok

  """
  def subscribe(pubsub_server, pool_size, pid, topic, opts \\ []) when is_atom(pubsub_server) do
    {local, gc} =
      pid
      |> :erlang.phash2(pool_size)
      |> pools_for_shard(pubsub_server)

    :ok = GenServer.call(local, {:monitor, pid, opts})
    true = :ets.insert(gc, {pid, topic})
    true = :ets.insert(local, {topic, {pid, opts[:fastlane]}})

    :ok
  end

  @doc """
  Unsubscribes the pid from the topic.

    * `pubsub_server` - The registered server name
    * `pool_size` - The size of the pool
    * `pid` - The subscriber pid
    * `topic` - The string topic, for example "users:123"

  ## Examples

      iex> unsubscribe(MyApp.PubSub, 1, self, "foo")
      :ok

  """
  def unsubscribe(pubsub_server, pool_size, pid, topic) when is_atom(pubsub_server) do
    {local, gc} =
      pid
      |> :erlang.phash2(pool_size)
      |> pools_for_shard(pubsub_server)

    true = :ets.match_delete(gc, {pid, topic})
    true = :ets.match_delete(local, {topic, {pid, :_}})

    case :ets.select_count(gc, [{{pid, :_}, [], [true]}]) do
      0 -> :ok = GenServer.call(local, {:demonitor, pid})
      _ -> :ok
    end
  end

  @doc """
  Sends a message to all subscribers of a topic.

    * `pubsub_server` - The registered server name
    * `pool_size` - The size of the pool
    * `topic` - The string topic, for example "users:123"

  ## Examples

      iex> broadcast(MyApp.PubSub, 1, self, "foo")
      :ok
      iex> broadcast(MyApp.PubSub, 1, :none, "bar")
      :ok

  """
  def broadcast(fastlane, pubsub_server, 1 = _pool_size, from, topic, msg) when is_atom(pubsub_server) do
    do_broadcast(fastlane, pubsub_server, _shard = 0, from, topic, msg)
    :ok
  end
  def broadcast(fastlane, pubsub_server, pool_size, from, topic, msg) when is_atom(pubsub_server) do
    parent = self()
    for shard <- 0..(pool_size - 1) do
      Task.async(fn ->
        do_broadcast(fastlane, pubsub_server, shard, from, topic, msg)
        Process.unlink(parent)
      end)
    end |> Enum.map(&Task.await(&1, :infinity))
    :ok
  end

  defp do_broadcast(nil, pubsub_server, shard, from, topic, msg) do
    pubsub_server
    |> subscribers_with_fastlanes(topic, shard)
    |> Enum.each(fn
      {pid, _} when pid == from -> :noop
      {pid, _} -> send(pid, msg)
    end)
  end

  defp do_broadcast(fastlane, pubsub_server, shard, from, topic, msg) do
    pubsub_server
    |> subscribers_with_fastlanes(topic, shard)
    |> fastlane.fastlane(from, msg) # TODO: Test this contract
  end

  @doc """
  Returns a set of subscribers pids for the given topic.

    * `pubsub_server` - The registered server name or pid
    * `topic` - The string topic, for example "users:123"
    * `shard` - The shard, for example `1`


  ## Examples

      iex> subscribers(:pubsub_server, "foo", 1)
      [#PID<0.48.0>, #PID<0.49.0>]

  """
  def subscribers(pubsub_server, topic, shard) when is_atom(pubsub_server) do
    pubsub_server
    |> subscribers_with_fastlanes(topic, shard)
    |> Enum.map(fn {pid, _fastlanes} -> pid end)
  end

  @doc """
  Returns a set of subscribers pids for the given topic with fastlane tuples.
  See `subscribers/1` for more information.
  """
  def subscribers_with_fastlanes(pubsub_server, topic, shard) when is_atom(pubsub_server) do
    try do
      shard
      |> local_for_shard(pubsub_server)
      |> :ets.lookup_element(topic, 2)
    catch
      :error, :badarg -> []
    end
  end

  @doc false
  # This is an expensive and private operation. DO NOT USE IT IN PROD.
  def list(pubsub_server, shard) when is_atom(pubsub_server) do
    shard
    |> local_for_shard(pubsub_server)
    |> :ets.select([{{:'$1', :_}, [], [:'$1']}])
    |> Enum.uniq
  end

  @doc false
  # This is an expensive and private operation. DO NOT USE IT IN PROD.
  def subscription(pubsub_server, pool_size, pid) when is_atom(pubsub_server) do
    {local, _gc} =
      pid
      |> :erlang.phash2(pool_size)
      |> pools_for_shard(pubsub_server)

    GenServer.call(local, {:subscription, pid})
  end

  @doc false
  def local_name(pubsub_server, shard) do
    Module.concat(["#{pubsub_server}.Local#{shard}"])
  end

  @doc false
  def gc_name(pubsub_server, shard) do
    Module.concat(["#{pubsub_server}.GC#{shard}"])
  end

  def init({local, gc}) do
    ^local = :ets.new(local, [:duplicate_bag, :named_table, :public,
                              read_concurrency: true, write_concurrency: true])
    ^gc = :ets.new(gc, [:duplicate_bag, :named_table, :public,
                        read_concurrency: true, write_concurrency: true])

    Process.flag(:trap_exit, true)
    {:ok, %{monitors: %{}, gc: gc}}
  end

  def handle_call({:monitor, pid, opts}, _from, state) do
    if opts[:link], do: Process.link(pid)
    {:reply, :ok, put_new_monitor(state, pid)}
  end

  def handle_call({:demonitor, pid}, _from, state) do
    {:reply, :ok, drop_monitor(state, pid)}
  end

  def handle_call({:subscription, pid}, _from, state) do
    topics = GenServer.call(state.gc, {:subscription, pid})
    {:reply, {state.monitors[pid], topics}, state}
  end

  def handle_info({:DOWN, _ref, _type, pid, _info}, state) do
    Phoenix.PubSub.GC.down(state.gc, pid)
    {:noreply, drop_monitor(state, pid)}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  defp local_for_shard(shard, pubsub_server) do
    {local_server, _gc_server} = pools_for_shard(shard, pubsub_server)
    local_server
  end

  defp pools_for_shard(shard, pubsub_server) do
    {_, _} = servers = :ets.lookup_element(pubsub_server, shard, 2)
    servers
  end

  defp put_new_monitor(%{monitors: monitors} = state, pid) do
    case Map.fetch(monitors, pid) do
      {:ok, _ref} -> state
      :error -> %{state | monitors: Map.put(monitors, pid, Process.monitor(pid))}
    end
  end

  defp drop_monitor(%{monitors: monitors} = state, pid) do
    case Map.fetch(monitors, pid) do
      {:ok, ref} ->
        Process.demonitor(ref)
        %{state | monitors: Map.delete(monitors, pid)}
      :error -> state
    end
  end
end
