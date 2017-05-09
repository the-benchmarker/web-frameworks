defmodule Phoenix.Tracker do
  @moduledoc ~S"""
  Provides distributed Presence tracking to processes.

  Tracker servers use a heartbeat protocol and CRDT to replicate presence
  information across a cluster in an eventually consistent, conflict-free
  manner. Under this design, there is no single source of truth or global
  process. Instead, each node runs one or more `Phoenix.Tracker` servers and
  node-local changes are replicated across the cluster and handled locally as
  a diff of changes.

    * `tracker` - The name of the tracker handler module implementing the
      `Phoenix.Tracker` behaviour
    * `tracker_opts` - The list of options to pass to the tracker handler
    * `server_opts` - The list of options to pass to the tracker server

  ## Required `server_opts`:

    * `:name` - The name of the server, such as: `MyApp.Tracker`
    * `:pubsub_server` - The name of the PubSub server, such as: `MyApp.PubSub`

  ## Optional `server_opts`:

    * `broadcast_period` - The interval in milliseconds to send delta broadcats
      across the cluster. Default `1500`
    * `max_silent_periods` - The max integer of broadcast periods for which no
      delta broadcasts have been sent. Defaults `10` (15s heartbeat)
    * `down_period` - The interval in milliseconds to flag a replica
      as down temporarily down. Default `broadcast_period * max_silent_periods * 2`
      (30s down detection). Note: This must be at least 2x the `broadcast_period`.
    * `permdown_period` - The interval in milliseconds to flag a replica
      as permanently down, and discard its state.
      Note: This must be at least greater than the `down_period`.
      Default `1_200_000` (20 minutes)
    * `clock_sample_periods` - The numbers of heartbeat windows to sample
      remote clocks before collapsing and requesting transfer. Default `2`
    * `max_delta_sizes` - The list of delta generation sizes to keep before
      falling back to sending entire state. Defaults `[100, 1000, 10_000]`.
    * log_level - The log level to log events, defaults `:debug` and can be
      disabled with `false`

  ## Implementing a Tracker

  To start a tracker, first add the tracker to your supervision tree:

      worker(MyTracker, [[name: MyTracker, pubsub_server: MyPubSub]])

  Next, implement `MyTracker` with support for the `Phoenix.Tracker`
  behaviour callbacks. An example of a minimal tracker could include:

      defmodule MyTracker do
        @behaviour Phoenix.Tracker

        def start_link(opts) do
          opts = Keyword.merge([name: __MODULE__], opts)
          GenServer.start_link(Phoenix.Tracker, [__MODULE__, opts, opts], name: __MODULE__)
        end

        def init(opts) do
          server = Keyword.fetch!(opts, :pubsub_server)
          {:ok, %{pubsub_server: server, node_name: Phoenix.PubSub.node_name(server)}}
        end

        def handle_diff(diff, state) do
          for {topic, {joins, leaves}} <- diff do
            for {key, meta} <- joins do
              IO.puts "presence join: key \"#{key}\" with meta #{inspect meta}"
              msg = {:join, key, meta}
              Phoenix.PubSub.direct_broadcast!(state.node_name, state.pubsub_server, topic, msg)
            end
            for {key, meta} <- leaves do
              IO.puts "presence leave: key \"#{key}\" with meta #{inspect meta}"
              msg = {:leave, key, meta}
              Phoenix.PubSub.direct_broadcast!(state.node_name, state.pubsub_server, topic, msg)
            end
          end
          {:ok, state}
        end
      end

  Trackers must implement `start_link/1`, `init/1`, and `handle_diff/2`.
  The `init/1` callback allows the tracker to manage its own state when
  running within the `Phoenix.Tracker` server. The `handle_diff` callback
  is invoked with a diff of presence join and leave events, grouped by
  topic. As replicas heartbeat and replicate data, the local tracker state is
  merged with the remote data, and the diff is sent to the callback. The
  handler can use this information to notify subscribers of events, as
  done above.

  ## Special Considerations

  Operations within `handle_diff/2` happen *in the tracker server's context*.
  Therefore, blocking operations should be avoided when possible, and offloaded
  to a supervised task when required. Also, a crash in the `handle_diff/2` will
  crash the tracker server, so operations that may crash the server should be
  offloaded with a `Task.Supervisor` spawned process.
  """
  use GenServer
  alias Phoenix.Tracker.{Clock, State, Replica, DeltaGeneration}
  require Logger

  @type presence :: {key :: String.t, meta :: Map.t}
  @type topic :: String.t

  @callback init(Keyword.t) :: {:ok, pid} | {:error, reason :: term}
  @callback handle_diff(%{topic => {joins :: [presence], leaves :: [presence]}}, state :: term) :: {:ok, state :: term}

  ## Client

  @doc """
  Tracks a presence.

    * `server_name` - The registered name of the tracker server
    * `pid` - The Pid to track
    * `topic` - The `Phoenix.PubSub` topic for this presence
    * `key` - The key identifying this presence
    * `meta` - The map of metadata to attach to this presence

  A process may be tracked multipled times, provided the topic and key pair
  are unique for any prior calls for the given process.

  ## Examples

      iex> Phoenix.Tracker.track(MyTracker, self(), "lobby", u.id, %{stat: "away"})
      {:ok, "1WpAofWYIAA="}

      iex> Phoenix.Tracker.track(MyTracker, self(), "lobby", u.id, %{stat: "away"})
      {:error, {:already_tracked, #PID<0.56.0>, "lobby", "123"}}
  """
  @spec track(atom, pid, topic, term, Map.t) :: {:ok, ref :: binary} | {:error, reason :: term}
  def track(server_name, pid, topic, key, meta) when is_pid(pid) and is_map(meta) do
    GenServer.call(server_name, {:track, pid, topic, key, meta})
  end

  @doc """
  Untracks a presence.

    * `server_name` - The registered name of the tracker server
    * `pid` - The Pid to untrack
    * `topic` - The `Phoenix.PubSub` topic to untrack for this presence
    * `key` - The key identifying this presence

  All presences for a given Pid can be untracked by calling the
  `Phoenix.Tracker.untrack/2` signature of this function.

  ## Examples

      iex> Phoenix.Tracker.untrack(MyTracker, self, "lobby", u.id)
      :ok
      iex> Phoenix.Tracker.untrack(MyTracker, self)
      :ok
  """
  @spec untrack(atom, pid, topic, term) :: :ok
  def untrack(server_name, pid, topic, key) when is_pid(pid) do
    GenServer.call(server_name, {:untrack, pid, topic, key})
  end
  def untrack(server_name, pid) when is_pid(pid) do
    GenServer.call(server_name, {:untrack, pid})
  end

  @doc """
  Updates a presence's metadata.

    * `server_name` - The registered name of the tracker server
    * `pid` - The Pid being tracked
    * `topic` - The `Phoenix.PubSub` topic to update for this presence
    * `key` - The key identifying this presence

  ## Examples

      iex> Phoenix.Tracker.update(MyTracker, self, "lobby", u.id, %{stat: "zzz"})
      {:ok, "1WpAofWYIAA="}

      iex> Phoenix.Tracker.update(MyTracker, self, "lobby", u.id, fn meta -> Map.put(meta, :away, true) end)
      {:ok, "1WpAofWYIAA="}
  """
  @spec update(atom, pid, topic, term, Map.t | (Map.t -> Map.t)) :: {:ok, ref :: binary} | {:error, reason :: term}
  def update(server_name, pid, topic, key, meta) when is_pid(pid) and (is_map(meta) or is_function(meta)) do
    GenServer.call(server_name, {:update, pid, topic, key, meta})
  end

  @doc """
  Lists all presences tracked under a given topic.

    * `server_name` - The registered name of the tracker server
    * `topic` - The `Phoenix.PubSub` topic to update for this presence

  Returns a lists of presences in key/metadata tuple pairs.

  ## Examples

      iex> Phoenix.Tracker.list(MyTracker, "lobby")
      [{123, %{name: "user 123"}}, {456, %{name: "user 456"}}]
  """
  @spec list(atom, topic) :: [presence]
  def list(server_name, topic) do
    # TODO avoid extra map (ideally crdt does an ets select only returning {key, meta})
    server_name
    |> GenServer.call({:list, topic})
    |> State.get_by_topic(topic)
    |> Enum.map(fn {{_topic, _pid, key},  meta, _tag} -> {key, meta} end)
  end

  @doc """
  Gracefully shuts down by broadcasting permdown to all replicas.

  ## Examples

      iex> Phoenix.Tracker.graceful_permdown(MyTracker)
      :ok
  """
  @spec graceful_permdown(atom) :: :ok
  def graceful_permdown(server_name) do
    GenServer.call(server_name, :graceful_permdown)
  end


  ## Server

  def start_link(tracker, tracker_opts, server_opts) do
    name = Keyword.fetch!(server_opts, :name)
    GenServer.start_link(__MODULE__, [tracker, tracker_opts, server_opts], name: name)
  end

  def init([tracker, tracker_opts, opts]) do
    Process.flag(:trap_exit, true)
    pubsub_server        = Keyword.fetch!(opts, :pubsub_server)
    server_name          = Keyword.fetch!(opts, :name)
    broadcast_period     = opts[:broadcast_period] || 1500
    max_silent_periods   = opts[:max_silent_periods] || 10
    down_period          = opts[:down_period] || (broadcast_period * max_silent_periods * 2)
    permdown_period      = opts[:permdown_period] || 1_200_000
    clock_sample_periods = opts[:clock_sample_periods] || 2
    log_level            = Keyword.get(opts, :log_level, false)

    with :ok <- validate_down_period(down_period, broadcast_period),
         :ok <- validate_permdown_period(permdown_period, down_period),
         {:ok, tracker_state} <- tracker.init(tracker_opts) do

      node_name        = Phoenix.PubSub.node_name(pubsub_server)
      namespaced_topic = namespaced_topic(server_name)
      replica          = Replica.new(node_name)

      subscribe(pubsub_server, namespaced_topic)
      send_stuttered_heartbeat(self(), broadcast_period)

      {:ok, %{server_name: server_name,
              pubsub_server: pubsub_server,
              tracker: tracker,
              tracker_state: tracker_state,
              replica: replica,
              namespaced_topic: namespaced_topic,
              log_level: log_level,
              replicas: %{},
              pending_clockset: [],
              presences: State.new(Replica.ref(replica)),
              broadcast_period: broadcast_period,
              max_silent_periods: max_silent_periods,
              silent_periods: max_silent_periods,
              down_period: down_period,
              permdown_period: permdown_period,
              clock_sample_periods: clock_sample_periods,
              deltas: [],
              max_delta_sizes: [100, 1000, 10_000],
              current_sample_count: clock_sample_periods}}
    end
  end

  def validate_down_period(d_period, b_period) when d_period < (2 * b_period) do
    {:error, "down_period must be at least twice as large as the broadcast_period"}
  end
  def validate_down_period(_d_period, _b_period), do: :ok

  def validate_permdown_period(p_period, d_period) when p_period <= d_period do
    {:error, "permdown_period must be at least larger than the down_period"}
  end
  def validate_permdown_period(_p_period, _d_period), do: :ok


  defp send_stuttered_heartbeat(pid, interval) do
    Process.send_after(pid, :heartbeat, Enum.random(0..trunc(interval * 0.25)))
  end

  def handle_info(:heartbeat, state) do
    {:noreply, state
               |> broadcast_delta_heartbeat()
               |> request_transfer_from_replicas_needing_synced()
               |> detect_downs()
               |> schedule_next_heartbeat()}
  end

  def handle_info({:pub, :heartbeat, {name, vsn}, :empty, clocks}, state) do
    {:noreply, state
               |> put_pending_clock(clocks)
               |> handle_heartbeat({name, vsn})}
  end
  def handle_info({:pub, :heartbeat, {name, vsn}, delta, clocks}, state) do
    {presences, joined, left} = State.merge(state.presences, delta)

    {:noreply, state
               |> report_diff(joined, left)
               |> put_presences(presences)
               |> put_pending_clock(clocks)
               |> push_delta_generation(delta)
               |> handle_heartbeat({name, vsn})}
  end

  def handle_info({:pub, :transfer_req, ref, {name, _vsn}, {_, clocks}}, state) do
    log state, fn -> "#{state.replica.name}: transfer_req from #{inspect name}" end
    delta = DeltaGeneration.extract(state.presences, state.deltas, clocks)
    msg = {:pub, :transfer_ack, ref, Replica.ref(state.replica), delta}
    direct_broadcast(state, name, msg)

    {:noreply, state}
  end

  def handle_info({:pub, :transfer_ack, _ref, {name, _vsn}, remote_presences}, state) do
    log(state, fn -> "#{state.replica.name}: transfer_ack from #{inspect name}" end)
    {presences, joined, left} = State.merge(state.presences, remote_presences)

    {:noreply, state
               |> report_diff(joined, left)
               |> push_delta_generation(remote_presences)
               |> put_presences(presences)}
  end

  def handle_info({:pub, :graceful_permdown, {_name, _vsn} = ref}, state) do
    case Replica.fetch_by_ref(state.replicas, ref) do
      {:ok, replica} -> {:noreply, state |> down(replica) |> permdown(replica)}
      :error         -> {:noreply, state}
    end
  end

  def handle_info({:EXIT, pid, _reason}, state) do
    {:noreply, drop_presence(state, pid)}
  end

  def handle_call({:track, pid, topic, key, meta}, _from, state) do
    case State.get_by_pid(state.presences, pid, topic, key) do
      nil ->
        {state, ref} = put_presence(state, pid, topic, key, meta)
        {:reply, {:ok, ref}, state}
      _ ->
        {:reply, {:error, {:already_tracked, pid, topic, key}}, state}
    end
  end

  def handle_call({:untrack, pid, topic, key}, _from, state) do
    new_state = drop_presence(state, pid, topic, key)
    if State.get_by_pid(new_state.presences, pid) == [] do
      Process.unlink(pid)
    end
    {:reply, :ok, new_state}
  end

  def handle_call({:untrack, pid}, _from, state) do
    Process.unlink(pid)
    {:reply, :ok, drop_presence(state, pid)}
  end

  def handle_call({:update, pid, topic, key, meta_updater}, _from, state) when is_function(meta_updater) do
    handle_update({pid, topic, key, meta_updater}, state)
  end

  def handle_call({:update, pid, topic, key, new_meta}, _from, state) do
    handle_update({pid, topic, key, fn _ -> new_meta end}, state)
  end

  def handle_call(:graceful_permdown, _from, state) do
    broadcast_from(state, self(), {:pub, :graceful_permdown, Replica.ref(state.replica)})
    {:stop, :normal, :ok, state}
  end

  def handle_call({:list, _topic}, _from, state) do
    {:reply, state.presences, state}
  end

  def handle_call(:replicas, _from, state) do
    {:reply, state.replicas, state}
  end

  def handle_call(:unsubscribe, _from, state) do
    Phoenix.PubSub.unsubscribe(state.pubsub_server, state.namespaced_topic)
    {:reply, :ok, state}
  end

  def handle_call(:resubscribe, _from, state) do
    subscribe(state.pubsub_server, state.namespaced_topic)
    {:reply, :ok, state}
  end

  defp subscribe(pubsub_server, namespaced_topic) do
    Phoenix.PubSub.subscribe(pubsub_server, namespaced_topic, link: true)
  end

  defp put_update(state, pid, topic, key, meta, %{phx_ref: ref} = prev_meta) do
    state
    |> put_presences(State.leave(state.presences, pid, topic, key))
    |> put_presence(pid, topic, key, Map.put(meta, :phx_ref_prev, ref), prev_meta)
  end
  defp put_presence(state, pid, topic, key, meta, prev_meta \\ nil) do
    Process.link(pid)
    ref = random_ref()
    meta = Map.put(meta, :phx_ref, ref)
    new_state =
      state
      |> report_diff_join(topic, key, meta, prev_meta)
      |> put_presences(State.join(state.presences, pid, topic, key, meta))

    {new_state, ref}
  end

  defp put_presences(state, %State{} = presences), do: %{state | presences: presences}

  defp drop_presence(state, pid, topic, key) do
    if leave = State.get_by_pid(state.presences, pid, topic, key) do
      state
      |> report_diff([], [leave])
      |> put_presences(State.leave(state.presences, pid, topic, key))
    else
      state
    end
  end
  defp drop_presence(state, pid) do
    leaves = State.get_by_pid(state.presences, pid)

    state
    |> report_diff([], leaves)
    |> put_presences(State.leave(state.presences, pid))
  end

  defp handle_heartbeat(state, {name, vsn}) do
    case Replica.put_heartbeat(state.replicas, {name, vsn}) do
      {replicas, nil, %Replica{status: :up} = upped} ->
        up(%{state | replicas: replicas}, upped)

      {replicas, %Replica{vsn: ^vsn, status: :up}, %Replica{vsn: ^vsn, status: :up}} ->
        %{state | replicas: replicas}

      {replicas, %Replica{vsn: ^vsn, status: :down}, %Replica{vsn: ^vsn, status: :up} = upped} ->
        up(%{state | replicas: replicas}, upped)

      {replicas, %Replica{vsn: old, status: :up} = downed, %Replica{vsn: ^vsn, status: :up} = upped} when old != vsn ->
        %{state | replicas: replicas} |> down(downed) |> permdown(downed) |> up(upped)

      {replicas, %Replica{vsn: old, status: :down} = downed, %Replica{vsn: ^vsn, status: :up} = upped} when old != vsn ->
        %{state | replicas: replicas} |> permdown(downed) |> up(upped)
    end
  end

  defp request_transfer_from_replicas_needing_synced(%{current_sample_count: 1} = state) do
    needs_synced = clockset_to_sync(state)
    for replica <- needs_synced, do: request_transfer(state, replica)

    %{state | pending_clockset: [], current_sample_count: state.clock_sample_periods}
  end
  defp request_transfer_from_replicas_needing_synced(state) do
    %{state | current_sample_count: state.current_sample_count - 1}
  end

  defp request_transfer(state, {name, _vsn}) do
    log state, fn -> "#{state.replica.name}: transfer_req from #{name}" end
    ref = make_ref()
    msg = {:pub, :transfer_req, ref, Replica.ref(state.replica), clock(state)}
    direct_broadcast(state, name, msg)
  end

  defp detect_downs(%{permdown_period: perm_int, down_period: temp_int} = state) do
    Enum.reduce(state.replicas, state, fn {_name, replica}, acc ->
      case Replica.detect_down(acc.replicas, replica, temp_int, perm_int) do
        {replicas, %Replica{status: :up}, %Replica{status: :permdown} = down_rep} ->
          %{acc | replicas: replicas} |> down(down_rep) |> permdown(down_rep)

        {replicas, %Replica{status: :down}, %Replica{status: :permdown} = down_rep} ->
          permdown(%{acc | replicas: replicas}, down_rep)

        {replicas, %Replica{status: :up}, %Replica{status: :down} = down_rep} ->
          down(%{acc | replicas: replicas}, down_rep)

        {replicas, %Replica{status: unchanged}, %Replica{status: unchanged}} ->
          %{acc | replicas: replicas}
      end
    end)
  end

  defp schedule_next_heartbeat(state) do
    Process.send_after(self(), :heartbeat, state.broadcast_period)
    state
  end

  defp clock(state), do: State.clocks(state.presences)

  @spec clockset_to_sync(%{pending_clockset: [State.replica_context]}) :: [State.replica_name]
  defp clockset_to_sync(state) do
    my_ref = Replica.ref(state.replica)

    state.pending_clockset
    |> Clock.append_clock(clock(state))
    |> Clock.clockset_replicas()
    |> Enum.filter(fn ref -> ref != my_ref end)
  end

  defp put_pending_clock(state, clocks) do
    %{state | pending_clockset: Clock.append_clock(state.pending_clockset, clocks)}
  end

  defp up(state, remote_replica) do
    log state, fn -> "#{state.replica.name}: replica up from #{inspect remote_replica.name}" end
    {presences, joined, []} = State.replica_up(state.presences, Replica.ref(remote_replica))

    state
    |> report_diff(joined, [])
    |> put_presences(presences)
  end

  defp down(state, remote_replica) do
    log state, fn -> "#{state.replica.name}: replica down from #{inspect remote_replica.name}" end
    {presences, [], left} = State.replica_down(state.presences, Replica.ref(remote_replica))

    state
    |> report_diff([], left)
    |> put_presences(presences)
  end

  defp permdown(state, %Replica{name: name} = remote_replica) do
    log state, fn -> "#{state.replica.name}: permanent replica down detected #{name}" end
    replica_ref = Replica.ref(remote_replica)
    presences = State.remove_down_replicas(state.presences, replica_ref)
    deltas = DeltaGeneration.remove_down_replicas(state.deltas, replica_ref)

    case Replica.fetch_by_ref(state.replicas, replica_ref) do
      {:ok, _replica} ->
        replicas = Map.delete(state.replicas, name)
        %{state | presences: presences, replicas: replicas, deltas: deltas}
      _ ->
        %{state | presences: presences, deltas: deltas}
    end
  end

  defp namespaced_topic(server_name) do
    "phx_presence:#{server_name}"
  end

  defp broadcast_from(state, from, msg) do
    Phoenix.PubSub.broadcast_from!(state.pubsub_server, from, state.namespaced_topic, msg)
  end

  defp direct_broadcast(state, target_node, msg) do
    Phoenix.PubSub.direct_broadcast!(target_node, state.pubsub_server, state.namespaced_topic, msg)
  end

  defp broadcast_delta_heartbeat(%{presences: presences} = state) do
    cond do
      State.has_delta?(presences) ->
        delta = presences.delta
        new_presences = presences |> State.reset_delta() |> State.compact()

        broadcast_from(state, self(), {:pub, :heartbeat, Replica.ref(state.replica), delta, clock(state)})
        %{state | presences: new_presences, silent_periods: 0}
        |> push_delta_generation(delta)

      state.silent_periods >= state.max_silent_periods ->
        broadcast_from(state, self(), {:pub, :heartbeat, Replica.ref(state.replica), :empty, clock(state)})
        %{state | silent_periods: 0}

      true -> update_in(state.silent_periods, &(&1 + 1))
    end
  end

  defp report_diff(state, [], []), do: state
  defp report_diff(state, joined, left) do
    join_diff = Enum.reduce(joined, %{}, fn {{topic, _pid, key}, meta, _}, acc ->
      Map.update(acc, topic, {[{key, meta}], []}, fn {joins, leaves} ->
        {[{key, meta} | joins], leaves}
      end)
    end)
    full_diff = Enum.reduce(left, join_diff, fn {{topic, _pid, key}, meta, _}, acc ->
      Map.update(acc, topic, {[], [{key, meta}]}, fn {joins, leaves} ->
        {joins, [{key, meta} | leaves]}
      end)
    end)

    full_diff
    |> state.tracker.handle_diff(state.tracker_state)
    |> handle_tracker_result(state)
  end

  defp report_diff_join(state, topic, key, meta, nil = _prev_meta) do
    %{topic => {[{key, meta}], []}}
    |> state.tracker.handle_diff(state.tracker_state)
    |> handle_tracker_result(state)
  end
  defp report_diff_join(state, topic, key, meta, prev_meta) do
    %{topic => {[{key, meta}], [{key, prev_meta}]}}
    |> state.tracker.handle_diff(state.tracker_state)
    |> handle_tracker_result(state)
  end

  defp handle_tracker_result({:ok, tracker_state}, state) do
    %{state | tracker_state: tracker_state}
  end
  defp handle_tracker_result(other, state) do
    raise ArgumentError, """
    expected #{state.tracker} to return {:ok, state}, but got:

        #{inspect other}
    """
  end

  defp handle_update({pid, topic, key, meta_updater}, state) do
    case State.get_by_pid(state.presences, pid, topic, key) do
      nil ->
        {:reply, {:error, :nopresence}, state}
      {{_topic, _pid, ^key}, prev_meta, {_replica, _}} ->
        {state, ref} = put_update(state, pid, topic, key, meta_updater.(prev_meta), prev_meta)
        {:reply, {:ok, ref}, state}
    end
  end

  defp push_delta_generation(state, {%State{mode: :normal}, _}) do
    %{state | deltas: []}
  end
  defp push_delta_generation(%{deltas: deltas} = state, %State{mode: :delta} = delta) do
    new_deltas = DeltaGeneration.push(state.presences, deltas, delta, state.max_delta_sizes)
    %{state | deltas: new_deltas}
  end

  defp random_ref() do
    :crypto.strong_rand_bytes(8) |> Base.encode64()
  end

  defp log(%{log_level: false}, _msg_func), do: :ok
  defp log(%{log_level: level}, msg), do: Logger.log(level, msg)
end
