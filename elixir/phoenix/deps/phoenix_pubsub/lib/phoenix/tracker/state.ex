defmodule Phoenix.Tracker.State do
  @moduledoc """
  Provides an ORSWOT CRDT.
  """
  alias Phoenix.Tracker.{State, Clock}

  @type name       :: term
  @type topic      :: String.t
  @type key        :: term
  @type meta       :: Map.t
  @type ets_id     :: pos_integer
  @type clock      :: pos_integer
  @type tag        :: {name, clock}
  @type cloud      :: MapSet.t
  @type context    :: %{name => clock}
  @type values     :: ets_id | :extracted | %{tag => {pid, topic, key, meta}}
  @type value      :: {{topic, pid, key}, meta, tag}
  @type delta      :: %State{mode: :delta}
  @type pid_lookup :: {pid, topic, key}

  @type t :: %State{
    replica:  name,
    context:  context,
    cloud:    cloud,
    values:   values,
    pids:     ets_id,
    mode:     :unset | :delta | :normal,
    delta:    :unset | delta,
    replicas: %{name => :up | :down},
    range:    {context, context}
  }

  defstruct replica: nil,
            context: %{},
            cloud: MapSet.new(),
            values: nil,
            pids: nil,
            mode: :unset,
            delta: :unset,
            replicas: %{},
            range: {%{}, %{}}

  @doc """
  Creates a new set for the replica.

  ## Examples

      iex> Phoenix.Tracker.State.new(:replica1)
      %Phoenix.Tracker.State{...}

  """
  @spec new(name) :: t
  def new(replica) do
    reset_delta(%State{
      replica: replica,
      mode: :normal,
      values: :ets.new(:values, [:ordered_set]),
      pids: :ets.new(:pids, [:duplicate_bag]),
      replicas: %{replica => :up}})
  end

  @doc """
  Returns the causal context for the set.
  """
  @spec clocks(t) :: {name, context}
  def clocks(%State{replica: rep, context: ctx}), do: {rep, ctx}

  @doc """
  Adds a new element to the set.
  """
  @spec join(t, pid, topic, key, meta) :: t
  def join(%State{} = state, pid, topic, key, meta \\ %{}) do
    add(state, pid, topic, key, meta)
  end

  @doc """
  Removes an element from the set.
  """
  @spec leave(t, pid, topic, key) :: t
  def leave(%State{pids: pids} = state, pid, topic, key) do
    pids
    |> :ets.match_object({pid, topic, key})
    |> case do
      [{^pid, ^topic, ^key}] -> remove(state, pid, topic, key)
      [] -> state
    end
  end

  @doc """
  Removes all elements from the set for the given pid.
  """
  @spec leave(t, pid) :: t
  def leave(%State{pids: pids} = state, pid) do
    pids
    |> :ets.lookup(pid)
    |> Enum.reduce(state, fn {^pid, topic, key}, acc ->
      remove(acc, pid, topic, key)
    end)
  end

  @doc """
  Returns a list of elements in the set belonging to an online replica.
  """
  @spec online_list(t) :: [value]
  def online_list(%State{values: values} = state) do
    replicas = down_replicas(state)
    :ets.select(values, [{ {:_, :_, {:"$1", :_}},
      not_in(:"$1", replicas), [:"$_"]}])
  end

  @doc """
  Returns a list of elements for the topic who belong to an online replica.
  """
  @spec get_by_topic(t, topic) :: [value]
  def get_by_topic(%State{values: values} = state, topic) do
    replicas = down_replicas(state)
    :ets.select(values, [{ {{topic, :_, :_}, :_, {:"$1", :_}},
      not_in(:"$1", replicas), [:"$_"]}])
  end
  defp not_in(_pos, []), do: []
  defp not_in(pos, replicas), do: [not: ors(pos, replicas)]
  defp ors(pos, [rep]), do: {:"==", pos, {rep}}
  defp ors(pos, [rep | rest]), do: {:or, {:"==", pos, {rep}}, ors(pos, rest)}

  @doc """
  Returns the element matching the pid, topic, and key.
  """
  @spec get_by_pid(t, pid, topic, key) :: value | nil
  def get_by_pid(%State{values: values}, pid, topic, key) do
    case :ets.lookup(values, {topic, pid, key}) do
      [] -> nil
      [one] -> one
    end
  end

  @doc """
  Returns all elements for the pid.
  """
  @spec get_by_pid(t, pid) :: [value]
  def get_by_pid(%State{pids: pids, values: values}, pid) do
    case :ets.lookup(pids, pid) do
      [] -> []
      matches ->
        :ets.select(values, Enum.map(matches, fn {^pid, topic, key} ->
          {{{topic, pid, key}, :_, :_}, [], [:"$_"]}
        end))
    end
  end

  @doc """
  Checks if set has a non-empty delta.
  """
  @spec has_delta?(t) :: boolean
  def has_delta?(%State{delta: %State{cloud: cloud}}) do
    MapSet.size(cloud) != 0
  end

  @doc """
  Resets the set's delta.
  """
  @spec reset_delta(t) :: t
  def reset_delta(%State{context: ctx, replica: replica} = state) do
    delta_ctx = Map.take(ctx, [replica])
    delta = %State{replica: replica,
                   values: %{},
                   range: {delta_ctx, delta_ctx},
                   mode: :delta}
    %State{state | delta: delta}
  end

  @doc """
  Extracts the set's elements from ets into a mergeable list.

  Used when merging two sets.
  """
  @spec extract(t) :: {t, values}
  def extract(%State{values: values} = state) do
    map = foldl(values, [], fn {{topic, pid, key}, meta, tag}, acc ->
      [{tag, {pid, topic, key, meta}} | acc]
    end) |> :maps.from_list()
    {%State{state | pids: nil, values: nil, delta: :unset}, map}
  end

  @doc """
  Merges two sets, or a delta into a set.

  Returns a 3-tuple of the updated set, and the joined and left elements.

  ## Examples

      iex> {s1, joined, left} =
           Phoenix.Tracker.State.merge(s1, Phoenix.Tracker.State.extract(s2))

      {%Phoenix.Tracker.State{}, [...], [...]}
  """
  @spec merge(local :: t, {remote :: t, values} | delta) :: {new_local :: t, joins :: [value], leaves :: [value]}
  def merge(%State{} = local, %State{mode: :delta} = remote) do
    merge(local, {remote, remote.values})
  end
  def merge(%State{} = local, {%State{} = remote, remote_map}) do
    {pids, joins} = accumulate_joins(local, remote_map)
    {cloud, delta, leaves} = observe_removes(local, remote, remote_map)
    true = :ets.insert(local.values, joins)
    true = :ets.insert(local.pids, pids)
    ctx = Clock.upperbound(local.context, remote.context)
    new_state =
      %State{local | cloud: cloud, delta: delta}
      |> put_context(ctx)
      |> compact()

    {new_state, joins, leaves}
  end

  @spec accumulate_joins(t, values) :: joins :: {[pid_lookup], [values]}
  defp accumulate_joins(local, remote_map) do
    Enum.reduce(remote_map, {[], []}, fn {tag, {pid, topic, key, meta}}, {pids, adds} ->
      if in?(local, tag) do
        {pids, adds}
      else
        {[{pid, topic, key} | pids], [{{topic, pid, key}, meta, tag} | adds]}
      end
    end)
  end

  @spec observe_removes(t, t, [value]) :: {cloud, delta, leaves :: [value]}
  defp observe_removes(%State{pids: pids, values: values} = local, remote, remote_map) do
    unioned_cloud = MapSet.union(local.cloud, remote.cloud)
    init = {unioned_cloud, local.delta, []}

    foldl(local.values, init, fn {{topic, pid, key}, _, tag} = el, {cloud, delta, leaves} ->
      if in?(remote, tag) and not Map.has_key?(remote_map, tag) do
        1 = :ets.select_delete(values, [{el, [], [true]}])
        1 = :ets.select_delete(pids, [{{pid, topic, key}, [], [true]}])
        {MapSet.delete(cloud, tag), remove_delta_tag(delta, tag), [el | leaves]}
      else
        {cloud, delta, leaves}
      end
    end)
  end

  def merge_deltas(%State{mode: :delta} = local, %State{mode: :delta, values: remote_values} = remote) do
    local_values = local.values
    {local_start, local_end} = local.range
    {remote_start, remote_end} = remote.range

    if Clock.dominates_or_equal?(local_end, remote_start) do
      new_start = Clock.lowerbound(local_start, remote_start)
      new_end = Clock.upperbound(local_end, remote_end)
      cloud  = MapSet.union(local.cloud, remote.cloud)

      filtered_locals = for {tag, value} <- local_values,
                        Map.has_key?(remote_values, tag) or not in?(remote, tag),
                        into: %{},
                        do: {tag, value}

      merged_vals = for {tag, value} <- remote_values,
                    !Map.has_key?(local_values, tag) and not in?(local, tag),
                    into: filtered_locals,
                    do: {tag, value}

      {:ok, %State{local | cloud: cloud, values: merged_vals, range: {new_start, new_end}}}
    else
      {:error, :not_contiguous}
    end
  end

  @doc """
  Marks a replica as up in the set and returns rejoined users.
  """
  @spec replica_up(t, name) :: {t, joins :: [values], leaves :: []}
  def replica_up(%State{replicas: replicas} = state, replica) do
    {%State{state | replicas: Map.put(replicas, replica, :up)}, replica_users(state, replica), []}
  end

  @doc """
  Marks a replica as down in the set and returns left users.
  """
  @spec replica_down(t, name) :: {t, joins:: [], leaves :: [values]}
  def replica_down(%State{replicas: replicas} = state, replica) do
    {%State{state | replicas: Map.put(replicas, replica, :down)}, [], replica_users(state, replica)}
  end

  @doc """
  Removes all elements for replicas that are permanently gone.
  """
  @spec remove_down_replicas(t, name) :: t
  def remove_down_replicas(%State{mode: :normal, context: ctx, values: values, pids: pids} = state, replica) do
    new_ctx = for {rep, clock} <- ctx, rep != replica, into: %{}, do: {rep, clock}
    match_spec = {:_, :_, {replica, :_}}
    new_cloud =
      values
      |> :ets.match_object(match_spec)
      |> Enum.reduce(state.cloud, fn {{topic, pid, key}, _meta, tag}, cloud ->
        1 = :ets.select_delete(pids, [{{pid, topic, key}, [], [true]}])
        MapSet.delete(cloud, tag)
      end)

    new_delta = remove_down_replicas(state.delta, replica)
    true = :ets.match_delete(values, match_spec)

    %State{state | context: new_ctx, cloud: new_cloud, delta: new_delta}
  end
  def remove_down_replicas(%State{mode: :delta, range: range} = delta, replica) do
    {start_ctx, end_ctx} = range
    new_start = for {rep, clock} <- start_ctx, rep != replica, into: %{}, do: {rep, clock}
    new_end = for {rep, clock} <- end_ctx, rep != replica, into: %{}, do: {rep, clock}

    {new_cloud, new_vals} = Enum.reduce(delta.values, {delta.cloud, delta.values}, fn
      {{^replica, _clock} = tag, {_pid, _topic, _key, _meta}}, {cloud, vals} ->
        {MapSet.delete(cloud, tag), Map.delete(vals, tag)}
      {{_replica, _clock} = _tag, {_pid, _topic, _key, _meta}}, {cloud, vals} ->
        {cloud, vals}
    end)

    %State{delta | range: {new_start, new_end}, cloud: new_cloud, values: new_vals}
  end

  @doc """
  Returns the dize of the delta.
  """
  @spec delta_size(delta) :: pos_integer
  def delta_size(%State{mode: :delta, cloud: cloud, values: values}) do
    MapSet.size(cloud) + map_size(values)
  end

  @spec add(t, pid, topic, key, meta) :: t
  defp add(%State{} = state, pid, topic, key, meta) do
    state
    |> bump_clock()
    |> do_add(pid, topic, key, meta)
  end
  defp do_add(%State{delta: delta} = state, pid, topic, key, meta) do
    true = :ets.insert(state.values, {{topic, pid, key}, meta, tag(state)})
    true = :ets.insert(state.pids, {pid, topic, key})
    new_delta = %State{delta | values: Map.put(delta.values, tag(state), {pid, topic, key, meta})}
    %State{state | delta: new_delta}
  end

  @spec remove(t, pid, topic, key) :: t
  defp remove(%State{pids: pids, values: values} = state, pid, topic, key) do
    [{{^topic, ^pid, ^key}, _meta, tag}] = :ets.lookup(values, {topic, pid, key})
    1 = :ets.select_delete(values, [{{{topic, pid, key}, :_, :_}, [], [true]}])
    1 = :ets.select_delete(pids, [{{pid, topic, key}, [], [true]}])
    pruned_cloud = MapSet.delete(state.cloud, tag)
    new_delta = remove_delta_tag(state.delta, tag)

    bump_clock(%State{state | cloud: pruned_cloud, delta: new_delta})
  end

  @spec remove_delta_tag(delta, tag) :: delta
  defp remove_delta_tag(%State{mode: :delta, values: values, cloud: cloud} = delta, tag) do
    %State{delta | cloud: MapSet.put(cloud, tag), values: Map.delete(values, tag)}
  end

  @doc """
  Compacts a sets causal history.

  Called as needed and after merges.
  """
  @spec compact(t) :: t
  def compact(%State{context: ctx, cloud: cloud} = state) do
    {new_ctx, new_cloud} = do_compact(ctx, Enum.sort(cloud))
    put_context(%State{state | cloud: new_cloud}, new_ctx)
  end
  @spec do_compact(context, sorted_cloud_list :: list) :: {context, cloud}
  defp do_compact(ctx, cloud) do
    Enum.reduce(cloud, {ctx, MapSet.new()}, fn {replica, clock} = tag, {ctx_acc, cloud_acc} ->
      case {Map.get(ctx_acc, replica), clock} do
        {nil, 1} ->
          {Map.put(ctx_acc, replica, clock), cloud_acc}
        {nil, _} ->
          {ctx_acc, MapSet.put(cloud_acc, tag)}
        {ctx_clock, clock} when ctx_clock + 1 == clock ->
          {Map.put(ctx_acc, replica, clock), cloud_acc}
        {ctx_clock, clock} when ctx_clock >= clock ->
          {ctx_acc, cloud_acc}
        {_, _} ->
          {ctx_acc, MapSet.put(cloud_acc, tag)}
      end
    end)
  end

  @spec in?(t, tag) :: boolean
  defp in?(%State{context: ctx, cloud: cloud}, {replica, clock} = tag) do
    Map.get(ctx, replica, 0) >= clock or MapSet.member?(cloud, tag)
  end

  @spec tag(t) :: tag
  defp tag(%State{replica: rep} = state), do: {rep, clock(state)}

  @spec clock(t) :: clock
  defp clock(%State{replica: rep, context: ctx}), do: Map.get(ctx, rep, 0)

  @spec bump_clock(t) :: t
  defp bump_clock(%State{mode: :normal, replica: rep, cloud: cloud, context: ctx, delta: delta} = state) do
    new_clock = clock(state) + 1
    new_ctx = Map.put(ctx, rep, new_clock)

    %State{state |
           cloud: MapSet.put(cloud, {rep, new_clock}),
           delta: %State{delta | cloud: MapSet.put(delta.cloud, {rep, new_clock})}}
    |> put_context(new_ctx)
  end
  defp put_context(%State{delta: delta, replica: rep} = state, new_ctx) do
    {start_clock, end_clock} = delta.range
    new_end = Map.put(end_clock, rep, Map.get(new_ctx, rep, 0))
    %State{state |
           context: new_ctx,
           delta: %State{delta | range: {start_clock, new_end}}}
  end

  @spec down_replicas(t) :: [name]
  defp down_replicas(%State{replicas: replicas})  do
    for {replica, :down} <- replicas, do: replica
  end

  @spec replica_users(t, name) :: [value]
  defp replica_users(%State{values: values}, replica) do
    :ets.match_object(values, {:_, :_, {replica, :_}})
  end

  defp foldl(values, initial, func), do: :ets.foldl(func, initial, values)
end
