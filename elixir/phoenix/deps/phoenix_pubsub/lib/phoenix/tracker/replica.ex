defmodule Phoenix.Tracker.Replica do
  @moduledoc false
  alias Phoenix.Tracker.Replica

  @type name :: String.t
  @type vsn :: term
  @type replica_ref :: {name, vsn}

  @type t :: %Replica{
    name: name,
    vsn: vsn,
    last_heartbeat_at: pos_integer,
    status: :up | :down | :permdown
  }

  defstruct name: nil,
            vsn: nil,
            last_heartbeat_at: nil,
            status: :up


  @type op_result :: {%{name => Replica.t}, previous_node :: Replica.t | nil, updated_node :: Replica.t}

  @doc """
  Returns a new Replica with a unique vsn.
  """
  @spec new(name) :: Replica.t
  def new(name) do
    %Replica{name: name, vsn: unique_vsn()}
  end

  @spec ref(Replica.t) :: replica_ref
  def ref(%Replica{name: name, vsn: vsn}), do: {name, vsn}

  @spec put_heartbeat(%{name => Replica.t}, replica_ref) :: op_result
  def put_heartbeat(replicas, {name, vsn}) do
    case Map.fetch(replicas, name) do
      :error ->
        new_replica = touch_last_heartbeat(%Replica{name: name, vsn: vsn, status: :up})
        {Map.put(replicas, name, new_replica), nil, new_replica}

      {:ok, %Replica{} = prev_replica} ->
        updated_replica = touch_last_heartbeat(%Replica{prev_replica | vsn: vsn, status: :up})
        {Map.put(replicas, name, updated_replica), prev_replica, updated_replica}
    end
  end

  @spec detect_down(%{name => Replica.t}, Replica.t, pos_integer, pos_integer) :: op_result
  def detect_down(replicas, replica, temp_interval, perm_interval, now \\ now_ms()) do
    downtime = now - replica.last_heartbeat_at
    cond do
      downtime > perm_interval -> {Map.delete(replicas, replica.name), replica, permdown(replica)}
      downtime > temp_interval ->
        updated_replica = down(replica)
        {Map.put(replicas, replica.name, updated_replica), replica, updated_replica}
      true -> {replicas, replica, replica}
    end
  end

  @doc """
  Fetches a replica from the map with matching name and version from the ref.
  """
  @spec fetch_by_ref(%{name => Replica.t}, replica_ref) :: {:ok, Replica.t} | :error
  def fetch_by_ref(replicas, {name, vsn}) do
    case Map.fetch(replicas, name) do
      {:ok, %Replica{vsn: ^vsn} = replica} -> {:ok, replica}
      {:ok, %Replica{vsn: _vsn}} -> :error
      :error -> :error
    end
  end

  defp permdown(replica), do: %Replica{replica | status: :permdown}

  defp down(replica), do: %Replica{replica | status: :down}

  defp touch_last_heartbeat(replica) do
    %Replica{replica | last_heartbeat_at: now_ms()}
  end

  defp now_ms, do: System.system_time(:milli_seconds)

  defp unique_vsn do
    System.system_time(:micro_seconds) + System.unique_integer([:positive])
  end
end
