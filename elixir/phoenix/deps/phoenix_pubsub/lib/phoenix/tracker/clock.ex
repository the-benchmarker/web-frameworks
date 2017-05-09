defmodule Phoenix.Tracker.Clock do
  @moduledoc false
  alias Phoenix.Tracker.State

  @type context :: State.context
  @type clock :: {State.name, context}

  @doc """
  Returns a list of replicas from a list of contexts.
  """
  @spec clockset_replicas([clock]) :: [State.name]
  def clockset_replicas(clockset) do
    for {replica, _} <- clockset, do: replica
  end

  @doc """
  Adds a replicas context to a clockset, keeping only dominate contexts.
  """
  @spec append_clock([clock], clock) :: [clock]
  def append_clock(clockset, {_, clock}) when map_size(clock) == 0, do: clockset
  def append_clock(clockset, {node, clock}) do
    big_clock = combine_clocks(clockset)
    cond do
      dominates?(clock, big_clock) -> [{node, clock}]
      dominates?(big_clock, clock) -> clockset
      true -> filter_clocks(clockset, {node, clock})
    end
  end

  @doc """
  Checks of one clock causally dominates the other for all replicas.
  """
  @spec dominates?(context, context) :: boolean
  def dominates?(c1, c2) when map_size(c1) < map_size(c2), do: false
  def dominates?(c1, c2) do
    Enum.reduce_while(c2, true, fn {replica, clock}, true ->
      if Map.get(c1, replica, 0) >= clock do
        {:cont, true}
      else
        {:halt, false}
      end
    end)
  end

  @doc """
  Checks of one clock causally dominates the other for their shared replicas.
  """
  def dominates_or_equal?(c1, c2) when c1 == %{} and c2 == %{}, do: true
  def dominates_or_equal?(c1, _c2) when c1 == %{}, do: false
  def dominates_or_equal?(c1, c2) do
    Enum.reduce_while(c1, true, fn {replica, clock}, true ->
      if clock >= Map.get(c2, replica, 0) do
        {:cont, true}
      else
        {:halt, false}
      end
    end)
  end

  @doc """
  Returns the upper bound causal context of two clocks.
  """
  def upperbound(c1, c2) do
    Map.merge(c1, c2, fn _, v1, v2 -> max(v1, v2) end)
  end

  @doc """
  Returns the lower bound causal context of two clocks.
  """
  def lowerbound(c1, c2) do
    Map.merge(c1, c2, fn _, v1, v2 -> min(v1, v2) end)
  end


  defp filter_clocks(clockset, {node, clock}) do
    clockset
    |> Enum.reduce({[], false}, fn {node2, clock2}, {set, insert} ->
      if dominates?(clock, clock2) do
        {set, true}
      else
        {[{node2, clock2}| set], insert || !dominates?(clock2, clock)}
      end
    end)
    |> case do
      {new_clockset, true} -> [{node, clock} | new_clockset]
      {new_clockset, false} -> new_clockset
    end
  end

  defp combine_clocks(clockset) do
    clockset
    |> Enum.map(fn {_, clocks} -> clocks end)
    |> Enum.reduce(%{}, &upperbound(&1, &2))
  end
end
