defmodule Phoenix.PubSub.GC do
  @moduledoc """
  A garbage collector process that cleans up the table used
  by `Phoenix.PubSub.Local`.
  """

  use GenServer

  @doc """
  Starts the server.

    * `server_name` - The name to register the server under
    * `table_name` - The name of the local table

  """
  def start_link(server_name, local_name) do
    GenServer.start_link(__MODULE__, {server_name, local_name}, name: server_name)
  end

  @doc """
  Force table clean up because the given pid is down asynchronously.

    * `gc_server` - The registered server name or pid
    * `pid` - The subscriber pid

  ## Examples

      iex> down(:gc_server, self)
      :ok

  """
  def down(gc_server, pid) when is_atom(gc_server) do
    GenServer.cast(gc_server, {:down, pid})
  end

  def init({server_name, local_name}) do
    {:ok, %{topics: local_name, pids: server_name}}
  end

  def handle_call({:subscription, pid}, _from, state) do
    {:reply, subscription(state.pids, pid), state}
  end

  def handle_cast({:down, pid}, state) do
    try do
      topics = :ets.lookup_element(state.pids, pid, 2)
      for topic <- topics do
        true = :ets.match_delete(state.topics, {topic, {pid, :_}})
      end
      true = :ets.match_delete(state.pids, {pid, :_})
    catch
      :error, :badarg -> :badarg
    end

    {:noreply, state}
  end

  defp subscription(pids_table, pid) do
    try do
      :ets.lookup_element(pids_table, pid, 2)
    catch
      :error, :badarg -> []
    end
  end
end
