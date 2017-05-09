defmodule Plug.Supervisor do
  @moduledoc false

  def start_link() do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    import Supervisor.Spec

    children = [
      worker(Plug.Upload, [])
    ]

    Plug.Keys = :ets.new(Plug.Keys, [:named_table, :public, read_concurrency: true])
    supervise(children, strategy: :one_for_one)
  end
end
