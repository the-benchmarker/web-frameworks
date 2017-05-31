defmodule MyElli.Application do
  @moduledoc false

  def start(_type, _args) do
    {:ok, _} = :elli.start_link(callback: MyElli.Callback, port: 3000)
  end
end
