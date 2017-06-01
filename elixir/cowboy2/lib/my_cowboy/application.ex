defmodule MyCowboy.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    {:ok, _pid} = :cowboy.start_clear(:http, 100, trans_opts, proto_opts)
  end
end
