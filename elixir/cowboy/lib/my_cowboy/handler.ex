defmodule MyCowboy.Handler do

  def init(%{method: method, path: path} = req, opts) do
    handle(method, split_path(path), req, opts)
  end

  defp handle("GET", [], req, opts) do
    {:ok, :cowboy_req.reply(200, %{}, "", req), opts}
  end
  defp handle("GET", ["user", id], req, opts) do
    {:ok, :cowboy_req.reply(200, %{}, id, req), opts}
  end
  defp handle("POST", ["user"], req, opts) do
    {:ok, :cowboy_req.reply(200, %{}, "", req), opts}
  end

  defp split_path(path) do
    segments = :binary.split(path, "/", [:global])
    for segment <- segments, segment != "", do: segment
  end
end
