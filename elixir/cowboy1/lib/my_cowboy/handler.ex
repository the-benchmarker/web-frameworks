defmodule MyCowboy.Handler do

  def init(_type, req, opts) do
    {:ok, req, opts}
  end

  def handle(req, opts) do
    {method, req} = :cowboy_req.method(req)
    {path, req} = :cowboy_req.path(req)
    {:ok, req} = reply(method, split_path(path), req)
    {:ok, req, opts}
  end

  def terminate(_, _, _), do: :ok

  defp reply("GET", [], req), do: :cowboy_req.reply(200, [], "", req)
  defp reply("GET", ["user", id], req), do: :cowboy_req.reply(200, [], id, req)
  defp reply("POST", ["user"], req), do: :cowboy_req.reply(200, [], "", req)

  defp split_path(path) do
    segments = :binary.split(path, "/", [:global])
    for segment <- segments, segment != "", do: segment
  end
end
