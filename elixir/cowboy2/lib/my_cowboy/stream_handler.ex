defmodule MyCowboy.StreamHandler do
  @behaviour :cowboy_stream

  def init(_stream_id, %{method: method, path: path}, _opts) do
    {[handle(method, split_path(path))], []}
  end

  def data(_stream_id, _is_fin, _data, state), do: {[], state}

  def info(_stream_id, _info, state), do: {[], state}

  def terminate(_stream_id, _reason, _state), do: :ok

  def early_error(_stream_id, _reason, _partial_req, resp, _opts), do: resp


  defp handle("GET", []) do
    {:response, 200, %{"content-length" => "0"}, ""}
  end
  defp handle("GET", ["user", id]) do
    {:response, 200, %{"content-length" => "#{:erlang.size(id)}"}, id}
  end
  defp handle("POST", ["user"]) do
    {:response, 200, %{"content-length" => "0"}, ""}
  end

  defp split_path(path) do
    segments = :binary.split(path, "/", [:global])
    for segment <- segments, segment != "", do: segment
  end
end
