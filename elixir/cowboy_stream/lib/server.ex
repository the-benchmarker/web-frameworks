defmodule Server do
  @behaviour :cowboy_stream

  @compile {:inline, response: 0, response: 1}
  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def init(_stream_id, %{method: "GET", path: "/"}, _opts) do
    {response(), []}
  end

  def init(_stream_id, %{method: "GET", path: "/user/" <> id}, _opts) do
    {response(id), []}
  end

  def init(_stream_id, %{method: "POST", path: "/user"}, _opts) do
    {response(), []}
  end

  defp response(body \\ "") do
    [
      {:response, 200, %{"Content-Length" => Integer.to_string(byte_size(body))}, body},
      :stop
    ]
  end

  def data(_stream_id, _is_fin, _data, state) do
    {[], state}
  end

  def info(_stream_id, _info, state) do
    {[], state}
  end

  def terminate(_stream_id, _reason, _state) do
    :ok
  end

  def early_error(_stream_id, _reason, _partial_req, resp, _opts) do
    resp
  end
end
