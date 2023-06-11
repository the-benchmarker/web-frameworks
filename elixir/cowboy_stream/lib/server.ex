defmodule Server do
  @behaviour :cowboy_stream

  @compile {:inline, response: 0, response: 1}
  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  @impl :cowboy_stream
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

  @impl :cowboy_stream
  def data(_stream_id, _is_fin, _data, state), do: {[], state}

  @impl :cowboy_stream
  def info(_stream_id, _info, state), do: {[], state}

  @impl :cowboy_stream
  def terminate(_stream_id, _reason, _state), do: :ok

  @impl :cowboy_stream
  def early_error(_stream_id, _reason, _partial_req, resp, _opts), do: resp
end
