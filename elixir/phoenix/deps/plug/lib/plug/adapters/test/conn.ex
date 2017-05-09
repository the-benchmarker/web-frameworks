defmodule Plug.Adapters.Test.Conn do
  @behaviour Plug.Conn.Adapter
  @moduledoc false

  ## Test helpers

  def conn(conn, method, uri, body_or_params) do
    maybe_flush()

    uri    = URI.parse(uri)
    method = method |> to_string |> String.upcase
    query  = uri.query || ""
    owner  = self()

    {body, params, req_headers} = body_or_params(body_or_params, query, conn.req_headers)
    state = %{method: method, params: params, req_body: body,
              chunks: nil, ref: make_ref(), owner: owner}

    %Plug.Conn{conn |
      adapter: {__MODULE__, state},
      host: uri.host || conn.host || "www.example.com",
      method: method,
      owner: owner,
      path_info: split_path(uri.path),
      port: uri.port || 80,
      peer: {{127, 0, 0, 1}, 111_317},
      remote_ip: {127, 0, 0, 1},
      req_headers: req_headers,
      request_path: uri.path,
      query_string: query,
      params: params || %Plug.Conn.Unfetched{aspect: :params},
      scheme: (uri.scheme || "http") |> String.downcase |> String.to_atom}
  end

  ## Connection adapter

  def send_resp(%{method: "HEAD"} = state, status, headers, _body) do
    do_send state, status, headers, ""
  end
  def send_resp(state, status, headers, body) do
    do_send state, status, headers, IO.iodata_to_binary(body)
  end

  def send_file(%{method: "HEAD"} = state, status, headers, _path, _offset, _length) do
    do_send state, status, headers, ""
  end
  def send_file(state, status, headers, path, offset, length) do
    %File.Stat{type: :regular, size: size} = File.stat!(path)

    length =
      cond do
        length == :all -> size
        is_integer(length) -> length
      end

    {:ok, data} = File.open!(path, [:read, :binary], fn device ->
      :file.pread(device, offset, length)
    end)

    do_send state, status, headers, data
  end

  def send_chunked(state, _status, _headers),
    do: {:ok, "", %{state | chunks: ""}}
  def chunk(%{method: "HEAD"} = state, _body),
    do: {:ok, "", state}
  def chunk(%{chunks: chunks} = state, body) do
    body = chunks <> IO.iodata_to_binary(body)
    {:ok, body, %{state | chunks: body}}
  end

  defp do_send(%{owner: owner, ref: ref} = state, status, headers, body) do
    send owner, {ref, {status, headers, body}}
    {:ok, body, state}
  end

  def read_req_body(%{req_body: body} = state, opts \\ []) do
    size = min(byte_size(body), Keyword.get(opts, :length, 8_000_000))
    data = :binary.part(body, 0, size)
    rest = :binary.part(body, size, byte_size(body) - size)
    tag =
      case rest do
        "" -> :ok
        _  -> :more
      end
    {tag, data, %{state | req_body: rest}}
  end

  def parse_req_multipart(%{params: multipart} = state, _limit, _callback) do
    {:ok, multipart, %{state | params: nil}}
  end

  ## Private helpers

  defp body_or_params(nil, _query, headers),
    do: {"", nil, headers}

  defp body_or_params(body, _query, headers) when is_binary(body) do
    {body, nil, headers}
  end

  defp body_or_params(params, query, headers) when is_list(params) do
    body_or_params(Enum.into(params, %{}), query, headers)
  end

  defp body_or_params(params, query, headers) when is_map(params) do
    content_type = List.keyfind(headers, "content-type", 0, {"content-type", "multipart/mixed; charset: utf-8"})
    headers = List.keystore(headers, "content-type", 0, content_type)
    params = Map.merge(Plug.Conn.Query.decode(query), stringify_params(params))
    {"", params, headers}
  end

  defp stringify_params([{_, _}|_] = params),
    do: Enum.into(params, %{}, &stringify_kv/1)
  defp stringify_params([_|_] = params),
    do: Enum.map(params, &stringify_params/1)
  defp stringify_params(%{__struct__: mod} = struct) when is_atom(mod),
    do: struct
  defp stringify_params(%{} = params),
    do: Enum.into(params, %{}, &stringify_kv/1)
  defp stringify_params(other),
    do: other

  defp stringify_kv({k, v}),
    do: {to_string(k), stringify_params(v)}

  defp split_path(path) do
    segments = :binary.split(path, "/", [:global])
    for segment <- segments, segment != "", do: segment
  end

  @already_sent {:plug_conn, :sent}

  defp maybe_flush() do
    receive do
      @already_sent -> :ok
    after
      0 -> :ok
    end
  end
end
