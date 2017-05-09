defmodule Plug.Adapters.Cowboy.Conn do
  @behaviour Plug.Conn.Adapter
  @moduledoc false

  def conn(req, transport) do
    {path, req} = :cowboy_req.path req
    {host, req} = :cowboy_req.host req
    {port, req} = :cowboy_req.port req
    {meth, req} = :cowboy_req.method req
    {hdrs, req} = :cowboy_req.headers req
    {qs, req}   = :cowboy_req.qs req
    {peer, req} = :cowboy_req.peer req
    {remote_ip, _} = peer

    %Plug.Conn{
      adapter: {__MODULE__, req},
      host: host,
      method: meth,
      owner: self(),
      path_info: split_path(path),
      peer: peer,
      port: port,
      remote_ip: remote_ip,
      query_string: qs,
      req_headers: hdrs,
      request_path: path,
      scheme: scheme(transport)
   }
  end

  def send_resp(req, status, headers, body) do
    status = Integer.to_string(status) <> " " <> Plug.Conn.Status.reason_phrase(status)
    {:ok, req} = :cowboy_req.reply(status, headers, body, req)
    {:ok, nil, req}
  end

  def send_file(req, status, headers, path, offset, length) do
    %File.Stat{type: :regular, size: size} = File.stat!(path)

    length =
      cond do
        length == :all -> size
        is_integer(length) -> length
      end

    body_fun = fn(socket, transport) -> transport.sendfile(socket, path, offset, length) end

    {:ok, req} = :cowboy_req.reply(status, headers, :cowboy_req.set_resp_body_fun(length, body_fun, req))
    {:ok, nil, req}
  end

  def send_chunked(req, status, headers) do
    {:ok, req} = :cowboy_req.chunked_reply(status, headers, req)
    {:ok, nil, req}
  end

  def chunk(req, body) do
    :cowboy_req.chunk(body, req)
  end

  def read_req_body(req, opts \\ []) do
    :cowboy_req.body(req, opts)
  end

  def parse_req_multipart(req, opts, callback) do
    # We need to remove the length from the list
    # otherwise cowboy will attempt to load the
    # whole length at once.
    {limit, opts} = Keyword.pop(opts, :length, 8_000_000)

    # We need to construct the header opts using defaults here,
    # since once opts are passed cowboy defaults are not applied anymore.
    {headers_opts, opts} = Keyword.pop(opts, :headers, [])
    headers_opts = headers_opts ++ [length: 64_000, read_length:  64_000, read_timeout: 5000]

    {:ok, limit, acc, req} = parse_multipart(:cowboy_req.part(req, headers_opts), limit, opts, headers_opts, [], callback)

    params = Enum.reduce(acc, %{}, &Plug.Conn.Query.decode_pair/2)

    if limit > 0 do
      {:ok, params, req}
    else
      {:more, params, req}
    end
  end

  ## Helpers

  defp scheme(:tcp), do: :http
  defp scheme(:ssl), do: :https

  defp split_path(path) do
    segments = :binary.split(path, "/", [:global])
    for segment <- segments, segment != "", do: segment
  end

  ## Multipart

  defp parse_multipart({:ok, headers, req}, limit, opts, headers_opts, acc, callback) when limit >= 0 do
    case callback.(headers) do
      {:binary, name} ->
        {:ok, limit, body, req} =
          parse_multipart_body(:cowboy_req.part_body(req, opts), limit, opts, "")

        Plug.Conn.Utils.validate_utf8!(body, Plug.Parsers.BadEncodingError, "multipart body")
        parse_multipart(:cowboy_req.part(req, headers_opts), limit, opts, headers_opts,
                        [{name, body}|acc], callback)

      {:file, name, path, %Plug.Upload{} = uploaded} ->
        {:ok, file} = File.open(path, [:write, :binary, :delayed_write, :raw])

        {:ok, limit, req} =
          parse_multipart_file(:cowboy_req.part_body(req, opts), limit, opts, file)

        :ok = File.close(file)
        parse_multipart(:cowboy_req.part(req, headers_opts), limit, opts, headers_opts,
                        [{name, uploaded}|acc], callback)

      :skip ->
        parse_multipart(:cowboy_req.part(req, headers_opts), limit, opts, headers_opts,
                        acc, callback)
    end
  end

  defp parse_multipart({:ok, _headers, req}, limit, _opts, _headers_opts, acc, _callback) do
    {:ok, limit, acc, req}
  end

  defp parse_multipart({:done, req}, limit, _opts, _headers_opts, acc, _callback) do
    {:ok, limit, acc, req}
  end

  defp parse_multipart_body({:more, tail, req}, limit, opts, body) when limit >= byte_size(tail) do
    parse_multipart_body(:cowboy_req.part_body(req, opts), limit - byte_size(tail), opts, body <> tail)
  end

  defp parse_multipart_body({:more, tail, req}, limit, _opts, body) do
    {:ok, limit - byte_size(tail), body, req}
  end

  defp parse_multipart_body({:ok, tail, req}, limit, _opts, body) when limit >= byte_size(tail) do
    {:ok, limit - byte_size(tail), body <> tail, req}
  end

  defp parse_multipart_body({:ok, tail, req}, limit, _opts, body) do
    {:ok, limit - byte_size(tail), body, req}
  end

  defp parse_multipart_file({:more, tail, req}, limit, opts, file) when limit >= byte_size(tail) do
    IO.binwrite(file, tail)
    parse_multipart_file(:cowboy_req.part_body(req, opts), limit - byte_size(tail), opts, file)
  end

  defp parse_multipart_file({:more, tail, req}, limit, _opts, _file) do
    {:ok, limit - byte_size(tail), req}
  end

  defp parse_multipart_file({:ok, tail, req}, limit, _opts, file) when limit >= byte_size(tail) do
    IO.binwrite(file, tail)
    {:ok, limit - byte_size(tail), req}
  end

  defp parse_multipart_file({:ok, tail, req}, limit, _opts, _file) do
    {:ok, limit - byte_size(tail), req}
  end
end
