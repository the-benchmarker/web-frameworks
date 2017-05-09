defmodule Plug.Parsers.URLENCODED do
  @moduledoc """
  Parses urlencoded request body.
  """

  @behaviour Plug.Parsers
  alias Plug.Conn

  def parse(conn, "application", "x-www-form-urlencoded", _headers, opts) do
    case Conn.read_body(conn, opts) do
      {:ok, body, conn} ->
        Plug.Conn.Utils.validate_utf8!(body, Plug.Parsers.BadEncodingError, "urlencoded body")
        {:ok, Plug.Conn.Query.decode(body), conn}
      {:more, _data, conn} ->
        {:error, :too_large, conn}
      {:error, :timeout} ->
        raise Plug.TimeoutError
      {:error, _} ->
        raise Plug.BadRequestError
    end
  end

  def parse(conn, _type, _subtype, _headers, _opts) do
    {:next, conn}
  end
end
