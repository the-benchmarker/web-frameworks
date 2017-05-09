defmodule Plug.Head do
  @moduledoc """
  A Plug to convert `HEAD` requests to `GET` requests.

  ## Examples

      Plug.Head.call(conn, [])
  """

  @behaviour Plug

  alias Plug.Conn

  def init([]), do: []

  def call(%Conn{method: "HEAD"} = conn, []), do: %{conn | method: "GET"}
  def call(conn, []), do: conn
end
