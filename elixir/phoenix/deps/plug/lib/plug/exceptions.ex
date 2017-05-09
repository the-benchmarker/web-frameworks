# This file defines the Plug.Exception protocol and
# the exceptions that implement such protocol.

defprotocol Plug.Exception do
  @moduledoc """
  A protocol that extends exceptions to be status-code aware.

  By default, it looks for an implementation of the protocol,
  otherwise checks if the exception has the `:plug_status` field
  or simply returns 500.
  """

  @fallback_to_any true

  @doc """
  Receives an exception and returns its HTTP status code.
  """
  @spec status(t) :: Plug.Conn.status
  def status(exception)
end

defimpl Plug.Exception, for: Any do
  def status(%{plug_status: status}) when is_integer(status), do: status
  def status(_), do: 500
end

defmodule Plug.BadRequestError do
  @moduledoc """
  The request will not be processed due to a client error.
  """

  defexception message: nil, plug_status: 400
end

defmodule Plug.TimeoutError do
  @moduledoc """
  Timeout while waiting for the request.
  """

  defexception message: nil, plug_status: 408
end
