defmodule Plug.Conn.WrapperError do
  @moduledoc """
  Wraps the connection in an error which is meant
  to be handled upper in the stack.

  Used by both `Plug.Debugger` and `Plug.ErrorHandler`.
  """
  defexception [:conn, :kind, :reason, :stack]

  def message(%{kind: kind, reason: reason, stack: stack}) do
    Exception.format_banner(kind, reason, stack)
  end

  @doc """
  Reraises an error or a wrapped one.
  """
  def reraise(_conn, :error, %__MODULE__{stack: stack} = reason) do
    :erlang.raise(:error, reason, stack)
  end
  def reraise(conn, :error, reason) do
    stack   = System.stacktrace
    wrapper = %__MODULE__{conn: conn, kind: :error, reason: reason, stack: stack}
    :erlang.raise(:error, wrapper, stack)
  end
  def reraise(_conn, kind, reason) do
    :erlang.raise(kind, reason, System.stacktrace)
  end
end
