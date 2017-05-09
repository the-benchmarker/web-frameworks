defmodule Plug.ErrorHandler do
  @moduledoc """
  A module to be used in your existing plugs in order to provide
  error handling.

      defmodule AppRouter do
        use Plug.Router
        use Plug.ErrorHandler

        plug :match
        plug :dispatch

        get "/hello" do
          send_resp(conn, 200, "world")
        end

        def handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
          send_resp(conn, conn.status, "Something went wrong")
        end
      end

  Once this module is used, a callback named `handle_errors/2` should
  be defined in your plug. This callback should accept a connection and a map
  containing:

    * the exception kind (`:throw`, `:error` or `:exit`),
    * the reason (an exception for errors or a term for others)
    * the stacktrace

  After the callback is invoked, the error is re-raised.

  It is advised to do as little work as possible when handling errors
  and avoid accessing data like parameters and session, as the parsing
  of those is what could have led the error to trigger in the first place.

  Also notice that these pages are going to be shown in production. If
  you are looking for error handling to help during development, consider
  using `Plug.Debugger`.

  **Note:** If this module is used with `Plug.Debugger`, it must be used
  after `Plug.Debugger`.
  """
  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @before_compile Plug.ErrorHandler

      @doc false
      def handle_errors(conn, assigns) do
        Plug.Conn.send_resp(conn, conn.status, "Something went wrong")
      end

      defoverridable [handle_errors: 2]
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote location: :keep do
      defoverridable [call: 2]

      def call(conn, opts) do
        try do
          super(conn, opts)
        catch
          kind, reason ->
            Plug.ErrorHandler.__catch__(conn, kind, reason, &handle_errors/2)
        end
      end
    end
  end

  @already_sent {:plug_conn, :sent}

  @doc false
  def __catch__(_conn, :error, %Plug.Conn.WrapperError{} = wrapper, handle_errors) do
    %{conn: conn, kind: kind, reason: reason, stack: stack} = wrapper
    __catch__(conn, kind, wrapper, reason, stack, handle_errors)
  end

  def __catch__(conn, kind, reason, handle_errors) do
    __catch__(conn, kind, reason, reason, System.stacktrace, handle_errors)
  end

  defp __catch__(conn, kind, reason, wrapped_reason, stack, handle_errors) do
    receive do
      @already_sent ->
        send self(), @already_sent
    after
      0 ->
        normalized_reason = Exception.normalize(kind, wrapped_reason, stack)
        conn
        |> Plug.Conn.put_status(status(kind, normalized_reason))
        |> handle_errors.(%{kind: kind, reason: normalized_reason, stack: stack})
    end

    :erlang.raise(kind, reason, stack)
  end

  defp status(:error, error),  do: Plug.Exception.status(error)
  defp status(:throw, _throw), do: 500
  defp status(:exit, _exit),   do: 500
end
