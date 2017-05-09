defmodule Plug.Adapters.Translator do
  @moduledoc """
  A translator module shared by adapters that ship with Plug.

  We host all translations in a single module which is added
  to Logger when the :plug application starts.
  """

  ## Entry point

  @doc """
  The `translate/4` function expected by custom Logger translators.
  """
  def translate(min_level, :error, :format,
                {~c"Ranch listener" ++ _, [ref, protocol, pid, reason]}) do
    translate_ranch(min_level, ref, protocol, pid, reason)
  end

  def translate(_min_level, _level, _kind, _data) do
    :none
  end

  ## Ranch/Cowboy

  defp translate_ranch(min_level, _ref, :cowboy_protocol, pid,
                       {reason, {mod, :call, [%Plug.Conn{} = conn, _opts]}}) do

    if non_500_exception?(reason) do
      :skip
    else
      {:ok, [inspect(pid), " running ", inspect(mod), " terminated\n",
             conn_info(min_level, conn) |
             Exception.format(:exit, reason, [])]}
    end
  end

  defp translate_ranch(_min_level, ref, protocol, pid, reason) do
    {:ok, ["Ranch protocol ", inspect(pid), " (", inspect(protocol),
           ") of listener ", inspect(ref), " terminated\n" |
           Exception.format(:exit, reason, [])]}
  end

  defp non_500_exception?({%{__exception__: true} = exception, _}),
    do: Plug.Exception.status(exception) < 500
  defp non_500_exception?(_),
    do: false

  ## Helpers

  defp conn_info(_min_level, conn) do
    [server_info(conn), request_info(conn)]
  end

  defp server_info(%Plug.Conn{host: host, port: port, scheme: scheme}) do
    ["Server: ", host, ":", Integer.to_string(port), ?\s, ?(, Atom.to_string(scheme), ?), ?\n]
  end

  defp request_info(%Plug.Conn{method: method, query_string: query_string} = conn) do
    ["Request: ", method, ?\s, path_to_iodata(conn.request_path, query_string), ?\n]
  end

  defp path_to_iodata(path, ""), do: path
  defp path_to_iodata(path, qs), do: [path, ??, qs]
end
