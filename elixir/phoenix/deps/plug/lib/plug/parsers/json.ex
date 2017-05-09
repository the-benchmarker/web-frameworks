defmodule Plug.Parsers.JSON do
  @moduledoc """
  Parses JSON request body.

  JSON arrays are parsed into a `"_json"` key to allow
  proper param merging.

  An empty request body is parsed as an empty map.
  """

  @behaviour Plug.Parsers
  import Plug.Conn

  def parse(conn, "application", subtype, _headers, opts) do
    if subtype == "json" || String.ends_with?(subtype, "+json") do
      decoder = Keyword.get(opts, :json_decoder) ||
                  raise ArgumentError, "JSON parser expects a :json_decoder option"
      conn
      |> read_body(opts)
      |> decode(decoder)
    else
      {:next, conn}
    end
  end

  def parse(conn, _type, _subtype, _headers, _opts) do
    {:next, conn}
  end

  defp decode({:more, _, conn}, _decoder) do
    {:error, :too_large, conn}
  end

  defp decode({:error, :timeout}, _decoder) do
    raise Plug.TimeoutError
  end

  defp decode({:error, _}, _decoder) do
    raise Plug.BadRequestError
  end

  defp decode({:ok, "", conn}, _decoder) do
    {:ok, %{}, conn}
  end

  defp decode({:ok, body, conn}, decoder) do
    case decoder.decode!(body) do
      terms when is_map(terms) ->
        {:ok, terms, conn}
      terms ->
        {:ok, %{"_json" => terms}, conn}
    end
  rescue
    e -> raise Plug.Parsers.ParseError, exception: e
  end
end
