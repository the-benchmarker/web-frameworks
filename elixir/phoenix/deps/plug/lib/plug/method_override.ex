defmodule Plug.MethodOverride do
  @moduledoc """
  This plug overrides the request's `POST` method with the method defined in
  the `_method` request parameter.

  The `POST` method can be overridden only by these HTTP methods:

    * `PUT`
    * `PATCH`
    * `DELETE`

  This plug expects the body parameters to be already parsed and
  fetched. Those can be fetched with `Plug.Parsers`.

  This plug doesn't accept any options.

  ## Examples

      Plug.MethodOverride.call(conn, [])
  """

  @behaviour Plug

  @allowed_methods ~w(DELETE PUT PATCH)

  def init([]), do: []

  def call(%Plug.Conn{method: "POST", body_params: body_params} = conn, []),
    do: override_method(conn, body_params)
  def call(%Plug.Conn{} = conn, []),
    do: conn

  defp override_method(conn, %Plug.Conn.Unfetched{}) do
    # Just skip it because maybe it is a content-type that
    # we could not parse as parameters (for example, text/gps)
    conn
  end

  defp override_method(conn, body_params) do
    method = String.upcase(body_params["_method"] || "")

    if method in @allowed_methods do
      %{conn | method: method}
    else
      conn
    end
  end
end
