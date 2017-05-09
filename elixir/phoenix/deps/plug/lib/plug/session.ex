defmodule Plug.Session do
  @moduledoc """
  A plug to handle session cookies and session stores.

  The session is accessed via functions on `Plug.Conn`. Cookies and
  session have to be fetched with `Plug.Conn.fetch_session/1` before the
  session can be accessed.

  Consider using `Plug.CSRFProtection` when using `Plug.Session`.

  ## Session stores

  See `Plug.Session.Store` for the specification session stores are required to
  implement.

  Plug ships with the following session stores:

    * `Plug.Session.ETS`
    * `Plug.Session.COOKIE`

  ## Options

    * `:store` - session store module (required);
    * `:key` - session cookie key (required);
    * `:domain` - see `Plug.Conn.put_resp_cookie/4`;
    * `:max_age` - see `Plug.Conn.put_resp_cookie/4`;
    * `:path` - see `Plug.Conn.put_resp_cookie/4`;
    * `:secure` - see `Plug.Conn.put_resp_cookie/4`;
    * `:http_only` - see `Plug.Conn.put_resp_cookie/4`;
    * `:extra` - see `Plug.Conn.put_resp_cookie/4`;

  Additional options can be given to the session store, see the store's
  documentation for the options it accepts.

  ## Examples

      plug Plug.Session, store: :ets, key: "_my_app_session", table: :session
  """

  alias Plug.Conn
  @behaviour Plug

  @cookie_opts [:domain, :max_age, :path, :secure, :http_only, :extra]

  def init(opts) do
    store        = convert_store(Keyword.fetch!(opts, :store))
    key          = Keyword.fetch!(opts, :key)
    cookie_opts  = Keyword.take(opts, @cookie_opts)
    store_opts   = Keyword.drop(opts, [:store, :key] ++ @cookie_opts)
    store_config = store.init(store_opts)

    %{store: store,
      store_config: store_config,
      key: key,
      cookie_opts: cookie_opts}
  end

  def call(conn, config) do
    Conn.put_private(conn, :plug_session_fetch, fetch_session(config))
  end

  defp convert_store(store) do
    case Atom.to_string(store) do
      "Elixir." <> _ -> store
      reference      -> Module.concat(Plug.Session, String.upcase(reference))
    end
  end

  defp fetch_session(config) do
    %{store: store, store_config: store_config, key: key} = config

    fn conn ->
      {sid, session} =
        if cookie = conn.cookies[key] do
          store.get(conn, cookie, store_config)
        else
          {nil, %{}}
        end

      session = Map.merge(session, Map.get(conn.private, :plug_session, %{}))

      conn
      |> Conn.put_private(:plug_session, session)
      |> Conn.put_private(:plug_session_fetch, :done)
      |> Conn.register_before_send(before_send(sid, config))
    end
  end

  defp before_send(sid, config) do
    fn conn ->
      case Map.get(conn.private, :plug_session_info) do
        :write ->
          value = put_session(sid, conn, config)
          put_cookie(value, conn, config)
        :drop ->
          drop_session(sid, conn, config)
        :renew ->
          renew_session(sid, conn, config)
        :ignore ->
          conn
        nil ->
          conn
      end
    end
  end

  defp drop_session(sid, conn, config) do
    if sid do
      delete_session(sid, conn, config)
      delete_cookie(conn, config)
    else
      conn
    end
  end

  defp renew_session(sid, conn, config) do
    if sid, do: delete_session(sid, conn, config)
    value = put_session(nil, conn, config)
    put_cookie(value, conn, config)
  end

  defp put_session(sid, conn, %{store: store, store_config: store_config}),
    do: store.put(conn, sid, conn.private[:plug_session], store_config)

  defp delete_session(sid, conn, %{store: store, store_config: store_config}),
    do: store.delete(conn, sid, store_config)

  defp put_cookie(value, conn, %{cookie_opts: cookie_opts, key: key}),
    do: Conn.put_resp_cookie(conn, key, value, cookie_opts)

  defp delete_cookie(conn, %{cookie_opts: cookie_opts, key: key}),
    do: Conn.delete_resp_cookie(conn, key, cookie_opts)
end
