defmodule Plug.Adapters.Cowboy do
  @moduledoc """
  Adapter interface to the Cowboy webserver.

  ## Options

  * `:ip` - the ip to bind the server to.
    Must be a tuple in the format `{x, y, z, w}`.

  * `:port` - the port to run the server.
    Defaults to 4000 (http) and 4040 (https).

  * `:acceptors` - the number of acceptors for the listener.
    Defaults to 100.

  * `:max_connections` - max number of connections supported.
    Defaults to `16_384`.

  * `:dispatch` - manually configure Cowboy's dispatch.
    If this option is used, the given plug won't be initialized
    nor dispatched to (and doing so becomes the user's responsibility).

  * `:ref` - the reference name to be used.
    Defaults to `plug.HTTP` (http) and `plug.HTTPS` (https).
    This is the value that needs to be given on shutdown.

  * `:compress` - Cowboy will attempt to compress the response body.
    Defaults to false.

  * `:timeout` - Time in ms with no requests before Cowboy closes the connection.
    Defaults to 5000ms.

  * `:protocol_options` - Specifies remaining protocol options,
    see [Cowboy protocol docs](http://ninenines.eu/docs/en/cowboy/1.0/manual/cowboy_protocol/).

  All other options are given to the underlying transport.
  """

  # Made public with @doc false for testing.
  @doc false
  def args(scheme, plug, opts, cowboy_options) do
    {cowboy_options, non_keyword_options} =
      Enum.partition(cowboy_options, &is_tuple(&1) and tuple_size(&1) == 2)

    cowboy_options
    |> Keyword.put_new(:max_connections, 16_384)
    |> Keyword.put_new(:ref, build_ref(plug, scheme))
    |> Keyword.put_new(:dispatch, cowboy_options[:dispatch] || dispatch_for(plug, opts))
    |> normalize_cowboy_options(scheme)
    |> to_args(non_keyword_options)
  end

  @doc """
  Run cowboy under http.

  ## Example

      # Starts a new interface
      Plug.Adapters.Cowboy.http MyPlug, [], port: 80

      # The interface above can be shutdown with
      Plug.Adapters.Cowboy.shutdown MyPlug.HTTP

  """
  @spec http(module(), Keyword.t, Keyword.t) ::
        {:ok, pid} | {:error, :eaddrinuse} | {:error, term}
  def http(plug, opts, cowboy_options \\ []) do
    run(:http, plug, opts, cowboy_options)
  end

  @doc """
  Run cowboy under https.

  Besides the options described in the module documentation,
  this module also accepts all options defined in [the `ssl`
  erlang module] (http://www.erlang.org/doc/man/ssl.html),
  like keyfile, certfile, cacertfile, dhfile and others.

  The certificate files can be given as a relative path.
  For such, the `:otp_app` option must also be given and
  certificates will be looked from the priv directory of
  the given application.

  ## Example

      # Starts a new interface
      Plug.Adapters.Cowboy.https MyPlug, [],
        port: 443,
        password: "SECRET",
        otp_app: :my_app,
        keyfile: "priv/ssl/key.pem",
        certfile: "priv/ssl/cert.pem",
        dhfile: "priv/ssl/dhparam.pem"

      # The interface above can be shutdown with
      Plug.Adapters.Cowboy.shutdown MyPlug.HTTPS

  """
  @spec https(module(), Keyword.t, Keyword.t) ::
        {:ok, pid} | {:error, :eaddrinuse} | {:error, term}
  def https(plug, opts, cowboy_options \\ []) do
    Application.ensure_all_started(:ssl)
    run(:https, plug, opts, cowboy_options)
  end

  @doc """
  Shutdowns the given reference.
  """
  def shutdown(ref) do
    :cowboy.stop_listener(ref)
  end

  @doc """
  Returns a child spec to be supervised by your application.

  ## Example

  Presuming your Plug module is named `MyRouter` you can add it to your
  supervision tree like so using this function:

      defmodule MyApp do
        use Application

        def start(_type, _args) do
          import Supervisor.Spec

          children = [
            Plug.Adapters.Cowboy.child_spec(:http, MyRouter, [], [port: 4001])
          ]

          opts = [strategy: :one_for_one, name: MyApp.Supervisor]
          Supervisor.start_link(children, opts)
        end
      end
  """
  def child_spec(scheme, plug, opts, cowboy_options \\ []) do
    [ref, nb_acceptors, trans_opts, proto_opts] = args(scheme, plug, opts, cowboy_options)
    ranch_module = case scheme do
      :http  -> :ranch_tcp
      :https -> :ranch_ssl
    end
    :ranch.child_spec(ref, nb_acceptors, ranch_module, trans_opts, :cowboy_protocol, proto_opts)
  end

  ## Helpers

  @protocol_options [:timeout, :compress]

  defp run(scheme, plug, opts, cowboy_options) do
    case Application.ensure_all_started(:cowboy) do
      {:ok, _} ->
        :ok
      {:error, {:cowboy, _}} ->
        raise "could not start the cowboy application. Please ensure it is listed " <>
              "as a dependency both in deps and application in your mix.exs"
    end
    apply(:cowboy, :"start_#{scheme}", args(scheme, plug, opts, cowboy_options))
  end

  defp normalize_cowboy_options(cowboy_options, :http) do
    Keyword.put_new cowboy_options, :port, 4000
  end

  defp normalize_cowboy_options(cowboy_options, :https) do
    assert_ssl_options(cowboy_options)
    cowboy_options = Keyword.put_new cowboy_options, :port, 4040
    cowboy_options = Enum.reduce [:keyfile, :certfile, :cacertfile, :dhfile], cowboy_options, &normalize_ssl_file(&1, &2)
    cowboy_options = Enum.reduce [:password], cowboy_options, &to_char_list(&2, &1)
    cowboy_options
  end

  defp to_args(opts, non_keyword_opts) do
    opts = Keyword.delete(opts, :otp_app)
    {ref, opts} = Keyword.pop(opts, :ref)
    {dispatch, opts} = Keyword.pop(opts, :dispatch)
    {acceptors, opts} = Keyword.pop(opts, :acceptors, 100)
    {protocol_options, opts} = Keyword.pop(opts, :protocol_options, [])

    dispatch = :cowboy_router.compile(dispatch)
    {extra_options, transport_options} = Keyword.split(opts, @protocol_options)
    protocol_options = [env: [dispatch: dispatch]] ++ protocol_options ++ extra_options

    [ref, acceptors, non_keyword_opts ++ transport_options, protocol_options]
  end

  defp build_ref(plug, scheme) do
    Module.concat(plug, scheme |> to_string |> String.upcase)
  end

  defp dispatch_for(plug, opts) do
    opts = plug.init(opts)
    [{:_, [{:_, Plug.Adapters.Cowboy.Handler, {plug, opts}}]}]
  end

  defp normalize_ssl_file(key, cowboy_options) do
    value = cowboy_options[key]

    cond do
      is_nil(value) ->
        cowboy_options
      Path.type(value) == :absolute ->
        put_ssl_file cowboy_options, key, value
      true ->
        put_ssl_file cowboy_options, key, Path.expand(value, otp_app(cowboy_options))
    end
  end

  defp assert_ssl_options(cowboy_options) do
    unless Keyword.has_key?(cowboy_options, :key) or
           Keyword.has_key?(cowboy_options, :keyfile) do
      fail "missing option :key/:keyfile"
    end
    unless Keyword.has_key?(cowboy_options, :cert) or
           Keyword.has_key?(cowboy_options, :certfile) do
      fail "missing option :cert/:certfile"
    end
  end

  defp put_ssl_file(cowboy_options, key, value) do
    value = to_char_list(value)
    unless File.exists?(value) do
      fail "the file #{value} required by SSL's #{inspect key} either does not exist, or the application does not have permission to access it"
    end
    Keyword.put(cowboy_options, key, value)
  end

  defp otp_app(cowboy_options) do
    if app = cowboy_options[:otp_app] do
      Application.app_dir(app)
    else
      fail "to use a relative certificate with https, the :otp_app " <>
           "option needs to be given to the adapter"
    end
  end

  defp to_char_list(cowboy_options, key) do
    if value = cowboy_options[key] do
      Keyword.put cowboy_options, key, to_char_list(value)
    else
      cowboy_options
    end
  end

  defp fail(message) do
    raise ArgumentError, message: "could not start Cowboy adapter, " <> message
  end
end
