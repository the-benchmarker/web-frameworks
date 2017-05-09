defmodule Plug.Router do
  @moduledoc ~S"""
  A DSL to define a routing algorithm that works with Plug.

  It provides a set of macros to generate routes. For example:

      defmodule AppRouter do
        use Plug.Router

        plug :match
        plug :dispatch

        get "/hello" do
          send_resp(conn, 200, "world")
        end

        match _ do
          send_resp(conn, 404, "oops")
        end
      end

  Each route needs to return a connection, as per the Plug spec.
  A catch-all `match` is recommended to be defined as in the example
  above, otherwise routing fails with a function clause error.

  The router is itself a plug, which means it can be invoked as:

      AppRouter.call(conn, AppRouter.init([]))

  Notice the router contains a plug pipeline and by default it requires
  two plugs: `match` and `dispatch`. `match` is responsible for
  finding a matching route which is then forwarded to `dispatch`.
  This means users can easily hook into the router mechanism and add
  behaviour before match, before dispatch or after both.

  To specify private options on `match` that can be used by plugs
  before `dispatch` pass an option with key `:private` containing a map.
  Example:

      get "/hello", private: %{an_option: :a_value} do
        send_resp(conn, 200, "world")
      end

  These options are assigned to `:private` in the call's `Plug.Conn`.

  ## Routes

      get "/hello" do
        send_resp(conn, 200, "world")
      end

  In the example above, a request will only match if it is a `GET` request and
  the route is "/hello". The supported HTTP methods are `get`, `post`, `put`,
  `patch`, `delete` and `options`.

  A route can also specify parameters which will then be
  available in the function body:

      get "/hello/:name" do
        send_resp(conn, 200, "hello #{name}")
      end

  The `:name` parameter will also be available in the function body as
  `conn.params["name"]` and `conn.path_params["name"]`.

  Routes allow for globbing which will match the remaining parts
  of a route and can be available as a parameter in the function
  body. Also note that a glob can't be followed by other segments:

      get "/hello/*_rest" do
        send_resp(conn, 200, "matches all routes starting with /hello")
      end

      get "/hello/*glob" do
        send_resp(conn, 200, "route after /hello: #{inspect glob}")
      end

  Finally, a general `match` function is also supported:

      match "/hello" do
        send_resp(conn, 200, "world")
      end

  A `match` will match any route regardless of the HTTP method.
  Check `match/3` for more information on how route compilation
  works and a list of supported options.

  ## Parameter Parsing

  Handling request data can be done through the
  [`Plug.Parsers`](https://hexdocs.pm/plug/Plug.Parsers.html#content) plug. It
  provides support for parsing URL-encoded, form-data, and JSON data as well as
  providing a behaviour that others parsers can adopt.

  Here is an example of `Plug.Parsers` can be used in a `Plug.Router` router to
  parse the JSON-encoded body of a POST request:

      defmodule AppRouter do
        use Plug.Router

        plug :match
        plug Plug.Parsers, parsers: [:json],
                           pass:  ["application/json"],
                           json_decoder: Poison
        plug :dispatch

        post "/hello" do
          IO.inspect conn.body_params # Prints JSON POST body
          send_resp(conn, 200, "Success!")
        end
      end

  It is important that `Plug.Parsers` is placed before the `:dispatch` plug in
  the pipeline, otherwise the matched clause route will not receive the parsed
  body in its `Plug.Conn` argument when dispatched.

  `Plug.Parsers` can also be plugged between `:match` and `:dispatch` (like in
  the example above): this means that `Plug.Parsers` will run only if there is a
  matching route. This can be useful to perform actions such as authentication
  *before* parsing the body, which should only be parsed if a route matches
  afterwards.

  ## Error handling

  In case something goes wrong in a request, the router by default
  will crash, without returning any response to the client. This
  behaviour can be configured in two ways, by using two different
  modules:

  * `Plug.ErrorHandler` - allows the developer to customize exactly
    which page is sent to the client via the `handle_errors/2` function;

  * `Plug.Debugger` - automatically shows debugging and request information
    about the failure. This module is recommended to be used only in a
    development environment.

  Here is an example of how both modules could be used in an application:

      defmodule AppRouter do
        use Plug.Router

        if Mix.env == :dev do
          use Plug.Debugger
        end

        use Plug.ErrorHandler

        plug :match
        plug :dispatch

        get "/hello" do
          send_resp(conn, 200, "world")
        end

        defp handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
          send_resp(conn, conn.status, "Something went wrong")
        end
      end

  ## Routes compilation

  All routes are compiled to a match function that receives
  three arguments: the method, the request path split on `/`
  and the connection. Consider this example:

      match "/foo/bar", via: :get do
        send_resp(conn, 200, "hello world")
      end

  It is compiled to:

      defp match("GET", ["foo", "bar"], conn) do
        send_resp(conn, 200, "hello world")
      end

  This opens up a few possibilities. First, guards can be given
  to `match`:

      match "/foo/bar/:baz" when size(baz) <= 3, via: :get do
        send_resp(conn, 200, "hello world")
      end

  Second, a list of split paths (which is the compiled result) is
  also allowed:

      match ["foo", "bar", baz], via: :get do
        send_resp(conn, 200, "hello world")
      end

  After a match is found, the block given as `do/end` is stored
  as a function in the connection. This function is then retrieved
  and invoked in the `dispatch` plug.

  ## Options

  When used, the following options are accepted by `Plug.Router`:

    * `:log_on_halt` - accepts the level to log whenever the request is halted
  """

  @doc false
  defmacro __using__(opts) do
    quote location: :keep do
      import Plug.Router
      @before_compile Plug.Router

      use Plug.Builder, unquote(opts)

      @doc false
      def match(conn, _opts) do
        do_match(conn, conn.method, Enum.map(conn.path_info, &URI.decode/1), conn.host)
      end

      @doc false
      def dispatch(%Plug.Conn{assigns: assigns} = conn, _opts) do
        Map.get(conn.private, :plug_route).(conn)
      end

      defoverridable [match: 2, dispatch: 2]
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      import Plug.Router, only: []
    end
  end

  ## Match

  @doc """
  Main API to define routes.

  It accepts an expression representing the path and many options
  allowing the match to be configured.

  The route can dispatch either to a function body or a Plug module.

  ## Examples

      match "/foo/bar", via: :get do
        send_resp(conn, 200, "hello world")
      end

      match "/baz", to: MyPlug, init_opts: [an_option: :a_value]

  ## Options

  `match/3` and the other route macros accept the following options:

    * `:host` - the host which the route should match. Defaults to `nil`,
      meaning no host match, but can be a string like "example.com" or a
      string ending with ".", like "subdomain." for a subdomain match.

    * `:private` - assigns values to `conn.private` for use in the match

    * `:assigns` - assigns values to `conn.assigns` for use in the match

    * `:via` - matches the route against some specific HTTP method (specified as
      an atom, like `:get` or `:put`.

    * `:do` - contains the implementation to be invoked in case
      the route matches.

    * `:to` - a Plug that will be called in case the route matches.

    * `:init_opts` - the options for the target Plug given by `:to`.

  A route should specify only one of `:do` or `:to` options.
  """
  defmacro match(path, options, contents \\ []) do
    compile(nil, path, options, contents)
  end

  @doc """
  Dispatches to the path only if the request is a GET request.
  See `match/3` for more examples.
  """
  defmacro get(path, options, contents \\ []) do
    compile(:get, path, options, contents)
  end

  @doc """
  Dispatches to the path only if the request is a POST request.
  See `match/3` for more examples.
  """
  defmacro post(path, options, contents \\ []) do
    compile(:post, path, options, contents)
  end

  @doc """
  Dispatches to the path only if the request is a PUT request.
  See `match/3` for more examples.
  """
  defmacro put(path, options, contents \\ []) do
    compile(:put, path, options, contents)
  end

  @doc """
  Dispatches to the path only if the request is a PATCH request.
  See `match/3` for more examples.
  """
  defmacro patch(path, options, contents \\ []) do
    compile(:patch, path, options, contents)
  end

  @doc """
  Dispatches to the path only if the request is a DELETE request.
  See `match/3` for more examples.
  """
  defmacro delete(path, options, contents \\ []) do
    compile(:delete, path, options, contents)
  end

  @doc """
  Dispatches to the path only if the request is an OPTIONS request.
  See `match/3` for more examples.
  """
  defmacro options(path, options, contents \\ []) do
    compile(:options, path, options, contents)
  end

  @doc """
  Forwards requests to another Plug. The `path_info` of the forwarded
  connection will exclude the portion of the path specified in the
  call to `forward`. If the path contains any parameters, those will
  be available in the target Plug in `conn.params` and `conn.path_params`.

  ## Options

  `forward` accepts the following options:

    * `:to` - a Plug the requests will be forwarded to.
    * `:init_opts` - the options for the target Plug.
    * `:host` - a string representing the host or subdomain, exactly like in
      `match/3`.
    * `:private` - values for `conn.private`, exactly like in `match/3`.
    * `:assigns` - values for `conn.assigns`, exactly like in `match/3`.

  If `:init_opts` is undefined, then all remaining options are passed
  to the target plug.

  ## Examples

      forward "/users", to: UserRouter

  Assuming the above code, a request to `/users/sign_in` will be forwarded to
  the `UserRouter` plug, which will receive what it will see as a request to
  `/sign_in`.

      forward "/foo/:bar/qux", to: FooPlug

  Here, a request to `/foo/BAZ/qux` will be forwarded to the `FooPlug`
  plug, which will receive what it will see as a request to `/qux`,
  and `conn.params["bar"]` will be set to `"BAZ"`.

  Some other examples:

      forward "/foo/bar", to: :foo_bar_plug, host: "foobar."
      forward "/api", to: ApiRouter, plug_specific_option: true
      forward "/baz", to: BazPlug, init_opts: [plug_specific_option: true]
  """
  defmacro forward(path, options) when is_binary(path) do
    quote bind_quoted: [path: path, options: options] do
      # TODO: Require use of `:init_opts` for passing Plug options in 2.0
      {target, options}       = Keyword.pop(options, :to)
      {options, plug_options} = Keyword.split(options, [:host, :private, :assigns])
      plug_options = Keyword.get(plug_options, :init_opts, plug_options)

      if is_nil(target) or !is_atom(target) do
        raise ArgumentError, message: "expected :to to be an alias or an atom"
      end

      @plug_forward_target target
      @plug_forward_opts   target.init(plug_options)

      # Delegate the matching to the match/3 macro along with the options
      # specified by Keyword.split/2.
      match path <> "/*glob", options do
        Plug.Router.Utils.forward(
          var!(conn),
          var!(glob),
          @plug_forward_target,
          @plug_forward_opts
        )
      end
    end
  end

  ## Match Helpers

  @doc false
  def __route__(method, path, guards, options) do
    {method, guards} = build_methods(List.wrap(method || options[:via]), guards)
    {vars, match}    = Plug.Router.Utils.build_path_match(path)
    params_match = Plug.Router.Utils.build_path_params_match(vars)
    private    = extract_merger(options, :private)
    assigns    = extract_merger(options, :assigns)
    host_match = Plug.Router.Utils.build_host_match(options[:host])
    {quote(do: conn), method, match, params_match, host_match, guards, private, assigns}
  end

  # Entry point for both forward and match that is actually
  # responsible to compile the route.
  defp compile(method, expr, options, contents) do
    {body, options} =
      cond do
        Keyword.has_key?(contents, :do) ->
          {contents[:do], options}
        Keyword.has_key?(options, :do) ->
          Keyword.pop(options, :do)
        options[:to] ->
          {to, options} = Keyword.pop(options, :to)
          {init_opts, options} = Keyword.pop(options, :init_opts, [])
          body =
            quote do
              @plug_router_to.call(var!(conn), @plug_router_init)
            end
          options =
            quote do
              @plug_router_to unquote(to)
              @plug_router_init unquote(init_opts)
              unquote(options)
            end
          {body, options}
        true ->
          raise ArgumentError, message: "expected one of :to or :do to be given as option"
      end

    {path, guards} = extract_path_and_guards(expr)

    quote bind_quoted: [method: method,
                        path: path,
                        options: options,
                        guards: Macro.escape(guards, unquote: true),
                        body: Macro.escape(body, unquote: true)] do
      route = Plug.Router.__route__(method, path, guards, options)
      {conn, method, match, params, host, guards, private, assigns} = route

      defp do_match(unquote(conn), unquote(method), unquote(match), unquote(host)) when unquote(guards) do
        unquote(private)
        unquote(assigns)

        merge_params = fn
          %Plug.Conn.Unfetched{} -> unquote({:%{}, [], params})
          fetched -> Map.merge(fetched, unquote({:%{}, [], params}))
        end
        conn = update_in unquote(conn).params, merge_params
        conn = update_in conn.path_params, merge_params

        Plug.Conn.put_private(conn, :plug_route, fn var!(conn) -> unquote(body) end)
      end
    end
  end

  defp extract_merger(options, key) when is_list(options) do
    if option = Keyword.get(options, key) do
      quote do
        conn = update_in conn.unquote(key), &Map.merge(&1, unquote(Macro.escape(option)))
      end
    end
  end

  # Convert the verbs given with `:via` into a variable and guard set that can
  # be added to the dispatch clause.
  defp build_methods([], guards) do
    {quote(do: _), guards}
  end

  defp build_methods([method], guards) do
    {Plug.Router.Utils.normalize_method(method), guards}
  end

  defp build_methods(methods, guards) do
    methods = Enum.map methods, &Plug.Router.Utils.normalize_method(&1)
    var     = quote do: method
    guards  = join_guards(quote(do: unquote(var) in unquote(methods)), guards)
    {var, guards}
  end

  defp join_guards(fst, true), do: fst
  defp join_guards(fst, snd),  do: (quote do: unquote(fst) and unquote(snd))

  # Extract the path and guards from the path.
  defp extract_path_and_guards({:when, _, [path, guards]}), do: {extract_path(path), guards}
  defp extract_path_and_guards(path), do: {extract_path(path), true}

  defp extract_path({:_, _, var}) when is_atom(var), do: "/*_path"
  defp extract_path(path), do: path
end
