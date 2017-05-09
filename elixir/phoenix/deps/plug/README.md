# Plug

[![Build Status](https://travis-ci.org/elixir-lang/plug.svg?branch=master)](https://travis-ci.org/elixir-lang/plug)
[![Inline docs](http://inch-ci.org/github/elixir-lang/plug.svg?branch=master)](http://inch-ci.org/github/elixir-lang/plug)

Plug is:

1. A specification for composable modules between web applications
2. Connection adapters for different web servers in the Erlang VM

[Documentation for Plug is available online](http://hexdocs.pm/plug/).

## Hello world

```elixir
defmodule MyPlug do
  import Plug.Conn

  def init(options) do
    # initialize options

    options
  end

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello world")
  end
end
```

The snippet above shows a very simple example on how to use Plug. Save that snippet to a file and run it inside the plug application with:

    $ iex -S mix
    iex> c "path/to/file.ex"
    [MyPlug]
    iex> {:ok, _} = Plug.Adapters.Cowboy.http MyPlug, []
    {:ok, #PID<...>}

Access "http://localhost:4000/" and we are done! For now, we have directly started the server in our terminal but, for production deployments, you likely want to start it in your supervision tree. See the "Supervised handlers" section below.

## Installation

You can use plug in your projects in two steps:

1. Add plug and your webserver of choice (currently cowboy) to your `mix.exs` dependencies:

    ```elixir
    def deps do
      [{:cowboy, "~> 1.0.0"},
       {:plug, "~> 1.0"}]
    end
    ```

2. List both `:cowboy` and `:plug` as your application dependencies:

    ```elixir
    def application do
      [applications: [:cowboy, :plug]]
    end
    ```

## The Plug.Conn

In the hello world example, we defined our first plug. What is a plug after all?

A plug takes two shapes. A function plug receives a connection and a set of options as arguments and returns the connection:

```elixir
def hello_world_plug(conn, _opts) do
  conn
  |> put_resp_content_type("text/plain")
  |> send_resp(200, "Hello world")
end
```

A module plug implements an `init/1` function to initialize the options and a `call/2` function which receives the connection and initialized options and returns the connection:

```elixir
defmodule MyPlug do
  def init([]), do: false
  def call(conn, _opts), do: conn
end
```

As per the specification above, a connection is represented by the `Plug.Conn` struct:

```elixir
%Plug.Conn{host: "www.example.com",
           path_info: ["bar", "baz"],
           ...}
```

Data can be read directly from the connection and also pattern matched on. Manipulating the connection often happens with the use of the functions defined in the `Plug.Conn` module. In our example, both `put_resp_content_type/2` and `send_resp/3` are defined in `Plug.Conn`.

Remember that, as everything else in Elixir, **a connection is immutable**, so every manipulation returns a new copy of the connection:

```elixir
conn = put_resp_content_type(conn, "text/plain")
conn = send_resp(conn, 200, "ok")
conn
```

Finally, keep in mind that a connection is a **direct interface to the underlying web server**. When you call `send_resp/3` above, it will immediately send the given status and body back to the client. This makes features like streaming a breeze to work with.

## The Plug Router

In practice, developers rarely write their own plugs. For example, Plug ships with a router that allows developers to quickly match on incoming requests and perform some action:

```elixir
defmodule MyRouter do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/hello" do
    send_resp(conn, 200, "world")
  end

  forward "/users", to: UsersRouter

  match _ do
    send_resp(conn, 404, "oops")
  end
end
```

The router is a plug and, not only that, it contains its own plug pipeline too. The example above says that when the router is invoked, it will invoke the `:match` plug, represented by a local `match/2` function, and then call the `:dispatch` plug which will execute the matched code.

Plug ships with many plugs that you can add to the router plug pipeline, allowing you to plug something before a route matches or before a route is dispatched to. For example, if you want to add logging to the router, just do:

```elixir
plug Plug.Logger
plug :match
plug :dispatch
```

Note `Plug.Router` compiles all of your routes into a single function and relies on the Erlang VM to optimize the underlying routes into a tree lookup, instead of a linear lookup that would instead match route-per-route. This means route lookups are extremely fast in Plug!

This also means that a catch all `match` is recommended to be defined, as in the example above, otherwise routing fails with a function clause error (as it would in any regular Elixir function).

Each route needs to return the connection as per the Plug specification. See `Plug.Router` docs for more information.

## Supervised handlers

On a production system, you likely want to start your Plug application under your application's supervision tree. Plug provides the `child_spec/3` function to do just that. Start a new Elixir project with the `--sup` flag:

```elixir
$ mix new my_app --sup
```

and then update `lib/my_app.ex` as follows:

```elixir
defmodule MyApp do
  use Application

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      # Define workers and child supervisors to be supervised
      Plug.Adapters.Cowboy.child_spec(:http, MyRouter, [], [port: 4001])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

## Testing plugs

Plug ships with a `Plug.Test` module that makes testing your plugs easy. Here is how we can test the router from above (or any other plug):

```elixir
defmodule MyPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  @opts AppRouter.init([])

  test "returns hello world" do
    # Create a test connection
    conn = conn(:get, "/hello")

    # Invoke the plug
    conn = AppRouter.call(conn, @opts)

    # Assert the response and status
    assert conn.state == :sent
    assert conn.status == 200
    assert conn.resp_body == "world"
  end
end
```

### Available Plugs

This project aims to ship with different plugs that can be re-used across applications:

  * `Plug.CSRFProtection` - adds Cross-Site Request Forgery protection to your application. Typically required if you are using `Plug.Session`;
  * `Plug.Head` - converts HEAD requests to GET requests;
  * `Plug.Logger` - logs requests;
  * `Plug.MethodOverride` - overrides a request method with one specified in headers;
  * `Plug.Parsers` - responsible for parsing the request body given its content-type;
  * `Plug.RequestId` - sets up a request ID to be used in logs;
  * `Plug.Session` - handles session management and storage;
  * `Plug.SSL` - enforce requests through SSL;
  * `Plug.Static` - serves static files;

You can go into more details about each of them [in our docs](http://hexdocs.pm/plug/).

### Helper modules

Modules that can be used after you use `Plug.Router` or `Plug.Builder` to help development:

  * `Plug.Debugger` - shows a helpful debugging page every time there is a failure in a request;
  * `Plug.ErrorHandler` - allows developers to customize error pages in case of crashes instead of sending a blank one;

## Contributing

We welcome everyone to contribute to Plug and help us tackle existing issues!

Use the [issue tracker][issues] for bug reports or feature requests. You may also start a discussion on the [mailing list][ML] or the **[#elixir-lang][IRC]** channel on [Freenode][freenode] IRC. Open a [pull request][pulls] when you are ready to contribute.

When submitting a pull request you should not update the `CHANGELOG.md`.

If you are planning to contribute documentation, [please check our best practices for writing documentation][writing-docs].

Finally, remember all interactions in our official spaces follow our [Code of Conduct][code-of-conduct].

## License

Plug source code is released under Apache 2 License.
Check LICENSE file for more information.

  [issues]: https://github.com/elixir-lang/plug/issues
  [pulls]: https://github.com/elixir-lang/plug/pulls
  [ML]: https://groups.google.com/group/elixir-lang-core
  [code-of-conduct]: https://github.com/elixir-lang/elixir/blob/master/CODE_OF_CONDUCT.md
  [writing-docs]: http://elixir-lang.org/docs/stable/elixir/writing-documentation.html
  [IRC]: https://webchat.freenode.net/?channels=#elixir-lang
  [freenode]: http://www.freenode.net
