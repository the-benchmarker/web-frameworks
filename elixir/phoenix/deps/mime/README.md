# MIME

A library that maps mime types to extensions and vice-versa.

## Installation

The package can be installed as:

1. Add mime to your list of dependencies in `mix.exs`:

  ```elixir
  def deps do
    [{:mime, "~> 1.1"}]
  end
  ```

2. If there is an `applications` key in your `mix.exs`, add `:mime` to the list. This step is not necessary if you have `extra_applications` instead.

  ```elixir
  def application do
    [applications: [:mime]]
  end
  ```
  
## Usage

MIME types can be extended in your application `config/config.exs` as follows:

```elixir
config :mime, :types, %{
  "application/vnd.api+json" => ["json-api"]
}
```

And then run `mix deps.clean --build mime` to force mime to be recompiled across all environments.

## License

MIME source code is released under Apache 2 License.

Check LICENSE file for more information.
