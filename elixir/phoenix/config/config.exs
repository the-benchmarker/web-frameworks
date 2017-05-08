# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :my_phoenix,
  ecto_repos: [MyPhoenix.Repo]

# Configures the endpoint
config :my_phoenix, MyPhoenix.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "eJQGBAkhoqat49OeguToy3eM72UD6JYb0rsS6koTzrLzl37AapXPW2Uqffa2yc8W",
  render_errors: [view: MyPhoenix.ErrorView, accepts: ~w(json)],
  pubsub: [name: MyPhoenix.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
