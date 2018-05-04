defmodule MyPhoenix.Mixfile do
  use Mix.Project

  def project do
    [app: :my_phoenix,
     version: "0.0.1",
     elixir: "~> 1.6.4",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [mod: {MyPhoenix, []},
     applications: [:phoenix, :cowboy, :logger, :gettext]]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_),     do: ["lib", "web"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [{:phoenix, "~> 1.3.0"},
     {:gettext, "~> 0.11"},
     {:cowboy, "~> 1.0"},
     {:distillery, "~> 1.0"}]
  end
end
