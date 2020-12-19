defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      # App config
      app: :server,
      version: "0.1.0",

      # Elixir config
      elixir: "~> 1.9",
      compilers: [:phoenix] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Releases
      releases: [
        server: [
          include_executables_for: [:unix]
        ]
      ]
    ]
  end

  def application do
    [
      mod: {Server.Application, []}
    ]
  end

  defp deps do
    [
      {:phoenix, "~> 1.5.0"},
      {:jason, "~> 1.2"},
      {:plug_cowboy, "~> 2.0"}
    ]
  end
end
