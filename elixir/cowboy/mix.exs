defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      # App config
      app: :server,
      version: "0.1.0",

      # Elixir config
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Release
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
      {:cowboy, "~> 2.8.0"}
    ]
  end
end
