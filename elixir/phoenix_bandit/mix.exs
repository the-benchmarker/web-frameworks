defmodule Server.MixProject do
  use Mix.Project

  @version "1.0.0"
  @source_url "https://github.com/your-org/server"

  def project do
    [
      app: :server,
      version: @version,
      elixir: "~> 1.19",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: releases(),
      
      description: "Production-grade Phoenix Bandit web server",
      package: package(),
      elixirc_options: elixirc_options(Mix.env())
    ]
  end

  def application do
    [
      mod: {Server.Application, []},
      extra_applications: [:logger, :phoenix, :bandit]
    ]
  end

  defp deps do
    [
      {:phoenix, "~> 1.8.1"},
      {:bandit, "~> 1.12.0"},
      {:jason, "~> 1.4"}
    ]
  end

  defp releases do
    [
      server: [
        include_executables_for: [:unix],
        steps: [:assemble]
      ]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp elixirc_options(:prod), do: [optimize: true, warnings_as_errors: true]
  defp elixirc_options(_), do: []

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => @source_url},
      maintainers: ["Your Name <your.email@example.com>"]
    ]
  end
end
