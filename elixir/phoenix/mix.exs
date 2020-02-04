defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      # App config
      app: :server,
      version: "0.1.0",

      # Elixir config
      elixir: "~> 1.10",
      compilers: [:phoenix] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Server.Application, []}
    ]
  end

  defp deps do
    [
      {:phoenix, "~> 1.4"},
      {:jason, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"}
    ]
  end
end
