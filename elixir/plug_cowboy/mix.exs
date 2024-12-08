defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      app: :server,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: [server: [include_executables_for: [:unix]]]
    ]
  end

  def application, do: [mod: {Server.Application, []}]

  defp deps do
    [
      {:plug, "~> 1.16.1"},
      {:plug_cowboy, "~> 2.7.2"}
    ]
  end
end
