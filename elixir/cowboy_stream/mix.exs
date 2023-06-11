defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      app: :server,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: [{:cowboy, "~> 2.10.0"}],
      releases: [server: [include_executables_for: [:unix]]]
    ]
  end

  def application, do: [mod: {Server.Application, []}]
end
