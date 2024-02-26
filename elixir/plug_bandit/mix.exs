defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      app: :server,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      releases: [server: [include_executables_for: [:unix]]],
      deps: [
        {:plug, "~> 1.15.3"},
        {:bandit, "~> 1.2.3"}
      ]
    ]
  end

  def application, do: [mod: {Server.Application, []}]
end
