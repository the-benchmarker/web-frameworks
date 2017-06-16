defmodule MyElli.Mixfile do
  use Mix.Project

  def project do
    [app: :my_elli,
     version: "0.1.0",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [mod: {MyElli.Application, []}]
  end

  defp deps do
    [{:elli, github: "knutin/elli"},
     {:distillery, "~> 1.0", runtime: false}]
  end
end
