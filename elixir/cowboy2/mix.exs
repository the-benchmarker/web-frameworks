defmodule MyCowboy.Mixfile do
  use Mix.Project

  def project do
    [app: :my_cowboy,
     version: "0.1.0",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [mod: {MyCowboy.Application, []}]
  end


  defp deps do
    [{:cowboy, github: "ninenines/cowboy", tag: "2.0.0-pre.9"},
     {:distillery, "~> 1.0", runtime: false}]
  end
end
