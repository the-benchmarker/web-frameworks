defmodule Poison.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :poison,
     version: @version,
     elixir: "~> 1.1",
     description: "An incredibly fast, pure Elixir JSON library",
     deps: deps(),
     package: package(),
     consolidate_protocols: Mix.env != :test]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: []]
  end

  # Dependencies can be hex.pm packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [{:earmark, "~> 0.2", only: :docs},
     {:ex_doc, "~> 0.11", only: :docs},
     {:benchfella, "~> 0.3", only: :bench},
     {:jiffy, github: "davisp/jiffy", only: :bench},
     {:exjsx, github: "talentdeficit/exjsx", only: :bench},
     {:jazz, github: "meh/jazz", only: :bench}]
  end

  defp package do
    [files: ~w(lib mix.exs README.md LICENSE UNLICENSE VERSION),
     maintainers: ["Devin Torres"],
     licenses: ["Unlicense"],
     links: %{"GitHub" => "https://github.com/devinus/poison"}]
  end
end
