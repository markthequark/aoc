defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code,
      version: "1.0.0",
      elixir: "~> 1.16-rc",
      escript: [main_module: AdventOfCode],
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:libgraph, "~> 0.16.0"},
      {:mox, "~> 1.1", only: :test}
    ]
  end
end
