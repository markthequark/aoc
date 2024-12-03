defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code,
      version: "1.0.0",
      elixir: "~> 1.17.0",
      elixirc_paths: elixirc_paths(Mix.env()),
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

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:libgraph, "~> 0.16.0"},
      {:req, "~> 0.4.0"},
      {:req_easyhtml, "~> 0.1.0"},
      {:dotenvy, "~> 0.8.0"},
      {:nimble_parsec, "~> 1.4.0"},
      {:benchee, "~> 1.3.1"},
      {:mox, "~> 1.2", only: :test}
    ]
  end
end
