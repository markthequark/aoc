defmodule AdventOfCode do
  @moduledoc """
  Solutions for the 2022 Advent of Code challenge
  https://adventofcode.com/2022/
  """

  def main([day]) do
    module = String.to_existing_atom("Elixir.AdventOfCode.Day#{day}")
    p1 = module.p1()
    p2 = module.p2()

    IO.puts("""
    Part 1:
    #{inspect(p1)}

    Part 2:
    #{inspect(p2)}
    """)
  end

  def main(_args) do
    IO.puts("""
    Usage: ./advent_of_code DAY
    DAY may be any integer between 1..25
    """)
  end
end
