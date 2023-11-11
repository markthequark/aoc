defmodule AdventOfCode.Day4 do
  @moduledoc """
  Solution to Day 4: Camp Cleanup.
  https://adventofcode.com/2022/day/4
  """
  alias AdventOfCode.Util

  def p1() do
    for pair <- Util.read_input(4),
        [assignment1, assignment2] = parse(pair),
        fully_contains?(assignment1, assignment2),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  def p2() do
    for pair <- Util.read_input(4),
        [assignment1, assignment2] = parse(pair),
        any_contains?(assignment1, assignment2),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  def parse(pair) do
    for elf <- String.split(pair, ",") do
      String.split(elf, "-")
      |> Enum.map(&String.to_integer/1)
    end
  end

  def fully_contains?([start1, end1], [start2, end2]) do
    (start1 <= start2 and end1 >= end2) or
      (start2 <= start1 and end2 >= end1)
  end

  def any_contains?([start1, end1], [start2, end2]) do
    (start1 >= start2 and start1 <= end2) or
      (end1 >= start2 and end1 <= end2) or
      (start2 >= start1 and start2 <= end1) or
      (end2 >= start1 and end2 <= end1)
  end
end
