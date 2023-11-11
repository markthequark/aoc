
defmodule AdventOfCode.Day3 do
  @moduledoc """
  Solution to Day 3: Rucksack Reorganization.
  https://adventofcode.com/2022/day/3
  """
  alias AdventOfCode.Util

  def p1() do
    for rucksack <- Util.read_input(3, :charlist) do
      num_items = length(rucksack)
      :lists.split(div(num_items, 2), rucksack)
      |> Tuple.to_list()
      |> Enum.map(&MapSet.new/1)
      |> then(&apply(MapSet, :intersection, &1))
      |> MapSet.to_list()
      |> hd()
      |> priority()
    end
    |> Enum.sum()
  end

  def p2() do
    rucksacks = Util.read_input(3, :charlist)
    for group <- Enum.chunk_every(rucksacks, 3) do
      [set1, set2, set3] = Enum.map(group, &MapSet.new/1)
      set1
      |> MapSet.intersection(set2)
      |> MapSet.intersection(set3)
      |> MapSet.to_list()
      |> hd()
      |> priority()
    end
    |> Enum.sum()
  end
  
  def priority(item) when item <= ?Z and item >= ?A do
    item - ?A + 27
  end
  def priority(item) when item <= ?z and item >= ?a do
    item - ?a + 1
  end
end
