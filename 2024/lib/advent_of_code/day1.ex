defmodule AdventOfCode.Day1 do
  @moduledoc """
  Solution to Day 1: Historian Hysteria
  https://adventofcode.com/2024/day/1
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(1)

    [list1, list2] = parse_lists(lines) |> Enum.map(&Enum.sort/1)

    Enum.zip_reduce(list1, list2, 0, fn id1, id2, acc -> abs(id1 - id2) + acc end)
  end

  def p2() do
    lines = Util.read_input(1)

    [list1, list2] = parse_lists(lines) |> Enum.map(&Enum.sort/1)
    frequency_map_2 = Enum.frequencies(list2)

    for location_id <- list1, reduce: 0 do
      acc ->
        acc + location_id * Map.get(frequency_map_2, location_id, 0)
    end
  end

  @spec parse_lists(lines) :: location_id_lists
        when lines: [String.t()],
             location_id_lists: [[integer()]]
  def parse_lists(lines) do
    for line <- lines, reduce: [[], []] do
      [list1, list2] ->
        [location_id_1, location_id_2] =
          String.split(line, " ", trim: true) |> Enum.map(&String.to_integer/1)

        [[location_id_1 | list1], [location_id_2 | list2]]
    end
  end
end
