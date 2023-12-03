defmodule AdventOfCode.Day3 do
  @moduledoc """
  Solution to Day 3: Gear Ratios
  https://adventofcode.com/2023/day/3
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(3, :charlist)
    grid = to_grid(lines)
    part_numbers = find_part_numbers(lines)

    for {positions, part_number} <- part_numbers, reduce: 0 do
      acc ->
        had_adjacent_symbol =
          Enum.map(positions, &adjacent_positions/1)
          |> List.flatten()
          |> Enum.uniq()
          |> Enum.any?(fn position ->
            grid[position] not in ?0..?9 and grid[position] not in [?., nil]
          end)

        if had_adjacent_symbol do
          acc + part_number
        else
          acc
        end
    end
  end

  def p2() do
    lines = Util.read_input(3, :charlist)
    part_numbers = find_part_numbers(lines)

    for {line, line_num} <- Enum.with_index(lines),
        {elem, column_num} <- Enum.with_index(line),
        elem == ?*,
        reduce: 0 do
      acc ->
        gear_position = {line_num, column_num}
        gear_adjacent_positions = adjacent_positions(gear_position)

        adjacent_part_numbers =
          part_numbers
          |> Enum.filter(fn {part_num_positions, _number} ->
            contains_any?(gear_adjacent_positions, part_num_positions)
          end)

        case adjacent_part_numbers do
          [{_, number1}, {_, number2}] ->
            acc + number1 * number2

          _ ->
            acc
        end
    end
  end

  def to_grid(lines) do
    for {line, line_num} <- Enum.with_index(lines),
        {elem, column_num} <- Enum.with_index(line),
        into: %{} do
      {{line_num, column_num}, elem}
    end
  end

  def find_part_numbers(lines) do
    acc0 = %{part_numbers: [], part_number: [], line_num: nil}

    for {line, line_num} <- Enum.with_index(lines),
        {elem, column_num} <- Enum.with_index(line),
        reduce: acc0 do
      %{part_number: []} = acc when elem in ?0..?9 ->
        position = {line_num, column_num}
        %{acc | part_number: [{position, elem}], line_num: line_num}

      %{part_number: []} = acc ->
        acc

      %{part_number: part_number, line_num: ^line_num} = acc when elem in ?0..?9 ->
        position = {line_num, column_num}
        %{acc | part_number: [{position, elem} | part_number]}

      acc ->
        %{number: number, positions: positions} = to_number(acc.part_number)

        acc
        |> Map.put(:part_number, [])
        |> Map.update!(:part_numbers, &[{positions, number} | &1])
    end
    |> then(& &1.part_numbers)
  end

  def to_number(part_number) do
    for {position, digit} <- part_number, reduce: %{tens: 0, number: 0, positions: []} do
      acc ->
        digit = digit - ?0
        number = acc.number + digit * 10 ** acc.tens

        acc
        |> Map.put(:number, number)
        |> Map.update!(:tens, &(&1 + 1))
        |> Map.update!(:positions, &[position | &1])
    end
  end

  def adjacent_positions({x, y}) do
    offsets = [{0, 1}, {1, 0}, {-1, 0}, {0, -1}, {1, 1}, {-1, 1}, {-1, -1}, {1, -1}]

    for {dx, dy} <- offsets do
      {x + dx, y + dy}
    end
  end

  def contains_any?(list1, list2) do
    intersection = MapSet.intersection(MapSet.new(list1), MapSet.new(list2))
    MapSet.size(intersection) > 0
  end
end
