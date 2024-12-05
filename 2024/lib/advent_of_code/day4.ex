defmodule AdventOfCode.Day4 do
  @moduledoc """
  Solution to Day 4: Ceres Search
  https://adventofcode.com/2024/day/4
  """
  alias AdventOfCode.Util

  def p1() do
    input = Util.read_input(4, :charlist)
    grid = Util.to_grid(input)
    max_x = length(hd(input))
    max_y = length(input)

    for x <- 0..max_x, y <- 0..max_y, reduce: 0 do
      count -> count + count_xmas(grid, x, y)
    end
  end

  def p2() do
    input = Util.read_input(4, :charlist)
    grid = Util.to_grid(input)
    max_x = length(hd(input))
    max_y = length(input)

    for x <- 0..max_x, y <- 0..max_y, reduce: 0 do
      count -> if is_x_mas(grid, x, y), do: count + 1, else: count
    end
  end

  def is_x_mas(grid, x, y) do
    up_right_diagonal = [Map.get(grid, {x - 1, y + 1}), Map.get(grid, {x + 1, y - 1})]
    down_right_diagonal = [Map.get(grid, {x - 1, y - 1}), Map.get(grid, {x + 1, y + 1})]

    Map.get(grid, {x, y}) == ?A and
      Enum.sort(up_right_diagonal) == [?M, ?S] and
      Enum.sort(down_right_diagonal) == [?M, ?S]
  end

  def count_xmas(grid, x, y) do
    stay = fn coordinate, _offset -> coordinate end
    add = fn coordinate, offset -> coordinate + offset end
    subtract = fn coordinate, offset -> coordinate - offset end
    operations = [stay, add, subtract]

    possible_finds =
      for op1 <- operations, op2 <- operations, not (op1 == stay and op2 == stay) do
        for offset <- 0..3 do
          Map.get(grid, {op1.(x, offset), op2.(y, offset)})
        end
      end

    for find <- possible_finds, find == [?X, ?M, ?A, ?S], reduce: 0 do
      count -> count + 1
    end
  end
end
