defmodule AdventOfCode.Day8 do
  @moduledoc """
  Solution to Day 8: Treetop Tree House.
  https://adventofcode.com/2022/day/8
  """
  alias AdventOfCode.Util

  def p1() do
    input = Util.read_input(8, :digits)
    tree_grid = Enum.map(input, &List.to_tuple/1) |> List.to_tuple()
    max_y = tuple_size(tree_grid) - 1
    max_x = tuple_size(elem(tree_grid, 0)) - 1

    for x <- 0..max_x, y <- 0..max_y, reduce: 0 do
      count ->
        if visible?(tree_grid, x, y),
          do: count + 1,
          else: count
    end
  end

  def p2() do
    input = Util.read_input(8, :digits)
    tree_grid = Enum.map(input, &List.to_tuple/1) |> List.to_tuple()
    max_y = tuple_size(tree_grid) - 1
    max_x = tuple_size(elem(tree_grid, 0)) - 1

    for x <- 0..max_x, y <- 0..max_y, reduce: 0 do
      acc -> max(acc, scenic_score(tree_grid, x, y))
    end
  end

  def scenic_score(tree_grid, x, y) do
    tree_height = get(tree_grid, x, y)
    acc0 = 0

    fun = fn
      _other_tree_height, {:halt, score} ->
        {:halt, score}

      other_tree_height, score ->
        if tree_height > other_tree_height,
          do: score + 1,
          else: {:halt, score + 1}
    end

    for direction <- ~w(north south east west)a do
      traverse_single_direction(tree_grid, x, y, direction, acc0, fun)
    end
    |> Enum.map(fn
      {:halt, score} -> score
      score -> score
    end)
    |> Enum.product()
  end

  def visible?(tree_grid, x, y) do
    tree_height = get(tree_grid, x, y)
    acc0 = true

    fun = fn
      _other_tree_height, false ->
        false

      other_tree_height, true ->
        tree_height > other_tree_height
    end

    for direction <- ~w(north south east west)a do
      traverse_single_direction(tree_grid, x, y, direction, acc0, fun)
    end
    |> Enum.any?(&(&1 == true))
  end

  def traverse_single_direction(grid, x, y, :north, acc0, fun) do
    do_traverse_single_direction(grid, x, y, x, 0, acc0, fun)
  end

  def traverse_single_direction(grid, x, y, :south, acc0, fun) do
    max_y = tuple_size(grid) - 1
    do_traverse_single_direction(grid, x, y, x, max_y, acc0, fun)
  end

  def traverse_single_direction(grid, x, y, :east, acc0, fun) do
    max_x = tuple_size(elem(grid, 0)) - 1
    do_traverse_single_direction(grid, x, y, max_x, y, acc0, fun)
  end

  def traverse_single_direction(grid, x, y, :west, acc0, fun) do
    do_traverse_single_direction(grid, x, y, 0, y, acc0, fun)
  end

  def do_traverse_single_direction(grid, x, y, last_x, last_y, acc0, fun) do
    for other_y <- y..last_y,
        other_x <- x..last_x,
        not (other_y == y and other_x == x),
        reduce: acc0 do
      acc ->
        other_value = get(grid, other_x, other_y)
        fun.(other_value, acc)
    end
  end

  def get(tree_grid, x, y) do
    tree_grid |> elem(y) |> elem(x)
  end
end
