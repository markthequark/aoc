defmodule AdventOfCode.Day12 do
  @moduledoc """
  Solution to Day 12: Hill Climbing Algorithm
  https://adventofcode.com/2022/day/12
  """
  alias AdventOfCode.Util

  @infinity Integer.pow(2, 32)

  def p1() do
    input = Util.read_input(12, :charlist)

    height_map =
      for {row, row_num} <- Stream.with_index(input),
          {element, col_num} <- Stream.with_index(row),
          into: %{} do
        {{row_num, col_num}, element}
      end

    start_pos = start_pos(height_map)
    end_pos = end_pos(height_map)

    height_map = %{height_map | start_pos => ?a, end_pos => ?z}

    dijkstra(height_map, start_pos, end_pos)
  end

  def p2() do
    input = Util.read_input(12, :charlist)

    height_map =
      for {row, row_num} <- Stream.with_index(input),
          {element, col_num} <- Stream.with_index(row),
          into: %{} do
        {{row_num, col_num}, element}
      end

    start_pos = start_pos(height_map)
    end_pos = end_pos(height_map)
    height_map = %{height_map | start_pos => ?a, end_pos => ?z}

    graph =
      for {pos, _height} <- height_map,
          neighbour <- neighbours(height_map, pos),
          reduce: Graph.new() do
        graph -> Graph.add_edge(graph, pos, neighbour)
      end

    height_map
    |> Stream.filter(fn {_pos, height} -> height == ?a end)
    |> Stream.map(fn {pos, _height} -> Graph.dijkstra(graph, pos, end_pos) end)
    |> Stream.reject(&is_nil/1)
    |> Stream.map(&(length(&1) - 1))
    |> Enum.min()
  end

  def dijkstra(map, start, target) do
    unvisited = Map.keys(map) |> MapSet.new()

    distance_map =
      Map.new(map, fn
        {^start, _} -> {start, 0}
        {pos, _} -> {pos, @infinity}
      end)

    dijkstra(map, start, target, unvisited, distance_map)
  end

  def dijkstra(_map, target, target, _unvisited, distance_map) do
    distance_map[target]
  end

  def dijkstra(map, current, target, unvisited, distance_map) do
    neighbours =
      neighbours(map, current)
      |> Stream.filter(&(&1 in unvisited))

    new_distance_map =
      for neighbour <- neighbours, reduce: distance_map do
        distance_map ->
          distance = min(distance_map[neighbour], distance_map[current] + 1)
          %{distance_map | neighbour => distance}
      end

    new_unvisited = MapSet.delete(unvisited, current)
    next = Enum.min_by(new_unvisited, &new_distance_map[&1])

    dijkstra(map, next, target, new_unvisited, new_distance_map)
  end

  def neighbours(map, current) do
    adjacent_positions(map, current)
    |> Stream.filter(&(map[current] + 1 >= map[&1]))
  end

  def adjacent_positions(map, {x, y}) do
    offsets = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]

    for {x_off, y_off} <- offsets,
        pos = {x + x_off, y + y_off},
        pos in Map.keys(map) do
      pos
    end
  end

  def start_pos(grid), do: find_pos(grid, ?S)
  def end_pos(grid), do: find_pos(grid, ?E)

  def find_pos(grid, element) do
    Enum.find_value(grid, fn {pos, value} -> if value == element, do: pos end)
  end
end
