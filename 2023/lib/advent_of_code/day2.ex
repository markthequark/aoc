defmodule AdventOfCode.Day2 do
  @moduledoc """
  Solution to Day 2: Cube Conundrum
  https://adventofcode.com/2023/day/2
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(2)

    for line <- lines, reduce: %{} do
      state ->
        [game_id | pulls] = parse_line(line)
        max_cubes = max_cubes(pulls)
        Map.put_new(state, game_id, max_cubes)
    end
    |> Map.filter(&valid_p1/1)
    |> Map.keys()
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  def p2() do
    lines = Util.read_input(2)

    for line <- lines, reduce: %{} do
      state ->
        [game_id | pulls] = parse_line(line)
        max_cubes = max_cubes(pulls)
        Map.put_new(state, game_id, max_cubes)
    end
    |> Map.values()
    |> Enum.map(&cube_power/1)
    |> Enum.sum()
  end

  def parse_line(line) do
    String.split(line, ["Game ", ":", ";", ","], trim: true)
    |> Enum.map(&String.trim/1)
  end

  def max_cubes(pulls, acc \\ %{"red" => 0, "green" => 0, "blue" => 0})

  def max_cubes([], acc), do: acc

  def max_cubes([pull | rest], acc) do
    [number, colour] = String.split(pull, " ")
    number = String.to_integer(number)

    acc = Map.update!(acc, colour, &max(&1, number))
    max_cubes(rest, acc)
  end

  def cube_power(cubes) do
    Map.values(cubes)
    |> Enum.product()
  end

  def valid_p1({_game_id, max_pulls}) do
    max_pulls["red"] <= 12 and
      max_pulls["green"] <= 13 and
      max_pulls["blue"] <= 14
  end
end
