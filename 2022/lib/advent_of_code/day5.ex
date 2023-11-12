defmodule AdventOfCode.Day5 do
  @moduledoc """
  Solution to Day 5: Supply Stacks.
  https://adventofcode.com/2022/day/5
  """
  alias AdventOfCode.Util

  def p1() do
    input = Util.read_input(5)
    {stacks_raw, commands} = Enum.split_while(input, &(not String.starts_with?(&1, "move")))
    stacks = parse_stacks(stacks_raw)

    for command <- commands, reduce: stacks do
      stacks ->
        [n, from, to] = String.split(command, ["move", "from", "to", " "], trim: true)
        n = String.to_integer(n)
        move(stacks, n, from, to)
    end
    |> Map.values()
    |> Enum.map(&hd/1)
    |> Enum.join()
  end

  def p2() do
    input = Util.read_input(5)
    {stacks_raw, commands} = Enum.split_while(input, &(not String.starts_with?(&1, "move")))
    stacks = parse_stacks(stacks_raw)

    for command <- commands, reduce: stacks do
      stacks ->
        [n, from, to] = String.split(command, ["move", "from", "to", " "], trim: true)
        n = String.to_integer(n)
        move_n(stacks, n, from, to)
    end
    |> Map.values()
    |> Enum.map(&hd/1)
    |> Enum.join()
  end

  def parse_command(command) do
    String.split(command, ["move", "from", "to", " "], trim: true)
  end

  def parse_stacks(input) do
    [num_stacks | rows] = Enum.reverse(input)
    keys = String.split(num_stacks, ~r"\s", trim: true)
    stacks = Map.from_keys(keys, [])

    for row <- rows, reduce: stacks do
      stacks -> parse_row(stacks, row)
    end
  end

  def parse_row(stacks, row, column \\ 1)

  def parse_row(stacks, <<"[", crate::binary-size(1), "]">>, column) do
    key = Integer.to_string(column)
    Map.update!(stacks, key, fn stack -> [crate | stack] end)
  end

  def parse_row(stacks, <<"[", crate::binary-size(1), "] ", row::binary>>, column) do
    key = Integer.to_string(column)
    stacks = Map.update!(stacks, key, fn stack -> [crate | stack] end)
    parse_row(stacks, row, column + 1)
  end

  def parse_row(stacks, <<"    ", row::binary>>, column) do
    parse_row(stacks, row, column + 1)
  end

  def move(stacks, n, from, to) do
    for _ <- 1..n, reduce: stacks do
      stacks -> move(stacks, from, to)
    end
  end

  def move(stacks, from, to) do
    {crate, stacks} = Map.get_and_update(stacks, from, fn [crate | rest] -> {crate, rest} end)
    Map.update!(stacks, to, fn stack -> [crate | stack] end)
  end

  def move_n(stacks, n, from, to) do
    {crates, stacks} = Map.get_and_update(stacks, from, &Enum.split(&1, n))
    Map.update!(stacks, to, fn stack -> crates ++ stack end)
  end
end
