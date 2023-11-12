defmodule AdventOfCode.Day5 do
  @moduledoc """
  Solution to Day 5: Supply Stacks.
  https://adventofcode.com/2022/day/5
  """
  alias AdventOfCode.Util

  for part <- [:p1, :p2] do
    def unquote(part)() do
      input = Util.read_input(5)
      {stacks_raw, commands} = Enum.split_while(input, &(not String.starts_with?(&1, "move")))

      stacks_raw
      |> parse_stacks()
      |> process_commands(commands, unquote(part))
      |> read_first_crates()
    end
  end

  def process_commands(stacks, commands, method) do
    for command <- commands, reduce: stacks do
      stacks ->
        [n, from, to] = parse_command(command)

        case method do
          :p1 -> move(stacks, n, from, to)
          :p2 -> move_n(stacks, n, from, to)
        end
    end
  end

  def parse_command(command) do
    String.split(command, ["move", "from", "to", " "], trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def parse_stacks(input) do
    [column_names | rows] = Enum.reverse(input)

    stacks =
      String.split(column_names, ~r"\s", trim: true)
      |> Enum.map(&String.to_integer/1)
      |> Map.from_keys([])

    for row <- rows, reduce: stacks do
      stacks -> parse_row(stacks, row)
    end
  end

  def parse_row(stacks, row, column \\ 1)

  def parse_row(stacks, <<"[", crate::binary-size(1), "]">>, column) do
    Map.update!(stacks, column, fn stack -> [crate | stack] end)
  end

  def parse_row(stacks, <<"[", crate::binary-size(1), "] ", row::binary>>, column) do
    stacks = Map.update!(stacks, column, fn stack -> [crate | stack] end)
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

  def read_first_crates(stacks) do
    stacks
    |> Map.values()
    |> Enum.map(&hd/1)
    |> Enum.join()
  end
end
