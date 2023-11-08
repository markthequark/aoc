defmodule AdventOfCode.Day1 do
  @moduledoc """
  Solution to Day 1: Calorie Counting.
  https://adventofcode.com/2022/day/1
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(1, :string, trim: false)
    elf_inventories = chunk_by_empty_line(lines)

    for elf_inventory <- elf_inventories do
      Enum.map(elf_inventory, &String.to_integer/1)
      |> Enum.sum()
    end
    |> Enum.max()
  end

  def p2() do
    lines = Util.read_input(1, :string, trim: false)
    elf_inventories = chunk_by_empty_line(lines)

    for elf_inventory <- elf_inventories do
      Enum.map(elf_inventory, &String.to_integer/1)
      |> Enum.sum()
    end
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.sum()
  end

  defp chunk_by_empty_line(enum) do
    chunk_fun = fn
      "", acc ->
        {:cont, acc, []}

      element, acc ->
        {:cont, [element | acc]}
    end

    after_fun = fn
      [] ->
        {:cont, []}

      acc ->
        {:cont, Enum.reverse(acc), []}
    end

    Enum.chunk_while(enum, [], chunk_fun, after_fun)
  end
end
