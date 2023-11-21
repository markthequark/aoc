defmodule AdventOfCode.Day13 do
  @moduledoc """
  Solution to Day 13: Distress Signal
  https://adventofcode.com/2022/day/13
  """
  alias AdventOfCode.Util

  def p1() do
    pairs =
      Util.read_input(13)
      |> Enum.map(fn line ->
        {result, _} = Code.eval_string(line)
        result
      end)
      |> Enum.chunk_every(2)
      |> Enum.with_index(1)

    for {[left, right], index} <- pairs, in_order?(left, right) do
      index
    end
    |> Enum.sum()
  end

  def p2() do
    received_packets =
      Util.read_input(13)
      |> Enum.map(fn line ->
        {result, _} = Code.eval_string(line)
        result
      end)

    divider_packets = [[[2]], [[6]]]

    packets =
      (divider_packets ++ received_packets)
      |> Enum.sort(&in_order?/2)

    divider_packets
    |> Enum.map(&(Enum.find_index(packets, fn packet -> packet == &1 end) + 1))
    |> Enum.product()
  end

  def in_order?(left, right), do: compare(left, right) == :lt

  def compare(left, right)

  def compare([], [_ | _]), do: :lt
  def compare([_ | _], []), do: :gt
  def compare([], []), do: :eq

  def compare(left, right) when is_integer(left) and is_integer(right) do
    cond do
      left < right -> :lt
      left == right -> :eq
      true -> :gt
    end
  end

  def compare(left, right) when is_integer(left) or is_integer(right) do
    compare(List.wrap(left), List.wrap(right))
  end

  def compare(left, right) do
    case compare(hd(left), hd(right)) do
      :lt -> :lt
      :gt -> :gt
      :eq -> compare(tl(left), tl(right))
    end
  end
end
