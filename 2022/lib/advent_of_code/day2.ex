defmodule AdventOfCode.Day2 do
  @moduledoc """
  Solution to Day 2: Rock Paper Scissors.
  https://adventofcode.com/2022/day/2
  """
  alias AdventOfCode.Util

  def p1() do
    for <<enemy_choice::utf8, " ", my_choice::utf8>> <- Util.read_input(2) do
      calculate_score(enemy_choice, my_choice)
    end
    |> Enum.sum()
  end

  def p2() do
    for <<enemy_choice::utf8, " ", outcome::utf8>> <- Util.read_input(2) do
      calculate_score_2(enemy_choice, outcome)
    end
    |> Enum.sum()
  end

  # rock     (A/X) = 1p
  # paper    (B/Y) = 2p
  # scissors (C/Z) = 3p
  # win = 6p
  # draw = 3p
  # lose = 0p

  defp calculate_score(enemy_choice, my_choice)

  defp calculate_score(?A, ?X), do: 1 + 3
  defp calculate_score(?A, ?Y), do: 2 + 6
  defp calculate_score(?A, ?Z), do: 3 + 0
  defp calculate_score(?B, ?X), do: 1 + 0
  defp calculate_score(?B, ?Y), do: 2 + 3
  defp calculate_score(?B, ?Z), do: 3 + 6
  defp calculate_score(?C, ?X), do: 1 + 6
  defp calculate_score(?C, ?Y), do: 2 + 0
  defp calculate_score(?C, ?Z), do: 3 + 3

  defp calculate_score_2(enemy_choice, outcome)

  defp calculate_score_2(?A, ?X), do: 0 + 3
  defp calculate_score_2(?A, ?Y), do: 3 + 1
  defp calculate_score_2(?A, ?Z), do: 6 + 2
  defp calculate_score_2(?B, ?X), do: 0 + 1
  defp calculate_score_2(?B, ?Y), do: 3 + 2
  defp calculate_score_2(?B, ?Z), do: 6 + 3
  defp calculate_score_2(?C, ?X), do: 0 + 2
  defp calculate_score_2(?C, ?Y), do: 3 + 3
  defp calculate_score_2(?C, ?Z), do: 6 + 1
end
