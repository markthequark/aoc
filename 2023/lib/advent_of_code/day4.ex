defmodule AdventOfCode.Day4 do
  @moduledoc """
  Solution to Day 4: Scratchcards
  https://adventofcode.com/2023/day/4
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(4)

    for line <- lines, reduce: 0 do
      acc ->
        card = parse_line(line)
        value = card_points(card)
        acc + value
    end
  end

  def p2() do
    lines = Util.read_input(4)
    acc0 = %{card_copies_won: %{}, total_cards_won: 0}

    for line <- lines, reduce: acc0 do
      acc ->
        card = parse_line(line)
        num_cards = 1 + (acc.card_copies_won[card.card_num] || 0)
        acc = Map.update!(acc, :total_cards_won, &(&1 + num_cards))

        for n <- 1..num_matching_numbers(card)//1, reduce: acc do
          acc ->
            new_copy_card_num = card.card_num + n

            update_in(acc.card_copies_won[new_copy_card_num], fn
              nil -> num_cards
              copies -> copies + num_cards
            end)
        end
    end
    |> then(& &1.total_cards_won)
  end

  def parse_line("Card " <> rest) do
    {[card_num | winning_numbers], ["|" | my_numbers]} =
      String.split(rest, [":", " "], trim: true)
      |> Enum.split_while(&(&1 != "|"))

    card_num = String.to_integer(card_num)
    %{card_num: card_num, winning_numbers: winning_numbers, my_numbers: my_numbers}
  end

  def num_matching_numbers(card) do
    MapSet.intersection(MapSet.new(card.winning_numbers), MapSet.new(card.my_numbers))
    |> MapSet.size()
  end

  def card_points(card) do
    num_matching = num_matching_numbers(card)

    case num_matching do
      0 -> 0
      _ -> 2 ** (num_matching - 1)
    end
  end
end
