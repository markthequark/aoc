defmodule AdventOfCode.Day11 do
  @moduledoc """
  Solution to Day 11: Monkey in the Middle
  https://adventofcode.com/2022/day/11
  """
  alias AdventOfCode.Util

  defmodule Monkey do
    defstruct [:id, :items, :operation, :test, :divisor, :when_true, :when_false, throws: 0]
  end

  for {part, num_rounds} <- [p1: 20, p2: 10_000] do
    def unquote(part)() do
      lines =
        Util.read_input(11)
        |> Enum.map(&String.trim/1)

      monkey_map =
        Enum.chunk_every(lines, 6)
        |> Enum.map(&parse_monkey/1)
        |> Enum.into(%{}, fn monkey -> {monkey.id, monkey} end)

      least_common_divisor =
        monkey_map
        |> Map.values()
        |> Enum.map(& &1.divisor)
        |> lcd()

      for _ <- 1..unquote(num_rounds), reduce: monkey_map do
        monkey_map -> play_round(monkey_map, least_common_divisor, unquote(part))
      end
      |> Map.values()
      |> Enum.map(& &1.throws)
      |> Enum.sort(:desc)
      |> Enum.take(2)
      |> Enum.product()
    end
  end

  def play_round(monkey_map, lcd, part) do
    for id <- Map.keys(monkey_map), reduce: monkey_map do
      monkey_map -> take_turn(monkey_map[id], monkey_map, lcd, part)
    end
  end

  def take_turn(monkey, monkey_map, lcd, part) do
    for item <- monkey.items, reduce: monkey_map do
      monkey_map ->
        new_item = inspect_item(monkey, item, lcd, part)
        monkey_to_throw_to = who_to_throw_to(monkey, new_item)
        throw_item(monkey_map, monkey.id, monkey_to_throw_to, new_item)
    end
    |> put_in([monkey.id, Access.key!(:items)], [])
  end

  def throw_item(monkey_map, thrower_id, catcher_id, item) do
    monkey_map
    |> update_in([catcher_id, Access.key!(:items)], &(&1 ++ [item]))
    |> update_in([thrower_id, Access.key!(:throws)], &(&1 + 1))
  end

  def who_to_throw_to(monkey, item) do
    if monkey.test.(item),
      do: monkey.when_true,
      else: monkey.when_false
  end

  def inspect_item(monkey, item, _lcd, :p1) do
    item
    |> monkey.operation.()
    |> then(&div(&1, 3))
  end

  def inspect_item(monkey, item, lcd, :p2) do
    item
    |> monkey.operation.()
    |> then(&rem(&1, lcd))
  end

  def lcd(list), do: Enum.reduce(list, &lcd/2)
  def lcd(a, b), do: div(abs(a * b), Integer.gcd(a, b))

  def parse_monkey(input) do
    for line <- input, reduce: %Monkey{} do
      monkey -> do_parse_monkey(line, monkey)
    end
  end

  def do_parse_monkey("Monkey " <> id, monkey) do
    {id, _} = Integer.parse(id)
    %Monkey{monkey | id: id}
  end

  def do_parse_monkey("Starting items: " <> items, monkey) do
    items =
      String.split(items, ", ", trim: true)
      |> Enum.map(&String.to_integer/1)

    %Monkey{monkey | items: items}
  end

  def do_parse_monkey("Operation: new = " <> expression, monkey) do
    operation = fn old ->
      {result, _bindings} = Code.eval_string(expression, old: old)
      result
    end

    %Monkey{monkey | operation: operation}
  end

  def do_parse_monkey("Test: divisible by " <> divisor, monkey) do
    divisor = String.to_integer(divisor)
    test = fn x -> rem(x, divisor) == 0 end
    %Monkey{monkey | test: test, divisor: divisor}
  end

  def do_parse_monkey("If true: throw to monkey " <> id, monkey) do
    monkey_id = String.to_integer(id)
    %Monkey{monkey | when_true: monkey_id}
  end

  def do_parse_monkey("If false: throw to monkey " <> id, monkey) do
    monkey_id = String.to_integer(id)
    %Monkey{monkey | when_false: monkey_id}
  end
end
