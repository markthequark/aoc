defmodule AdventOfCode.Day5 do
  @moduledoc """
  Solution to Day 5: Print Queue
  https://adventofcode.com/2024/day/5
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(5)
    {rules, updates} = Enum.split_while(lines, fn line -> line =~ "|" end)
    rules = parse_rules(rules)
    updates = parse_updates(updates)

    updates
    |> Enum.filter(&valid_update?(&1, rules))
    |> Enum.map(&middle_number/1)
    |> Enum.sum()
  end

  def p2() do
    lines = Util.read_input(5)
    {rules, updates} = Enum.split_while(lines, fn line -> line =~ "|" end)
    rules = parse_rules(rules)
    updates = parse_updates(updates)

    page_order = determine_page_order(rules)

    updates
    |> Enum.reject(&valid_update?(&1, rules))
    |> Enum.map(&order_correctly(&1, page_order))
    |> Enum.map(&middle_number/1)
    |> Enum.sum()
  end

  def order_correctly(update, page_order) do
    page_order |> Enum.filter(fn page -> page in update end)
  end

  def determine_page_order(rules) do
    # group rules by left
    grouped_rules = rules |> Enum.group_by(& &1.left)

    # merge rules with same left
    merged_rules =
      for {left, rules} <- grouped_rules do
        %{left: left, right: Enum.map(rules, & &1.right)}
      end

    # merge all rules
    for rule <- merged_rules, reduce: [] do
      acc ->
        acc = Enum.uniq(acc ++ rule.right)

        if rule.left not in acc do
          Enum.uniq([rule.left | acc])
        else
          left_index = Enum.find_index(acc, &(&1 == rule.left))

          bad_right_pages =
            for page <- rule.right do
              acc_index = Enum.find_index(acc, &(&1 == page))
              %{page: page, acc_index: acc_index}
            end
            |> Enum.filter(fn %{page: _page, acc_index: acc_index} -> acc_index < left_index end)
            |> Enum.sort_by(& &1.acc_index)
            |> Enum.map(& &1.page)

          acc
          |> List.insert_at(left_index + 1, bad_right_pages)
          |> List.flatten()
          |> Kernel.--(bad_right_pages)
        end
    end
  end

  def middle_number(update) do
    index = div(length(update), 2)
    Enum.at(update, index)
  end

  def valid_update?(update, rules) do
    Enum.all?(rules, fn rule -> apply_rule?(update, rule) end)
  end

  def parse_rules(rules) do
    for rule <- rules do
      [left, right] = String.split(rule, "|") |> Enum.map(&String.to_integer/1)
      %{left: left, right: right}
    end
  end

  def parse_updates(updates) do
    for update <- updates do
      update |> String.split(",", trim: true) |> Enum.map(&String.to_integer/1)
    end
  end

  def apply_rule?(update, rule, acc \\ %{found_left: false, found_right: false})

  def apply_rule?([left | _rest], %{left: left}, %{found_right: true}) do
    false
  end

  def apply_rule?([left | rest], %{left: left} = rule, acc) do
    apply_rule?(rest, rule, %{acc | found_left: true})
  end

  def apply_rule?([right | _rest], %{right: right}, %{found_left: true}) do
    true
  end

  def apply_rule?([right | rest], %{right: right} = rule, acc) do
    apply_rule?(rest, rule, %{acc | found_right: true})
  end

  def apply_rule?([_ | rest], rule, acc) do
    apply_rule?(rest, rule, acc)
  end

  def apply_rule?([], _rule, _acc) do
    true
  end
end
