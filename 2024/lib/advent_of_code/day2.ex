defmodule AdventOfCode.Day2 do
  @moduledoc """
  Solution to Day 2: Red-Nosed Reports
  https://adventofcode.com/2024/day/2
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(2)

    reports = parse_reports(lines)

    reports
    |> Enum.filter(&is_report_safe?/1)
    |> Enum.count()
  end

  def p2() do
    lines = Util.read_input(2)

    reports = parse_reports(lines)

    reports
    |> Enum.filter(fn report -> is_report_safe?(report, %{tolerance: 1}) end)
    |> Enum.count()
  end

  # quick bit of code to allow recusing the entire function without rewriting the solution
  # only needed for part 2, but better would be a new solution
  def is_report_safe?([level_1, _level_2 | rest] = report, options \\ %{tolerance: 0}) do
    if options.tolerance > 1 do
      do_is_report_safe?(report, options) or
        do_is_report_safe?(tl(report), Map.update!(options, :tolerance, &(&1 - 1))) or
        do_is_report_safe?([level_1 | rest], Map.update!(options, :tolerance, &(&1 - 1)))
    else
      do_is_report_safe?(report, options)
    end
  end

  def do_is_report_safe?([level_1, level_2 | _] = report, options \\ %{tolerance: 0}) do
    predicates =
      if increasing?(level_2, level_1) do
        [&increasing?/2, &gradual_change?/2]
      else
        [&decreasing?/2, &gradual_change?/2]
      end

    acc = %{predicates: predicates, prev: hd(report), tolerance: options.tolerance}

    result =
      for current_level <- tl(report), reduce: acc do
        :unsafe ->
          :unsafe

        acc ->
          if Enum.all?(acc.predicates, fn predicate -> predicate.(current_level, acc.prev) end) do
            %{acc | prev: current_level}
          else
            if acc.tolerance < 1 do
              :unsafe
            else
              %{acc | tolerance: acc.tolerance - 1}
            end
          end
      end

    result != :unsafe
  end

  def parse_reports(lines) do
    for line <- lines do
      String.split(line, " ") |> Enum.map(&String.to_integer/1)
    end
  end

  def gradual_change?(element, prev) do
    delta = abs(element - prev)
    delta > 0 and delta <= 3
  end

  def increasing?(element, prev) do
    element > prev
  end

  def decreasing?(element, prev) do
    element < prev
  end
end
