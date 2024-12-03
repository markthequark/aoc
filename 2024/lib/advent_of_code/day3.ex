defmodule AdventOfCode.Day3 do
  @moduledoc """
  Solution to Day 3: Mull It Over
  https://adventofcode.com/2024/day/3
  """
  alias AdventOfCode.Util

  def p1() do
    Util.read_input(3) |> Enum.map(&solve/1) |> Enum.sum()
  end

  def p2() do
    Util.read_input(3) |> Enum.map(&solve_p2/1) |> Enum.sum()
  end

  @default_acc %{
    phase: nil,
    left_digits: [],
    right_digits: [],
    sum: 0,
    checkpoint: nil,
    do?: true,
    part: 1
  }

  def solve_p2(line) do
    acc = %{@default_acc | part: 2}

    solve(line, acc)
  end

  def solve(line, acc \\ @default_acc)

  def solve(line, %{phase: :complete} = acc) do
    left = acc.left_digits |> Enum.reverse() |> Integer.undigits()
    right = acc.right_digits |> Enum.reverse() |> Integer.undigits()

    acc = %{acc | phase: nil, sum: acc.sum + left * right}
    solve(line, acc)
  end

  def solve(<<"do()", rest::binary>>, %{part: 2} = acc) do
    acc = %{acc | do?: true, phase: nil}
    solve(rest, acc)
  end

  def solve(<<"don't()", rest::binary>>, %{part: 2} = acc) do
    acc = %{acc | do?: false, phase: nil}
    solve(rest, acc)
  end

  def solve(<<"mul(", rest::binary>>, %{do?: true} = acc) do
    acc = %{acc | phase: :left_digits, left_digits: [], checkpoint: rest}
    solve(rest, acc)
  end

  def solve(line, %{phase: phase} = acc)
      when (phase == :left_digits and length(acc.left_digits) > 3) or
             (phase == :right_digits and length(acc.right_digits) > 3) do
    acc = %{acc | phase: nil}
    solve(line, acc)
  end

  def solve(<<digit, rest::binary>>, %{phase: phase} = acc)
      when digit in ?0..?9 and phase in [:left_digits, :right_digits] do
    acc = Map.update!(acc, phase, &[digit - ?0 | &1])
    solve(rest, acc)
  end

  def solve(<<?,, rest::binary>>, %{phase: :left_digits} = acc) do
    acc =
      case length(acc.left_digits) do
        0 -> %{acc | phase: nil}
        _ -> %{acc | phase: :right_digits, right_digits: []}
      end

    solve(rest, acc)
  end

  def solve(<<?), rest::binary>>, %{phase: :right_digits} = acc) do
    acc =
      case length(acc.right_digits) do
        0 -> %{acc | phase: nil}
        _ -> %{acc | phase: :complete}
      end

    solve(rest, acc)
  end

  # invalid end to a series of digits
  def solve(<<_, _rest::binary>>, acc) when acc.phase in [:left_digits, :right_digits] do
    acc = %{acc | phase: nil}

    solve(acc.checkpoint, acc)
  end

  def solve(<<_, rest::binary>>, acc) do
    solve(rest, acc)
  end

  def solve(<<>>, acc) do
    acc.sum
  end

  ### nimble parsec solution credit to Sgiath: https://github.com/Sgiath/advent-of-code/blob/master/lib/2024/day03.ex

  ### Benchee benchmark

  # running benchmark like so:
  # MIX_ENV=prod mix run -e "alias AdventOfCode.Day3; Benchee.run(%{\"manual\" => fn -> Day3.p2() end, \"nimble\" => fn -> Day3.nimble_p2() end})"

  #  Name             ips        average  deviation         median         99th %
  #  manual        1.09 K      914.27 μs    ±14.82%      886.71 μs     1259.78 μs
  #  nimble        1.06 K      942.16 μs    ±14.49%      914.64 μs     1420.66 μs
  #
  #  Comparison:
  #  manual        1.09 K
  #  nimble        1.06 K - 1.03x slower +27.88 μs

  import NimbleParsec

  mul =
    ignore(string("mul("))
    |> integer(min: 1, max: 3)
    |> ignore(string(","))
    |> integer(min: 1, max: 3)
    |> ignore(string(")"))
    |> tag(:mul)

  enable =
    ignore(string("do()"))
    |> tag(:do)

  disable =
    ignore(string("don't()"))
    |> tag(:dont)

  defparsec(:program, choice([mul, enable, disable]) |> eventually() |> repeat())

  def nimble_p2() do
    Util.read_input(3) |> Enum.map(&nimble_solve/1) |> Enum.sum()
  end

  def nimble_solve(line) do
    {:ok, instructions, _rest, _context, _line, _offset} = program(line)

    run2(instructions)
  end

  def run2(mem, result \\ 0, enabled \\ true)
  def run2([{:do, _args} | mem], result, _enabled), do: run2(mem, result, true)
  def run2([{:dont, _args} | mem], result, _enabled), do: run2(mem, result, false)
  def run2([{:mul, [a, b]} | mem], result, true), do: run2(mem, result + a * b, true)
  def run2([_disabled | mem], result, false), do: run2(mem, result, false)
  def run2([], result, _enabled), do: result
end
