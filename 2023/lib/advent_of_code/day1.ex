defmodule AdventOfCode.Day1 do
  @moduledoc """
  Solution to Day 1: Trebuchet?!
  https://adventofcode.com/2023/day/1
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(1, :charlist)

    for line <- lines do
      numbers =
        for char <- line, char in ?0..?9 do
          char
        end

      [List.first(numbers), List.last(numbers)]
      |> :string.to_integer()
      |> elem(0)
    end
    |> Enum.sum()
  end

  def p2() do
    lines = Util.read_input(1)

    for line <- lines do
      first = find_first_number(line)
      last = find_reversed_first_number(String.reverse(line))

      first * 10 + last
    end
    |> Enum.sum()
  end

  def find_first_number(<<char, rest::binary>>) when char in ?0..?9 do
    char - ?0
  end

  def find_first_number("zero" <> rest), do: 0
  def find_first_number("one" <> rest), do: 1
  def find_first_number("two" <> rest), do: 2
  def find_first_number("three" <> rest), do: 3
  def find_first_number("four" <> rest), do: 4
  def find_first_number("five" <> rest), do: 5
  def find_first_number("six" <> rest), do: 6
  def find_first_number("seven" <> rest), do: 7
  def find_first_number("eight" <> rest), do: 8
  def find_first_number("nine" <> rest), do: 9

  def find_first_number(<<_, rest::binary>>), do: find_first_number(rest)

  def find_reversed_first_number(<<char, rest::binary>>) when char in ?0..?9 do
    char - ?0
  end

  def find_reversed_first_number("orez" <> rest), do: 0
  def find_reversed_first_number("eno" <> rest), do: 1
  def find_reversed_first_number("owt" <> rest), do: 2
  def find_reversed_first_number("eerht" <> rest), do: 3
  def find_reversed_first_number("ruof" <> rest), do: 4
  def find_reversed_first_number("evif" <> rest), do: 5
  def find_reversed_first_number("xis" <> rest), do: 6
  def find_reversed_first_number("neves" <> rest), do: 7
  def find_reversed_first_number("thgie" <> rest), do: 8
  def find_reversed_first_number("enin" <> rest), do: 9

  def find_reversed_first_number(<<_, rest::binary>>), do: find_reversed_first_number(rest)
end
