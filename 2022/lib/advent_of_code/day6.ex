defmodule AdventOfCode.Day6 do
  @moduledoc """
  Solution to Day 6: Tuning Trouble.
  https://adventofcode.com/2022/day/6
  """
  alias AdventOfCode.Util

  def p1() do
    [input] = Util.read_input(6)
    start_of_packet_marker(input)
  end

  def p2() do
    [input] = Util.read_input(6)
    start_of_message_marker(input)
  end

  def start_of_packet_marker(<<a, b, c, d, rest::binary>>, counter \\ 4) do
    if all_unique?([a, b, c, d]) do
      counter
    else
      start_of_packet_marker(<<b, c, d>> <> rest, counter + 1)
    end
  end

  def start_of_message_marker(datastream, counter \\ 14) do
    {first_14_chars, _rest} = String.split_at(datastream, 14)

    if all_unique?(String.to_charlist(first_14_chars)) do
      counter
    else
      <<_, datastream_tail::binary>> = datastream
      start_of_message_marker(datastream_tail, counter + 1)
    end
  end

  def all_unique?(enum) do
    length(enum) == length(Enum.uniq(enum))
  end
end
