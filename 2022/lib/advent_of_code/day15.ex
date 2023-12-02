defmodule AdventOfCode.Day15 do
  @moduledoc """
  Solution to Day 15: Beacon Exclusion Zone
  https://adventofcode.com/2022/day/15
  """
  alias AdventOfCode.Util

  def p1(target_y \\ 2_000_000) do
    lines = Util.read_input(15)

    for line <- lines do
      reading = parse_line(line)
      distance = manhattan_distance(reading)
      Map.put_new(reading, :distance, distance)
    end
    |> scan_line_y_equals(target_y)
    |> then(fn [first..last] ->
      abs(first - last)
    end)
  end

  def p2(max_coordinate \\ 4_000_000) do
    lines = Util.read_input(15)

    readings =
      for line <- lines do
        reading = parse_line(line)
        distance = manhattan_distance(reading)
        Map.put_new(reading, :distance, distance)
      end

    {x, y} =
      Enum.find_value(0..max_coordinate, fn y ->
        case scan_line_y_equals(readings, y) do
          [_] -> false
          [range1, _range2] -> {range1.last + 1, y}
        end
      end)

    tuning_frequency(x, y)
  end

  def intersects_line_y_equals?(reading, target_y) do
    y = reading.sensor.y
    dist = reading.distance

    target_y in (y - dist)..(y + dist)
  end

  # returns a minimal list of ranges of x coordinates the distress beacon cannot be on
  # in the line y=`target_y`
  #
  # For example, to get the list of ranges of x coordinates the distress beacon cannot be on
  # for the line y=10 we call scan_line_y_equals(readings, 10)
  # which may return [-3..15, 17..23],
  # which would indicate the distress beacon has coordinates {16, 10}
  # or if the return had a single range such as [-3..23],
  # then we know the distress beacon is not on the line y=10
  def scan_line_y_equals(readings, target_y) do
    for reading <- readings, intersects_line_y_equals?(reading, target_y) do
      x = reading.sensor.x
      y = reading.sensor.y
      delta_x = reading.distance - abs(target_y - y)

      (x - delta_x)..(x + delta_x)
    end
    |> merge_ranges()
  end

  def parse_line(line) do
    [x, y, x2, y2] =
      String.split(
        line,
        ["Sensor at x=", ", y=", ": closest beacon is at x=", ", y="],
        trim: true
      )
      |> Enum.map(&String.to_integer/1)

    %{sensor: %{x: x, y: y}, beacon: %{x: x2, y: y2}}
  end

  def manhattan_distance(reading) do
    abs(reading.sensor.x - reading.beacon.x) + abs(reading.sensor.y - reading.beacon.y)
  end

  def merge_ranges(ranges) do
    ranges
    |> Enum.sort()
    |> Enum.reduce(nil, fn
      range, nil ->
        [range]

      range, [merged_range | rest] = acc ->
        if can_merge?(merged_range, range) do
          [merge_range(merged_range, range) | rest]
        else
          [range | acc]
        end
    end)
    |> Enum.reverse()
  end

  def can_merge?(first1..last1//1 = range1, first2..last2//1 = range2) do
    min(last1, last2) + 1 == max(first1, first2) or
      not Range.disjoint?(range1, range2)
  end

  def merge_range(first1..last1//1, first2..last2//1) do
    min(first1, first2)..max(last1, last2)
  end

  def tuning_frequency(x, y) do
    x * 4_000_000 + y
  end
end
