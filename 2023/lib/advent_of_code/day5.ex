defmodule AdventOfCode.Day5 do
  @moduledoc """
  Solution to Day 5: If You Give A Seed A Fertilizer
  https://adventofcode.com/2023/day/5
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(5)
    almanac = parse_input(lines, :p1)

    map_from_to(almanac, almanac.seed_ids, "seed", "location")
    |> Enum.min()
  end

  def p2() do
    _lines = Util.read_input(5)
    # spent many hours on this, disaster, moving on
    :not_impl
  end

  def map_from_to(_almanac, ids, target_destination, target_destination) do
    ids
  end

  def map_from_to(almanac, source_ids, source_name, target_destination) do
    {destination_name, mappers} =
      Enum.find_value(almanac, fn
        {[^source_name, destination_name], value} -> {destination_name, value}
        _ -> nil
      end)

    default_mapper = fn id -> id end

    destination_ids =
      for id <- source_ids do
        Enum.find_value(mappers ++ [default_mapper], fn mapper ->
          mapper.(id)
        end)
      end

    map_from_to(almanac, destination_ids, destination_name, target_destination)
  end

  def parse_input(lines, part) when part in [:p1, :p2] do
    acc0 = %{current_map: nil, part: part}
    parse_input(lines, acc0)
  end

  def parse_input([], acc), do: acc

  def parse_input(["seeds: " <> seed_ids | rest], %{part: :p1} = acc) do
    seed_ids =
      String.split(seed_ids, " ")
      |> Enum.map(&String.to_integer/1)

    acc = Map.put(acc, :seed_ids, seed_ids)
    parse_input(rest, acc)
  end

  def parse_input(["seeds: " <> seed_ids | rest], %{part: :p2} = acc) do
    seed_ids =
      String.split(seed_ids, " ")
      |> Enum.map(&String.to_integer/1)

    acc = Map.put(acc, :seed_ids, seed_ids)
    parse_input(rest, acc)
  end

  def parse_input([<<c, _::binary>> = line | rest], acc) when c in ?0..?9 do
    [destination_start, source_start, range_length] =
      String.split(line, " ") |> Enum.map(&String.to_integer/1)

    mapper = fn
      id when id in source_start..(source_start + range_length) ->
        id - source_start + destination_start

      _id ->
        nil
    end

    acc = Map.update(acc, acc.current_map, [mapper], &[mapper | &1])
    parse_input(rest, acc)
  end

  def parse_input([line | rest], acc) do
    map_name =
      line
      |> String.trim_trailing(" map:")
      |> String.split("-to-")

    acc = %{acc | current_map: map_name}
    parse_input(rest, acc)
  end

  def merge_ranges() do
    min_in = 0
    max_out = 110

    source_ranges = [
      {50..97, 52..99},
      {98..99, 50..51}
    ]

    destination_ranges = [
      {0..14, 39..53},
      {15..51, 0..36},
      {52..53, 37..38}
    ]

    pass_through_source_ranges =
      for {source_in, _source_out} <- source_ranges, reduce: [min_in..max_out] do
        all_in_ranges ->
          all_in_ranges
          |> Enum.map(&subtract_range(&1, source_in))
          |> List.flatten()
      end
      |> Enum.map(&{&1, &1})

    all_source_ranges = source_ranges ++ pass_through_source_ranges

    merge_mappers(all_source_ranges, destination_ranges, [])
    |> Enum.sort()
  end

  def merge_mappers([source_mappers, destination_mappers | other_mappers]) do
    merge_mappers(source_mappers, destination_mappers, other_mappers)
  end

  def merge_mappers(source_mappers, destination_mappers, other_mappers) do
    merged_mappers =
      for {_source_in, source_out} = source_mapper <- source_mappers do
        matched_destination_mappers =
          destination_mappers
          |> Enum.reject(fn {destination_in, _destination_out} ->
            Range.disjoint?(source_out, destination_in)
          end)

        _separated_source_map = do_separate(source_mapper, matched_destination_mappers)

        matched_destination_mappers
        |> Enum.map(&do_merge_mappers(source_mapper, &1))
      end
      |> List.flatten()

    case other_mappers do
      [] ->
        merged_mappers

      [next_mappers | rest_mappers] ->
        merge_mappers(merged_mappers, next_mappers, rest_mappers)
    end
  end

  def do_separate(mapper, matched_mappers) do
    matched_mappers = matched_mappers |> Enum.sort()

    split_numbers =
      for {in_first..in_last, _out_first.._out_last} <- matched_mappers do
        [in_first, in_last]
      end
      |> List.flatten()
      |> List.delete_at(0)
      |> List.delete_at(-1)

    acc0 = %{mapper: mapper, split_parts: []}

    for split_number <- split_numbers, reduce: acc0 do
      acc ->
        {left_mapper, rest_mapper} = split_mapper(acc.mapper, split_number)

        acc
        |> Map.put(:mapper, rest_mapper)
        |> Map.update!(:split_parts, &[left_mapper | &1])
    end
  end

  def split_mapper({in_range, out_range}, split) do
    {in_range_left, in_range_rest} = Range.split(in_range, split)
    {out_range_left, out_range_rest} = Range.split(out_range, split)

    {{in_range_left, out_range_left}, {in_range_rest, out_range_rest}}
  end

  def do_merge_mappers({source_in, source_out}, {dest_in, dest_out}) do
    overlap_start = max(source_out.first, dest_in.first)
    overlap_end = min(source_out.last, dest_in.last)
    length_overlap = overlap_end - overlap_start + 1

    ## source
    # before overlap
    length_left_source_range = overlap_start - source_out.first
    {pre_overlap_source_in, rest_source_in} = Range.split(source_in, length_left_source_range)
    {pre_overlap_source_out, rest_source_out} = Range.split(source_out, length_left_source_range)

    # merged overlap and after overlap
    {overlap_source_in, post_overlap_source_in} = Range.split(rest_source_in, length_overlap)
    {_overlap_source_out, post_overlap_source_out} = Range.split(rest_source_out, length_overlap)

    ## destination
    # before overlap
    length_left_destination_range = overlap_start - dest_in.first
    {pre_overlap_dest_in, rest_dest_in} = Range.split(dest_in, length_left_destination_range)
    {pre_overlap_dest_out, rest_dest_out} = Range.split(dest_out, length_left_destination_range)

    # merged_overlap and after overlap
    {_overlap_dest_in, post_overlap_dest_in} = Range.split(rest_dest_in, length_overlap)
    {overlap_dest_out, post_overlap_dest_out} = Range.split(rest_dest_out, length_overlap)

    [
      {pre_overlap_source_in, pre_overlap_source_out},
      {pre_overlap_dest_in, pre_overlap_dest_out},
      {overlap_source_in, overlap_dest_out},
      #      {overlap_dest_in, overlap_dest_out},
      {post_overlap_source_in, post_overlap_source_out},
      {post_overlap_dest_in, post_overlap_dest_out}
    ]
    |> Enum.reject(fn {range_in, _range_out} ->
      Range.size(range_in) == 0
    end)
  end

  def subtract_range(start1..end1 = range1, start2..end2) when end2 < start1 or start2 > end1 do
    [range1]
  end

  def subtract_range(start1..end1, start2..end2) do
    case {start2 <= start1, end2 >= end1} do
      {true, true} ->
        []

      {true, false} ->
        [(end2 + 1)..end1]

      {false, true} ->
        [start1..(start2 - 1)]

      {false, false} ->
        [start1..(start2 - 1), (end2 + 1)..end1]
    end
  end
end
