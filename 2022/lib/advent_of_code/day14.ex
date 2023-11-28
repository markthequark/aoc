defmodule AdventOfCode.Day14 do
  @moduledoc """
  Solution to Day 14: Regolith Reservoir
  https://adventofcode.com/2022/day/14
  """
  alias AdventOfCode.Util

  def p1() do
    state =
      build_state()
      |> Map.put(:part, 1)
      |> add_sand()

    state.sand_count
  end

  def p2() do
    state =
      build_state()
      |> Map.put(:part, 2)
      |> add_sand()

    state.sand_count
  end

  def add_sand(state, position \\ {500, 0}) do
    case can_sand_fall(state, position) do
      {true, new_position} ->
        add_sand(state, new_position)

      false ->
        if position == {500, 0} do
          # sand can't fall from the source. stop adding sand
          state
          |> track_sand_stopped_falling(position)
        else
          state
          |> track_sand_stopped_falling(position)
          |> add_sand()
        end

      :floor ->
        case state.part do
          1 ->
            # sand is endlessly falling into void. stop adding sand
            state

          2 ->
            # sand hit floor, add more sand from origin
            state
            |> track_sand_stopped_falling(position)
            |> add_sand()
        end
    end
  end

  def can_sand_fall(state, {x, y}) do
    below = {x, y + 1}
    left = {x - 1, y + 1}
    right = {x + 1, y + 1}

    cond do
      y > state.floor -> :floor
      not MapSet.member?(state.sand_rock_set, below) -> {true, below}
      not MapSet.member?(state.sand_rock_set, left) -> {true, left}
      not MapSet.member?(state.sand_rock_set, right) -> {true, right}
      true -> false
    end
  end

  def track_sand_stopped_falling(state, position) do
    state
    |> Map.update!(:sand_count, &(&1 + 1))
    |> Map.update!(:sand_rock_set, &MapSet.put(&1, position))
  end

  def build_state() do
    input = Util.read_input(14)

    state0 = %{
      prev_corner: {nil, nil},
      sand_rock_set: MapSet.new(),
      sand_count: 0,
      floor: nil,
      part: nil
    }

    state =
      for rock_path <- parse_input(input), [x, y] <- rock_path, reduce: state0 do
        %{prev_corner: {^rock_path, {x2, y2}}} = state ->
          for x_new <- x..x2, y_new <- y..y2, reduce: state do
            state -> Map.update!(state, :sand_rock_set, &MapSet.put(&1, {x_new, y_new}))
          end
          |> Map.put(:prev_corner, {rock_path, {x, y}})

        state ->
          %{state | prev_corner: {rock_path, {x, y}}}
      end

    floor =
      state.sand_rock_set
      |> Stream.map(fn {_x, y} -> y end)
      |> Enum.max()

    %{state | floor: floor}
  end

  def parse_input(lines) do
    lines
    |> Stream.map(fn line ->
      String.split(line, [" -> ", ","])
      |> Stream.map(&String.to_integer/1)
      |> Stream.chunk_every(2)
    end)
  end
end
