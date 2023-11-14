defmodule AdventOfCode.Day9 do
  @moduledoc """
  Solution to Day 9: Rope Bridge.
  https://adventofcode.com/2022/day/9
  """
  alias AdventOfCode.Util

  def p1() do
    state = %{head: {0, 0}, tail: {0, 0}, tail_visited: MapSet.new()}
    solve(state)
  end

  def p2() do
    state = %{knots: List.duplicate({0, 0}, 10), tail_visited: MapSet.new()}
    solve(state)
  end

  def solve(state0) do
    motions = Util.read_input(9)

    for <<direction::binary-size(1), " ", amount::binary>> <- motions,
        _ <- 1..String.to_integer(amount),
        reduce: state0 do
      state ->
        state
        |> move_knots(direction)
        |> track_tail_position()
    end
    |> Map.get(:tail_visited)
    |> MapSet.size()
  end

  def track_tail_position(%{tail: tail} = state) do
    Map.update!(state, :tail_visited, &MapSet.put(&1, tail))
  end

  def track_tail_position(%{knots: knots} = state) do
    Map.update!(state, :tail_visited, &MapSet.put(&1, List.last(knots)))
  end

  def move_knots(%{knots: [head | rest]} = state, direction) do
    new_head = move_knot(head, direction)

    new_knots =
      for knot <- rest, reduce: [new_head] do
        knots ->
          leader = hd(knots)
          new_knot = move_knot(leader, knot)
          [new_knot | knots]
      end
      |> Enum.reverse()

    %{state | knots: new_knots}
  end

  def move_knots(%{head: head, tail: tail} = state, direction) do
    new_head = move_knot(head, direction)
    new_tail = move_knot(new_head, tail)

    %{state | head: new_head, tail: new_tail}
  end

  def move_knot({x, y}, "U"), do: {x, y + 1}
  def move_knot({x, y}, "D"), do: {x, y - 1}
  def move_knot({x, y}, "L"), do: {x - 1, y}
  def move_knot({x, y}, "R"), do: {x + 1, y}

  def move_knot(leading_knot, knot) do
    {x, y} = knot
    delta_x = delta_x(leading_knot, knot)
    delta_y = delta_y(leading_knot, knot)

    case {abs(delta_x), abs(delta_y)} do
      {2, 0} -> {x + div(delta_x, 2), y}
      {0, 2} -> {x, y + div(delta_y, 2)}
      {2, 1} -> {x + div(delta_x, 2), y + delta_y}
      {1, 2} -> {x + delta_x, y + div(delta_y, 2)}
      {2, 2} -> {x + div(delta_x, 2), y + div(delta_y, 2)}
      _ -> knot
    end
  end

  def delta_x({x, _y}, {x2, _y2}), do: x - x2
  def delta_y({_x, y}, {_x2, y2}), do: y - y2

  ## debug functions

  def draw(state) do
    max_x = 26
    max_y = 21
    IO.puts("\n")

    for y <- max_y..1, x <- 1..max_x do
      if x == 1, do: IO.puts("")
      do_draw(state, {x, y})
    end

    IO.puts("\n")
  end

  def do_draw(state, pos) do
    candidate_positions = state.knots ++ [{12, 6}]
    candidate_symbols = ["H" | 1..9 |> Enum.map(&to_string/1)] ++ ["s"]
    candidates = Enum.zip(candidate_positions, candidate_symbols)

    for {candidate_pos, candidate_symbol} <- candidates,
        pos == candidate_pos,
        reduce: "." do
      "." -> candidate_symbol
      symbol -> symbol
    end
    |> IO.write()
  end
end
