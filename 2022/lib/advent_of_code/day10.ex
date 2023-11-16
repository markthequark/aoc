defmodule AdventOfCode.Day10 do
  @moduledoc """
  Solution to Day 10: Cathode-Ray Tube.
  https://adventofcode.com/2022/day/10
  """
  alias AdventOfCode.Util

  def p1() do
    instructions = Util.read_input(10)
    state = %{cycle: 0, x: 1, signal_strengths: [], crt_out: []}

    instructions
    |> Enum.reduce(state, &process_instruction/2)
    |> Map.get(:signal_strengths)
    |> Enum.sum()
  end

  def p2() do
    instructions = Util.read_input(10)
    state = %{cycle: 0, x: 1, signal_strengths: [], crt_out: []}

    instructions
    |> Enum.reduce(state, &process_instruction/2)
    |> Map.get(:crt_out)
    |> :erlang.iolist_to_binary()
    |> String.reverse()
    |> tap(&IO.puts/1)
  end

  def process_instruction("addx " <> value, state) do
    state = process_instruction("noop", state)
    process_instruction("ADDX " <> value, state)
  end

  def process_instruction(instruction, state) do
    state
    |> before_process_instruction()
    |> do_process_instruction(instruction)
    |> after_process_instruction()
  end

  def do_process_instruction(state, "ADDX " <> value) do
    value = String.to_integer(value)
    Map.update!(state, :x, &(&1 + value))
  end

  def do_process_instruction(state, "noop") do
    state
  end

  def before_process_instruction(state) do
    state
    |> track_signal_strength()
    |> draw_crt_newline()
    |> draw_crt_pixel()
  end

  def after_process_instruction(state) do
    state
    |> Map.update!(:cycle, &(&1 + 1))
  end

  def track_signal_strength(%{cycle: cycle} = state)
      when (cycle + 1) in [20, 60, 100, 140, 180, 220] do
    signal_strength = (cycle + 1) * state.x
    Map.update!(state, :signal_strengths, &[signal_strength | &1])
  end

  def track_signal_strength(state) do
    state
  end

  def draw_crt_newline(%{cycle: cycle} = state) when rem(cycle, 40) == 0 do
    Map.update!(state, :crt_out, &["\n" | &1])
  end

  def draw_crt_newline(state) do
    state
  end

  def draw_crt_pixel(state) do
    sprite_range = (state.x - 1)..(state.x + 1)
    draw_pos = rem(state.cycle, 40)
    pixel = if draw_pos in sprite_range, do: "#", else: "."

    Map.update!(state, :crt_out, &[pixel | &1])
  end
end
