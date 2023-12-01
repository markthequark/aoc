defmodule AdventOfCode.Util do
  @moduledoc """
  Utility functions shared between challenges
  """
  def read_input(day_number, type \\ :string, options \\ [trim: true])

  def read_input(day_number, :string, options) do
    day_number
    |> input_filepath()
    |> File.read!()
    |> String.split("\n", options)
  end

  def read_input(day_number, :integer, options) do
    read_input(day_number, :string, options)
    |> Enum.map(&String.to_integer/1)
  end

  def read_input(day_number, :charlist, options) do
    read_input(day_number, :string, options)
    |> Enum.map(&String.to_charlist/1)
  end

  # lines of numbers "1234" are parsed as a list of digits [1, 2, 3, 4]
  def read_input(day_number, :digits, options) do
    for line <- read_input(day_number, :string, options) do
      String.codepoints(line)
      |> Enum.map(&String.to_integer/1)
    end
  end

  def input_filepath(day_number) do
    case Application.get_env(:advent_of_code, :input_type) do
      :test -> "test/input/day#{day_number}.txt"
      :non_test -> "priv/input/day#{day_number}.txt"
    end
  end
end
