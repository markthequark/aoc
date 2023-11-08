defmodule AdventOfCode.Util do
  @moduledoc """
  Utility functions shared between challenges
  """

  def read_input(day_number_or_filepath, type \\ :string, options \\ [trim: true])

  def read_input(day_number, type, options) when is_integer(day_number) do
    filepath = "priv/day#{day_number}.txt"
    read_input(filepath, type, options)
  end

  def read_input(filepath, :string, options) do
    File.read!(filepath)
    |> String.split("\n", options)
  end

  def read_input(filepath, :integer, options) do
    read_input(filepath, :string, options)
    |> Enum.map(&String.to_integer/1)
  end
end
