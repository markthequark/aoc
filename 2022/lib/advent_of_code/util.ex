defmodule AdventOfCode.Util do
  @moduledoc """
  Utility functions shared between challenges
  """
  @callback read!(Path.t()) :: binary()

  def read_input(day_number, type \\ :string, options \\ [trim: true])

  def read_input(day_number, :string, options) do
    filepath = "priv/day#{day_number}.txt"

    read!(filepath)
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

  ## Mockable functions
  def read!(filepath), do: impl().read!(filepath)
  defp impl(), do: Application.get_env(:advent_of_code, :file_module, File)
end
