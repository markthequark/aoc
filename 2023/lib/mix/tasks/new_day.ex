defmodule Mix.Tasks.NewDay do
  @moduledoc """
  Generate files for a new day
  """
  use Mix.Task

  @requirements ["app.config", "app.start"]

  def run(_args) do
    day = next_day_number()

    create_non_test_input_file(day)
    create_test_input_file(day)
    create_lib_module(day)
    create_test_module(day)
    update_iex_exs(day)
  end

  def next_day_number() do
    for filename <- File.ls!("lib/advent_of_code/"),
        match?(<<"day", _, ".ex">>, filename) or match?(<<"day", _, _, ".ex">>, filename) do
      filename
      |> String.trim_leading("day")
      |> String.trim_trailing(".ex")
      |> String.to_integer()
    end
    |> then(&[0 | &1])
    |> Enum.max()
    |> then(&(&1 + 1))
  end

  def create_non_test_input_file(day) do
    contents =
      aoc_web_request()
      |> Req.get!(url: "/2023/day/#{day}/input")
      |> then(& &1.body)

    File.write!("priv/input/day#{day}.txt", contents)
  end

  def create_test_input_file(day) do
    contents = test_input(day)

    File.write!("test/input/day#{day}.txt", contents)
  end

  def create_lib_module(day) do
    contents = """
    defmodule AdventOfCode.Day#{day} do
      @moduledoc \"""
      Solution to #{day_title(day)}
      https://adventofcode.com/2023/day/#{day}
      \"""
      alias AdventOfCode.Util

      def p1() do
        lines = Util.read_input(#{day})
        :not_impl
      end

      def p2() do
        lines = Util.read_input(#{day})
        :not_impl
      end
    end
    """

    File.write!("lib/advent_of_code/day#{day}.ex", contents)
  end

  def create_test_module(day) do
    contents = """
    defmodule AdventOfCode.Day#{day}Test do
      alias AdventOfCode.Day#{day}
      use ExUnit.Case

      test "part1" do
        assert _ = Day#{day}.p1()
      end

      test "part2" do
        assert _ = Day#{day}.p2()
      end
    end
    """

    File.write!("test/advent_of_code/day#{day}_test.exs", contents)
  end

  def update_iex_exs(day) do
    contents = "alias AdventOfCode.Day#{day}\n"

    File.write!(".iex.exs", contents, [:append])
  end

  def day_title(day_number) do
    aoc_web_request()
    |> Req.get!(url: "/2023/day/#{day_number}")
    |> then(& &1.body["h2"])
    |> to_string()
    |> String.trim_leading("--- ")
    |> String.trim_trailing(" ---")
  end

  def aoc_web_request() do
    cookie = Application.get_env(:advent_of_code, :aoc_session_cookie)

    Req.new(base_url: "https://adventofcode.com/", headers: %{cookie: cookie})
    |> ReqEasyHTML.attach()
  end

  def test_input(day_number) do
    day_desc_elems =
      aoc_web_request()
      |> Req.get!(url: "/2023/day/#{day_number}")
      |> then(&(&1.body["html body main article.day-desc"].nodes |> hd() |> elem(2)))

    [{"code", _, [input]}] = find_test_input_elem(day_desc_elems)
    input
  end

  # get the html element immediately after the first one to contain the string "For example"
  def find_test_input_elem(day_desc_elems) do
    {:halt, test_input_elem} =
      for {_elem_type, _properties, contents} <- day_desc_elems, reduce: [] do
        {:halt, _} = result ->
          result

        prev_contents ->
          default = contents

          Enum.find_value(prev_contents, default, fn
            text when is_binary(text) ->
              if text =~ "For example" or
                   text =~ "Here is an example " do
                {:halt, contents}
              end

            _ ->
              nil
          end)
      end

    test_input_elem
  end
end
