defmodule AdventOfCode.Day14Test do
  alias AdventOfCode.Day14
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 24 = Day14.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 93 = Day14.p2()
  end

  defp test_input() do
    # line 1: 5 rocks
    # line 2: 
    """
    498,4 -> 498,6 -> 496,6
    503,4 -> 502,4 -> 502,9 -> 494,9
    """
  end
end
