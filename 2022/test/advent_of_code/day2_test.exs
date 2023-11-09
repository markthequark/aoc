defmodule AdventOfCode.Day2Test do
  alias AdventOfCode.Day2
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 15 = Day2.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 12 = Day2.p2()
  end

  defp test_input do
    """
    A Y
    B X
    C Z
    """
  end
end
