defmodule AdventOfCode.Day4Test do
  alias AdventOfCode.Day4
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 2 = Day4.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 4 = Day4.p2()
  end

  defp test_input do
    """
    2-4,6-8
    2-3,4-5
    5-7,7-9
    2-8,3-7
    6-6,4-6
    2-6,4-8
    """
  end
end
