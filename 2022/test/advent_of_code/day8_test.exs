defmodule AdventOfCode.Day8Test do
  alias AdventOfCode.Day8
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 21 = Day8.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 8 = Day8.p2()
  end

  defp test_input do
    """
    30373
    25512
    65332
    33549
    35390
    """
  end
end
