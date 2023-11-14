defmodule AdventOfCode.Day9Test do
  alias AdventOfCode.Day9
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 13 = Day9.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input2() end)
    assert 36 = Day9.p2()
  end

  defp test_input do
    """
    R 4
    U 4
    L 3
    D 1
    R 4
    D 1
    L 5
    R 2
    """
  end

  defp test_input2 do
    """
    R 5
    U 8
    L 8
    D 3
    R 17
    D 10
    L 25
    U 20
    """
  end
end
