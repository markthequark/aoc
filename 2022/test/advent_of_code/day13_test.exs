defmodule AdventOfCode.Day13Test do
  alias AdventOfCode.Day13
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 13 = Day13.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 140 = Day13.p2()
  end

  defp test_input() do
    """
    [1,1,3,1,1]
    [1,1,5,1,1]

    [[1],[2,3,4]]
    [[1],4]

    [9]
    [[8,7,6]]

    [[4,4],4,4]
    [[4,4],4,4,4]

    [7,7,7,7]
    [7,7,7]

    []
    [3]

    [[[]]]
    [[]]

    [1,[2,[3,[4,[5,6,7]]]],8,9]
    [1,[2,[3,[4,[5,6,0]]]],8,9]
    """
  end
end
