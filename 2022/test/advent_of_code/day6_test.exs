defmodule AdventOfCode.Day6Test do
  alias AdventOfCode.Day6
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 7 = Day6.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 19 = Day6.p2()
  end

  defp test_input do
    """
    mjqjpqmgbljsphdztnvjfqwrcgsmlb
    """
  end
end
