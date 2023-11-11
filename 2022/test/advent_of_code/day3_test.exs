defmodule AdventOfCode.Day3Test do
  alias AdventOfCode.Day3
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 157 = Day3.p1()
  end

  test "part2" do
    expect(MockFile, :read!, fn _filename -> test_input() end)
    assert 70 = Day3.p2()
  end

  defp test_input do
    """
    vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw
    """
  end
end
