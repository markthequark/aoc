defmodule AdventOfCode.Day1Test do
  alias AdventOfCode.Day1
  use ExUnit.Case

  test "part1" do
    assert 11 = Day1.p1()
  end

  test "part2" do
    assert 31 = Day1.p2()
  end
end
