defmodule AdventOfCode.Day2Test do
  alias AdventOfCode.Day2
  use ExUnit.Case

  test "part1" do
    assert 8 = Day2.p1()
  end

  test "part2" do
    assert 2286 = Day2.p2()
  end
end
