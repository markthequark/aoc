defmodule AdventOfCode.Day3Test do
  alias AdventOfCode.Day3
  use ExUnit.Case

  test "part1" do
    assert 4361 = Day3.p1()
  end

  test "part2" do
    assert 467_835 = Day3.p2()
  end
end
