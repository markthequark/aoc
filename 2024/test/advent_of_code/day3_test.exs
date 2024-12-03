defmodule AdventOfCode.Day3Test do
  alias AdventOfCode.Day3
  use MyCase
  import Mox

  setup :verify_on_exit!

  test "part1" do
    assert 161 = Day3.p1()
  end

  test "part2" do
    input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    expect(MockFile, :read!, fn _filename -> input end)

    assert 48 = Day3.p2()
  end
end
