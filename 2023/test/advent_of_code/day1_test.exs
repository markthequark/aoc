defmodule AdventOfCode.Day1Test do
  alias AdventOfCode.Day1
  use ExUnit.Case
  import Mox

  setup :verify_on_exit!

  test "part1" do
    setup_mock()
    assert 142 = Day1.p1()
    teardown_mock()
  end

  test "part2" do
    assert 281 = Day1.p2()
  end

  def setup_mock() do
    Mox.defmock(MockFile, for: AdventOfCode.Util)
    Application.put_env(:advent_of_code, :file_module, MockFile)
    stub(MockFile, :read!, fn _filename -> part1_input() end)
  end

  def teardown_mock() do
    Application.put_env(:advent_of_code, :file_module, File)
  end

  def part1_input() do
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """
  end
end
