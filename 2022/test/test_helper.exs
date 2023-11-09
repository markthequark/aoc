ExUnit.start()

Mox.defmock(MockFile, for: AdventOfCode.Util)
Application.put_env(:advent_of_code, :file_module, MockFile)
