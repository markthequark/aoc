IEx.configure(inspect: [charlists: :as_lists])
Application.put_env(:advent_of_code, :input_type, :test)

alias Mix.Tasks.NewDay
alias AdventOfCode.Util
alias AdventOfCode.Day1
