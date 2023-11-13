defmodule AdventOfCode.Day7 do
  @moduledoc """
  Solution to Day 7: No Space Left On Device.
  https://adventofcode.com/2022/day/7
  """
  alias AdventOfCode.Util

  def p1() do
    lines = Util.read_input(7)
    file_sys = build_file_sys(lines)

    sum_dir_sizes = fn _dir_name, contents, acc ->
      if contents.total_file_size < 100_000,
        do: acc + contents.total_file_size,
        else: acc
    end

    depth_first_search(file_sys, sum_dir_sizes, 0)
  end

  def p2() do
    lines = Util.read_input(7)
    file_sys = build_file_sys(lines)

    total_space = 70_000_000
    required_space = 30_000_000
    used_space = get_in(file_sys, ["/", :total_file_size])
    need_to_free = required_space - (total_space - used_space)

    find_smallest_dir_to_delete = fn _dir_name, contents, best_dir_size ->
      dir_size = contents.total_file_size

      if dir_size >= need_to_free and dir_size < best_dir_size,
        do: dir_size,
        else: best_dir_size
    end

    depth_first_search(file_sys, find_smallest_dir_to_delete, used_space)
  end

  def depth_first_search(file_sys, fun, acc0) do
    for {dir_name, contents} <- file_sys, is_map(contents), reduce: acc0 do
      acc ->
        acc = depth_first_search(contents, fun, acc)
        fun.(dir_name, contents, acc)
    end
  end

  def build_file_sys(lines) do
    state = %{cur_dir: nil, file_sys: %{} |> add_dir(["/"])}

    state =
      for line <- lines, reduce: state do
        state ->
          case line do
            "$ cd " <> dir ->
              process_cd(dir, state)

            "$ ls" ->
              state

            _ ->
              process_ls(line, state)
          end
      end

    state.file_sys
  end

  def process_cd("/", state) do
    %{state | cur_dir: ["/"]}
  end

  def process_cd("..", state) do
    Map.update!(state, :cur_dir, &tl/1)
  end

  def process_cd(dir, state) do
    Map.update!(state, :cur_dir, &[dir | &1])
  end

  def process_ls("dir " <> dir_name, state) do
    dir_path = Enum.reverse([dir_name | state.cur_dir])
    Map.update!(state, :file_sys, &add_dir(&1, dir_path))
  end

  def process_ls(file, state) do
    [file_size, file_name] = String.split(file, " ")
    file_size = String.to_integer(file_size)
    dir_path = Enum.reverse(state.cur_dir)
    Map.update!(state, :file_sys, &add_file(&1, dir_path, file_name, file_size))
  end

  def add_dir(file_sys, dir_path) do
    if get_in(file_sys, dir_path) == nil do
      put_in(file_sys, dir_path, %{total_file_size: 0, file_names: []})
    else
      file_sys
    end
  end

  def add_file(file_sys, dir_path, file_name, file_size) do
    existing_files = get_in(file_sys, dir_path ++ [:file_names])

    if file_name not in existing_files do
      do_add_file(file_sys, dir_path, file_name, file_size)
    else
      file_sys
    end
  end

  def do_add_file(file_sys, dir_path, file_name, file_size) do
    file_sys = update_in(file_sys, dir_path ++ [:file_names], &[file_name | &1])

    # for each directory in the dir_path, update total_file_size
    # e.g. for dir_path ["/", "outer", "inner"]
    # we'll update ["/"] then ["/", "outer"] then ["/", "outer", "inner"]
    for n <- 1..length(dir_path), reduce: file_sys do
      file_sys ->
        path = Enum.take(dir_path, n)
        update_in(file_sys, path ++ [:total_file_size], &(&1 + file_size))
    end
  end
end
