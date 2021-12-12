-module(day_12).

-format(ignore).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
%%  Graph = to_graph(Filename),
%%  get_num_paths(Graph, <<"start">>, <<"end">>, p1).
  ok.

p2(Filename) ->
%%  Graph = to_graph(Filename),
  Graph = to_graph("priv/test"),
  get_num_paths(Graph, <<"start">>, <<"end">>, p2).

%%====================================================================
%% Internal functions
%%====================================================================

to_graph(Filename) ->
  Input = helper:read_lines(Filename, binary),
  Edges = lists:map(fun(Line) -> string:split(Line, "-") end, Input),
  Vertices = unique(lists:flatten(Edges)),
  G = digraph:new(),
  lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, Vertices),
  lists:foreach(fun([V1, V2]) ->
                  digraph:add_edge(G, V1, V2),
                  digraph:add_edge(G, V2, V1)
                end,
                Edges),
  G.

get_num_paths(G, Start, End, Part) ->
  Paths = [[Start]],
  Acc = 0,
  get_num_paths(G, End, Paths, Acc, Part).

get_num_paths(_G, _End, [], Acc, _Part) ->
  Acc;
get_num_paths(G, End, Paths, Acc, Part) ->
  io:format("number of finished paths: ~p~n", [Acc]),
  io:format("reamining paths: ~p~n",
    [lists:map(fun(Path) -> lists:map(fun binary_to_list/1, Path) end, Paths)]),
  AllPaths = flatten_n(1, [
    [
      [NeighbourCave | Path]
      ||
      NeighbourCave <- digraph:out_neighbours(G, hd(Path)),
      can_visit(NeighbourCave, Path, Part)
    ]
    ||
    Path <- Paths
  ]),

  {FinishedPaths, NewPaths} = lists:partition(fun(Path) -> hd(Path) == End end, AllPaths),

  get_num_paths(G, End, NewPaths, Acc + length(FinishedPaths), Part).

-compile(export_all).
can_visit(Cave, Path, Part) ->
  case {Part, is_small(Cave)} of
    {p1, true} ->
      not lists:member(Cave, Path);
    {p2, true} ->
      not lists:member(Cave, Path)
      orelse not (lists:member(Cave, [<<"start">>, <<"end">>])
        orelse visited_small_cave_twice_already(Path));
    {_, false} ->
      true
  end.

visited_small_cave_twice_already(Path) ->
  SmallCaves = lists:filter(fun is_small/1, Path),
  length(SmallCaves) /= length(unique(SmallCaves)).

is_small(Cave) when is_binary(Cave) ->
  String = binary_to_list(Cave),
  string:to_lower(String) == String.

unique(List) ->
  sets:to_list(sets:from_list(List)).

%% @doc flatten a list by n levels
%% Examples:
%% > flatten_n(1, [["a", "b"], ["c", "d"]]).
%% ["a", "b", "c", "d"]
%% > flatten_n(2, [["a", "b"], ["c", "d"]]).
%% "abcd"
flatten_n(0, ListOfLists) ->
  ListOfLists;
flatten_n(N, ListOfLists) ->
  NewList = lists:foldl(fun lists:merge/2, hd(ListOfLists), tl(ListOfLists)),
  flatten_n(N - 1, NewList).
