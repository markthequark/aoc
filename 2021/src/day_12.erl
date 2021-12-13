-module(day_12).

-format(ignore).

%% API
-export([p1/1, p2/1]).
-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Graph = to_graph(Filename),
  get_num_paths_p1(Graph, <<"start">>, <<"end">>).

p2(Filename) ->
  Graph = to_graph(Filename),
  get_num_paths_p2(Graph, <<"start">>, <<"end">>).

%%====================================================================
%% Internal functions
%%====================================================================

to_graph(Filename) ->
  Input = helper:read_lines(Filename, binary),
  Edges = lists:map(fun(Line) -> string:split(Line, "-") end, Input),
  Vertices = ordsets:from_list(lists:flatten(Edges)),
  Graph = digraph:new(),
  lists:foreach(fun(V) -> digraph:add_vertex(Graph, V) end, Vertices),
  lists:foreach(fun([V1, V2]) ->
                  digraph:add_edge(Graph, V1, V2),
                  digraph:add_edge(Graph, V2, V1)
                end,
                Edges),
  Graph.

%%====================================================================
%% Part 1 - BFS
%%====================================================================

get_num_paths_p1(Graph, Start, End) ->
  get_num_paths_p1(Graph, End, [[Start]], 0).

get_num_paths_p1(_Graph, _End, [], Acc) ->
  Acc;
get_num_paths_p1(Graph, End, Paths, Acc) ->
  AllPaths = helper:flatten_n(1,
    [
      [
        case NeighbourCave of
          End -> finished;
          _ -> [NeighbourCave | Path]
        end
        ||
        NeighbourCave <- digraph:out_neighbours(Graph, hd(Path)),
        can_visit_p1(NeighbourCave, Path)
      ]
      ||
      Path <- Paths
    ]),

  {FinishedPaths, NewPaths} = lists:partition(fun(Path) -> Path == finished end, AllPaths),
  get_num_paths_p1(Graph, End, NewPaths, Acc + length(FinishedPaths)).

can_visit_p1(Cave, Path) ->
  not is_small(Cave)
  orelse not lists:member(Cave, Path).

%%====================================================================
%% Part 2 - DFS
%%====================================================================

get_num_paths_p2(Graph, Start, End) ->
  get_num_paths_p2(Graph, End, [[Start]], 0).

get_num_paths_p2(Graph, _End, [], NumPaths) ->
  NumPaths;
get_num_paths_p2(Graph, End, [Path | Rest], NumPaths) ->
  Paths = [
    case NeighbourCave of
      End -> finished;
      _ -> [NeighbourCave | Path]
    end
    ||
    NeighbourCave <- digraph:out_neighbours(Graph, hd(Path)),
    can_visit_p2(NeighbourCave, Path)
  ],

  {FinishedPaths, NewPaths} = lists:partition(fun(Path) -> Path == finished end, Paths),
  get_num_paths_p2(Graph, End, NewPaths ++ Rest, NumPaths + length(FinishedPaths)).

can_visit_p2(Cave, Path) ->
  not is_small(Cave)
  orelse not lists:member(Cave, Path)
  orelse not (Cave == <<"start">>
    orelse Cave == <<"end">>
    orelse visited_small_cave_twice_already(Path)).

visited_small_cave_twice_already(Path) ->
  SmallCaves = lists:filter(fun is_small/1, Path),
  length(SmallCaves) /= sets:size(sets:from_list(SmallCaves)).

is_small(<<X:8, _/binary>>) ->
  X >= $a andalso X =< $z.
