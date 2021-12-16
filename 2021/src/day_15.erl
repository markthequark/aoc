-module(day_15).
-format(ignore).
-export([p1/1, p2/1]).
-compile(export_all).
-define(INFINITY, 576_460_752_303_423_487).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Input = helper:read_lines(Filename, string),
  Grid = to_grid(Input),
  dijkstra(Grid, {1, 1}, {length(hd(Input)), length(Input)}).

p2(Filename) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

dijkstra(Grid, Start, End) ->
  Unvisited = Grid,
  Distances0 = maps:map(fun(_, _) -> ?INFINITY end, Grid),
  Distances = Distances0#{Start := 0},

  dijkstra(Grid, Start, End, Unvisited, Distances).

dijkstra(_Grid, End, End, _Unvisited, Distances) ->
  map_get(End, Distances);
dijkstra(Grid, Cur, End, Unvisited, Distances) ->
  Neighbours = [N || N <- neighbours(Cur, Grid), sets:is_element(N, Unvisited)],

  NewDistances = lists:foldl(
    fun(N, Distances) ->
      NewDist = min(map_get(N, Distances), map_get(Cur, Distances) + map_get(N, Grid)),
      Distances#{N := NewDist}
    end,
    Distances,
    Neighbours),

  NewUnvisited = sets:del_element(Cur, Unvisited),
  Next = helper:min_by(fun(Elem) -> map_get(Elem, NewDistances) end, maps:keys(NewUnvisited)),

  dijkstra(Grid, Next, End, NewUnvisited, NewDistances).

to_grid(ListOfLists) ->
  maps:from_list([
    {{X, Y}, Val - $0}
    ||
    {Y, Row} <- lists:zip(lists:seq(1, length(ListOfLists)), ListOfLists),
    {X, Val} <- lists:zip(lists:seq(1, length(Row)), Row)
  ]).

neighbours({X, Y}, Grid) ->
  Offsets = [{1, 0}, {0, -1}, {0, 1}, {-1, 0}],
  [Pos || {XOff, YOff} <- Offsets, Pos <- [{X + XOff, Y + YOff}], is_map_key(Pos, Grid)].
