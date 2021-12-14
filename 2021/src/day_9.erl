-module(day_9).
-format(ignore).
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Input = helper:read_lines(Filename, string),
  Grid = to_grid(Input),
  LowPoints = [
    Pos
    ||
    Pos <- maps:keys(Grid),
    map_get(Pos, Grid) < lists:min(lists:map(fun(Pos) -> map_get(Pos, Grid) end, neighbours(Pos, Grid)))
  ],

  lists:sum(
    lists:map(fun(Pos) -> map_get(Pos, Grid) + 1 end,
      LowPoints)).

p2(Filename) ->
  Input = helper:read_lines(Filename, string),
  Grid = to_grid(Input),
  LowPoints = [
    Pos
    ||
    Pos <- maps:keys(Grid),
    map_get(Pos, Grid) < lists:min(lists:map(fun(Pos) -> map_get(Pos, Grid) end, neighbours(Pos, Grid)))
  ],

  lists:foldl(fun erlang:'*'/2,
    1,
    element(1,
      lists:split(3,
        lists:reverse(
          lists:sort(
            lists:map(fun sets:size/1,
              lists:map(fun(LowPoint) -> get_basin(Grid, LowPoint) end,
                LowPoints))))))).

%%====================================================================
%% Internal functions
%%====================================================================

get_basin(Grid, LowPoint) ->
  get_basin(Grid, [LowPoint], sets:new([{version, 2}])).

get_basin(_Grid, [], Visited) ->
  Visited;
get_basin(Grid, [Pos | Rest], Visited) ->
  NewPositions = [
    Neighbour
    ||
    Neighbour <- neighbours(Pos, Grid),
    map_get(Neighbour, Grid) /= 9,
    not sets:is_element(Neighbour, Visited)
  ],

  get_basin(Grid, NewPositions ++ Rest, sets:add_element(Pos, Visited)).

neighbours({X, Y}, Grid) ->
  Offsets = [{1, 0}, {0, -1}, {0, 1}, {-1, 0}],
  [Pos || {XOff, YOff} <- Offsets, Pos <- [{X + XOff, Y + YOff}], is_map_key(Pos, Grid)].

to_grid(ListOfLists) ->
  maps:from_list([
    {{X, Y}, Val - $0}
    ||
    {X, Row} <- lists:zip(lists:seq(1, length(ListOfLists)), ListOfLists),
    {Y, Val} <- lists:zip(lists:seq(1, length(Row)), Row)
  ]).
