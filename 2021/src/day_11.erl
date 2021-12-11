-module(day_11).

-format(ignore).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Input = helper:read_lines(Filename, string),
  step_n(100, to_grid(Input)).

p2(Filename) ->
  Input = helper:read_lines(Filename, string),
  step_until_sync(to_grid(Input)).

%%====================================================================
%% Internal functions
%%====================================================================

step_until_sync(Grid) -> 
  step_until_sync(Grid, 0).

step_until_sync(Grid, NumSteps) ->
  case lists:all(fun(X) -> X == 0 end, maps:values(Grid)) of
    true ->
      NumSteps;
    false ->
      {NewGrid, _} = step(Grid),
      step_until_sync(NewGrid, NumSteps + 1)
  end.

step_n(N, Grid) ->
  step_n(N, Grid, 0).

step_n(0, _Grid, NumFlash) ->
  NumFlash;
step_n(N, Grid, NumFlash) ->
  {NewGrid, NewNumFlash} = step(Grid),
  step_n(N - 1, NewGrid, NumFlash + NewNumFlash).

step(Grid) ->
  step(Grid, [], maps:keys(Grid), 0).

step(Grid, [], [], NumFlash) ->
  NewGrid = maps:map(fun (_Key, flashed) -> 0; (_Key, Value) -> Value end, Grid),
  {NewGrid, NumFlash};
step(Grid, ToFlash, [Key | RestToInc], Acc) ->
  case map_get(Key, Grid) of
    9 ->
      step(Grid#{Key := flash}, [Key | ToFlash], RestToInc, Acc);
    Value when Value == flash orelse Value == flashed ->
      step(Grid, ToFlash, RestToInc, Acc);
    Value ->
      step(Grid#{Key := Value + 1}, ToFlash, RestToInc, Acc)
  end;
step(Grid, [Key | RestToFlash], [], Acc) ->
  step(Grid#{Key := flashed}, RestToFlash, neighbours(Key, Grid), Acc + 1).

neighbours({X, Y}, Grid) ->
  Offsets = [{1, -1}, {1, 0}, {1, 1}, {0, -1}, {0, 1}, {-1, -1}, {-1, 0}, {-1, 1}],
  [Pos || {XOff, YOff} <- Offsets, Pos <- [{X + XOff, Y + YOff}], is_map_key(Pos, Grid)].

to_grid(ListOfLists) ->
  maps:from_list([
    {{X, Y}, Val - $0}
    ||
    {X, Row} <- lists:zip(lists:seq(1, length(ListOfLists)), ListOfLists),
    {Y, Val} <- lists:zip(lists:seq(1, length(Row)), Row)
  ]).
