-module(day_13).
-format(ignore).
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Grid = solve(Filename),
  lists:sum(lists:flatten(Grid)).

p2(Filename) ->
  Grid = solve(Filename),
  lists:foreach(fun(Row) ->
                  lists:foreach(fun(0) -> io:format(".");
                                   (1) -> io:format("#")
                                end, Row),
                  io:format("~n")
                end, Grid).

%%====================================================================
%% Internal functions
%%====================================================================

solve(Filename) ->
  Input = helper:read_lines(Filename, string),
  Lines = lists:map(fun parse_line/1, Input),
  PosMap = maps:from_list([{{X, Y}, 1} || {X, Y} <- Lines]),
  MaxX = lists:max([X || {X, _} <- maps:keys(PosMap)]),
  MaxY = lists:max([Y || {_, Y} <- maps:keys(PosMap)]),

  Grid = [
    [
      maps:get({X, Y}, PosMap, 0)
      ||
      X <- lists:seq(0, MaxX)
    ]
    ||
    Y <- lists:seq(0, MaxY)
  ],
  Folds = [Fold || {fold, _, _} = Fold <- Lines],

  solve(Grid, Folds).

solve(Grid, []) ->
  Grid;
solve(Grid, [{fold, $x, ColNum} | Rest]) ->
  RowLength = length(hd(Grid)) div 2,
  NewGrid = [
    begin
      {Lhs, Rhs} = lists:split(ColNum, Row),
      LeftInt = helper:undigits(Lhs, 2),
      RightInt = helper:undigits(lists:reverse(tl(Rhs)), 2),
      helper:to_digits(LeftInt bor RightInt, 2, RowLength)
    end
    ||
    Row <- Grid
  ],

  solve(NewGrid, Rest);
solve(Grid, [{fold, $y, RowNum} | Rest]) ->
  GridAsBinary = lists:map(fun(Digits) -> helper:undigits(Digits, 2) end, Grid),
  {Top, NotTop} = lists:split(RowNum, GridAsBinary),
  Bot = lists:reverse(tl(NotTop)),
  RowLength = length(hd(Grid)),

  NewGridAsBinary = lists:zipwith(fun erlang:'bor'/2, Top, Bot),
  NewGrid = lists:map(fun(Integer) -> helper:to_digits(Integer, 2, RowLength) end, NewGridAsBinary),

  solve(NewGrid, Rest).

parse_line(Line) ->
  case string:lexemes(Line, ", =") of
    [X, Y] ->
      {list_to_integer(X), list_to_integer(Y)};
    [_, _, [Axis], Value] ->
      {fold, Axis, list_to_integer(Value)}
  end.
