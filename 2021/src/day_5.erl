-module(day_5).

-import(lists, [foldl/3, map/2, seq/2]).

-type point() :: {integer(), integer()}.
-type line() :: {point(), point()}.

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  AllLines = parse_lines(helper:read_lines(Filename, string)),
  Lines = lists:filter(fun({{X1, Y1}, {X2, Y2}}) -> X1 == X2 orelse Y1 == Y2 end, AllLines),
  Points = lists:flatmap(fun to_points/1, Lines),

  maps:size(
    maps:filter(fun(_K, V) -> V > 1 end, histogram(Points))).

p2(Filename) ->
  Lines = parse_lines(helper:read_lines(Filename, string)),
  Points = lists:flatmap(fun to_points/1, Lines),

  maps:size(
    maps:filter(fun(_K, V) -> V > 1 end, histogram(Points))).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_lines([string()]) -> [line()].
parse_lines(InputLines) ->
  map(fun(Line) ->
         [X1, Y1, X2, Y2] = map(fun list_to_integer/1, string:lexemes(Line, "-> ,")),
         list_to_tuple(lists:sort([{X1, Y1}, {X2, Y2}]))
      end,
      InputLines).

-spec to_points(line()) -> [point()].
to_points({{X, Y1}, {X, Y2}}) ->
  map(fun(Y) -> {X, Y} end, seq(Y1, Y2));
to_points({{X1, Y}, {X2, Y}}) ->
  map(fun(X) -> {X, Y} end, seq(X1, X2));
to_points({{X1, Y1}, {X2, Y2}}) when X2 - X1 == Y2 - Y1 ->
  map(fun(X) -> {X, Y1 + X - X1} end, seq(X1, X2));
to_points({{X1, Y1}, {X2, Y2}}) when X2 - X1 == Y1 - Y2 ->
  map(fun(X) -> {X, Y1 - X + X1} end, seq(X1, X2)).

-spec inc(integer()) -> integer().
inc(X) when is_integer(X) ->
  X + 1.

-spec histogram([T]) -> #{T => integer()}.
histogram(List) when is_list(List) ->
  foldl(fun(Elem, Acc) -> maps:update_with(Elem, fun inc/1, 1, Acc) end, #{}, List).
