-module(day_1).

%% API exports
-export([p1/1, p2/1]).

p1(Filename) ->
  Lines = lists:map(fun binary_to_integer/1, helper:read_lines(Filename, binary)),
  p1(Lines, 0).

p1([_], Acc) ->
  Acc;
p1([H1 | [H2 | _] = T], Acc) when H2 > H1 ->
  p1(T, Acc + 1);
p1([_ | T], Acc) ->
  p1(T, Acc).

p2(Filename) ->
  Lines = lists:map(fun binary_to_integer/1, helper:read_lines(Filename, binary)),
  p2(Lines, 0).

p2([_, _, _], Acc) ->
  Acc;
p2([H1 | [H2, H3, H4 | _] = T], Acc) when H2 + H3 + H4 > H1 + H2 + H3 ->
  p2(T, Acc + 1);
p2([_ | [_, _, _ | _] = T], Acc) ->
  p2(T, Acc).
