-module(day_9).

-import(lists, [map/2, sum/1]).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Input = helper:read_lines(Filename, string),
  Lines = map(fun parse_line/1, Input),

  sum([scan(A, B, C) || [A, B, C] <- groups_of_3(Lines)]).

p2(Filename) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

parse_line(Line) ->
  map(fun(Digit) -> Digit - $0 end, Line).

scan(none, Mid, Bot) ->
  scan([10 || _ <- Mid], Mid, Bot);
scan(Top, Mid, none) ->
  scan(Top, Mid, [10 || _ <- Mid]);
scan([Above | _] = Top, [Elem, After | _] = Mid, [Below | _] = Bot)
  when Elem < Above, Elem < After, Elem < Below ->
  scan(Top, Mid, Bot, Elem + 1);
scan([_ | _] = Top, [_ | _] = Mid, [_ | _] = Bot) ->
  scan(Top, Mid, Bot, 0).

scan([_, Above], [Before, Elem], [_, Below], Acc)
  when Elem < Above, Elem < Before, Elem < Below ->
  Acc + Elem + 1;
scan([_, _], [_, _], [_, _], Acc) ->
  Acc;
scan([_, Above | _] = Top,
     [Before, Elem, After | _] = Mid,
     [_, Below | _] = Bot,
     Acc)
  when Elem < Above, Elem < Before, Elem < After, Elem < Below ->
  scan(tl(Top), tl(Mid), tl(Bot), Acc + Elem + 1);
scan(Top, Mid, Bot, Acc) ->
  scan(tl(Top), tl(Mid), tl(Bot), Acc).

groups_of_3([E1, E2 | _] = List) ->
  groups_of_3(List, [[none, E1, E2]]).

groups_of_3([E1, E2], Acc) ->
  lists:reverse([[E1, E2, none] | Acc]);
groups_of_3([E1 | [E2, E3 | _] = Rest], Acc) ->
  groups_of_3(Rest, [[E1, E2, E3] | Acc]).
