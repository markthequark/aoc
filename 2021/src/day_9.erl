-module(day_9).

-import(lists, [map/2, sum/1]).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
    ok.

p2(Filename) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

parse_line(Line) ->
    lists:map(fun(Digit) -> Digit - $0 end, Line).

scan_for_low([Above | RestTop], [Before | [Elem, After | _] = RestMid], [Below | RestBot]) ->
    ok.

scan([_, Above | _] = Top, [Before, Elem, After | _] = Mid, [_, Below | _] = Bot, Acc)
    when Elem < Above, Elem < Before, Elem < After, Elem < Below ->
    scan(tl(Top), tl(Mid), tl(Bot), Acc + Elem + 1);
