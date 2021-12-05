-module(advent_of_code).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main([Day]) ->
    Module = list_to_atom("day_" ++ Day),
    Filename = "priv/day" ++ Day ++ ".txt",
    Answer1 = Module:p1(Filename),
    Answer2 = Module:p2(Filename),
    io:format("Part 1 answer: ~p~nPart 2 answer: ~p~n", [Answer1, Answer2]),
    erlang:halt(0);
main(_) ->
    io:format("Bad args, call script as ./script <day>~ne.g. ./script 3~n").
