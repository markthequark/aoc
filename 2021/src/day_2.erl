-module(day_2).

%% API exports
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
    Lines = lists:map(fun parse_input/1, helper:read_lines(Filename, binary)),
    p1(Lines, 0, 0).

p1([], Horizontal, Depth) ->
    Horizontal * Depth;
p1([{forward, N} | T], Horizontal, Depth) ->
    p1(T, Horizontal + N, Depth);
p1([{up, N} | T], Horizontal, Depth) ->
    p1(T, Horizontal, Depth - N);
p1([{down, N} | T], Horizontal, Depth) ->
    p1(T, Horizontal, Depth + N).

p2(Filename) ->
    Lines = lists:map(fun parse_input/1, helper:read_lines(Filename, binary)),
    p2(Lines, 0, 0, 0).

p2([], Horizontal, Depth, _Aim) ->
    Horizontal * Depth;
p2([{forward, N} | T], Horizontal, Depth, Aim) ->
    p2(T, Horizontal + N, Depth + Aim * N, Aim);
p2([{up, N} | T], Horizontal, Depth, Aim) ->
    p2(T, Horizontal, Depth, Aim - N);
p2([{down, N} | T], Horizontal, Depth, Aim) ->
    p2(T, Horizontal, Depth, Aim + N).

%%====================================================================
%% Internal functions
%%====================================================================

parse_input(<<"forward ", Amount/binary>>) ->
    {forward, binary_to_integer(Amount)};
parse_input(<<"up ", Amount/binary>>) ->
    {up, binary_to_integer(Amount)};
parse_input(<<"down ", Amount/binary>>) ->
    {down, binary_to_integer(Amount)}.
