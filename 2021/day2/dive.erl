#!/usr/bin/env escript
%% escript Entry point
main([Filename]) ->
    Input = parse_input(Filename),
    Answer1 = solve_p1(Input),
    Answer2 = solve_p2(Input),
    io:format("Part 1 answer: ~p~nPart 2 answer: ~p~n", [Answer1, Answer2]),
    erlang:halt(0);
main(_Args) ->
    main(["priv/input.txt"]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_input(file:name_all()) -> [{atom(), integer()}].
parse_input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    [parse_line(string:trim(X)) || X <- string:lexemes(Binary, "\n")].

parse_line(<<"forward ", Amount/binary>>) ->
    {forward, binary_to_integer(Amount)};
parse_line(<<"up ", Amount/binary>>) ->
    {up, binary_to_integer(Amount)};
parse_line(<<"down ", Amount/binary>>) ->
    {down, binary_to_integer(Amount)}.

%% Part 1

-spec solve_p1([{atom(), integer()}]) -> integer().
solve_p1(List) when is_list(List) ->
    solve_p1(List, 0, 0).

solve_p1([], Horizontal, Depth) ->
    Horizontal * Depth;
solve_p1([{forward, N} | T], Horizontal, Depth) ->
    solve_p1(T, Horizontal + N, Depth);
solve_p1([{up, N} | T], Horizontal, Depth) ->
    solve_p1(T, Horizontal, Depth - N);
solve_p1([{down, N} | T], Horizontal, Depth) ->
    solve_p1(T, Horizontal, Depth + N).

%% Part 2

-spec solve_p2([{atom(), integer()}]) -> integer().
solve_p2(List) when is_list(List) ->
    solve_p2(List, 0, 0, 0).

solve_p2([], Horizontal, Depth, _Aim) ->
    Horizontal * Depth;
solve_p2([{forward, N} | T], Horizontal, Depth, Aim) ->
    solve_p2(T, Horizontal + N, Depth + Aim * N, Aim);
solve_p2([{up, N} | T], Horizontal, Depth, Aim) ->
    solve_p2(T, Horizontal, Depth, Aim - N);
solve_p2([{down, N} | T], Horizontal, Depth, Aim) ->
    solve_p2(T, Horizontal, Depth, Aim + N).
