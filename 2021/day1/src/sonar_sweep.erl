-module(sonar_sweep).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
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

-spec parse_input(file:name_all()) -> [integer()].
parse_input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    [binary_to_integer(X) || X <- string:lexemes(Binary, "\n")].

%% Part 1

-spec solve_p1([integer()]) -> non_neg_integer().
solve_p1(List) when is_list(List) ->
    solve_p1(List, 0).

solve_p1([_], Acc) ->
    Acc;
solve_p1([H1 | [H2 | _] = T], Acc) when H2 > H1 ->
    solve_p1(T, Acc + 1);
solve_p1([_ | T], Acc) ->
    solve_p1(T, Acc).

%% Part 2

-spec solve_p2([integer()]) -> non_neg_integer().
solve_p2(List) when is_list(List) ->
    solve_p2(List, 0).

solve_p2([_], Acc) ->
    Acc;
solve_p2([_, _], Acc) ->
    Acc;
solve_p2([_, _, _], Acc) ->
    Acc;
solve_p2([H1 | [H2, H3, H4 | _] = T], Acc) ->
    IsLarger = lists:sum([H2, H3, H4]) > lists:sum([H1, H2, H3]),
    if IsLarger ->
           solve_p2(T, Acc + 1);
       true ->
           solve_p2(T, Acc)
    end.
