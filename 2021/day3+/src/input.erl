-module(input).

%% API
-export([parse_input/2]).

%% read Filename and apply Fn to each line
-spec parse_input(file:name_all(), function()) -> [term()].
parse_input(Filename, Fn) ->
    {ok, Binary} = file:read_file(Filename),
    [Fn(string:trim(X)) || X <- string:lexemes(Binary, "\n")].

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
