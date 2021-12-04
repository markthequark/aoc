-module(day_3).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
    Lines = p1_parse_file(Filename),
    BitList = p1_most_common_bits(Lines),

    GammaRate = list_to_integer(BitList, 2),
    BitNegate =
        fun ($0) ->
                $1;
            ($1) ->
                $0
        end,
    EpsilonRate = list_to_integer(lists:map(BitNegate, BitList), 2),

    GammaRate * EpsilonRate.

p2(Filename) ->
    ok.

%%====================================================================
%% Part 1 Internal functions
%%====================================================================

%% Returns a list of trimmed strings for each line in the file
-spec p1_parse_file(string()) -> [string()].
p1_parse_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    lists:map(fun(Bin) -> binary:bin_to_list(Bin) end, Lines).

%% Takes a list of strings of the same length, formed only of 1s and 0s
%% and returns a string containing the most common character at each position.
%% In a tie, 1 is chosen.
%% Examples:
%% > p1_most_common_bits(["1", "1"]).
%% "1"
%% > p1_most_common_bits(["1", "0"]).
%% "1"
%% > p1_most_common_bits(["11", "11", "00"]).
%% "11"
%% > p1_most_common_bits(["1111", "1110", "1100", "1000", "0000"]).
%% "1100"
-spec p1_most_common_bits([string()]) -> string().
p1_most_common_bits(Lines) ->
    ToBinary =
        fun ({Zeros, Ones}) when Zeros > Ones ->
                $0;
            (_) ->
                $1
        end,
    lists:map(ToBinary, p1_bit_distribution(Lines)).

%% Takes a list of strings of the same length, formed only of 1s and 0s
%% and returns the number of 1s and 0s in each position
%% Examples:
%% > p1_bit_distribution(["1", "1"]).
%% [{0,2}]
%% > p1_bit_distribution(["10", "10", "10"]).
%% [{0,3}, {3,0}]
%% > p1_bit_distribution(["0001", "0011", "0111"]).
%% [{3,0}, {2,1}, {1,2}, {0,3}]
-spec p1_bit_distribution([string()]) -> [{integer(), integer()}].
p1_bit_distribution(Lines) ->
    Acc0 = [{0, 0} || _ <- lists:seq(1, length(hd(Lines)))],
    ZipFn =
        fun ($0, {Zeros, Ones}) ->
                {Zeros + 1, Ones};
            ($1, {Zeros, Ones}) ->
                {Zeros, Ones + 1}
        end,
    ReduceFn = fun(Line, Acc) -> lists:zipwith(ZipFn, Line, Acc) end,
    lists:foldl(ReduceFn, Acc0, Lines).

%%====================================================================
%% Part 2 Internal functions
%%====================================================================
