-module(day_3).

-import(lists, [nth/2, foldl/3, map/2, zipwith/3, seq/2]).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
    Lines = helper:read_lines(Filename, string),
    BitList = most_common_bits(Lines),

    GammaRate = list_to_integer(BitList, 2),
    EpsilonRate = list_to_integer(map(fun(X) -> negate_bit(X) end, BitList), 2),

    GammaRate * EpsilonRate.

p2(Filename) ->
    Lines = helper:read_lines(Filename, string),

    OxygenGeneratorRating = list_to_integer(p2_filter(Lines, most_common), 2),
    CO2ScrubberRating = list_to_integer(p2_filter(Lines, least_common), 2),

    OxygenGeneratorRating * CO2ScrubberRating.

%%====================================================================
%% Internal functions
%%====================================================================

-spec negate_bit(char()) -> char().
negate_bit($0) ->
    $1;
negate_bit($1) ->
    $0.

%% Takes a list of strings of the same length, formed only of 1s and 0s
%% and returns a string containing the most common character at each position.
%% In a tie, 1 is chosen.
%% Examples:
%% > most_common_bits(["1", "1"]).
%% "1"
%% > most_common_bits(["1", "0"]).
%% "1"
%% > most_common_bits(["11", "11", "00"]).
%% "11"
%% > most_common_bits(["1111", "1110", "1100", "1000", "0000"]).
%% "1100"
-spec most_common_bits([string()]) -> string().
most_common_bits(Lines) ->
    ToChar =
        fun ({Zeros, Ones}) when Zeros > Ones ->
                $0;
            (_) ->
                $1
        end,
    map(ToChar, bit_distribution(Lines)).

%% Takes a list of strings of the same length, formed only of 1s and 0s
%% and returns the number of 1s and 0s in each position
%% Examples:
%% > bit_distribution(["1", "1"]).
%% [{0,2}]
%% > bit_distribution(["10", "10", "10"]).
%% [{0,3}, {3,0}]
%% > bit_distribution(["0001", "0011", "0111"]).
%% [{3,0}, {2,1}, {1,2}, {0,3}]
-spec bit_distribution([string()]) -> [{integer(), integer()}].
bit_distribution(Lines) ->
    Acc0 = [{0, 0} || _ <- seq(1, length(hd(Lines)))],
    ZipFn =
        fun ($0, {Zeros, Ones}) ->
                {Zeros + 1, Ones};
            ($1, {Zeros, Ones}) ->
                {Zeros, Ones + 1}
        end,
    ReduceFn = fun(Line, Acc) -> zipwith(ZipFn, Line, Acc) end,
    foldl(ReduceFn, Acc0, Lines).

%% Follows the completely contrived algorithm as described on part 2
%% https://adventofcode.com/2021/day/3
-spec p2_filter([string()], most_common | least_common) -> string().
p2_filter(Lines, Method) ->
    p2_filter(Lines, most_common_bits(Lines), 1, Method).

p2_filter([Line], _, _, _) ->
    Line;
p2_filter(Lines, BitCriteria, Offset, most_common) when is_list(BitCriteria) ->
    Match = nth(Offset, BitCriteria),
    p2_filter(Lines, Match, Offset, most_common);
p2_filter(Lines, BitCriteria, Offset, least_common) when is_list(BitCriteria) ->
    Match = negate_bit(nth(Offset, BitCriteria)),
    p2_filter(Lines, Match, Offset, least_common);
p2_filter(Lines, Match, Offset, Method) ->
    RemainingLines = [Line || Line <- Lines, Match == nth(Offset, Line)],
    NewBitCriteria = most_common_bits(RemainingLines),
    p2_filter(RemainingLines, NewBitCriteria, Offset + 1, Method).
