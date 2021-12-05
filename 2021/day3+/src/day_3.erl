-module(day_3).

-import(lists, [nth/2, foldl/3, map/2, zipwith/3]).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
    Lines = helper:read_lines(Filename, string),
    BitList = most_common_bits(Lines),

    GammaRate = list_to_integer(BitList, 2),
    EpsilonRate = list_to_integer(map(fun negate_bit/1, BitList), 2),

    GammaRate * EpsilonRate.

p2(Filename) ->
    Lines = helper:read_lines(Filename, string),

    OxygenGeneratorRating = list_to_integer(p2_filter(Lines, most_common), 2),
    CO2ScrubberRating = list_to_integer(p2_filter(Lines, least_common), 2),

    OxygenGeneratorRating * CO2ScrubberRating.

%%====================================================================
%% Internal functions
%%====================================================================

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
bit_distribution(Lines) ->
    Acc0 = [{0, 0} || _ <- hd(Lines)],
    ZipFn =
        fun ($0, {Zeros, Ones}) ->
                {Zeros + 1, Ones};
            ($1, {Zeros, Ones}) ->
                {Zeros, Ones + 1}
        end,
    ReduceFn = fun(Line, Acc) -> zipwith(ZipFn, Line, Acc) end,
    foldl(ReduceFn, Acc0, Lines).

%% Takes a list of strings of the same length, formed only of 1s and 0s
%% and returns the string which most closely matches the most/least common bits
%% where the most/least common bits are recalculated after filtering on bit 1, 2 etc.
%% More information: https://adventofcode.com/2021/day/3#part2
p2_filter(Lines, Method) when Method == most_common orelse Method == least_common ->
    p2_filter(Lines, most_common_bits(Lines), 1, Method).

p2_filter([Line], _, _, _) ->
    Line;
p2_filter(Lines, BitCriteria, Offset, Method) ->
    Bit = case Method of
              most_common ->
                  nth(Offset, BitCriteria);
              least_common ->
                  negate_bit(nth(Offset, BitCriteria))
          end,
    RemainingLines = [Line || Line <- Lines, nth(Offset, Line) == Bit],
    NewBitCriteria = most_common_bits(RemainingLines),
    p2_filter(RemainingLines, NewBitCriteria, Offset + 1, Method).
