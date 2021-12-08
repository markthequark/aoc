-module(day_8).

-import(lists, [map/2, sum/1]).

-format(ignore).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Notes = map(fun parse_line/1, helper:read_lines(Filename, string)),
  sum([
    sum([1 || Digit <- DisplayedDigits, Len <- [length(Digit)], Len /= 5, Len /= 6])
    ||
    {_, DisplayedDigits} <- Notes
  ]).

p2(Filename) ->
  Notes = map(fun parse_line/1, helper:read_lines(Filename, string)),
  sum([
    helper:undigits(map(fun(Digit) -> map_get(Digit, DigitDecoder) end, DisplayedDigits))
    ||
    {AllDigits, DisplayedDigits} <- Notes, DigitDecoder <- [decode(AllDigits)]
  ]).

%%====================================================================
%% Internal functions
%%====================================================================

parse_line(Line) ->
  lists:split(10, map(fun ordsets:from_list/1, string:lexemes(Line, " |"))).

decode(AllDigits) ->
  #{1 := One,
    4 := Four,
    7 := Seven,
    8 := Eight,
    length5 := TwoThreeFive
  } = group_digits(AllDigits),

  Three = determine_three(TwoThreeFive),

  [B] = ordsets:intersection(Four, Eight -- Three),

  {value, Five} = lists:search(fun(X) -> lists:member(B, X) end, TwoThreeFive -- [Three]),
  [Two] = TwoThreeFive -- [Five, Three],

  [A] = Seven -- One,
  [C] = One -- Five,
  [F] = One -- [C],
  [D] = Four -- [B, C, F],
  [G] = Three -- [A, C, D, F],
  [E] = Two -- [A, C, D, G],
  Zero = Eight -- [D],
  Six = Eight -- [C],
  Nine = Eight -- [E],

  #{Zero => 0,
    One => 1,
    Two => 2,
    Three => 3,
    Four => 4,
    Five => 5,
    Six => 6,
    Seven => 7,
    Eight => 8,
    Nine => 9}.

group_digits(AllDigits) ->
  group_digits(AllDigits, digit_map()).

group_digits([], Map) ->
  Map;
group_digits([Digit | Rest], Map) ->
  NewMap =
    case length(Digit) of
      2 -> Map#{1 := Digit};
      3 -> Map#{7 := Digit};
      4 -> Map#{4 := Digit};
      5 -> Map#{length5 := [Digit | map_get(length5, Map)]};
      _ -> Map
    end,
  group_digits(Rest, NewMap).

digit_map() ->
  #{1 => undefined,
    4 => undefined,
    7 => undefined,
    8 => "abcdefg",
    length5 => []}.

determine_three([Digit1, Digit2, Digit3]) ->
  case {length(Digit1 -- Digit2),
        length(Digit1 -- Digit3),
        length(Digit2 -- Digit3)} of
    {2, 1, 1} -> Digit3;
    {1, 2, 1} -> Digit2;
    {1, 1, 2} -> Digit1
  end.
