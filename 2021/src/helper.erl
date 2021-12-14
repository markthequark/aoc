-module(helper).

-import(lists, [map/2, foldl/3]).

%% API
-export([read_input/3, read_lines/2, undigits/1, undigits/2, frequency_map/1,
         chunk_every/2, flatten_n/2, to_digits/1, to_digits/2, to_digits/3]).

%% @doc Returns a list of trimmed strings or binaries for each line in the file, ignoring empty lines
-spec read_lines(string(), binary | string) -> [binary() | string()].
read_lines(Filename, Mode) ->
  read_input(Filename, <<"\n">>, Mode).

%% @doc Delimits the file by the given Delimiter and returns a list of trimmed strings or binaries
-spec read_input(string(), binary() | [binary()], binary | string) ->
                  [binary() | string()].
read_input(Filename, Delimiter, binary) ->
  {ok, Binary} = file:read_file(Filename),
  binary:split(Binary, Delimiter, [global, trim_all]);
read_input(Filename, Delimiter, string) ->
  Lines = read_input(Filename, Delimiter, binary),
  map(fun binary_to_list/1, Lines).

%% @doc Returns the integer represented by the ordered Digits
%% An optional Base value may be provided representing the radix for the Digits
undigits(Digits) ->
  undigits(Digits, 10).

undigits(Digits, Base) ->
  undigits(Digits, Base, 0).

undigits([], _Base, Acc) ->
  Acc;
undigits([Digit | _], Base, _) when Digit >= Base ->
  error(badarg, io_lib:format("invalid digit ~p in base ~p~n", [Digit, Base]));
undigits([Digit | Rest], Base, Acc) ->
  undigits(Rest, Base, Acc * Base + Digit).

%% @doc Opposite of undigits, takes an integer of a given base and converts
%% it to a list of digits
to_digits(Integer) when is_integer(Integer) ->
  to_digits(Integer, 10, 0).

to_digits(Integer, Base) when is_integer(Integer), is_integer(Base) ->
  to_digits(Integer, Base, 0).

to_digits(Integer, Base, MinLength)
  when is_integer(Integer), is_integer(Base), is_integer(MinLength) ->
  to_digits(Integer, Base, MinLength, []).

to_digits(LastDigit, Base, MinLength, Acc) when LastDigit < Base ->
  pad_list(MinLength, [LastDigit | Acc], 0);
to_digits(Integer, Base, MinLength, Acc) ->
  to_digits(Integer div Base, Base, MinLength, [Integer rem Base | Acc]).

%% @doc Returns a map of term => frequency for the given list.
-spec frequency_map([T]) -> #{T => integer()}.
frequency_map(List) when is_list(List) ->
  foldl(fun(Elem, Acc) -> maps:update_with(Elem, fun(X) -> X + 1 end, 1, Acc) end,
        #{},
        List).

chunk_every(List, N) ->
  chunk_every(List, N, [[]]).

chunk_every([], _N, Acc) ->
  lists:reverse(Acc);
%% use counter in function param instead of calculating length every iteration
chunk_every(List, N, [Chunk | Acc]) when length(Chunk) == N ->
  chunk_every(List, N, [[], lists:reverse(Chunk) | Acc]);
chunk_every([Elem | Rest], N, [Chunk | Acc]) ->
  chunk_every(Rest, N, [[Elem | Chunk] | Acc]).

%% @doc flatten a list by n levels
%% Examples:
%% > flatten_n(1, [["a", "b"], ["c", "d"]]).
%% ["c", "d", "b", "a"]
%% > flatten_n(2, [["a", "b"], ["c", "d"]]).
%% "bacd"
flatten_n(0, ListOfLists) ->
  ListOfLists;
flatten_n(N, ListOfLists) ->
  NewList = lists:foldl(fun erlang:'++'/2, hd(ListOfLists), tl(ListOfLists)),
  flatten_n(N - 1, NewList).

pad_list(N, List, _Value) when length(List) >= N ->
  List;
pad_list(N, List, Value) ->
  pad_list(N, [Value | List], Value).
