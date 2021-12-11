-module(helper).

-import(lists, [map/2, foldl/3]).

%% API
-export([read_input/3, read_lines/2, undigits/1, undigits/2, frequency_map/1,
         chunk_every/2]).

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
