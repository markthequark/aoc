-module(helper).

-import(lists, [map/2]).

%% API
-export([read_input/3, read_lines/2, undigits/1, undigits/2]).

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
  undigits(Digits, 2).

undigits(Digits, Base) ->
  undigits(Digits, Base, 0).

undigits([], _Base, Acc) ->
  Acc;
undigits([Digit | _], Base, _) when Digit >= Base ->
  error(badarg, io_lib:format("invalid digit ~p in base ~p~n", [Digit, Base]));
undigits([Digit | Rest], Base, Acc) ->
  undigits(Rest, Base, Acc * Base + Digit).
