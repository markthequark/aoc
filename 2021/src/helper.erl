-module(helper).

-import(lists, [map/2]).

%% API
-export([read_lines/2, undigits/1, undigits/2]).

%% Returns a list of trimmed strings or binaries for each line in the file, ignoring empty lines
-spec read_lines(string(), binary | string) -> [binary() | string()].
read_lines(Filename, binary) ->
  {ok, Binary} = file:read_file(Filename),
  binary:split(Binary, <<"\n">>, [global, trim_all]);
read_lines(Filename, string) ->
  Lines = read_lines(Filename, binary),
  map(fun binary_to_list/1, Lines).

%% Returns the integer represented by the ordered Digits
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
