-module(helper).

%% API
-export([read_lines/2]).

%% Returns a list of trimmed strings or binaries for each line in the file
-spec read_lines(string(), binary | string) -> [binary() | string()].
read_lines(Filename, binary) ->
    {ok, Binary} = file:read_file(Filename),
    binary:split(Binary, <<"\n">>, [global, trim]);
read_lines(Filename, string) ->
    Lines = read_lines(Filename, binary),
    lists:map(fun(Bin) -> binary:bin_to_list(Bin) end, Lines).
