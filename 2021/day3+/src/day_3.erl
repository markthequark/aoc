-module(day_3).

%% API
-export([p1/1, p2/1]).

p1(Filename) ->
    {ok, File} = file:open(Filename, read),
    {ok, Line} = file:read_line(File),
    LineLength = length(Line -- "\n"),
    GetLine =
        fun() ->
           case file:read(File, LineLength) of
               {ok, NewLine} ->
                   file:position(File, {cur, 1}),
                   NewLine;
               eof ->
                   eof;
               {error, _} = e ->
                   e
           end
        end,

    Acc0 = [{0, 0} || _ <- lists:seq(1, LineLength)],
    ZipFn =
        fun ({Zeros, Ones}, $0) ->
                {Zeros + 1, Ones};
            ({Zeros, Ones}, $1) ->
                {Zeros, Ones + 1}
        end,

    BitDist = p1_parse_lines(GetLine, ZipFn, Line -- "\n", Acc0),

    file:close(File),

    ToBinary =
        fun ({Zeros, Ones}) when Zeros > Ones ->
                $0;
            (_) ->
                $1
        end,
    Binary = lists:map(ToBinary, BitDist),
    GammaRate = list_to_integer(Binary, 2),

    ok.

p1_parse_lines(_GetLine, _ZipFn, eof, Acc) ->
    Acc;
p1_parse_lines(GetLine, ZipFn, "\n", Acc) ->
    p1_parse_lines(GetLine, ZipFn, GetLine(), Acc);
p1_parse_lines(GetLine, ZipFn, Line, Acc) ->
    NewAcc = lists:zipwith(ZipFn, Acc, Line),
    p1_parse_lines(GetLine, ZipFn, GetLine(), NewAcc).

p2(Filename) ->
    ok.
