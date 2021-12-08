-module(day_8).

-import(lists, [map/2, foldl/3, seq/2, min/1, max/1, sum/1]).

-compile(export_all).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  InputLines = helper:read_lines(Filename, string),
  Lines = map(fun(L) -> lists:split(10, string:lexemes(L, " |")) end, InputLines),
  foldl(fun({_, Out}, Acc) ->
           Acc
           + foldl(fun(E, A) ->
                      case length(E) of
                        N when N == 2; N == 3; N == 4; N == 7 ->
                          A + 1;
                        _ ->
                          A
                      end
                   end,
                   0,
                   Out)
        end,
        0,
        Lines).

p2(Filename) ->
  solve(Filename).

%%====================================================================
%% Internal functions
%%====================================================================

solve(Filename) ->
  ok.
