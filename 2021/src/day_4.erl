-module(day_4).

-import(lists, [nth/2, foldl/3, map/2, zipwith/3, seq/2]).

-define(BOARD_SIZE, 5).

-type board() :: [[{marked | unmarked, integer()}]].

%% API
-export([p1/1, p2/1]).

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Lines = helper:read_lines(Filename, string),
  CallNumbers = lists:map(fun list_to_integer/1, string:lexemes(hd(Lines), ",")),
  Boards = to_bingo_boards(tl(Lines)),

  {CalledNum, WinningBoard} = call_until_winner(CallNumbers, Boards),

  score(CalledNum, WinningBoard).

p2(Filename) ->
  Lines = helper:read_lines(Filename, string),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% The order of boards are reversed.
%% The order of lines in boards are reversed. In the game rules these are equivalent boards
-spec to_bingo_boards([string()]) -> [board()].
to_bingo_boards(Lines) ->
  to_bingo_boards(Lines, 0, [[]]).

to_bingo_boards([], _N, Boards) ->
  Boards;
to_bingo_boards(Lines, ?BOARD_SIZE, Boards) ->
  to_bingo_boards(Lines, 0, [[] | Boards]);
to_bingo_boards([Line | Rest], N, [Board | Boards]) when N < ?BOARD_SIZE ->
  ParseLine = fun(Num) -> {unmarked, list_to_integer(Num)} end,
  NewLine = map(ParseLine, string:lexemes(Line, " ")),
  to_bingo_boards(Rest, N + 1, [[NewLine | Board] | Boards]).

%% Returns the winning number and a board that won
-spec call_until_winner([integer()], [board()]) -> {integer(), board()}.
call_until_winner(CallNumbers, Boards) ->
  call_until_winner(CallNumbers, Boards, []).

call_until_winner([], [], _) ->
  no_winner;
call_until_winner([_ | Numbers], [], NewBoards) ->
  call_until_winner(Numbers, NewBoards, []);
call_until_winner([Number | _] = Numbers, [Board | Boards], NewBoards) ->
  NewBoard = call_number(Number, Board),
  case is_bingo(NewBoard) of
    true ->
      {Number, NewBoard};
    false ->
      call_until_winner(Numbers, Boards, [NewBoard | NewBoards])
  end.

-spec call_numbers([integer()], board()) -> board().
call_numbers(Calls, Board) ->
  foldl(fun call_number/2, Board, Calls).

-spec call_number(integer(), board()) -> board().
call_number(Called, Board) ->
  [[case Num of
      {unmarked, Called} ->
        {marked, Called};
      _ ->
        Num
    end
    || Num <- Line]
   || Line <- Board].

-spec is_bingo(board()) -> boolean().
is_bingo(Board) ->
  is_bingo(Board, lists:duplicate(?BOARD_SIZE, true)).

is_bingo([], Acc) ->
  lists:member(true, Acc);
is_bingo(_Board, {short_circuit, Return}) ->
  Return;
is_bingo([Line | Rest], Acc) ->
  case lists:any(fun(X) -> element(1, X) == unmarked end, Line) of
    false ->
      is_bingo(ok, {short_circuit, true});
    true ->
      ZipFn = fun(L, A) -> A andalso element(1, L) == marked end,
      is_bingo(Rest, lists:zipwith(ZipFn, Line, Acc))
  end.

-spec score(integer(), board()) -> integer().
score(CalledNum, Board) ->
  CalledNum
  * lists:foldl(fun ({unmarked, Num}, Acc) ->
                      Num + Acc;
                    ({marked, _Num}, Acc) ->
                      Acc
                end,
                0,
                lists:flatten(Board)).
