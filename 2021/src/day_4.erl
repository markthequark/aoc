-module(day_4).

-import(lists, [foldl/3, map/2]).

-define(BOARD_SIZE, 5).

-type board() :: [[{marked | unmarked, integer()}]].

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Lines = helper:read_lines(Filename, string),
  CallNumbers = map(fun list_to_integer/1, string:lexemes(hd(Lines), ",")),
  Boards = to_bingo_boards(tl(Lines)),

  {CalledNum, WinningBoard} = call_until_winner(CallNumbers, Boards, first),

  score(CalledNum, WinningBoard).

p2(Filename) ->
  Lines = helper:read_lines(Filename, string),
  CallNumbers = map(fun list_to_integer/1, string:lexemes(hd(Lines), ",")),
  Boards = to_bingo_boards(tl(Lines)),

  {CalledNum, WinningBoard} = call_until_winner(CallNumbers, Boards, last),

  score(CalledNum, WinningBoard).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Converts a list of strings to bingo boards of size ?BOARD_SIZE
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

%% @doc Returns the winning number and the first or last board to win
-spec call_until_winner([integer()], [board()], first | last) -> {integer(), board()}.
call_until_winner(CallNumbers, Boards, Method) ->
  call_until_winner(CallNumbers, Boards, [], Method).

call_until_winner(Numbers, [Board], [], last) ->
  call_until_winner(Numbers, [Board], [], first);
call_until_winner([_ | Numbers], [], NewBoards, Method) ->
  call_until_winner(Numbers, NewBoards, [], Method);
call_until_winner([Number | _] = Numbers, [Board | Boards], NewBoards, Method) ->
  NewBoard = call_number(Number, Board),
  case {is_bingo(NewBoard), Method} of
    {true, first} ->
      {Number, NewBoard};
    {true, last} ->
      call_until_winner(Numbers, Boards, NewBoards, Method);
    {false, _} ->
      call_until_winner(Numbers, Boards, [NewBoard | NewBoards], Method)
  end.

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
  * foldl(fun ({unmarked, Num}, Acc) ->
                Num + Acc;
              ({marked, _Num}, Acc) ->
                Acc
          end,
          0,
          lists:flatten(Board)).
