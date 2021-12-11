-module(day_10).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  Input = helper:read_lines(Filename, string),
  lists:sum([error_score(Char)
             || Line <- Input, Char <- [first_illegal_char(Line)], Char /= none]).

p2(Filename) ->
  Input = helper:read_lines(Filename, string),
  Scores =
    lists:sort([completion_score(ClosingChars)
                || Line <- Input,
                   first_illegal_char(Line) == none,
                   ClosingChars <- [complete_line(Line)]]),
  lists:nth(ceil(length(Scores) / 2), Scores).

%%====================================================================
%% Internal functions
%%====================================================================

first_illegal_char([Char | Rest]) ->
  case lists:member(Char, "([{<") of
    true ->
      first_illegal_char(Rest, [Char]);
    false ->
      Char
  end.

first_illegal_char([], _) ->
  none;
first_illegal_char([Char | Rest], OpenChunks) ->
  case lists:member(Char, "([{<") of
    true ->
      first_illegal_char(Rest, [Char | OpenChunks]);
    false ->
      case Char == matching_chunk(hd(OpenChunks)) of
        true ->
          first_illegal_char(Rest, tl(OpenChunks));
        false ->
          Char
      end
  end.

complete_line(Line) ->
  complete_line(Line, []).

complete_line([], ClosingChunks) ->
  ClosingChunks;
complete_line([Char | Rest], ClosingChunks) ->
  case lists:member(Char, "([{<") of
    true ->
      complete_line(Rest, [matching_chunk(Char) | ClosingChunks]);
    false ->
      complete_line(Rest, tl(ClosingChunks))
  end.

error_score($)) ->
  3;
error_score($]) ->
  57;
error_score($}) ->
  1197;
error_score($>) ->
  25137.

matching_chunk($() ->
  $);
matching_chunk($)) ->
  $(;
matching_chunk($[) ->
  $];
matching_chunk($]) ->
  $[;
matching_chunk(${) ->
  $};
matching_chunk($}) ->
  ${;
matching_chunk($<) ->
  $>;
matching_chunk($>) ->
  $<.

completion_score($)) ->
  1;
completion_score($]) ->
  2;
completion_score($}) ->
  3;
completion_score($>) ->
  4;
completion_score(List) when is_list(List) ->
  lists:foldl(fun(Char, Acc) -> Acc * 5 + completion_score(Char) end, 0, List).
