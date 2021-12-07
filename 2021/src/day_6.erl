-module(day_6).

-import(lists, [map/2]).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  solve(Filename, 80).

p2(Filename) ->
  solve(Filename, 256).

%%====================================================================
%% Internal functions
%%====================================================================

solve(Filename, Days) ->
  Input = helper:read_input(Filename, [<<",">>, <<"\n">>], string),
  Fish = map(fun list_to_integer/1, Input),

  NewFish = simulate(helper:frequency_map(Fish), Days),

  lists:sum(
    maps:values(NewFish)).

simulate(Fish, 0) ->
  Fish;
simulate(Fish, Days) ->
  NewFish =
    maps:fold(fun (0, Freq, Acc) ->
                    maps:update_with(6, fun(V) -> Freq + V + 1 end, Freq, Acc#{8 => Freq});
                  (Age, Freq, Acc) ->
                    maps:update_with(Age - 1, fun(V) -> Freq + V end, Freq, Acc)
              end,
              #{},
              Fish),
  simulate(NewFish, Days - 1).
