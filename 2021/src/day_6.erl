-module(day_6).

-import(lists, [nth/2, foldl/3, map/2, seq/2, min/1, max/1]).

%% The maximum number of days a single process can simulate a single fish for
-define(SIM_DAYS, 100).

%% API
-export([p1/1, p2/1]).

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  solve(Filename, 80).

p2(Filename) ->
  %% solving for 256 uses too much memory.
  %% perhaps we should split on list size, but we'd only know to split
  %% after doing a bunch of work, not parallel initially
  %%
  %% an optimisation idea - we don't need to solve for [1,1,2,2,3,3]
  %% we can solve for [1,2,3] and multiply the final length by 2
  solve(Filename, 80).

%%====================================================================
%% Internal functions
%%====================================================================

solve(Filename, Days) ->
  Input = helper:read_input(Filename, [<<",">>, <<"\n">>], string),
  Fish = map(fun list_to_integer/1, Input),

  lists:foreach(fun(F) -> spawn(?MODULE, simulate, [self(), F, Days]) end, Fish),

  rec_loop(length(Fish)).

rec_loop(N) ->
  rec_loop(N, 0).

rec_loop(0, Acc) ->
  Acc;
rec_loop(N, Acc) ->
  Fish =
    receive
      {fish, Fish} ->
        Fish
    end,
  rec_loop(N - 1, Fish + Acc).

%%====================================================================
%% Worker Processes
%%====================================================================

simulate(From, Fish, Days)
  when is_pid(From), is_integer(Fish), is_integer(Days), Days > 0 ->
  simulate(From, [Fish], Days);
simulate(From, Fish, Days) when is_pid(From), is_list(Fish), is_integer(Days), Days > 0 ->
  simulate(From, Fish, Days, []).

simulate(From, Fish, 0, _) ->
  From ! {fish, length(Fish)};
simulate(From, [], Days, NewFish) ->
  simulate(From, NewFish, Days - 1, []);
simulate(From, [0 | Rest], Days, NewFish) ->
  simulate(From, Rest, Days, [6, 8 | NewFish]);
simulate(From, [Fish | Rest], Days, NewFish) ->
  simulate(From, Rest, Days, [Fish - 1 | NewFish]).
