-module(day_7).

-import(lists, [map/2, seq/2, min/1, max/1, sum/1]).

-format(ignore).

%% API
-export([p1/1, p2/1]).

%%====================================================================
%% API functions
%%====================================================================

p1(Filename) ->
  FuelCost = fun(Crab, Target) -> abs(Crab - Target) end,
  solve(Filename, FuelCost).

p2(Filename) ->
  FuelCost =
    fun(Crab, Target) ->
       N = abs(Crab - Target),
       N * (N + 1) div 2
    end,
  solve(Filename, FuelCost).

%%====================================================================
%% Internal functions
%%====================================================================

solve(Filename, FuelCost) ->
  Input = helper:read_input(Filename, [<<",">>, <<"\n">>], string),
  Crabs = map(fun list_to_integer/1, Input),

  min([
    sum([FuelCost(Crab, Target) || Crab <- Crabs])
    ||
    Target <- seq(min(Crabs), max(Crabs))
  ]).
