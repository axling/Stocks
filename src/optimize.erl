%%%-------------------------------------------------------------------
%%% File    : optimize.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : General optimizaton algorithms
%%%
%%% Created : 30 May 2009 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(optimize).

-compile(export_all).

-define(MAX, 1000).

-include("stocks.hrl").

find() ->
    find_optimal({0,0}, {0, {0,0}}).

find_optimal({?MAX,?MAX+1}, {LargestSolution, {LP1, LP2}}) ->
    {LargestSolution, {LP1, LP2}};
find_optimal({?MAX, P2}, {LargestSolution, {LP1, LP2}}) ->
    Money = run_esim(?MAX/100,P2/100),
    case Money > LargestSolution of
	true ->
	    find_optimal({0, P2+1}, {Money, {?MAX,P2}});
	false ->
	    find_optimal({0, P2+1}, {LargestSolution, {LP1,LP2}})
    end;
find_optimal({P1,P2}, {LargestSolution, {LP1, LP2}}) ->
    Money = run_esim(P1/100,P2/100),
    case Money > LargestSolution of
	true ->
	    find_optimal({P1+1, P2}, {Money, {P1,P2}});
	false ->
	    find_optimal({P1+1, P2}, {LargestSolution, {LP1,LP2}})
    end.

run_esim(P1, P2) ->
    Market = esim:run([{ericb, "ericsson.dets"}],
		      [{buy,ericb,[{ericb,[{5,P1}]}]},
		       {sell,ericb,[{ericb,[{5,P2}]}]}], 
		      #market{}),
    Market#market.money.


hill_climb() ->
    hill_climb({random:uniform(?MAX),random:uniform(?MAX)}, {0, {0,0}}).

hill_climb({0,P2},Solution) ->
    Result = 
	[{run_esim(Param1/100,Param2/100), {Param1, Param2}} ||
	    {Param1, Param2} <- [{0,P2-1},{1, P2},{0,P2+1}]],
    calculate_result(Result, Solution);
hill_climb({?MAX,P2}, Solution) ->
    Result = 
	[{run_esim(Param1/100,Param2/100), {Param1, Param2}} ||
	    {Param1, Param2} <- [{?MAX-1,P2},{?MAX,P2-1},{?MAX,P2+1}]],
    calculate_result(Result, Solution);
hill_climb({P1,0}, Solution) ->
    Result = 
	[{run_esim(Param1/100,Param2/100), {Param1, Param2}} ||
	    {Param1, Param2} <- [{P1-1,0}, {P1+1, 0}, {P1,1}]],
    calculate_result(Result, Solution);
hill_climb({P1,?MAX}, Solution) ->
    Result = 
	[{run_esim(Param1/100,Param2/100), {Param1, Param2}} ||
	    {Param1, Param2} <- [{P1-1,?MAX},{P1,?MAX-1},{P1+1, ?MAX}]],
    calculate_result(Result, Solution);
hill_climb({P1,P2},{LS, {BP1,BP2}}) 
  when P1 > 0, P2 > 0, P1 < ?MAX, P2 < ?MAX ->
    Result = 
	[{run_esim(Param1/100,Param2/100), {Param1, Param2}} ||
	    {Param1, Param2} <- [{P1-1,P2},{P1,P2-1},{P1+1, P2},{P1,P2+1}]],
    calculate_result(Result, {LS,{BP1, BP2}}).

calculate_result(Result, {LS, {BP1, BP2}}) ->
    Max = lists:max(Result),
    MaxMoney = element(1, Max),
    MaxCoord = element(2, Max),
    if 
	MaxMoney > LS ->
	    hill_climb(MaxCoord, {MaxMoney, MaxCoord});
	true ->
	    {LS, {BP1, BP2}}
    end.

    
climb_until(Sum) -> 
    R = optimize:hill_climb(), 
    case element(1,R) > Sum of 
	true -> 
	    R; 
	false ->
	    climb_until(Sum)
    end.
