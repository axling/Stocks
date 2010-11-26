%%% File    : optimize_tests.erl
%%% Author  : Erik Axling <dude@CodeMachine>
%%% Description : 
%%% Created : 12 Nov 2010 by Erik Axling <dude@CodeMachine>

-module(optimize_tests).

-include_lib("eunit/include/eunit.hrl").

hill_climb_test() ->
    ?assertMatch({5, {0}}, 
		 optimize:hill_climb(fun({X}) -> 5 - X*X end, 1)),
    ?assertMatch({5, {0, 0}}, 
		 optimize:hill_climb(fun({X, Y}) -> 5 - X*X - Y*Y end, 2)),
    ?assertMatch({5, {0, 0, 0}}, 
		 optimize:hill_climb(fun({X, Y, Z}) -> 
					     5 - X*X - Y*Y - Z*Z end, 3, 6)),
    ?assertMatch({5, {0, 0, 0}}, 
		 optimize:hill_climb(
		   fun({X, Y, Z}) 
		      -> 5 - X*X - Y*Y - Z*Z end, {8, 43, 4})).

simulated_annealing_test() ->
    ?assertMatch({5, {0}}, 
		 optimize:simulated_annealing(
		   fun({X}) -> 5 - X*X end, {100}, 10000, 0.95, 10000)).
