%%% File    : analysis_lib_tests.erl
%%% Author  : Erik Axling <dude@CodeMachine>
%%% Description : 
%%% Created :  7 Nov 2010 by Erik Axling <dude@CodeMachine>

-module(analysis_lib_tests).

-include_lib("eunit/include/eunit.hrl").

-include("mnesia_defs.hrl").

%% analyse_macd_test() ->
%%     Dates = get_date_vector(),
%%     analyse(macd, [#stock{date=Date,
%% 			  highest=10,
%% 			  lowest=10,
%% 			  closing=10} || Date <- Dates], []).

analyse_mvg_avg_test() ->
    ?assertMatch([#mvg_avg{date={2000,1,30}, ten=10.0, thirty=10.0}],
		 analysis_lib:analyse(
		   mvg_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], [])),
    ?assertMatch([#mvg_avg{date={1999,12,4}, ten=20, thirty=20},
		  #mvg_avg{date={2000,1,30}, ten=10.0, thirty=10.0}],
		 analysis_lib:analyse(
		   mvg_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], 
		   [#mvg_avg{date={1999,12,4}, ten=20, thirty=20}])),
    ?assertMatch([#mvg_avg{date={2000,1,29}, ten=20, thirty=20},
		  #mvg_avg{date={2000,1,30}, ten=10.0, thirty=10.0}],
		 analysis_lib:analyse(
		   mvg_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], 
		   [#mvg_avg{date={2000,1,29}, ten=20, thirty=20}])),
    ?assertMatch([],
		 analysis_lib:analyse(
		   mvg_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_15_date_vector()], [])),
    ?assertMatch([#mvg_avg{date={2000, 2, 1}}],
		 analysis_lib:analyse(
		   mvg_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], 
		   [#mvg_avg{date={2000, 2, 1}}])).


analyse_ema_test() ->
    ?assertMatch([#exp_avg{date={2000,1,30}, ten=10.0, thirty=10.0}],
		 analysis_lib:analyse(
		   exp_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], [])),
    ?assertMatch([#exp_avg{date={1999,12,4}, ten=20, thirty=20},
		  #exp_avg{date={2000,1,30}, ten=10.0, thirty=10.0}],
		 analysis_lib:analyse(
		   exp_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], 
		   [#exp_avg{date={1999,12,4}, ten=20, thirty=20}])),
    ?assertMatch([#exp_avg{date={2000,1,29}, ten=20, thirty=20},
		  #exp_avg{date={2000,1,30}, ten=10.0, thirty=10.0}],
		 analysis_lib:analyse(
		   exp_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], 
		   [#exp_avg{date={2000,1,29}, ten=20, thirty=20}])),
    ?assertMatch([],
		 analysis_lib:analyse(
		   exp_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_15_date_vector()], [])),
    ?assertMatch([#exp_avg{date={2000, 2, 1}}],
		 analysis_lib:analyse(
		   exp_avg, [#stock{date=Date,
				    closing=10} 
			     || Date <- get_30_date_vector()], 
		   [#exp_avg{date={2000, 2, 1}}])).

analyse_stochastic_test() ->
    [#stochastic{date={2000, 1, 12}, percent_k=0.0, percent_d=0.0}] =
	analysis_lib:analyse(stochastic, [#stock{date=Date, highest=5,
						 lowest=2, closing=2} ||
					     Date <- get_12_date_vector()], []).

analyse_atr_test() ->
    [#atr{date={2000,1,14}, value=0.5542857142857146},
     #atr{date={2000,1,15}, value=0.5932653061224494}] =
	analysis_lib:analyse(atr, 
			     [#stock{date={2000,1,1},
				     highest=48.7,lowest=47.79, closing=48.16},
			      #stock{date={2000,1,2},
				     highest=48.72,lowest=48.14, closing=48.61},
			      #stock{date={2000,1,3},
				     highest=48.9,lowest=48.39, closing=48.75},
			      #stock{date={2000,1,4},
				     highest=48.87,lowest=48.37, closing=48.63},
			      #stock{date={2000,1,5},
				     highest=48.82,lowest=48.24, closing=48.74},
			      #stock{date={2000,1,6},
				     highest=49.05,lowest=48.64, closing=49.03},
			      #stock{date={2000,1,7},
				     highest=49.20,lowest=48.94, closing=49.07},
			      #stock{date={2000,1,8},
				     highest=49.35,lowest=48.86, closing=49.32},
			      #stock{date={2000,1,9},
				     highest=49.92,lowest=49.5, closing=49.91},
			      #stock{date={2000,1,10},
				     highest=50.19,lowest=49.87, closing=50.13},
			      #stock{date={2000,1,11},
				     highest=50.12,lowest=49.2, closing=49.53},
			      #stock{date={2000,1,12},
				     highest=49.66,lowest=48.9, closing=49.5},
			      #stock{date={2000,1,13},
				     highest=49.88,lowest=49.43, closing=49.75},
			      #stock{date={2000,1,14},
				     highest=50.19,lowest=49.73, closing=50.03},
			      #stock{date={2000,1,15},
				     highest=50.36,lowest=49.26, 
				     closing=50.31}], []).
analyse_macd_test() ->
    ?assertMatch([#macd{date={2000,2,3}, value=0.0, signal=0.0}],
		 analysis_lib:analyse(
		   macd, [#stock{date=Date,
				 closing=10} 
			  || Date <- get_34_date_vector()], [])),
    ?assertMatch([],
		 analysis_lib:analyse(
		   macd, [#stock{date=Date,
				 closing=10} 
			  || Date <- get_30_date_vector()], [])).

analyse_adx_test() ->
    Input = [#stock{date={2000, 1, 1},highest=48.7,lowest=47.79,closing=48.16},
	     #stock{date={2000, 1, 2},highest=48.72,lowest=48.14,closing=48.61},
	     #stock{date={2000, 1, 3},highest=48.9,lowest=48.39,closing=48.75},
	     #stock{date={2000, 1, 4},highest=48.87,lowest=48.37,closing=48.63},
	     #stock{date={2000, 1, 5},highest=48.82,lowest=48.24,closing=48.74},
	     #stock{date={2000, 1, 6},highest=49.05,lowest=48.64,closing=49.03},
	     #stock{date={2000, 1, 7},highest=49.20,lowest=48.94,closing=49.07},
	     #stock{date={2000, 1, 8},highest=49.35,lowest=48.86,closing=49.32},
	     #stock{date={2000, 1, 9},highest=49.92,lowest=49.5,closing=49.91},
	     #stock{date={2000,1,10},highest=50.19,lowest=49.87,closing=50.13},
	     #stock{date={2000,1,11},highest=50.12,lowest=49.2,closing=49.53},
	     #stock{date={2000,1,12},highest=49.66,lowest=48.9,closing=49.5},
	     #stock{date={2000,1,13},highest=49.88,lowest=49.43,closing=49.75},
	     #stock{date={2000,1,14},highest=50.19,lowest=49.73,closing=50.03},
	     #stock{date={2000,1,15},highest=50.36,lowest=49.26,closing=50.31},
	     #stock{date={2000,1,16},highest=48.7,lowest=47.79,closing=48.16},
	     #stock{date={2000,1,17},highest=48.72,lowest=48.14,closing=48.61},
	     #stock{date={2000,1,18},highest=48.9,lowest=48.39,closing=48.75},
	     #stock{date={2000,1,19},highest=48.87,lowest=48.37,closing=48.63},
	     #stock{date={2000,1,20},highest=48.82,lowest=48.24,closing=48.74},
	     #stock{date={2000,1,21},highest=49.05,lowest=48.64,closing=49.03},
	     #stock{date={2000,1,22},highest=49.20,lowest=48.94,closing=49.07},
	     #stock{date={2000,1,23},highest=49.35,lowest=48.86,closing=49.32},
	     #stock{date={2000,1,24},highest=49.92,lowest=49.5,closing=49.91},
	     #stock{date={2000,1,25},highest=50.19,lowest=49.87,closing=50.13},
	     #stock{date={2000,1,26},highest=50.12,lowest=49.2,closing=49.53},
	     #stock{date={2000,1,27},highest=49.66,lowest=48.9,closing=49.5},
	     #stock{date={2000,1,28},highest=50.19,lowest=49.87,closing=50.13}],
    ?assertMatch([#adx{date={2000,1,28}, value=20.91665236822243, 
		       di_plus = 20.487804878048728,
		       di_minus = 29.85365853658532}],
		 analysis_lib:analyse(
		   adx, Input, [])).

get_34_date_vector() ->
    [{2000,1, X} || X <- lists:seq(1, 31)] ++
	[{2000,2, X} || X <- lists:seq(1, 3)].

get_30_date_vector() ->
    [{2000,1, X} || X <- lists:seq(1, 30)].

get_15_date_vector() ->
    [{2000,1, X} || X <- lists:seq(1, 15)].

get_12_date_vector() ->
    [{2000,1, X} || X <- lists:seq(1, 12)].




