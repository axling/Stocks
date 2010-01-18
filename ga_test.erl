%%%-------------------------------------------------------------------
%%% File    : ga_test.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created : 27 May 2009 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(ga_test).

-include("stocks.hrl").

-export([test1/0, test1/1, esim_test/0, sca_test/1]).

test1() ->
    test1(2).

test1(Generations) ->
    ga:run([{ericb, "ericsson.dets"},{omx,"omx_telecom.dets"},{seb,"seb.dets"}],
	   [{buy,ericb,[{ericb,[1,5,10]},{omx,[1,2,4]}]},{buy,seb,[{seb,[2,10,50]}]},
	    {sell,ericb,[{ericb,[2,5,10]}]}, {sell,seb,[{seb,[3,15]}]}], 
	   #market{}, 96, Generations).

esim_test() ->
    esim:run([{ericb, "industrivarden.dets"}],
	     [{buy,ericb,[{ericb,[{10,0.45}]}]},{sell,ericb,[{ericb,[{10,0.56}]}]}], 
	     #market{risk=0.7}).

sca_test(Generations) ->
    ga:run([{sca, "industrivarden.dets"},{omx,"omx_30.dets"}],
	   [{buy,sca,[{sca,[3,5,10]},{omx,[3,25]}]},
	    {sell,sca,[{sca,[3,5,15]},{omx,[5,10]}]}], 
	   #market{}, 96, Generations).
