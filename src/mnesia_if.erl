%%%-------------------------------------------------------------------
%%% File    : mnesia_if.erl
%%% Author  : 
%%% Description : 
%%%
%%% Created : 22 Jan 2010
%%%-------------------------------------------------------------------
-module(mnesia_if).

-include_lib("stdlib/include/qlc.hrl").
-include("mnesia_defs.hrl").

%% API
-export([add_table/1, add_entry/1]).

add_table(Name) ->
    mnesia:create_table(Name, [{disc_copies, []},
			       {type, bag},
			       {attributes,
				record_info(fields, stocks)}]).
	       
add_entry(Entry) ->
    F = fun() ->
		mnesia:write(Entry)
	end,
    mnesia:transaction(F).
    
