%%%-------------------------------------------------------------------
%%% File    : mnesia_if.erl
%%% Author  : 
%%% Description : 
%%%
%%% Created : 22 Jan 2010
%%%-------------------------------------------------------------------
-module(mnesia_if).

%% API
-export([init/0, add_table/2, add_entry/1]).

init() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start().
    
add_table(Name, Record).
	       
add_entry(Entry) ->
    mnesia:write(Entry)
