-module(db_lib).

-export([init/0]).

-include("mnesia_defs.hrl").

init() ->
    io:format("Starting mnesia.~n", []),
    case mnesia:start() of
	ok ->
	    io:format("Mnesia Started, create tables.~n", []),
	    case create_tables([{sec, set, record_info(fields, sec)}, 
				{mvg_avg, set, record_info(fields, mvg_avg)},
				{exp_avg, set, record_info(fields, exp_avg)},
				{adx, set, record_info(fields, adx)},
				{macd, set, record_info(fields, macd)},
				{atr, set, record_info(fields, atr)}]) of
		ok ->
		    
		    {ok, #state{}};
		{error, Reason} ->
		    {stop, Reason}
	    end;
	{error, Reason} ->
	    {stop, Reason}
    end.

create_table({TableName, Type, Fields}) ->
    io:format("Create table ~p~n", [TableName]),
    Info = mnesia:system_info(tables),
    case lists:member(TableName, Info) of
	true ->
	    case mnesia:create_table(TableName, [{type, Type},
						 {disc_copies, [node()]},
						 {attributes,
						  Fields}]) of
		{atomic, ok} ->
		    io:format("Created table ~p~n", [TableName]),
		    ok;
		{aborted, Reason} ->
		    io:format("Create table ~p failed with ~p~n", [TableName, Reason]),
		    {error, {create_failed, TableName, Reason}}
	    end;
	false ->
	    io:format("Table ~p already exists~n", [TableName]),
	    ok
    end.

create_tables(TableList) ->
    Results = lists:map(fun create_table/1, TableList),
    case lists:filter(fun(Res) -> Res /= ok end, Results) of
	[] ->	    
	    ok;
	Else ->
	    {error, Else}
    end.
    
