-module(db_lib).

-export([init/0, t/1, dread/2, dwrite/1, read/2, write/1, sread/2, swrite/1,
	 e/1, h/1, delete/2]).

-include("mnesia_defs.hrl").

init() ->
    io:format("Starting mnesia.~n", []),
    case mnesia:start() of
	ok ->
	    io:format("Mnesia Started, create tables.~n", []),
	    case create_tables([{sec, set, record_info(fields, sec)}, 
				{analysis, bag, record_info(fields, analysis)},
				{task, set, record_info(fields, task)}]) of
		ok ->		    
		    ok;
		{error, Reason} ->
		    erlang:error(Reason)
	    end;
	{error, Reason} ->
	    erlang:error(Reason)
    end.

t(Fun) ->
    case mnesia:transaction(Fun) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    {error, Reason}
    end.    

dread(Tab, Key) ->
    mnesia:dirty_read({Tab, Key}).

dwrite(Record) ->
    mnesia:dirty_write(Record).

read(Tab, Key) ->
    mnesia:read({Tab, Key}).

write(Record) ->
    mnesia:write(Record).

sread(Tab, Key) ->
    t(fun() ->
	     read(Tab, Key)
      end).
swrite(Record) ->
    t(fun() ->
	     write(Record)
      end).

e(Q) ->
    t(fun() ->
	      qlc:e(Q)
      end).

h(Table) ->
    mnesia:table(Table).

delete(Table, Key) ->
    t(fun() -> 
	      mnesia:delete({Table, Key}) 
      end).
    
create_table({TableName, Type, Fields}) ->
    io:format("Create table ~p~n", [TableName]),
    Info = mnesia:system_info(tables),
    case lists:member(TableName, Info) of
	false ->
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
	true ->
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
    
