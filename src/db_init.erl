-module(db_init).

-include("mnesia_defs.hrl").

-export([init/0]).

init() ->
    {ok, DbTerms} = file:consult("db_init.db"),
    lists:foreach(
      fun({Name, Instrument}) ->
	      case db_handler:create_entry(
		     #company{name=Name,
			      instrument=Instrument}) of
		  ok ->
		      ok;
		  {error, Reason} ->
		      io:format("Error when putting company to database, ~p, ~p, Reason : ~p~n", 
				[Name, Instrument, Reason])
	      end
      end, DbTerms).
    
