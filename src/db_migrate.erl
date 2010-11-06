%%%-------------------------------------------------------------------
%%% File    : db_migrate.erl
%%% Author  : root <root@CodeMachine>
%%% Description :
%%%
%%% Created :  2 Nov 2010 by root <root@CodeMachine>
%%%-------------------------------------------------------------------
-module(db_migrate).

-export([migrate_company/1, correct_quotes/1, migrate_quotes/1]).

-include_lib("/home/dude/projects/mysql/include/mysql.hrl").
-include("mnesia_defs.hrl").

migrate_company(Name) ->
    [C] = db_lib:dread(sec, Name),
    Instrument = C#sec.instrument,
    Market =
	if
	    is_atom(C#sec.market) ->
		atom_to_list(C#sec.market);
	    true ->
		C#sec.market
	end,

    mysql:prepare(
      infuse_stock,
      list_to_bitstring("insert into quotes_company (name, instrument, market) values (\"" ++
			Name ++"\",\"" ++ Instrument ++ "\",\"" ++ Market
			++ "\")")),
    mysql:transaction(
      1,
      fun() ->

	      mysql:execute(infuse_stock)
      end).

correct_quotes(Name) ->
    [C] = db_lib:dread(sec, Name),
    Data = C#sec.data,
    Instrument = C#sec.instrument,
    {List, _Acc} =
	lists:mapfoldl(
	  fun(S, Previous) ->
		  Closing = if not is_number(S#stock.closing) ->
				    Previous#stock.closing;
			       true ->
				    S#stock.closing
			    end,
		  Highest = if not is_number(S#stock.highest) ->
				    Previous#stock.highest;
			       true ->
				    S#stock.highest
			    end,
		  Lowest = if not is_number(S#stock.lowest) ->
				   Previous#stock.lowest;
			      true ->
				   S#stock.lowest
			   end,
		  Turnover = if not is_integer(S#stock.turnover) ->
				     Previous#stock.turnover;
				true ->
				     S#stock.turnover
			     end,
		  Volume = if not is_integer(S#stock.volume) ->
				   Previous#stock.volume;
			      true ->
				   S#stock.volume
			   end,
		  Completions = if not is_integer(S#stock.completions) ->
					Previous#stock.completions;
				   true ->
					S#stock.completions
				end,
		  Curr = S#stock{closing=Closing,
				 highest=Highest,
				 lowest=Lowest,
				 turnover=Turnover,
				 volume=Volume,
				 completions=Completions},
		  {Curr, Curr}
	  end, #stock{closing=0,highest=0,lowest=0,turnover=0,
		      volume=0,completions=0}, Data),
    {Instrument, List}.

migrate_quotes(Name) ->
    {Instrument, Corrected} = correct_quotes(Name),
    {data, MySqlRes} = mysql:fetch(1, "select id from quotes_company where instrument = '" ++ Instrument ++ "'"),
    [[Id]] = mysql:get_result_rows(MySqlRes),
    lists:foreach(
      fun(S) ->
	      #stock{
	   date=Date,
	   closing=Closing,
	   highest=Highest,
	   lowest=Lowest,
	   turnover=Turnover,
	   volume=Volume,
	   completions=Completions} = S,
	      Insert= get_insert_string(Id, Date, Closing, Highest, Lowest,
					Turnover, Volume, Completions),
	      mysql:prepare(
		add_stock,
		list_to_bitstring(Insert)
	       ),
	      mysql:transaction(
		1,
		fun() ->
			mysql:execute(add_stock)
		end)
      end,
      Corrected).

number_to_list(N) when is_float(N) ->
    [S] = io_lib:format("~.2f", [N]),
    S;
number_to_list(N) when is_integer(N) ->
    integer_to_list(N).


get_insert_string(Id, Date, Closing, Highest, Lowest,
		  Turnover, Volume, Completions) ->
    "insert into quotes_stockquote (company_id, date, highest, lowest, closing, turnover, volume, completions) values ("
	++ integer_to_list(Id) ++ ", \"" ++
	date_lib:convert_date_e_s(Date) ++ "\", "
	++ number_to_list(Highest) ++
	", " ++ number_to_list(Lowest) ++ ", "
	++ number_to_list(Closing) ++ ", "
	++ integer_to_list(Turnover) ++ ", "
	++ integer_to_list(Volume) ++ ", "
	++ integer_to_list(Completions) ++ ")".
