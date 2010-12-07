%%%-------------------------------------------------------------------
%%% File    : db_mysql.erl
%%% Author  : Erik Axling <dude@CodeMachine>
%%% Description : 
%%%
%%% Created : 14 Nov 2010 by Erik Axling <dude@CodeMachine>
%%%-------------------------------------------------------------------
-module(db_mysql).

-behaviour(gen_server).

%% API
-export([start_link/0, 
	 lookup_stock/2, 
	 lookup_stock/3, 
	 insert_stock/2,
	 insert/3,
	 insert_optimize_result/6,
	 clear_optimize_result/1,
	 count_all_stocks/1,
	 get/3,
	 get_all/2,
	 get_all_companies/0,
	 get_all_stocks/1,
	 get_nr_of_stocks/2,
	 get_company_instrument/1,
	 get_latest_stock_date/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("mnesia_defs.hrl").

-record(state, {
	  pid,
	  monitor
	 }).

-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert_stock(Instrument, #stock{}=Stock)
  when is_list(Instrument) ->
    check_return(gen_server:call(?SERVER, {insert_stock, Instrument, Stock}, 
				 infinity)).
clear_optimize_result(Company) ->
    check_return(gen_server:call(?SERVER, {clear_optimize_result, Company})).

insert_optimize_result(Company, #market{}=Market, Point, StartDate, 
		       EndDate, Type)
  when is_tuple(Point), is_atom(Type), is_list(Company), is_tuple(StartDate),
       is_tuple(EndDate) ->
    check_return(gen_server:call(?SERVER, {insert_optimize_result, Company, 
					   Market, Point, StartDate, EndDate, 
					   Type})).

get_all_companies() ->
    check_return(gen_server:call(?SERVER, get_all_companies, infinity)).

count_all_stocks(Company) when is_list(Company) ->
    check_return(gen_server:call(?SERVER, {count_all_stocks, Company},
				 infinity)).
get_all_stocks(Company) when is_list(Company) ->
    check_return(gen_server:call(?SERVER, {get_all_stocks, Company}, infinity)).

get_nr_of_stocks(Company, Limit) when is_list(Company), is_integer(Limit) ->
    check_return(gen_server:call(?SERVER, {get_nr_of_stocks, Company, Limit},
				 infinity)).

get_all(Type, Company) when is_atom(Type), is_list(Company) ->
    check_return(gen_server:call(?SERVER, {get_all, convert_type(Type), 
					   Company}, infinity)).
get(Type, Company, Limit) when is_atom(Type), is_list(Company), 
			       is_integer(Limit) ->
    check_return(gen_server:call(?SERVER, {get, convert_type(Type), 
					   Company, Limit}, infinity)).

insert(Type, Company, Entry) when is_atom(Type), is_list(Company) ->    
    check_return(gen_server:call(?SERVER, {insert, convert_type(Type), 
					   Company, Entry}, infinity)).

get_company_instrument(Name) when is_list(Name) ->
    check_return(gen_server:call(?SERVER, {get_company_instrument, Name}, 
				 infinity)).

get_latest_stock_date(Instrument) when is_list(Instrument) ->
    check_return(gen_server:call(?SERVER, {get_latest_stock_date, Instrument}, 
				 infinity)).

lookup_stock(Name, Date)
  when is_list(Name), is_tuple(Date) ->
    check_return(gen_server:call(?SERVER, {lookup_stock, Name, Date}, 
				 infinity)).

lookup_stock(Name, FromDate, ToDate)
  when is_list(Name), is_tuple(FromDate), is_tuple(ToDate) ->
    check_return(gen_server:call(?SERVER, {lookup_stock, Name, FromDate, 
					   ToDate}, infinity)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {Pid, MonitorRef} = start_mysql("localhost", undefined, "stocks_user",
				    "djangostocksistheshit71", "stocks"),
    {ok, #state{pid=Pid, monitor=MonitorRef}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({lookup_stock, Name, Date}, _From, #state{}=State) ->
    Query = "select * from quotes_stockquote inner join quotes_company on quotes_stockquote.company_id = quotes_company.id where name='" ++ Name ++ "' and date='" ++ date_lib:convert_date_e_s(Date) ++"'",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    [[{date,Date}, Highest, Lowest, Closing, Turnover, Volume, 
	      Completions]] = mysql:get_result_rows(Res),
	    Return =
		#stock{date=Date, highest=Highest, 
		       lowest=Lowest, closing=Closing, turnover=Turnover, 
		       volume=Volume, completions=Completions},	
	    {reply, Return, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({lookup_stock, Name, FromDate, ToDate}, _From, #state{}=State) ->
    Query = "select date, highest, lowest, closing, turnover, volume, completions from quotes_stockquote inner join quotes_company on quotes_stockquote.company_id = quotes_company.id where name='" ++ Name ++ "' and date >='" ++ date_lib:convert_date_e_s(FromDate) ++"' and date <= '" ++ date_lib:convert_date_e_s(ToDate) ++ "' order by date asc",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    Return = [#stock{date=Date, highest=Highest, 
			     lowest=Lowest, closing=Closing, turnover=Turnover, 
			     volume=Volume, completions=Completions} 
		      || [{date, Date}, Highest, Lowest, Closing, Turnover, 
			  Volume, Completions] <- mysql:get_result_rows(Res)],
	    {reply, Return, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({count_all_stocks, Company}, _From, #state{}=State) ->
    Query = "select count(quotes_stockquote.id) from quotes_stockquote inner join quotes_company on quotes_stockquote.company_id = quotes_company.id where name='" ++ Company ++ "'",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    [[NrOfRows]] = mysql:get_result_rows(Res),
	    {reply, NrOfRows, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;

handle_call({get_all_stocks, Company}, _From, #state{}=State) ->
    Query = "select date, highest, lowest, closing, turnover, volume, completions from quotes_stockquote inner join quotes_company on quotes_stockquote.company_id = quotes_company.id where name='" ++ Company ++ "' order by date asc",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    Return = [#stock{date=Date, highest=Highest, 
			     lowest=Lowest, closing=Closing, turnover=Turnover, 
			     volume=Volume, completions=Completions} 
		      || [{date, Date}, Highest, Lowest, Closing, Turnover, 
			  Volume, Completions] <- mysql:get_result_rows(Res)],
	    {reply, Return, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({get_nr_of_stocks, Company, Limit}, _From, #state{}=State) ->
    Query = "select date, highest, lowest, closing, turnover, volume, completions from quotes_stockquote inner join quotes_company on quotes_stockquote.company_id = quotes_company.id where name='" ++ Company ++ 
	"' order by date asc limit 0, " ++ integer_to_list(Limit),
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    Return = [#stock{date=Date, highest=Highest, 
			     lowest=Lowest, closing=Closing, turnover=Turnover, 
			     volume=Volume, completions=Completions} 
		      || [{date, Date}, Highest, Lowest, Closing, Turnover, 
			  Volume, Completions] <- mysql:get_result_rows(Res)],
	    {reply, Return, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({get_all, Type, Company}, _From, #state{}=State) ->
    Query = "select " ++ get_fields(Type) ++ " from quotes_" ++ Type ++ 
	" inner join quotes_company on quotes_" ++ Type ++ 
	".company_id = quotes_company.id where name='" ++ Company ++ "'",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    Return = get_return_struct(Type, mysql:get_result_rows(Res)),     
	    {reply, Return, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({get, Type, Company, Limit}, _From, #state{}=State) ->
    Query = "select " ++ get_fields(Type) ++ " from quotes_" ++ Type ++ 
	" inner join quotes_company on quotes_" ++ Type ++ 
	".company_id = quotes_company.id where name='" ++ Company ++ 
	"' order by date asc limit 0, " ++ integer_to_list(Limit),
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    Return = get_return_struct(Type, mysql:get_result_rows(Res)),     
	    {reply, Return, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({insert_stock , Instrument, Stock}, _From, #state{}=State) ->
    FetchQuery = "select id from quotes_company where instrument='" ++
	Instrument ++ "'",
    {data, IdRes} = mysql:fetch(1, FetchQuery, infinity),
    [[CompanyId]] = mysql:get_result_rows(IdRes),
    InsertQuery = list_to_bitstring(
		    "insert into quotes_stockquote (company_id, date, highest, lowest, closing, turnover, volume, completions) values (" ++
		    integer_to_list(CompanyId) ++ ", \"" ++
		    date_lib:convert_date_e_s(Stock#stock.date) ++ "\", "
		    ++ number_to_list(Stock#stock.highest) ++
		    ", " ++ number_to_list(Stock#stock.lowest) ++ ", "
		    ++ number_to_list(Stock#stock.closing) ++ ", "
		    ++ integer_to_list(Stock#stock.turnover) ++ ", "
		    ++ integer_to_list(Stock#stock.volume) ++ ", "
		    ++ integer_to_list(Stock#stock.completions) ++ ")"),
    mysql:prepare(insert_stock, InsertQuery),
    case mysql:transaction(1,
			   fun() ->
				   mysql:execute(insert_stock)
			   end, infinity) of
	{aborted, {Reason, {rollback_result, Result}}} ->
	    log_handler:log(db_mysql, 
			    "Error in insert_stock with reason ~p for instrument ~s and entry ~p, rollback initated with result~p~n",
			    [Reason, Instrument, Stock, Result]),
	    {reply, {error, {Reason, Result}}, State};
	{atomic, _Result} ->
	    {reply, ok, State}
    end;
handle_call({insert, Type, Company, Entry}, _From, State) ->
    CompanyId = get_company_id(Company),
    InsertQuery = 
	list_to_bitstring(
	  "insert into quotes_" ++ Type ++ 
	  " (company_id, " ++ get_fields(Type) ++
	  ") values (" ++ convert_entry(CompanyId, Entry) ++ ")"),
    mysql:prepare(insert_analysis, InsertQuery),
    case mysql:transaction(1, 
			   fun() ->
				   mysql:execute(insert_analysis)
			   end, infinity) of
	{aborted, {Reason, {rollback_result, Result}}} ->
	    log_handler:log(db_mysql, 
			    "Error in insert with reason ~p for company ~s and "
			    "entry ~p, rollback initated with result~p~n",
			    [Reason, Company, Entry, Result]),
	    {reply, {error, {Reason, Result}}, State};
	{atomic, _Result} ->
	    {reply, ok, State}
    end;
handle_call({insert_optimize_result, Company, Market, {X1, X2, X3, X4},
	     StartDate, EndDate, Type}, _From, State) ->
    CompanyId = get_company_id(Company),    
    InsertResultQuery = 
	list_to_bitstring(
	  "insert into quotes_optimizeresult (company_id, date_created, type, " 
	  "point, tax_factor, courtage, start_money, end_money, start_date, "
	  "end_date) values (" ++
	  integer_to_list(CompanyId) ++ ", \"" ++ 
	  date_lib:convert_date_e_s(date_lib:today()) ++ 
	  "\", \"" ++ atom_to_list(Type) ++ "\", \"" ++ 
	  number_to_list(X1) ++ ";" ++ number_to_list(X2) ++ ";" ++ 
	  number_to_list(X3) ++ ";" ++ number_to_list(X4) ++ "\", " ++
	  number_to_list(Market#market.tax_factor) ++ ", " ++ 
	  integer_to_list(Market#market.courtage) ++ ", " ++ 
	  "5000" ++ ", " ++ number_to_list(Market#market.money) ++ 
	  ", \"" ++ date_lib:convert_date_e_s(StartDate) ++  
	  "\", \"" ++ date_lib:convert_date_e_s(EndDate) ++ "\")"),
    mysql:prepare(insert_optimize_result, InsertResultQuery),
    case mysql:transaction(1, 
			   fun() ->
				   mysql:execute(insert_optimize_result)
			   end, infinity) of
	{aborted, {Reason, {rollback_result, Result}}} ->
	    log_handler:log(db_mysql, 
			    "Error in insert_optimize_result with reason ~p for"
			    " company ~s and query: ~p~nRollback initated with"
			    " result~p~n",
			    [Reason, Company, InsertResultQuery, Result]),
	    {reply, {error, {Reason, Result}}, State};
	{atomic, _Result} ->
	    Result = insert_optimizeresult_history(Company, Market),
	    {reply, Result, State}
    end;
handle_call({clear_optimize_result, Company}, _From, State) ->
    CompanyId = get_company_id(Company),
    GetResultIdQuery = 
	"select id from quotes_optimizeresult where company_id=" ++ 
	integer_to_list(CompanyId),
    {data, ResultIdRes} = mysql:fetch(1, GetResultIdQuery),
    case mysql:get_result_rows(ResultIdRes) of
	[[ResultId]] ->
	    DeleteResultQuery = 
		list_to_bitstring("delete from quotes_optimizeresult where " 
				  "company_id=" ++ integer_to_list(CompanyId)),
	    DeleteResultHistoryQuery = 
		list_to_bitstring("delete from quotes_optimizehistory where "
				  "result_id=" ++ integer_to_list(ResultId)),
	    mysql:prepare(delete_result, DeleteResultQuery),
	    mysql:prepare(delete_history, DeleteResultHistoryQuery),
	    case mysql:transaction(1,
				   fun() ->
					   mysql:execute(delete_result),
					   mysql:execute(delete_history)
				   end, infinity) of
		{aborted, {Reason, {rollback_result, Result}}} ->
		    log_handler:log(db_mysql, 
				    "Error in delete_result or delete_history "
				    "with reason ~p for company ~s and queries:"
				    " ~p, ~p~nRollback initated with result "
				    "~p~n",
				    [Reason, Company, DeleteResultQuery, 
				     DeleteResultHistoryQuery, Result]),
		    {reply, {error, {Reason, Result}}, State};
		{atomic, _Result} ->
		    {reply, ok, State}
	    end;
	[] ->
	    {reply, ok, State}		
    end;		
handle_call(get_all_companies, _From,  State) ->
    Query = "select name, instrument from quotes_company",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    AllCompanies = [{bitstring_to_list(Name), 
			     bitstring_to_list(Instrument)} 
			    || [Name, Instrument] 
				   <- mysql:get_result_rows(Res)],
	    {reply, AllCompanies, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({get_company_instrument, Name}, _From, State) ->
    Query = "select instrument from quotes_company where name='" ++ Name ++ "'",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    [[Instrument]] = mysql:get_result_rows(Res),
	    {reply, bitstring_to_list(Instrument), State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call({get_latest_stock_date, Instrument}, _From, State) ->
    Query = "select max(date) from quotes_stockquote inner join quotes_company on quotes_stockquote.company_id = quotes_company.id  where instrument='" 
	++ Instrument ++ "'",
    case mysql:fetch(1, Query, infinity) of
	{data, Res} ->
	    [[{date, Date}]] = mysql:get_result_rows(Res),
	    {reply, Date, State};
	{error, Res} ->
	    {reply, {error, mysql:get_result_reason(Res)}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, process, Pid, _Info}, 
	    #state{pid=Pid, monitor=MonitorRef}=State) ->
    {NewPid, NewMonitorRef} = start_mysql("localhost", undefined, "stocks_user",
					  "djangostocksistheshit71", "stocks"),
    {noreply, State#state{pid=NewPid, monitor=NewMonitorRef}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_mysql(Host, Port, User, Password, Database) ->
    %% LogFun = 
    %% 	fun(Module,Line,_Level,Fun) -> 
    %% 		{Msg, Args} = apply(Fun, []),		
    %% 		log_handler:log("MySql",
    %% 				"~s.~s: " ++ Msg++"~n",
    %% 				[atom_to_list(Module),
    %% 				 integer_to_list(Line) |
    %% 				 Args])
    %% 	end,
    case mysql:start(1, Host, Port, User, Password, Database, 
		     fun(_,_,_,_) -> ok end) of
	{ok, Pid} ->
	    MonitorRef = erlang:monitor(process, Pid),
	    {ok, {Pid, MonitorRef}};
	{error, Reason} ->
	    {error, Reason}
    end.

get_company_id(Company) ->
    {data, CompanyRes} = mysql:fetch(1, "select id from quotes_company where "
				     "name = \"" ++ Company ++ "\""),
    [[CompanyId]] = mysql:get_result_rows(CompanyRes),
    CompanyId.

insert_optimizeresult_history(Company, Market) ->
    CompanyId = get_company_id(Company),
    FetchQuery = "select id from quotes_optimizeresult where company_id = " ++ 
	integer_to_list(CompanyId),
    {data, Res} = mysql:fetch(1, FetchQuery),
    [[Id]] = mysql:get_result_rows(Res),
    InsertHistoryList = 
	[list_to_bitstring(
	   "insert into quotes_optimizehistory (result_id, type, date, price, "
	   "number) values (" ++ integer_to_list(Id) ++ ", \"" ++ 
	   atom_to_list(SimType) ++ "\", \"" ++ 
	   date_lib:convert_date_e_s(SimDate) 
	   ++ "\", " ++ number_to_list(SimPrice) ++ ", " ++ 
	   integer_to_list(SimNumber) ++ ")") || 
	    {SimType, _, SimDate,SimPrice, _Amount, SimNumber} <- 
		Market#market.history],
    CompoundedRes = 
	lists:map(
	  fun(AQuery) ->
		  mysql:prepare(insert_result_history, AQuery),
		  case mysql:transaction(
			 1, fun() ->
				    mysql:execute(insert_result_history)
			    end, infinity) of
		      {aborted, {Reason, {rollback_result, Result}}} ->
			  log_handler:log(
			    db_mysql, 
			    "Error in insert of result history with reason ~p "
			    "for company ~s and query: ~p~nRollback initated "
			    "with result ~p~n",
			    [Reason, Company, AQuery, Result]),
			  {error, {Reason, Result}};
		      {atomic, _Result} ->
			  ok
		  end
	  end, InsertHistoryList),
    case lists:filter(fun(ok) -> false; (_E) -> true end, 
		      CompoundedRes) of
	[] ->
	    ok;
	Else ->
	    {error, {several_insert_result_history_errors, 
		     Else}}
    end.

		
number_to_list(N) when is_float(N) ->
    [S] = io_lib:format("~.3f", [N]),
    S;
number_to_list(N) when is_integer(N) ->
    integer_to_list(N).

get_return_struct("expavg", Result) ->
    [#exp_avg{date=Date, ten=Ten, thirty=Thirty} 
     || {{date, Date}, Ten, Thirty} <- Result];
get_return_struct("mvgavg", Result) ->
    [#mvg_avg{date=Date, ten=Ten, thirty=Thirty} 
     || {{date, Date}, Ten, Thirty} <- Result];
get_return_struct("atr", Result) ->
    [#atr{date=Date, value=Atr} || {{date, Date}, Atr} <- Result];
get_return_struct("adx", Result) ->
    [#adx{date=Date, value=Adx, di_plus=DiPlus, di_minus=DiMinus} 
     || {{date, Date}, Adx, DiPlus, DiMinus} <- Result];
get_return_struct("macd", Result) ->
    [#macd{date=Date, value=Macd, signal=Signal} 
     || {{date, Date}, Macd, Signal} <- Result];
get_return_struct("stochastic", Result) ->
    [#stochastic{date=Date, percent_k=PercentK, percent_d=PercentD} 
     || {{date, Date}, PercentK, PercentD} <- Result] .
    
get_fields("expavg") ->    
    "date, ten, thirty";
get_fields("mvgavg") ->
    "date, ten, thirty";
get_fields("atr") ->
    "date, atr";
get_fields("stochastic") ->
    "date, percent_k, percent_d";
get_fields("macd") ->
    "date, macd, signal";
get_fields("adx") ->
    "date, adx, di_plus, di_minus".

convert_type(exp_avg) ->
    "expavg";
convert_type(mvg_avg) ->
    "mvgavg";
convert_type(atr) ->
    "atr";
convert_type(adx) ->
    "adx";
convert_type(stochastic) ->
    "stochastic";
convert_type(macd) ->
    "macd".

convert_entry(CompanyId, #exp_avg{date=Date, ten=Ten, thirty=Thirty}) ->
    integer_to_list(CompanyId) ++ ", \"" ++ date_lib:convert_date_e_s(Date) 
	++ "\", " ++ number_to_list(Ten) ++ ", " ++ number_to_list(Thirty);
convert_entry(CompanyId, #mvg_avg{date=Date, ten=Ten, thirty=Thirty}) ->
    integer_to_list(CompanyId) ++ ", \"" ++ date_lib:convert_date_e_s(Date) ++ 
	"\", " ++ number_to_list(Ten) ++ ", " ++ number_to_list(Thirty);
convert_entry(CompanyId, #atr{date=Date, value=Atr}) ->
    integer_to_list(CompanyId) ++ ", \"" ++ date_lib:convert_date_e_s(Date) ++ 
	"\", " ++ number_to_list(Atr);
convert_entry(CompanyId, #adx{date=Date, value=Adx, di_plus=DiPlus, 
			      di_minus=DiMinus}) ->
    integer_to_list(CompanyId) ++ ", \"" ++ date_lib:convert_date_e_s(Date) ++ 
	"\", " ++ number_to_list(Adx) ++ ", " ++ number_to_list(DiPlus) ++ ", "
	++ number_to_list(DiMinus);
convert_entry(CompanyId, #stochastic{date=Date, percent_k=PercentK, 
				     percent_d=PercentD}) ->
    integer_to_list(CompanyId) ++ ", \"" ++ date_lib:convert_date_e_s(Date) ++ 
	"\", " ++ number_to_list(PercentK) ++ ", " ++ number_to_list(PercentD);
convert_entry(CompanyId, #macd{date=Date, value=Macd, signal=Signal}) ->
    integer_to_list(CompanyId) ++ ", \"" ++ date_lib:convert_date_e_s(Date) ++ 
	"\", " ++ number_to_list(Macd) ++ ", " ++ number_to_list(Signal).
    
check_return({error, Reason}) ->
    erlang:error(Reason);
check_return(Else) ->
    Else.
