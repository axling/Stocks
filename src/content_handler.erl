%%%-------------------------------------------------------------------
%%% File    : content_handler.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created : 26 Jan 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(content_handler).

-behaviour(gen_server).

-include("mnesia_defs.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(BASE_DATE, {1987,1,1}).

%% API
-export([start_link/0, update_content/0, update_content/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, updating_content/3, dump/0]).

-record(state, {current_company,
		updating_content=[],
		retry=[],
		retry_timer}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
dump() ->
    gen_server:call(?MODULE, dump, infinity).

update_content() ->
    update_content(all).
    
update_content(Companies) ->
    gen_server:cast(?MODULE, {update_content, Companies}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    process_flag(trap_exit, true),
    ok = inets:start(),
        
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(dump, _From, State) ->
    {reply, State, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update_content, all}, #state{updating_content=UpdComp}=State) ->
    AllCompanies = db_mysql:get_all_companies(),
    FilteredCompanies = 
	lists:filter(
	  fun({Name, _Instrument}) ->
		  not lists:keymember(Name, 1, UpdComp)
	  end, AllCompanies),
    CompaniesUpdating = update_from_database(FilteredCompanies),
    {noreply, State#state{updating_content=lists:append([UpdComp, CompaniesUpdating])}};

handle_cast({update_content, Companies}, 
	    #state{updating_content=UpdComp}=State) when is_list(Companies) ->
    CompaniesToUpdate = [{Name, db_mysql:get_company_instrument(Name)} 
			 || Name <- Companies],
    FilteredCompanies = 
	lists:filter(
	  fun({Name, _Instrument}) ->
		  not lists:keymember(Name, 1, UpdComp)
	  end, CompaniesToUpdate),
    CompaniesUpdating = update_from_database(FilteredCompanies),		      
    {noreply, State#state{updating_content=lists:append(
					     [UpdComp, CompaniesUpdating])}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({timeout, _, retry_timer_timeout}, #state{retry=[]}=State) ->
    {noreply, State};
handle_info({timeout, _, retry_timer_timeout}, #state{retry=RetryList}=State) ->
    CompaniesToUpdate = [{Name, db_mysql:get_company_instrument(Name)} 
			 || Name <- RetryList],
    UpdatingCompanies = update_from_database(CompaniesToUpdate),
    ok = send_start_for_next_batch(UpdatingCompanies),
    erlang:start_timer(300000, self(), retry_timer_timeout),
    {noreply, State#state{retry = [],
			  updating_content=UpdatingCompanies}};
    
handle_info({'DOWN', MonitorRef, process, Pid, Reason},
	    #state{updating_content=UpdCont,
		   retry=RetryList}=State) ->
    case lists:keyfind(MonitorRef, 2, UpdCont) of
	{Name, MonitorRef, Pid} ->	    
	    log_handler:log(
	      "content_handler",
	      "Received exit with reason ~p for company ~s~n", 
	      [Reason, Name]),
	    NewUpdCont = lists:keydelete(Name, 1, UpdCont),
	    ok = send_start_for_next_batch(NewUpdCont),
	    {noreply, State#state{retry = [Name | RetryList],
				  updating_content = NewUpdCont}};
	false ->
	    log_handler:log(
	      "content_handler",
	      "Error: Received exit with reason ~p for company that wasn't in the state ~p~n", 
	      [Reason, State]),
	    {noreply, State}
    end;

handle_info({no_update_needed, Name, Pid}, #state{updating_content=UpdCont}=State) ->
    case lists:keyfind(Name, 1, UpdCont) of
	{Name, MonitorRef, Pid} ->
	    erlang:demonitor(MonitorRef),
	    Pid ! finish_updating,	    
	    NewUpdCont = lists:keydelete(Name, 1, UpdCont),
	    ok = send_start_for_next_batch(NewUpdCont),
	    {noreply, State#state{updating_content=NewUpdCont}};
	false ->
	    Pid ! finish_updating,
	    log_handler:log(
	      "content_handler",
	      "Error: Received no_update_needed for company name ~p that wasn't in the state ~p~n", [Name, State]),
	    {noreply, State}
    end;

handle_info({updating_done, Name, Pid}, #state{updating_content=UpdCont}=State) ->
    case lists:keyfind(Name, 1, UpdCont) of
	{Name, MonitorRef, _Pid} ->
	    erlang:demonitor(MonitorRef),
	    Pid ! finish_updating,
	    NewUpdCont = lists:keydelete(Name, 1, UpdCont),
	    ok = send_start_for_next_batch(NewUpdCont),
	    {noreply, State#state{updating_content=NewUpdCont}};
	false ->
	    Pid ! finish_updating,
	    log_handler:log(
	      "content_handler",
	      "Error: Received updating_done for company name ~p that wasn't in the state ~p~n", 
	      [Name, State]),
	    {noreply, State}
    end.		    

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    inets:stop().

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
update_from_database(Companies) ->      
    UpdatingCompanies = 
	lists:map(
	  fun({Name, Instrument}) ->
		  Pid = spawn(?MODULE, updating_content, [self(), Name, Instrument]),
		  MonitorRef = erlang:monitor(process, Pid),		  
		  {Name, MonitorRef, Pid}
	  end, Companies),
    erlang:start_timer(300000, self(), retry_timer_timeout),
    ok = send_start_for_next_batch(UpdatingCompanies),
    UpdatingCompanies.

updating_content(Pid, Name, Instrument) ->
    Today = date_lib:today(),
    
    LatestDate = 
	case db_mysql:get_latest_stock_date(Instrument)  of
	    [] ->
		?BASE_DATE;
	    Date when is_tuple(Date) ->
		Date
	end,
    LatestDatePlusOne = calendar:date_to_gregorian_days(LatestDate) + 1,
    Latest = calendar:gregorian_days_to_date(LatestDatePlusOne),
    
    case date_lib:is_greater(Today, Latest) of
	true ->
	    receive
		start_updating ->
		    ok
	    end,
	    StockList = omx_db_pop:save_instrument(Instrument, Latest, Today),
	    [ok = db_mysql:insert_stock(Instrument, Stock) 
	     || Stock <- StockList],
	    Pid ! {updating_done, Name, self()};
	false ->
	    Pid ! {no_update_needed, Name, self()}
    end,
    receive
	finish_updating ->
	    ok
    end.

send_start_for_next_batch([]) ->
    ok;
send_start_for_next_batch([{_,_, Pid} | _Companies]) ->
    timer:sleep(2000),
    Pid ! start_updating,
    ok.
