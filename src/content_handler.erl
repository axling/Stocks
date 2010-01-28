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

-define(UPDATE_TIMEOUT, 120000).

-define(BASE_DATE, {1987,1,1}).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, updating_content/3, dump/0]).

-record(state, {updating_content=[],
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
    gen_server:call(?MODULE, dump).

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
    ok = db_handler:ready(),
    ok = inets:start(),
    http:set_options([{proxy, {{"www-proxy.ericsson.se", 8080}, ["localhost"]}}]),
    Qh = db_handler:get_query_handle(company),
    Q = qlc:q([Company || Company <- Qh]),
    Companies = db_handler:q(Q),
    UpdatingCompanies = 
	lists:map(
	  fun(#company{name=Name, instrument=Instrument}) ->
		  Pid = spawn(?MODULE, updating_content, [self(), Name, Instrument]),
		  MonitorRef = erlang:monitor(process, Pid),
		  TimerRef = erlang:start_timer(?UPDATE_TIMEOUT, self(), {update_timeout, {Name, Pid}}),
		  {Name, MonitorRef, TimerRef, Pid}
	  end, Companies),
    erlang:start_timer(300000, self(), retry_timer_timeout),
    {ok, #state{updating_content=UpdatingCompanies}}.

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
    {reply, State, State};
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
handle_info({timeout, _, retry_timer_timeout}, #state{retry=[]}=State) ->
    {noreply, State};
handle_info({timeout, _, retry_timer_timeout}, #state{retry=RetryList}=State) ->
    Qh = db_handler:get_query_handle(company),
    Q = qlc:q([Company || Company <- Qh]),
    Companies = lists:filter(
		  fun(#company{name=CompanyName}) ->
			  lists:member(CompanyName, RetryList)
		  end, db_handler:q(Q)),
    UpdatingCompanies = 
	lists:map(
	  fun(#company{name=Name, instrument=Instrument}) ->
		  Pid = spawn(?MODULE, updating_content, [self(), Name, Instrument]),
		  MonitorRef = erlang:monitor(process, Pid),
		  TimerRef = erlang:start_timer(?UPDATE_TIMEOUT, self(), {update_timeout, {Name, Pid}}),
		  {Name, MonitorRef, TimerRef, Pid}
	  end, Companies),
    {noreply, State#state{retry = [],
			  updating_content=UpdatingCompanies}};
    
handle_info({'DOWN', MonitorRef, process, Pid, _Reason},
	    #state{updating_content=UpdCont,
		   retry=RetryList}=State) ->
    case lists:keyfind(MonitorRef, 2, UpdCont) of
	{Name, MonitorRef, TimerRef, Pid} ->
	    erlang:cancel_timer(TimerRef),
	    NewUpdcont = lists:keydelete(Name, 1, UpdCont),
	    {noreply, State#state{retry = [Name | RetryList],
				  updating_content = NewUpdcont}};
	false ->
	    io:format("Error: Received timeout for company that wasn't in the state ~p~n", 
		      [State]),
	    {noreply, State}
    end;

handle_info({timeout, TimerRef, {update_timeout, {Name, Pid}}},  
	    #state{updating_content=UpdCont,
		   retry=RetryList}=State) ->
    case lists:keyfind(Name, 1, UpdCont) of
	 {Name, MonitorRef, TimerRef, Pid} ->
	    erlang:demonitor(MonitorRef),	     
	    exit(Pid, kill),
	    NewUpdcont = lists:keydelete(Name, 1, UpdCont),
	    {noreply, State#state{retry = [Name | RetryList],
				  updating_content = NewUpdcont}};
	 false ->
	    exit(Pid, kill),
	    io:format("Error: Received timeout for company name ~p that wasn't in the state ~p~n", 
		      [Name, State]),
	    {noreply, State}
     end;

handle_info({updating_done, Name, Pid}, #state{updating_content=UpdCont}=State) ->
    case lists:keyfind(Name, 1, UpdCont) of
	{Name, MonitorRef, TimerRef, _Pid} ->
	    erlang:demonitor(MonitorRef),
	    erlang:cancel_timer(TimerRef),
	    Pid ! finish_updating,
	    NewUpdCont = lists:keydelete(Name, 1, UpdCont),
	    {noreply, State#state{updating_content=NewUpdCont}};
	false ->
	    Pid ! finish_updating,
	    io:format("Error: Received updating_done for company name ~p that wasn't in the state ~p~n", 
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
updating_content(Pid, Name, Instrument) ->
    {Today, _} = calendar:now_to_local_time(now()),
   
    Qh = db_handler:get_query_handle(stocks),
    Q = qlc:q([Stock#stocks.date || Stock <- Qh, Stock#stocks.company == Name,
		       date_lib:is_greater(Stock#stocks.date, ?BASE_DATE),
		       date_lib:is_greater(Today, Stock#stocks.date)]),
    Stocks = db_handler:q(Q),
    LatestDate = 
	case date_lib:get_latest_date(Stocks) of
	    [] ->
		?BASE_DATE;
	    Date when is_tuple(Date) ->
		Date
	end,
    LatestDatePlusOne = calendar:date_to_gregorian_days(LatestDate) + 1,
    Latest = calendar:gregorian_days_to_date(LatestDatePlusOne),
    case date_lib:is_greater(Today, Latest) of
	true ->
	    ok = omx_db_pop:save_instrument(Instrument, Name, Latest, Today),
	    Pid ! {updating_done, Name, self()};
	false ->
	    Pid ! {updating_done, Name, self()}
    end,
    
    receive
	finish_updating ->
	    ok
    end.
	
	    
    
    
