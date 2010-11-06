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
    Qh = db_lib:h(sec),
    Companies = db_lib:e(qlc:q([{Company#sec.name, Company#sec.instrument} || Company <- Qh])),
    FilteredCompanies = 
	lists:filter(
	  fun({Name, _}) ->
		  not lists:keymember(Name, 1, UpdComp)
	  end, Companies),
    CompaniesUpdating = update_from_database(FilteredCompanies),
    {noreply, State#state{updating_content=lists:append([UpdComp, CompaniesUpdating])}};

handle_cast({update_content, Companies}, #state{updating_content=UpdComp}=State) 
  when is_list(Companies) ->
    Qh = db_lib:h(sec),    
    CompaniesToUpdate = db_lib:e(qlc:q([{C#sec.name, C#sec.instrument} || C <- Qh, lists:member(C#sec.name, Companies)])),
    FilteredCompanies = 
	lists:filter(
	  fun({Name, _}) ->
		  not lists:keymember(Name, 1, UpdComp)
	  end, CompaniesToUpdate),
    CompaniesUpdating = update_from_database(FilteredCompanies),		      
    {noreply, State#state{updating_content=lists:append([UpdComp, CompaniesUpdating])}};

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
    Qh = db_lib:h(sec),
    Companies = lists:filter(
		  fun({CompanyName, _Instrument}) ->
			  lists:member(CompanyName, RetryList)
		  end, db_lib:e(qlc:q([{Company#sec.name, Company#sec.instrument} || Company <- Qh]))),
    UpdatingCompanies = update_from_database(Companies),
    ok = send_start_for_next_batch(UpdatingCompanies),
    erlang:start_timer(300000, self(), retry_timer_timeout),
    {noreply, State#state{retry = [],
			  updating_content=UpdatingCompanies}};
    
handle_info({'DOWN', MonitorRef, process, Pid, _Reason},
	    #state{updating_content=UpdCont,
		   retry=RetryList}=State) ->
    case lists:keyfind(MonitorRef, 2, UpdCont) of
	{Name, MonitorRef, Pid} ->	    
	    NewUpdCont = lists:keydelete(Name, 1, UpdCont),
	    ok = send_start_for_next_batch(NewUpdCont),
	    {noreply, State#state{retry = [Name | RetryList],
				  updating_content = NewUpdCont}};
	false ->
	    io:format("Error: Received exit for company that wasn't in the state ~p~n", 
		      [State]),
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
	    io:format("Error: Received no_update_needed for company name ~p that wasn't in the state ~p~n", 
		      [Name, State]),
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
    
    [Company] = db_lib:sread(sec, Name),
    
    LatestDate = 
	case get_latest_date(Company#sec.data) of
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
	    MergedData = lists:append([StockList, Company#sec.data]),
	    Today1 = (hd(MergedData))#stock.date,
	    DayDate = date_lib:date_minus_days(Today1, 1),
	    WeekDate = date_lib:date_minus_days(Today1, 7),
	    MonthDate = date_lib:date_minus_days(Today1, 30),
	    YearDate = date_lib:date_minus_days(Today1, 365),
	    {DayTrend, WeekTrend, MonthTrend, YearTrend} = get_trends({DayDate, 1}, {WeekDate, 7}, {MonthDate, 30}, {YearDate, 365}, MergedData),
	    db_lib:swrite(Company#sec{data=MergedData,
				      day_trend=DayTrend,
				      week_trend=WeekTrend,
				      month_trend=MonthTrend,
				      year_trend=YearTrend}),
	    Pid ! {updating_done, Name, self()};
	false ->
	    Pid ! {no_update_needed, Name, self()}
    end,
    receive
	finish_updating ->
	    ok
    end.

send_start_for_next_batch(Companies) ->
    try hd(Companies) of
	{_,_,Pid} ->
	    timer:sleep(5000),
	    Pid ! start_updating,
	    ok
    catch _:_ ->
	    ok
    end.

get_latest_date([]) ->
    [];
get_latest_date(Data) ->
    First = hd(Data),
    First#stock.date.
	    
get_trends(DayDate, WeekDate, MonthDate, YearDate, Data) ->
    list_to_tuple([get_trend(Date, Data) || Date <- [DayDate, WeekDate, MonthDate, YearDate]]).

get_trend(_Date, []) ->
    0;
get_trend({Date, _Days}, Data) ->
    case date_lib:get_val_of_date([{D#stock.closing, D#stock.date} || D <- Data], Date) of
	{error, _} ->
	    0;
	Value ->
	    First = hd(Data),
	    (First#stock.closing - Value)/Value		
    end.
