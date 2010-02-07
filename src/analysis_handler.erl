%%%-------------------------------------------------------------------
%%% File    : analysis_handler.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created :  1 Feb 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(analysis_handler).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, analyse/0, analyse/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
analyse() ->
    analyse([7,30, 60, 365]).

analyse(DaysList) ->
    gen_server:cast(?MODULE, {start_analysis, DaysList}).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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
    Secs = date_lib:seconds_until_time({date_lib:tomorrow(), {0,0,0}}),
    erlang:start_timer(Secs*1000, self(), daily_update),
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
handle_call(_, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_analysis, DaysList}, State) ->
    ok = start_trend_analysis(DaysList),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({timeout,_, daily_update}, State) ->
    ok = start_trend_analysis([7, 30, 60, 365]),
    Secs = date_lib:seconds_until_time({date_lib:tomorrow(), {0,0,0}}),
    erlang:start_timer(Secs*1000, self(), daily_update),
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
start_trend_analysis(DaysList) ->
    Qh = db_handler:get_query_handle(company),
    Query = qlc:q([Company || Company <- Qh]),
    Companies = db_handler:q(Query),    
    DbUpdateList = 
	lists:append(
	  lists:map(
	    fun(Days) ->
		    analysis_lib:analyse_trends(Companies, Days)
	    end, DaysList)),
    {atomic, ok} = 
	mnesia:transaction(
	  fun() ->
		  lists:foreach(
		    fun(Entry) ->
			    ok = mnesia:write(Entry)
		    end, 
		    DbUpdateList)
	  end),
    ok.
    
