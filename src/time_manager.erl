%%%-------------------------------------------------------------------
%%% File    : time_manager.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 23 Jan 2010 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(time_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 dump/0,
	 add_task/1,
	 modify_task/1,
	 delete_task/1,
	 get_tasks/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("mnesia_defs.hrl").

-record(state, {tasks=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
dump() ->
    gen_server:call(time_manager, dump, infinity).

get_tasks() ->
    db_lib:e(qlc:q([T || T <- db_lib:h(task)])).

add_task(#task{}=T) ->
    gen_server:cast(time_manager, {add_task, T}).

modify_task(#task{}=T) ->
    gen_server:cast(time_manager, {modify_task, T}).

delete_task(Id) when is_integer(Id) ->
    gen_server:cast(time_manager, {delete_task, Id}).    

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
    ok = mnesia:wait_for_tables([task], infinity),
    Tasks = get_tasks(),
    StateList = lists:map(
		  fun(#task{id=Id, type=today, time=Time}=T) ->
			  case is_time_valid(today, Time) of
			      true ->
				  Seconds = get_seconds_until_time(T),
				  {ok, Tref} = timer:send_after(Seconds*1000, {T#task.action, Id}),
				  {Id, Tref};
			     false ->
				  db_lib:delete(task, Id),
				  skip
			  end;
		     (#task{id=Id, type=daily}=T) ->
			  Seconds = get_seconds_until_time(T),
			  {ok, Tref} = timer:send_after(Seconds*1000, {T#task.action, Id}),
			  {Id, Tref}
		  end, Tasks),
    Filtered = lists:filter(fun(skip) -> false; (_) -> true end, StateList),
    {ok, #state{tasks=Filtered}}.

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
handle_cast({add_task, T}, State) ->    
    Ids = db_lib:e(qlc:q([Task#task.id || Task <- db_lib:h(task)])),
    Id = get_first_avail_id(Ids),
    case is_time_valid(T#task.type, T#task.time) of
	true ->
	    Seconds = get_seconds_until_time(T),	    
	    {ok, Tref} = timer:send_after(Seconds*1000, {T#task.action, Id}),
	    db_lib:swrite(T#task{id=Id}),	    
	    {noreply, State#state{tasks=[{Id, Tref} | State#state.tasks]}};
	false ->
	    {noreply, State}
    end;

handle_cast({modify_task, T}, #state{tasks=Tasks}=State) ->   
    [Id] = db_lib:e(qlc:q([T1#task.id || T1 <- db_lib:h(task), T1#task.id==T#task.id])),	
    case proplists:is_defined(Id, Tasks) of
	true ->
	    case is_time_valid(T#task.type, T#task.time) of
		true ->
		    OldTref = proplists:get_value(Id, Tasks),
		    {ok, cancel} = timer:cancel(OldTref),
		    Seconds = get_seconds_until_time(T),	    		    
		    {ok, Tref} = timer:send_after(Seconds*1000, {T#task.action, Id}),
		    db_lib:swrite(T#task{id=Id}),	    
		    {noreply, State#state{tasks=[{Id, Tref} | proplists:delete(Id, Tasks)]}};
		false ->
		    {noreply, State}
	    end;
	false ->
	    db_lib:delete(task, Id),
	    {noreply, State}
    end;

handle_cast({delete_task, Id}, #state{tasks=Tasks}=State) ->   
    [Id] = db_lib:e(qlc:q([T#task.id || T <- db_lib:h(task), T#task.id==Id])),	
    case proplists:is_defined(Id, Tasks) of
	true ->
	    OldTref = proplists:get_value(Id, Tasks),
	    {ok, cancel} = timer:cancel(OldTref),
	    db_lib:delete(task, Id),		
	    {noreply, State#state{tasks=[proplists:delete(Id, Tasks)]}};
	false ->
	    {noreply, State}
    end;
	
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Action, Id}, #state{tasks=Tasks}=State) ->
    do_action(Action),
    [Task] = db_lib:e(qlc:q([T || T <- db_lib:h(task), T#task.id == Id])),
    case Task#task.type of
	today ->
	    db_lib:delete(task, Id),
	    {noreply, State#state{tasks=proplists:delete(Id, Tasks)}};
	daily ->
	    Seconds = get_seconds_until_time(Task),	    		    
	    {ok, Tref} = timer:send_after(Seconds*1000, {Task#task.action, Id}),
	    {noreply, State#state{tasks=[{Id, Tref} | proplists:delete(Id, Tasks)]}}
    end;
    
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
get_first_avail_id([]) ->
    0;
get_first_avail_id(Ids) when is_list(Ids) ->
    get_first_avail_id1(Ids, 0).

get_first_avail_id1(Ids, Index) ->
    case lists:member(Index, Ids) of
	true ->
	    get_first_avail_id1(Ids, Index+1);
	false ->
	    Index
    end.


get_seconds_until_time(#task{type=today, time=Time}) ->
    date_lib:seconds_until_time({date_lib:today(), Time});	    

get_seconds_until_time(#task{type=daily, time=Time}) ->
    case is_time_valid(today, Time) of
	true ->
	    date_lib:seconds_until_time({date_lib:today(), Time});	    
	false ->
	    date_lib:seconds_until_time({date_lib:tomorrow(), Time})
    end.

is_time_valid(today, Time) ->
    try date_lib:seconds_until_time({date_lib:today(), Time}) of
	Seconds when is_integer(Seconds) ->
	    true
    catch 
	error:until_date_in_the_past ->
	    false
    end;
is_time_valid(daily, _Time) ->
    true.

do_action(update) ->
    content_handler:update_content();
do_action(analyse) ->
    analysis_handler:analyse().
    
    
	    
	    
