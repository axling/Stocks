%%%-------------------------------------------------------------------
%%% File    : db_handler.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 23 Jan 2010 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(db_handler).

-behaviour(gen_server).

-include("mnesia_defs.hrl").

%% API
-export([start_link/0,
	 create_entry/1,
	 delete_entry/2,
	 get_entry/2,
	 get_query_handle/1,
	 ready/0,
	 q/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {tables = []
	       }).

-define(STOCKS, "stocks.dets").
-define(COMPANY, "company.dets").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
create_entry(Entry) ->
    gen_server:call(?MODULE, {create_entry, Entry}, infinity).

delete_entry(TableName, Entry) ->
    ok.

get_query_handle(TableName) ->
    gen_server:call(?MODULE, {get_query_handle, TableName}, infinity).

get_entry(TableName, Key) ->
    ok.

q(Query) ->
    gen_server:call(?MODULE, {q, Query}, infinity).

ready() ->
    gen_server:call(?MODULE, ready, infinity).

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
    case mnesia:start() of
	ok ->
	    case create_tables([{stocks, bag, record_info(fields, stocks)}, 
				{company, set, record_info(fields, company)},
				{analysis, bag, record_info(fields, analysis)}]) of
		ok ->
		    {ok, #state{}};
		{error, Reason} ->
		    {stop, Reason}
	    end;
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                        record_info(fields, TableName)              {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create_entry, Entry}, _From, State) ->
    case mnesia:transaction(
	   fun () ->
		   mnesia:write(Entry)
	   end) of
	{atomic, ok} ->
	    {reply, ok, State};
	{aborted, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({q,  Query}, _From, State) ->
    {atomic, Result} = 
	mnesia:transaction(
	  fun() ->
		  qlc:e(Query)
	  end),
    {reply, Result, State};
handle_call({get_query_handle, TableName}, _From, State) ->
    Table = mnesia:table(TableName),
    {reply, Table, State};

handle_call(ready, _From, State) ->
    {reply, ok, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{tables=Tables}) ->
    mnesia:stop().

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
create_table({TableName, Type, Fields}) ->
    case mnesia:create_table(TableName, [{type, Type},
					 {disc_copies, [node()]},
					 {attributes,
					  Fields}]) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, TableName}} ->
	    ok = mnesia:wait_for_tables([TableName], infinity);	    
	{aborted, Reason} ->
	    {error, Reason}
    end.

create_tables([]) ->
    ok;
create_tables([First |TableList]) ->
    case create_table(First) of
	ok ->
	    create_tables(TableList);
	{error, Reason} ->
	    {error, Reason}
    end.
	

