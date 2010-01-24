%%%-------------------------------------------------------------------
%%% File    : db_handler.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 23 Jan 2010 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(db_handler).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 create_table/1,
	 table_exists/1,
	 create_entry/1,
	 dump_to_disc/1,
	 set_backup_interval/2,
	 delete_entry/2,
	 delete_table/1,
	 get_entry/2]).

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
create_table(TableName) ->
    ok.

create_entry(Entry) ->
    gen_server:call(?MODULE, {create_entry, Entry}).

delete_table(TableName) ->
    ok.

delete_entry(TableName, Entry) ->
    ok.

dump_to_disc(TableName) ->
    ok.

get_entry(TableName, Key) ->
    ok.

set_backup_interval(TableName, Interval) ->
    ok.

table_exists(TableName) ->
    ok.

q(TableName, Query) ->
    ok.

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
    Stocks = filename:join("db", ?STOCKS),
    Company = filename:join("db", ?COMPANY),
    StocksTableHandle = init_table(?STOCKS, stocks),
    CompanyTableHandle = init_table(?COMPANY, company),    
    {ok, #state{tables = [{stocks, StocksTableHandle},
			  {company, CompanyTableHandle}]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create_entry, Entry}, _From, #state{tables=Tables}=State) ->
    case lists:keymember(element(1, Entry), 1, Tables) of
	true ->
	    {_, Tid} = lists:keyfind(element(1,Entry), 1, Tables),
	    true =  ets:insert(Tid, Entry),
	    {reply, ok, State};
	false ->
	    {reply, table_not_existing, State}
    end.

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
    lists:foreach(
      fun({TableName, Tid}) ->
	      save_table(TableName, Tid),
	      ets:delete(Tid)
      end, Tables).


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
init_table(TableFile, TableName) ->
    case filelib:is_file(TableFile) of
	%% Table exists, load it into ets
	true ->
	    {ok, TableName} = dets:open_file(TableName, [{file, TableFile}, {type,bag}, {keypos,2}]),
	    Tid = ets:new(TableName, [bag, {keypos,2}]),
	    true = ets:from_dets(TableName, TableName),
	    Tid;
	%% Table doesnt exist, create ets table
	false ->
	    Tid = ets:new(TableName, [bag, {keypos,2}])	    
    end.

save_table(TableName, Tid) ->
    FileName = filename:join("db", atom_to_list(TableName) ++ ".dets"),
    case filelib:is_file(FileName) of	
	true ->
	    {ok, TableName} = dets:open_file(TableName, 
					     [{file, FileName},
					      {type,bag}, {keypos,2}]),
	    true = ets:to_dets(Tid, TableName),
	    ok;
	false ->
	    {ok, TableName} = dets:open_file(TableName, 
					     [{file, FileName},
					      {type,bag}, {keypos,2}]),
	    true = ets:to_dets(Tid, TableName),
	    ok
    end.
	    
	
