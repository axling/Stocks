%%%-------------------------------------------------------------------
%%% File    : stocks_sup.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 23 Jan 2010 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(stocks_sup).

-behaviour(supervisor).

%% API
-export([start/0, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start() ->
    start_link().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    DbHandler = {db_handler,{db_handler,start_link,[]},
	      permanent,2000,worker,[db_handler]},
    TimeManager = {time_manager,{time_manager,start_link,[]},
	      permanent,2000,worker,[time_manager]},
    {ok,{{one_for_all,0,1}, [DbHandler, TimeManager]}}.

%%====================================================================
%% Internal functions
%%====================================================================
