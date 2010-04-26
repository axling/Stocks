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
    spawn_link(fun() ->
		       start_link(),
		       %% infinite loop, ugly solution for now
		       loop()
	       end).

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
    ContentHandler = {content_handler,{content_handler,start_link,[]},
		      permanent,2000,worker,[content_handler]},
    AnalysisHandler = {analysis_handler,{analysis_handler,start_link,[]},
		      permanent,2000,worker,[analysis_handler]},
    {ok,{{one_for_all,0,1}, [DbHandler, ContentHandler, AnalysisHandler]}}.

%%====================================================================
%% Internal functions
%%====================================================================

loop() ->
    timer:sleep(100),
    loop().
