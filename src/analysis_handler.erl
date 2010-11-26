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
-include("mnesia_defs.hrl").

%% API
-export([start_link/0, analyse/0, analyse/1, analyse/2,
	 analyse_company/3, dump/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {companies_analysing=[]}).

%%====================================================================
%% API
%%====================================================================
analyse() ->
    gen_server:cast(?MODULE, start_analysis).

analyse(Companies) when is_list(Companies) ->
    analyse(Companies, all).

analyse(Companies, Types) when is_list(Companies),
                               is_list(Types); is_atom(Types) ->
    gen_server:cast(?MODULE, {start_analysis, Companies, Types}).

dump() ->
    gen_server:call(?MODULE, dump).

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
handle_call(dump, _From, #state{companies_analysing=Companies}=State) ->
    {reply, Companies, State};

handle_call(_, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(start_analysis, #state{companies_analysing=Companies}=State) ->
    AllCompanies = [Name || {Name, _} <- db_mysql:get_all_companies()],
    AnalysingCompanies = start_analysis(AllCompanies, all),
    {noreply, State#state{companies_analysing=lists:usort(AnalysingCompanies
                                                          ++ Companies)}};
handle_cast({start_analysis, Companies, Types},
            #state{companies_analysing=StateCompanies}=State) ->
    AnalysingCompanies = start_analysis(Companies, Types),
    {noreply, State#state{companies_analysing=lists:usort(AnalysingCompanies
                                                          ++ StateCompanies)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, process, Pid, Reason},
            #state{companies_analysing=Companies}=State) ->
    case lists:keyfind(MonitorRef, 2, Companies) of
        {Name, MonitorRef, Pid} ->
            NewCompanies = lists:keydelete(Name, 1, Companies),
	    log_handler:log(analysis_handler,
			    "Error: Received exit with reason ~p for company in the state ~p~n",
			    [Reason, State]),
	    case NewCompanies of
		[] ->
		    ok;
		[{_, _, NewPid} | _Rest] ->
		    NewPid ! start_analysis
	    end,
            {noreply, State#state{companies_analysing = NewCompanies}};
        false ->
            log_handler:log(analysis_handler,
			    "Error: Received exit with reason ~p for company that wasn't in the state ~p~n",
			    [Reason, State]),
            {noreply, State}
    end;
handle_info({analysis_done, Company, Pid},
            #state{companies_analysing=Companies}=State) ->
    case lists:keyfind(Pid, 3, Companies) of
        {Name, MonitorRef, Pid} ->
            erlang:demonitor(MonitorRef),
            Pid ! finish_analysis,
            NewCompanies = lists:keydelete(Name, 1, Companies),
	    case NewCompanies of
		[] ->
		    ok;
		[{_, _, NewPid} | _Rest] ->
		    NewPid ! start_analysis
	    end,
            {noreply, State#state{companies_analysing = NewCompanies}};
        false ->
	    log_handler:log(analysis_handler, "Error: Received analysis_done for company ~p that wasn't in the state ~p~n",
			[Company, State]),
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
start_analysis(Companies, Types) ->
    List = lists:map(
	     fun(Name) ->
		     Pid = spawn(?MODULE, analyse_company,
				 [self(), Name, Types]),
		     MonitorRef = erlang:monitor(process, Pid),
		     {Name, MonitorRef, Pid}
      end, Companies),
    [{_, _, FirstPid} | _Rest] = List,
    FirstPid ! start_analysis,
    List.

analyse_company(Pid, Company, Types) ->
    receive start_analysis -> ok end,
    Stocks = case db_mysql:count_all_stocks(Company) > 40 of
		 true ->
		     db_mysql:get_nr_of_stocks(Company, 40);
		 false ->    
		     db_mysql:get_all_stocks(Company)
	     end,
    AnalysisFun =
	fun(Type) ->
		LatestAnalysed = db_mysql:get(Type, Company, 1),
		Results = analysis_lib:analyse(Type, Stocks, LatestAnalysed),
		[ok = db_mysql:insert(Type, Company, Res) || Res <- Results]
	end,
    case Types of
	all ->
	    lists:foreach(AnalysisFun, [macd, atr, adx, mvg_avg, exp_avg,
				       stochastic]);
	Types ->
	    lists:foreach(AnalysisFun, Types)
    end,
    Pid ! {analysis_done, Company, self()},
    receive finish_analysis -> ok end.


