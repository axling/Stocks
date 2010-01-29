%%%-------------------------------------------------------------------
%%% File    : analysis_lib.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created : 29 Jan 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(analysis_lib).

%% API
-export([]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
analyse_trends(CompanyList, TrendDays) ->
    Today = date_lib:today(),
    FromDate = date_lib:date_minus_days(Today, TrendDays).
%%     lists:map(
%%      fun(#company{name=Name, instrument=Instrument) ->
%% 		Qh = db_handler:get_query_handle(stocks),
%% 		Q1 = qlc:q([Stock || 
%% 			       Stock <- Qh,
%% 			       Stock#stocks.company  == Name]),
%% 		Stocks = db_handler:q(Q1),
%% 	       )

%%====================================================================
%% Internal functions
%%====================================================================
