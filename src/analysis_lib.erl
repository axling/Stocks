%%%-------------------------------------------------------------------
%%% File    : analysis_lib.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created : 29 Jan 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(analysis_lib).

-include("mnesia_defs.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([analyse_trends/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
analyse_trends(CompanyList, TrendDays) ->
    Today = date_lib:today(),
    FromDate = date_lib:date_minus_days(Today, TrendDays),
    io:format("Companies to analyse ~p~n", [CompanyList]),
    ModdedFromDate = date_lib:date_minus_days(Today, TrendDays+30),    
    io:format("FromDate: ~p~nModdedFromdate: ~p~n", [FromDate, ModdedFromDate]),
    lists:map(
      fun(#company{name=Name}) ->
	      Qh = db_handler:get_query_handle(stocks),
	      Q1 = qlc:q([Stock || 
			     Stock <- Qh,
			     Stock#stocks.company  == Name,
			     date_lib:is_greater(Today, Stock#stocks.date),
			     date_lib:is_greater(Stock#stocks.date, ModdedFromDate)]),
	      Stocks = lists:sort(
			 fun(#stocks{date = A}, #stocks{date = B}) ->
				 date_lib:is_greater(B, A)
			 end, db_handler:q(Q1)),
	      case Stocks of
		  [] ->
		      #analysis{company=Name, 
				date=Today, 
				type=trend,
				result = undefined};
		  Stocks ->
		      StartIndex = 
			  lists:foldl(
			    fun(#stocks{date=Date}, Index) ->
				    case date_lib:is_less_or_equal(Date, FromDate) of
					true ->
					    Index+1;
					false ->
					    Index
				    end
			    end, 0, Stocks),
		      %% Trim the stocks to get the right first date
		      TrimmedStocks = lists:nthtail(StartIndex-1,Stocks),
		      First = hd(TrimmedStocks),
		      Last = lists:last(TrimmedStocks),
		      Slope = (Last#stocks.closing - First#stocks.closing)/TrendDays,
		      Trend = (Last#stocks.closing - First#stocks.closing)/First#stocks.closing,		     
		      #analysis{company = Name, 
				date = Today,
				type = trend,
				result = #trend_result{date = Today,
						       days = TrendDays,
						       slope = Slope,
						       trend = Trend}}
	      end
      end, CompanyList).

%%====================================================================
%% Internal functions
%%====================================================================
