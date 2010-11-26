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
-export([analyse/3]).

-define('LENGTH', fun(atr) ->
			  ?ATR_LENGTH;
		     (adx) ->
			  ?ADX_LENGTH;
		     (macd) ->
			  ?MACD_LENGTH;
		     (mvg_avg) ->
			  ?MVG_AVG_LENGTH;
		     (exp_avg) ->
			  ?EXP_AVG_LENGTH;
		     (stochastic) ->
			  ?STOCHASTIC_LENGTH
		  end).
-define('ATR_LENGTH', 14).
-define('ADX_LENGTH', 28).
-define('MACD_LENGTH', 34).
-define('MVG_AVG_LENGTH', 30).
-define('EXP_AVG_LENGTH', 30).
-define('STOCHASTIC_LENGTH', 12).

%%====================================================================
%% API
%%====================================================================
analyse(Type, Stocks, AnalysisData) ->
    LastDate = get_latest_stock_date(Stocks),
    LastAnalysisDate = get_latest_analysis_date(Type, AnalysisData),
    if 
	((LastDate /= []) and (LastAnalysisDate /= []))  ->
	    case date_lib:is_greater(LastDate, LastAnalysisDate) of
		true ->
		    fix(Type, Stocks, LastAnalysisDate);
		false ->
		    []
	    end;
	((LastDate /= []) and (LastAnalysisDate == [])) ->
	    fix(Type, Stocks, beginning);
	true ->
	    []
    end.


%%====================================================================
%% Internal functions
%%====================================================================
get_latest_stock_date([]) ->
    [];
get_latest_stock_date(Data) ->
    Last = lists:last(Data),
    Last#stock.date.

get_latest_analysis_date(_Type, []) ->
    [];
get_latest_analysis_date(macd, Data) ->
    Last = lists:last(Data),
    Last#macd.date;
get_latest_analysis_date(atr, Data) ->
    Last = lists:last(Data),
    Last#atr.date;
get_latest_analysis_date(adx, Data) ->
    Last = lists:last(Data),
    Last#adx.date;
get_latest_analysis_date(mvg_avg, Data) ->
    Last = lists:last(Data),
    Last#mvg_avg.date;
get_latest_analysis_date(exp_avg, Data) ->
    Last = lists:last(Data),
    Last#exp_avg.date;
get_latest_analysis_date(stochastic, Data) ->
    Last = lists:last(Data),
    Last#stochastic.date.

fix(Type, Stocks, beginning) ->
    Series = filter_stocks(Type, Stocks),
    case length(Series) >= ?LENGTH(Type) of 
	true ->
	    get_analysis(Type, ?LENGTH(Type), Series);
	false ->
	    []
    end;
fix(Type, Stocks, From) ->
    Series = filter_stocks(Type, Stocks),
    case place_of_date(From, Series) of 
	no_date_was_found ->
	    fix(Type, Stocks, beginning);
	N when is_integer(N) ->
	    get_analysis(Type, N+1, Series)
    end.

get_analysis(macd, From, Stocks) when From >= ?MACD_LENGTH ->
    {Data, Date} = lists:unzip(Stocks),    
    {Macd, Signal} = data_lib:macd(lists:nthtail(From-?MACD_LENGTH, Data)),
    lists:map(
      fun({{FunMacd, FunSignal}, FunDate}) ->
	      #macd{date=FunDate,
		    value=FunMacd,
		    signal=FunSignal}
      end, lists:zip(lists:zip(Macd, Signal), 
		     lists:nthtail(From-1, Date)));

get_analysis(atr, From, Stocks) when From >= ?ATR_LENGTH  ->
    {Data, Date} = lists:unzip(Stocks),    
    Atr = data_lib:atr(lists:nthtail(From - 14, Data), 14),
    lists:map(
      fun({FunAtr, FunDate}) ->
	      #atr{date=FunDate,
		   value=FunAtr}
      end, lists:zip(Atr, lists:nthtail(From-1, Date)));

get_analysis(adx, From, Stocks) when From >= ?ADX_LENGTH ->
    {Data, Date} = lists:unzip(Stocks),    
    Adx = data_lib:adx(lists:nthtail(From - 28, Data), 14),
    {DiPlus, DiMinus} = data_lib:di(lists:nthtail(From-15, Data), 14),
    lists:map(
      fun({{FunAdx, {FunDiPlus, FunDiMinus}}, FunDate}) ->
	      #adx{date=FunDate,
		   value=FunAdx,
		   di_plus = FunDiPlus,
		   di_minus = FunDiMinus}
      end, lists:zip(lists:zip(Adx, lists:zip(DiPlus, DiMinus)), 
		     lists:nthtail(From-1, Date)));

get_analysis(mvg_avg, From, Stocks) when From >= ?MVG_AVG_LENGTH ->
    {Data, Date} = lists:unzip(Stocks),
    MvgAvg10 = data_lib:mvg_avg(lists:nthtail(From-10, Data), 10),
    MvgAvg30 = data_lib:mvg_avg(lists:nthtail(From-30, Data), 30),
    lists:map(
      fun({{FunMvgAvg10, FunMvgAvg30}, FunDate}) ->
	      #mvg_avg{date=FunDate,
		       ten=FunMvgAvg10,
		       thirty=FunMvgAvg30}
      end, lists:zip(lists:zip(MvgAvg10, MvgAvg30), 
		     lists:nthtail(From-1, Date)));

get_analysis(exp_avg, From, Stocks) when From >= ?EXP_AVG_LENGTH ->
    {Data, Date} = lists:unzip(Stocks),    
    ExpAvg10 = data_lib:ema(lists:nthtail(From-10, Data), 10),
    ExpAvg30 = data_lib:ema(lists:nthtail(From-30, Data), 30),
    lists:map(
      fun({{FunExpAvg10, FunExpAvg30}, FunDate}) ->
	      #exp_avg{date=FunDate,
		       ten=FunExpAvg10,
		       thirty=FunExpAvg30}
      end, lists:zip(lists:zip(ExpAvg10, ExpAvg30), 
		     lists:nthtail(From-1, Date)));

get_analysis(stochastic, From, Stocks) when From >= ?STOCHASTIC_LENGTH ->
    {Data, Date} = lists:unzip(Stocks),    
    {PercentK, PercentD} = data_lib:stochastic(lists:nthtail(From-12, Data),10),
    lists:map(
      fun({{FunPercentK, FunPercentD}, FunDate}) ->
	      #stochastic{date=FunDate,
			  percent_k=FunPercentK,
			  percent_d=FunPercentD}
      end, lists:zip(lists:zip(PercentK, PercentD), 
		     lists:nthtail(From-1, Date))).

place_of_date(From, Series) ->
    {Result, RightIndex} = 
	lists:foldl(
	  fun({_A, Date}, {searching, Index}) ->
		  if Date == From -> 
			  {done, Index};
		     true ->
			  {searching, Index + 1}
		  end;
	     ({_, _}, {done, Index}) ->
		  {done, Index}
	  end, {searching, 1}, Series),
    case Result of
	done ->
	    RightIndex;
	searching ->
	    no_date_was_found
    end.

filter_stocks(Type, Stocks)
  when Type == atr; Type==adx; Type==stochastic ->
    [{{S#stock.highest, S#stock.lowest, S#stock.closing}, S#stock.date} 
     || S <- Stocks];
filter_stocks(_Type, Stocks) ->
    [{S#stock.closing, S#stock.date} || S <- Stocks].
