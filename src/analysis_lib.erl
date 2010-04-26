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
-export([analyse_trends/2, analyse_macd/1, analyse_atr/1, analyse_adx/1,
	 analyse_mvg_avg/1, analyse_exp_avg/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
analyse_macd(Company) ->
    LastDate = get_latest_stock_date(Company),
    LastMacd = get_latest_macd(Company),
    if 
	((LastDate /= []) and (LastMacd /= []))  ->
	    case date_lib:is_greater(LastDate, LastMacd#macd.date) of
		true ->
		    fix_macd(Company, LastMacd#macd.date);
		false ->
		    []
	    end;
	((LastDate /= []) and (LastMacd == [])) ->
	    fix_macd(Company, beginning);
	true ->
	    []
    end.

analyse_atr(Company) ->
    LastDate = get_latest_stock_date(Company),
    LastAtr = get_latest_atr(Company),
    if 
	((LastDate /= []) and (LastAtr/= []))  ->
	    case date_lib:is_greater(LastDate, LastAtr#atr.date) of
		true ->
		    fix_atr(Company, LastAtr#atr.date);
		false ->
		    []
	    end;
	((LastDate /= []) and (LastAtr == [])) ->
	    fix_atr(Company, beginning);
	true ->
	    []
    end.

analyse_adx(Company) ->
    LastDate = get_latest_stock_date(Company),
    LastAdx = get_latest_adx(Company),
    if 
	((LastDate /= []) and (LastAdx/= []))  ->
	    case date_lib:is_greater(LastDate, LastAdx#adx.date) of
		true ->
		    fix_adx(Company, LastAdx#adx.date);
		false ->
		    []
	    end;
	((LastDate /= []) and (LastAdx == [])) ->
	    fix_adx(Company, beginning);
	true ->
	    []
    end.

analyse_mvg_avg(Company) ->
    LastDate = get_latest_stock_date(Company),
    LastMvgAvg = get_latest_mvg_avg(Company),
    if 
	((LastDate /= []) and (LastMvgAvg/= []))  ->
	    case date_lib:is_greater(LastDate, LastMvgAvg#mvg_avg.date) of
		true ->
		    fix_mvg_avg(Company, LastMvgAvg#mvg_avg.date);
		false ->
		    []
	    end;
	((LastDate /= []) and (LastMvgAvg == [])) ->
	    fix_mvg_avg(Company, beginning);
	true ->
	    []
    end.

analyse_exp_avg(Company) ->
    LastDate = get_latest_stock_date(Company),
    LastExpAvg = get_latest_exp_avg(Company),
    if 
	((LastDate /= []) and (LastExpAvg/= []))  ->
	    case date_lib:is_greater(LastDate, LastExpAvg#exp_avg.date) of
		true ->
		    fix_exp_avg(Company, LastExpAvg#exp_avg.date);
		false ->
		    []
	    end;
	((LastDate /= []) and (LastExpAvg == [])) ->
	    fix_exp_avg(Company, beginning);
	true ->
	    []
    end.

analyse_trends(Name, TrendDays) ->
    Today = date_lib:today(),
    FromDate = date_lib:date_minus_days(Today, TrendDays),
    ModdedFromDate = date_lib:date_minus_days(Today, TrendDays+30),    
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
    end.


%%====================================================================
%% Internal functions
%%====================================================================
get_all_stocks(Company) ->
    Qh = db_handler:get_query_handle(stocks),
    Q = qlc:q([H ||
		  H <- Qh, 
		  H#stocks.company == Company]),
    lists:sort(
      fun(StockA, StockB) ->
	      date_lib:is_greater(StockB#stocks.date, StockA#stocks.date)
      end, db_handler:q(Q)).

get_latest_stock_date(Company) ->
    Stocks = get_all_stocks(Company),
    Last = return_last(Stocks),
    Last#stocks.date.

get_latest_macd(Company) ->
    Qh = db_handler:get_query_handle(macd),
    Q = db_handler:q(qlc:q(
		       [E || E <- Qh,
			     E#macd.company == Company])),
    Sorted = lists:sort(
	       fun(A, B) ->
		       date_lib:is_greater(B#macd.date, A#macd.date)
	       end, db_handler:q(Q)),
    return_last(Sorted).

get_latest_atr(Company) ->
    Qh = db_handler:get_query_handle(atr),
    Q = db_handler:q(qlc:q(
		       [E || E <- Qh,
			     E#atr.company == Company])),
    Sorted = lists:sort(
		    fun(A, B) ->
			    date_lib:is_greater(B#atr.date, A#atr.date)
		    end, db_handler:q(Q)),
    return_last(Sorted).

get_latest_adx(Company) ->
    Qh = db_handler:get_query_handle(adx),
    Q = db_handler:q(qlc:q(
		       [E || E <- Qh,
			     E#adx.company == Company])),
    Sorted = lists:sort(
		    fun(A, B) ->
			    date_lib:is_greater(B#adx.date, A#adx.date)
		    end, db_handler:q(Q)),
    return_last(Sorted).

get_latest_mvg_avg(Company) ->
    Qh = db_handler:get_query_handle(mvg_avg),
    Q = db_handler:q(qlc:q(
		       [E || E <- Qh,
			     E#mvg_avg.company == Company])),
    Sorted = lists:sort(
		    fun(A, B) ->
			    date_lib:is_greater(B#mvg_avg.date, A#mvg_avg.date)
		    end, db_handler:q(Q)),
    return_last(Sorted).

get_latest_exp_avg(Company) ->
    Qh = db_handler:get_query_handle(exp_avg),
    Q = db_handler:q(qlc:q(
		       [E || E <- Qh,
			     E#exp_avg.company == Company])),
    Sorted = lists:sort(
		    fun(A, B) ->
			    date_lib:is_greater(B#exp_avg.date, A#exp_avg.date)
		    end, db_handler:q(Q)),
    return_last(Sorted).

fix_macd(Company, beginning) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    if 
	length(Series) > 35 ->
	    get_macd(Company, 36, Stocks);
	true ->
	    []
    end;
fix_macd(Company, From) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_macd(Company, N, Stocks).

fix_atr(Company, beginning) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    if 
	length(Series) > 14 ->
	    get_atr(Company, 15, Stocks);
	true ->
	    []
    end;
fix_atr(Company, From) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_atr(Company, N, Stocks).

fix_adx(Company, beginning) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    if 
	length(Series) > 28 ->
	    get_adx(Company, 29, Stocks);
	true ->
	    []
    end;
fix_adx(Company, From) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_adx(Company, N, Stocks).

fix_mvg_avg(Company, beginning) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    if 
	length(Series) > 30 ->
	    get_mvg_avg(Company, 31, Stocks);
	true ->
	    []
    end;
fix_mvg_avg(Company, From) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_mvg_avg(Company, N, Stocks).

fix_exp_avg(Company, beginning) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    if 
	length(Series) > 30 ->
	    get_exp_avg(Company, 31, Stocks);
	true ->
	    []
    end;
fix_exp_avg(Company, From) ->
    Stocks = get_all_stocks(Company),
    Series = [{S#stocks.closing, S#stocks.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_exp_avg(Company, N, Stocks).

get_macd(Company, From, Stocks) when From > 35 ->
    {Data, Date} = lists:unzip([{S#stocks.closing, S#stocks.date} || S <- Stocks]),    
    {Macd, Signal} = data_lib:macd(lists:nthtail(From - 34, Data)),
    lists:map(
     fun({{FunMacd, FunSignal}, FunDate}) ->
	     #macd{company=Company,
		   date=FunDate,
		   value=FunMacd,
		   signal=FunSignal}
     end, lists:zip(lists:zip(Macd, Signal), lists:nthtail(From-1, Date)));
get_macd(_Company, _From, _Stocks) ->
    [].

get_atr(Company, From, Stocks) when From > 14 ->
    {Data, Date} = lists:unzip([{{S#stocks.highest, S#stocks.lowest, S#stocks.closing}, 
				 S#stocks.date} || S <- Stocks]),    
    Atr = data_lib:atr(lists:nthtail(From - 15, Data), 14),
    lists:map(
     fun({FunAtr, FunDate}) ->
	     #atr{company=Company,
		  date=FunDate,
		  value=FunAtr}
     end, lists:zip(Atr, lists:nthtail(From-1, Date)));
get_atr(_Company, _From, _Stocks) ->
    [].

get_adx(Company, From, Stocks) when From > 27 ->
    {Data, Date} = lists:unzip([{{S#stocks.highest,
				  S#stocks.lowest,
				  S#stocks.closing}, 
				 S#stocks.date} || S <- Stocks]),    
    Adx = data_lib:adx(lists:nthtail(From - 28, Data), 14),
    {DiPlus, DiMinus} = data_lib:di(lists:nthtail(From - 15, Data), 14),
    lists:map(
     fun({{FunAdx, {FunDiPlus, FunDiMinus}}, FunDate}) ->
	     #adx{company=Company,
		  date=FunDate,
		  value=FunAdx,
		  di_plus = FunDiPlus,
		  di_minus = FunDiMinus}
     end, lists:zip(lists:zip(Adx, lists:zip(DiPlus, DiMinus)), lists:nthtail(From-1,Date)));
get_adx(_Company, _From, _Stocks) ->
    [].

get_mvg_avg(Company, From, Stocks) when From > 30 ->
    {Data, Date} = lists:unzip([{S#stocks.closing, S#stocks.date} || S <- Stocks]),    
    MvgAvg10 = data_lib:mvg_avg(lists:nthtail(From - 11, Data), 10),
    MvgAvg30 = data_lib:mvg_avg(lists:nthtail(From - 31, Data), 30),
    lists:map(
      fun({{FunMvgAvg10, FunMvgAvg30}, FunDate}) ->
	      #mvg_avg{company=Company,
		       date=FunDate,
		       ten=FunMvgAvg10,
		       thirty=FunMvgAvg30}
     end, lists:zip(lists:zip(MvgAvg10, MvgAvg30), lists:nthtail(From-1, Date)));
get_mvg_avg(_Company, _From, _Stocks) ->
    [].

get_exp_avg(Company, From, Stocks) when From > 30 ->
    {Data, Date} = lists:unzip([{S#stocks.closing, S#stocks.date} || S <- Stocks]),    
    ExpAvg10 = data_lib:ema(lists:nthtail(From - 10, Data), 10),
    ExpAvg30 = data_lib:ema(lists:nthtail(From - 30, Data), 30),
    lists:map(
      fun({{FunExpAvg10, FunExpAvg30}, FunDate}) ->
	      #exp_avg{company=Company,
		       date=FunDate,
		       ten=FunExpAvg10,
		       thirty=FunExpAvg30}
      end, lists:zip(lists:zip(ExpAvg10, ExpAvg30), lists:nthtail(From-1, Date)));
get_exp_avg(_Company, _From, _Stocks) ->
    [].

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
	  end, Series),
    case Result of
	done ->
	    RightIndex;
	searching ->
	    no_date_was_found
    end.

return_last([]) ->
    [];
return_last(List) ->
    lists:last(List).
