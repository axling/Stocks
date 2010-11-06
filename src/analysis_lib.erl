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
-export([analyse_macd/1, analyse_atr/1, analyse_adx/1,
	 analyse_mvg_avg/1, analyse_exp_avg/1, analyse_stochastic/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
analyse_macd(Company) ->
    Macd = case db_lib:e(qlc:q([A || A <- db_lib:h(analysis),
				     A#analysis.type==macd,
				     A#analysis.company==Company])) of
	       [] ->
		   #analysis{company=Company, type=macd, result=[]};
	       [Macd1] ->
		   Macd1
	   end,
    [Stocks] = db_lib:sread(sec, Company),
    LastDate = get_latest_stock_date(Stocks#sec.data),
    LastMacd = get_latest_macd(Macd#analysis.result),
    Res = if 
	      ((LastDate /= []) and (LastMacd /= []))  ->
		  case date_lib:is_greater(LastDate, LastMacd) of
		      true ->
			  fix_macd(Stocks#sec.data, LastMacd);
		      false ->
			  []
		  end;
	      ((LastDate /= []) and (LastMacd == [])) ->
		  fix_macd(Stocks#sec.data, beginning);
	      true ->
		  []
	  end,
    Macd#analysis{result=lists:append([Res, Macd#analysis.result])}.

analyse_atr(Company) ->
    Atr = case db_lib:e(qlc:q([A || A <- db_lib:h(analysis),
				    A#analysis.type==atr,
				    A#analysis.company==Company])) of
	      [] ->
		  #analysis{company=Company, type=atr, result=[]};
	      [Atr1] ->
		  Atr1
	  end,
    [Stocks] = db_lib:sread(sec, Company),
    LastDate = get_latest_stock_date(Stocks#sec.data),
    LastAtr = get_latest_atr(Atr#analysis.result),
    Res = if 
	      ((LastDate /= []) and (LastAtr/= []))  ->
		  case date_lib:is_greater(LastDate, LastAtr) of
		      true ->
			  fix_atr(Stocks#sec.data, LastAtr);
		      false ->
			  []
		  end;
	      ((LastDate /= []) and (LastAtr == [])) ->
		  fix_atr(Stocks#sec.data, beginning);
	      true ->
		  []
	  end,
    Atr#analysis{result=lists:append([Res, Atr#analysis.result])}.

analyse_adx(Company) ->
    Adx = case db_lib:e(qlc:q([A || A <- db_lib:h(analysis),
				    A#analysis.type==adx,
				    A#analysis.company==Company])) of
	      [] ->
		  #analysis{company=Company, type=adx, result=[]}; 
	      [Adx1] ->
		  Adx1
	  end,
    [Stocks] = db_lib:sread(sec, Company),
    LastDate = get_latest_stock_date(Stocks#sec.data),
    LastAdx = get_latest_adx(Adx#analysis.result),    
    Res = if 
	      ((LastDate /= []) and (LastAdx/= []))  ->
		  case date_lib:is_greater(LastDate, LastAdx) of
		      true ->
			  fix_adx(Stocks#sec.data, LastAdx);
		      false ->
			  []
		  end;
	      ((LastDate /= []) and (LastAdx == [])) ->
		  fix_adx(Stocks#sec.data, beginning);
	      true ->
		  []
	  end,
    Adx#analysis{result=lists:append([Res, Adx#analysis.result])}.

analyse_mvg_avg(Company) ->
    MvgAvg = case db_lib:e(qlc:q([A || A <- db_lib:h(analysis),
				       A#analysis.type==mvg_avg,
				       A#analysis.company==Company])) of
		 [] ->
		     #analysis{company=Company, type=mvg_avg, result=[]};
		 [MvgAvg1] ->
		     MvgAvg1
	     end,
    [Stocks] = db_lib:sread(sec, Company),
    LastDate = get_latest_stock_date(Stocks#sec.data),
    LastMvgAvg = get_latest_mvg_avg(MvgAvg#analysis.result),     
    Res = if 
	      ((LastDate /= []) and (LastMvgAvg/= []))  ->
		  case date_lib:is_greater(LastDate, LastMvgAvg) of
		      true ->
			  fix_mvg_avg(Stocks#sec.data, LastMvgAvg);
		      false ->
			  []
		  end;
	      ((LastDate /= []) and (LastMvgAvg == [])) ->
		  fix_mvg_avg(Stocks#sec.data, beginning);
	      true ->
		  []
	  end,
    MvgAvg#analysis{result=lists:append([Res, MvgAvg#analysis.result])}.

analyse_exp_avg(Company) ->
    ExpAvg = case db_lib:e(qlc:q([A || A <- db_lib:h(analysis),
				       A#analysis.type==exp_avg,
				       A#analysis.company==Company])) of
		 [] ->
		     #analysis{company=Company, type=exp_avg, result=[]};
		 [ExpAvg1] ->
		     ExpAvg1
	     end,
    [Stocks] = db_lib:sread(sec, Company),
    LastDate = get_latest_stock_date(Stocks#sec.data),
    LastExpAvg = get_latest_exp_avg(ExpAvg#analysis.result),
    Res = if 
	      ((LastDate /= []) and (LastExpAvg/= []))  ->
		  case date_lib:is_greater(LastDate, LastExpAvg) of
		      true ->
			  fix_exp_avg(Stocks#sec.data, LastExpAvg);
		      false ->
			  []
		  end;
	      ((LastDate /= []) and (LastExpAvg == [])) ->
		  fix_exp_avg(Stocks#sec.data, beginning);
	      true ->
		  []
	  end,
    ExpAvg#analysis{result=lists:append([Res, ExpAvg#analysis.result])}.

analyse_stochastic(Company) ->
    Stochastic = case db_lib:e(qlc:q([A || A <- db_lib:h(analysis),
				       A#analysis.type==stochastic,
				       A#analysis.company==Company])) of
		 [] ->
		     #analysis{company=Company, type=stochastic, result=[]};
		 [Stochastic1] ->
		     Stochastic1
	     end,
    [Stocks] = db_lib:sread(sec, Company),
    LastDate = get_latest_stock_date(Stocks#sec.data),
    LastStochastic = get_latest_stochastic(Stochastic#analysis.result),
    Res = if 
	      ((LastDate /= []) and (LastStochastic/= []))  ->
		  case date_lib:is_greater(LastDate, LastStochastic) of
		      true ->
			  fix_stochastic(Stocks#sec.data, LastStochastic);
		      false ->
			  []
		  end;
	      ((LastDate /= []) and (LastStochastic == [])) ->
		  fix_stochastic(Stocks#sec.data, beginning);
	      true ->
		  []
	  end,
    Stochastic#analysis{result=lists:append([Res, Stochastic#analysis.result])}.

%%====================================================================
%% Internal functions
%%====================================================================
get_latest_stock_date([]) ->
    [];
get_latest_stock_date(Data) ->
    Last = hd(Data),
    Last#stock.date.

get_latest_macd([]) ->
    [];
get_latest_macd(Data) ->
    First = hd(Data),
    First#macd.date.

get_latest_atr([]) ->
    [];
get_latest_atr(Data) ->
    First = hd(Data),
    First#atr.date.    

get_latest_adx([]) ->
    [];
get_latest_adx(Data) ->
    First = hd(Data),
    First#adx.date.   

get_latest_mvg_avg([]) ->
    [];
get_latest_mvg_avg(Data) ->
    First = hd(Data),
    First#mvg_avg.date.   

get_latest_exp_avg([]) ->
    [];
get_latest_exp_avg(Data) ->
    First = hd(Data),
    First#exp_avg.date. 
 
get_latest_stochastic([]) ->
    [];
get_latest_stochastic(Data) ->
    First = hd(Data),
    First#stochastic.date. 

fix_macd(Stocks, beginning) ->
    Series = [{S#stock.closing, S#stock.date} || S <- Stocks],
    if 
	length(Series) > 34 ->
	    get_macd(length(Series)-34, Series);
	true ->
	    []
    end;
fix_macd(Stocks, From) ->
    Series = [{S#stock.closing, S#stock.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_macd(N, Series).

fix_atr(Stocks, beginning) ->
    Series = [{{S#stock.highest, S#stock.lowest, S#stock.closing}, 
	       S#stock.date} || S <- Stocks],    
    if 
	length(Series) > 14 ->
	    get_atr(length(Series)-15, Series);
	true ->
	    []
    end;
fix_atr(Stocks, From) ->
    Series = [{{S#stock.highest, S#stock.lowest, S#stock.closing}, 
	       S#stock.date} || S <- Stocks],	
    N = place_of_date(From, Series),
    get_atr(N, Series).

fix_adx(Stocks, beginning) ->
    Series = [{{S#stock.highest,
		S#stock.lowest,
		S#stock.closing}, 
	       S#stock.date} || S <- Stocks],
    if 
	length(Series) > 28 ->
	    get_adx(length(Series)-28, Series);
	true ->
	    []
    end;
fix_adx(Stocks, From) ->
    Series = [{{S#stock.highest,
		S#stock.lowest,
		S#stock.closing}, 
	       S#stock.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_adx(N, Series).

fix_mvg_avg(Stocks, beginning) ->
    Series = [{S#stock.closing, S#stock.date} || S <- Stocks],
    if 
	length(Series) > 30 ->
	    get_mvg_avg(length(Series)-31, Series);
	true ->
	    []
    end;
fix_mvg_avg(Stocks, From) ->
    Series = [{S#stock.closing, S#stock.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_mvg_avg(N, Series).

fix_exp_avg(Stocks, beginning) ->
    Series = [{S#stock.closing, S#stock.date} || S <- Stocks],
    if 
	length(Series) > 30 ->
	    get_exp_avg(length(Series)-30, Series);
	true ->
	    []
    end;
fix_exp_avg(Stocks, From) ->
    Series = [{S#stock.closing, S#stock.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_exp_avg(N, Series).

fix_stochastic(Stocks, beginning) ->
    Series = [{{S#stock.closing, S#stock.highest, S#stock.lowest}, 
	       S#stock.date} || S <- Stocks],
    if 
	length(Series) > 13 ->
	    get_stochastic(length(Series)-13, Series);
	true ->
	    []
    end;
fix_stochastic(Stocks, From) ->
    Series = [{{S#stock.closing, S#stock.highest, S#stock.lowest}, 
	       S#stock.date} || S <- Stocks],
    N = place_of_date(From, Series),
    get_stochastic(N, Series).

get_macd(From, Stocks) ->
    ModdedStocks = lists:reverse(lists:sublist(Stocks, From+34)),
    {Data, Date} = lists:unzip(ModdedStocks),    
    {Macd, Signal} = data_lib:macd(Data),
    lists:reverse(
      lists:map(
	fun({{FunMacd, FunSignal}, FunDate}) ->
		#macd{date=FunDate,
		      value=FunMacd,
		      signal=FunSignal}
	end, lists:zip(lists:zip(Macd, Signal), lists:nthtail(length(Date)-From-1, Date)))).

get_atr(From, Stocks) ->
    ModdedStocks = lists:reverse(lists:sublist(Stocks, From+15)),
    {Data, Date} = lists:unzip(ModdedStocks),    
    Atr = data_lib:atr(Data, 14),
    lists:reverse(
      lists:map(
	fun({FunAtr, FunDate}) ->
		#atr{date=FunDate,
		     value=FunAtr}
	end, lists:zip(Atr, lists:nthtail(length(Date)-From-1, Date)))).

get_adx(From, Stocks) ->
    ModdedStocks = lists:reverse(lists:sublist(Stocks, From+28)),
    {Data, Date} = lists:unzip(ModdedStocks),    
    Adx = data_lib:adx(Data, 14),
    {DiPlus, DiMinus} = data_lib:di(lists:nthtail(length(Data)-From-15, 
						  Data), 14),
    lists:reverse(
      lists:map(
	fun({{FunAdx, {FunDiPlus, FunDiMinus}}, FunDate}) ->
		#adx{date=FunDate,
		     value=FunAdx,
		     di_plus = FunDiPlus,
		     di_minus = FunDiMinus}
	end, lists:zip(lists:zip(Adx, lists:zip(DiPlus, DiMinus)), 
		       lists:nthtail(length(Date)-From-1, Date)))).

get_mvg_avg(From, Stocks) ->
    ModdedStocks = lists:reverse(lists:sublist(Stocks, From+31)),
    {Data, Date} = lists:unzip(ModdedStocks),
    MvgAvg10 = data_lib:mvg_avg(lists:nthtail(length(Data)-From-11, Data), 10),
    MvgAvg30 = data_lib:mvg_avg(Data, 30),
    lists:reverse(
      lists:map(
	fun({{FunMvgAvg10, FunMvgAvg30}, FunDate}) ->
		#mvg_avg{date=FunDate,
			 ten=FunMvgAvg10,
			 thirty=FunMvgAvg30}
	end, lists:zip(lists:zip(MvgAvg10, MvgAvg30), 
		       lists:nthtail(length(Date)-From-1, Date)))).

get_exp_avg(From, Stocks) ->
    ModdedStocks = lists:reverse(lists:sublist(Stocks, From+30)),
    {Data, Date} = lists:unzip(ModdedStocks),    
    ExpAvg10 = data_lib:ema(lists:nthtail(length(Data)-From-10, Data), 10),
    ExpAvg30 = data_lib:ema(Data, 30),
    lists:reverse(
      lists:map(
	fun({{FunExpAvg10, FunExpAvg30}, FunDate}) ->
		#exp_avg{date=FunDate,
			 ten=FunExpAvg10,
			 thirty=FunExpAvg30}
	end, lists:zip(lists:zip(ExpAvg10, ExpAvg30), 
		       lists:nthtail(length(Date)-From-1, Date)))).

get_stochastic(From, Stocks) ->
    ModdedStocks = lists:reverse(lists:sublist(Stocks, From+12)),
    {Data, Date} = lists:unzip(ModdedStocks),    
    {PercentK, PercentD} = data_lib:stochastic(Data, 10),
    lists:reverse(
      lists:map(
	fun({{FunPercentK, FunPercentD}, FunDate}) ->
		#stochastic{date=FunDate,
			    percent_k=FunPercentK,
			    percent_d=FunPercentD}
	end, lists:zip(lists:zip(PercentK, PercentD), 
		       lists:nthtail(length(Date)-From-1, Date)))).

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
