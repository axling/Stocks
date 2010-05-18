%%%-------------------------------------------------------------------
%%% File    : db_conv.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 29 Apr 2010 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(db_conv).


-export([conv/1, conv_all/0, conv_macd/1, conv_all_macd/0,
	 conv_atr/1, conv_all_atr/0, conv_adx/1, conv_all_adx/0,
	 conv_mvg_avg/1, conv_all_mvg_avg/0, 
	 conv_exp_avg/1, conv_all_exp_avg/0]).
-include("mnesia_defs.hrl").
-include_lib("stdlib/include/qlc.hrl").

conv(Name) ->
    All = db_lib:e(qlc:q([S || S <- db_lib:h(stocks), S#stocks.company==Name])),
    [Company] = db_lib:sread(company, Name),
    Data = lists:map(
	     fun(#stocks{date=D, highest=H, lowest=L, closing=C,
			 average=A, turnover=T, completions=C2, volume=V}) ->
		     #stock{date=D, highest=H, lowest=L, closing=C,
			    average=A, turnover=T, completions=C2, volume=V}
	     end, All),
    SortedData = lists:sort(
		   fun(StockA, StockB) ->
			   date_lib:is_greater(StockA#stock.date, StockB#stock.date)
		   end, Data),
    Sec = #sec{name=Name, instrument=Company#company.instrument,
	       market=Company#company.market, data=SortedData},
    db_lib:swrite(Sec).

conv_all() ->
    All = db_lib:e(qlc:q([C#company.name || C <- db_lib:h(company)])),
    lists:foreach(
      fun(Name) ->
	     conv(Name)
      end, All).

conv_macd(Name) ->
    All = db_lib:e(qlc:q([S || S <- db_lib:h(macd), element(2,S) == Name])),
    Data = lists:map(
	     fun({macd, _Name, Date, Value, Signal}) ->
		     #macd{date=Date, value=Value, signal=Signal}
	     end, All),
    SortedData = lists:sort(
		  fun(A, B) ->
			  date_lib:is_greater(A#macd.date, B#macd.date)
		  end, Data),
    Analysis = #analysis{company=Name, type=macd, result=SortedData},
    db_lib:swrite(Analysis).

conv_all_macd() ->
    All = db_lib:e(qlc:q([C#company.name || C <- db_lib:h(company)])),
    lists:foreach(
      fun(Name) ->
	     conv_macd(Name)
      end, All).

conv_atr(Name) ->
    All = db_lib:e(qlc:q([S || S <- db_lib:h(atr), element(2,S) == Name])),
    Data = lists:map(
	     fun({atr, _Name, Date, Value}) ->
		     #atr{date=Date, value=Value}
	     end, All),
    SortedData = lists:sort(
		  fun(A, B) ->
			  date_lib:is_greater(A#atr.date, B#atr.date)
		  end, Data),
    Analysis = #analysis{company=Name, type=atr, result=SortedData},
    db_lib:swrite(Analysis).

conv_all_atr() ->
    All = db_lib:e(qlc:q([C#company.name || C <- db_lib:h(company)])),
    lists:foreach(
      fun(Name) ->
	     conv_atr(Name)
      end, All).


conv_adx(Name) ->
    All = db_lib:e(qlc:q([S || S <- db_lib:h(adx), element(2,S) == Name])),
    Data = lists:map(
	     fun({adx, _Name, Date, Value, DiPlus, DiMinus}) ->
		     #adx{date=Date, value=Value, di_plus=DiPlus, 
			  di_minus=DiMinus}
	     end, All),
    SortedData = lists:sort(
		  fun(A, B) ->
			  date_lib:is_greater(A#adx.date, B#adx.date)
		  end, Data),
    Analysis = #analysis{company=Name, type=adx, result=SortedData},
    db_lib:swrite(Analysis).

conv_all_adx() ->
    All = db_lib:e(qlc:q([C#company.name || C <- db_lib:h(company)])),
    lists:foreach(
      fun(Name) ->
	     conv_adx(Name)
      end, All).

conv_mvg_avg(Name) ->
    All = db_lib:e(qlc:q([S || S <- db_lib:h(mvg_avg), element(2,S) == Name])),
    Data = lists:map(
	     fun({mvg_avg, _Name, Date, Ten, Thirty}) ->
		     #mvg_avg{date=Date, ten=Ten, thirty=Thirty}
	     end, All),
    SortedData = lists:sort(
		  fun(A, B) ->
			  date_lib:is_greater(A#mvg_avg.date, B#mvg_avg.date)
		  end, Data),
    Analysis = #analysis{company=Name, type=mvg_avg, result=SortedData},
    db_lib:swrite(Analysis).

conv_all_mvg_avg() ->
    All = db_lib:e(qlc:q([C#company.name || C <- db_lib:h(company)])),
    lists:foreach(
      fun(Name) ->
	     conv_mvg_avg(Name)
      end, All).

conv_exp_avg(Name) ->
    All = db_lib:e(qlc:q([S || S <- db_lib:h(exp_avg), element(2,S) == Name])),
    Data = lists:map(
	     fun({exp_avg, _Name, Date, Ten, Thirty}) ->
		     #exp_avg{date=Date, ten=Ten, thirty=Thirty}
	     end, All),
    SortedData = lists:sort(
		  fun(A, B) ->
			  date_lib:is_greater(A#exp_avg.date, B#exp_avg.date)
		  end, Data),
    Analysis = #analysis{company=Name, type=exp_avg, result=SortedData},
    db_lib:swrite(Analysis).

conv_all_exp_avg() ->
    All = db_lib:e(qlc:q([C#company.name || C <- db_lib:h(company)])),
    lists:foreach(
      fun(Name) ->
	     conv_exp_avg(Name)
      end, All).
