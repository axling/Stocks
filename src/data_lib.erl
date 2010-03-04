%%% File    : data_lib.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%% Created :  2 Mar 2010 by  <eeriaxl@EV001A4B76217E>

-module(data_lib).

-export([cum_avg/2, cum_avg/3, ema/2, mvg_avg/2, mvg_avg/4, std_err/1, stochastic/2]).

-include("mnesia_defs.hrl").

mvg_avg(Stocks, Days) when Days < length(Stocks) -> 
    mvg_avg(Stocks, 1, Days, []).

mvg_avg(ValueList, Start, Days, AccValues) 
  when (Start+Days-1) < length(ValueList) ->
    MvgAvg = lists:sum(lists:sublist(ValueList, Start, Days))/Days,
    mvg_avg(ValueList, Start+1, Days, [MvgAvg | AccValues]);
mvg_avg(_ValueList, _Start, _Days, AccValues) ->
    lists:reverse(AccValues).

stochastic(ValueList, Period) ->
    stochastic(ValueList, Period, Period, []).

stochastic(ValueList, Period, Start, AccValues) when length(ValueList) >= Start ->
    {Close, _, _} = lists:nth(Start, ValueList),
    Frame = lists:sublist(ValueList, Start - (Period - 1), Period),
    HighestHigh = lists:max([High || {_, High, _} <- Frame]),
    LowestLow = lists:min([Low || {_, _, Low} <- Frame]),
    AccVal = 100 * ((Close - LowestLow)/(HighestHigh-LowestLow)),
    stochastic(ValueList, Period, Start + 1, [AccVal | AccValues]);
stochastic(_, _, _, AccValues) ->
    lists:reverse(AccValues).
    

ema(Values, Period) ->
    K = 2/(Period+1),
    ema(lists:nthtail(Period, Values), K, 
	[lists:sum(lists:sublist(Values, 1, Period))/Period]).

ema([], _K, EmaList) ->
    lists:reverse(EmaList);
ema([TodayPrice | Rest], K, [YesterdayEma | _RestOfEma]=EmaList) ->
    Ema = TodayPrice * K + YesterdayEma*(1-K),
    ema(Rest, K, [Ema | EmaList]).

cum_avg(Values, Start) ->
    {List1, RestList} = lists:split(Start+1, Values),
    CumSum = lists:sum(List1)/length(List1),
    cum_avg(RestList, [CumSum], Start+1).

cum_avg([], AccVal, _) ->
    lists:reverse(AccVal);
cum_avg([First | Rest], [CA | _Rest]=AccVal, I) ->
    Sum = (First + CA*(I-1))/I,
    cum_avg(Rest, [Sum | AccVal], I+1).

std_err(Values) ->   
    Avg = lists:sum(Values)/length(Values),
    SqrSum = 
	lists:sum(
	  lists:map(
	    fun(Val) ->
		    math:pow(Avg - Val, 2)
	    end, Values)),
    math:sqrt(SqrSum/(length(Values) -1))/Avg.
