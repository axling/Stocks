%%% File    : data_lib.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description :
%%% Created :  2 Mar 2010 by  <eeriaxl@EV001A4B76217E>

-module(data_lib).

-export([cum_avg/2, cum_avg/3, dm/1, ema/2, mvg_avg/2, mvg_avg/4, std_err/1,
	 stochastic/2, di/2, adx/2, atr/2, macd/1]).

-include("mnesia_defs.hrl").

mvg_avg(Stocks, Days) when Days =< length(Stocks) ->
    mvg_avg(Stocks, 1, Days, []).

mvg_avg(ValueList, Start, Days, AccValues)
  when (Start+Days-1) =< length(ValueList) ->
    MvgAvg = lists:sum(lists:sublist(ValueList, Start, Days))/Days,
    mvg_avg(ValueList, Start+1, Days, [MvgAvg | AccValues]);
mvg_avg(_ValueList, _Start, _Days, AccValues) ->
    lists:reverse(AccValues).

stochastic(ValueList, Period) ->
    stochastic(ValueList, Period, Period, []).

stochastic(ValueList, Period, Start, AccValues)
  when length(ValueList) >= Start ->
    {_, _, Close} = lists:nth(Start, ValueList),
    Frame = lists:sublist(ValueList, Start - (Period - 1), Period),
    HighestHigh = lists:max([High || {High, _, _} <- Frame]),
    LowestLow = lists:min([Low || {_, Low, _} <- Frame]),
    AccVal = 100 * ((Close - LowestLow)/(HighestHigh-LowestLow)),
    stochastic(ValueList, Period, Start + 1, [AccVal | AccValues]);
stochastic(_, _, _, AccValues) ->
    PercentK = lists:reverse(AccValues),
    PercentD = ema(PercentK, 3),
    {lists:nthtail(2, PercentK), PercentD}.

ema(Values, Period) when length(Values) >= Period ->
    K = 2/(Period+1),
    ema(lists:nthtail(Period, Values), K,
	[lists:sum(lists:sublist(Values, Period))/Period]).

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

dm(ValueList) when is_list(ValueList) ->
    {Dm, _} = lists:mapfoldl(
		fun({High, Low, _Close}=A, {PHigh, PLow, _PClose}) ->
			DHigh = High - PHigh,
			DLow = PLow - Low,
			if (((DHigh < 0) and (DLow < 0)) or (DHigh == DLow)) ->
				{{0,0}, A};
			   DHigh > DLow ->
				{{DHigh, 0}, A};
			   true ->
				{{0, DLow}, A}
			end
		end, hd(ValueList), tl(ValueList)),
    Dm.

di(ValueList, Period) when length(ValueList) > Period ->
    Dm = dm(ValueList),
    {DmPlus, DmMinus} = lists:unzip(Dm),
    Atr = atr(lists:nthtail(1, ValueList), Period),
    DiPlus = lists:map(fun({Ema, AtrVal}) ->
			       (100 * Ema)/AtrVal
		       end, lists:zip(ema(DmPlus, Period), Atr)),
    DiMinus = lists:map(fun({Ema, AtrVal}) ->
				(100 * Ema)/AtrVal
			end, lists:zip(ema(DmMinus, Period), Atr)),
    {DiPlus, DiMinus}.

adx(ValueList, Period) when length(ValueList) >= 2*Period ->
    {DiPlus, DiMinus} = di(ValueList, Period),
    ema(lists:map(fun({DiPlusVal, DiMinusVal}) ->
			  (100 * abs(DiPlusVal - DiMinusVal))/
			      (DiPlusVal + DiMinusVal)
		  end, lists:zip(DiPlus, DiMinus)), Period).

atr([{High, Low, _Close}=CurrentValue | ValueList]=List, Period) 
  when length(List) >= Period  ->
    atr(ValueList, CurrentValue, [High-Low], [], Period).

atr([], _Previous, [], Atrs, _Period) ->
    lists:reverse(Atrs);
atr([CurrentValue | Values], Previous,
    [], [PreviousAtr | _AtrRest]=Atrs, Period) ->
    Tr = calculate_tr(CurrentValue, Previous),
    Atr = ((PreviousAtr*(Period-1))+Tr)/Period,
    atr(Values, CurrentValue, [], [Atr | Atrs], Period);
atr([CurrentValue | Values], Previous,
    Trs, [], Period) when length(Trs) == Period-1  ->
    Tr = calculate_tr(CurrentValue, Previous),
    atr(Values, CurrentValue, [], [lists:sum([Tr | Trs])/Period], Period);
atr([CurrentValue | Values], Previous, 
    Trs, [], Period) when length(Trs) < Period   ->
    Tr = calculate_tr(CurrentValue, Previous),
    atr(Values, CurrentValue, [Tr | Trs], [],  Period).


macd(ValueList) when length(ValueList) >= 34 ->
    %% Valuelist is at least 34 long
    Ema12 = data_lib:ema(lists:nthtail(14, ValueList), 12),
    Ema26 = data_lib:ema(ValueList, 26),
    Macd = [E12 - E26 || {E12, E26} <- lists:zip(Ema12, Ema26)],
    Signal = data_lib:ema(Macd, 9),
    {lists:nthtail(8, Macd), Signal}.

calculate_tr({High, Low, _Close}, {_PHigh, _PLow, PClose}) ->
    lists:max(
      [High - Low,
       abs(High - PClose),
       abs(Low - PClose)
      ]
     ).
