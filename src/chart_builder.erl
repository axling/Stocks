%%%-------------------------------------------------------------------
%%% File    : chart_builder.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created :  3 Feb 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(chart_builder).

%% API
-export([build_chart/5]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
build_chart(Title, DataSet, Days, XSize, YSize) ->
    PeriodSet = lists:nthtail(length(DataSet)-Days, DataSet),    
    StrippedDataSet =
	if 
	    length(PeriodSet) > XSize ->
		strip_data(PeriodSet, XSize);
	    true ->
		PeriodSet
	end,
    {DateSet, ValueSet} = lists:unzip(StrippedDataSet),
    FirstDate = hd(DateSet),
    LastDate = lists:last(DateSet),
    MiddleDate = lists:nth(round(length(DateSet)/2), DateSet),
    Max = lists:max(ValueSet),
    Min = lists:min(ValueSet),
    ScaledData = scale(ValueSet),
    EncodedData = encode(ScaledData),    
    Base = "http://chart.apis.google.com/chart?",
    ModTitle = lists:concat(["chtt=",yaws_api:url_encode(Title),":+last+", Days, "+days"]),
    Size = lists:concat(["chs=",XSize,"x",YSize]),
    ChartType = "cht=lc",
    Data = lists:flatten(["chd=s:", EncodedData]),
    Axes = "chxt=x,y",
    Grid = lists:concat(["chg=", 30, ",", 20, ",", 2, ",", 4]),
    YAxis = lists:concat(["chxr=", 1, ",", io_lib:format("~.2f", [Min]), ",", 
			  io_lib:format("~.2f", [Max])]),
    XAxis = lists:concat(["chxl=", 0, ":|", date_lib:convert_date_e_s(FirstDate), "|",
			  date_lib:convert_date_e_s(MiddleDate), "|",
			  date_lib:convert_date_e_s(LastDate)]),
    LineStyle = lists:concat(["chls=", 3, ",", 3, ",", 0]),
    Base ++ string:join([ChartType, ModTitle, Size, Data, Axes, XAxis, YAxis, Grid, LineStyle], 
			"&amp;").
      

%%====================================================================
%% Internal functions
%%====================================================================
scale(Data) ->
    scale(Data, boxed).

scale(Data, Type) ->
    Max = lists:max(Data),
    Min = lists:min(Data),    	      
    scale(Data, [], Max, Min, Type).
    
scale([], AccData, _Max, _Min, _Type) ->
    lists:reverse(AccData);
scale([Val | Data], AccData, Max, _Min, from_zero) ->
    scale(Data, [round((Val/Max) * 61) | AccData],
	       Max, _Min, from_zero);
scale([Val | Data], AccData, Max, Min, _Type) ->
    scale(Data, [round(((Val-Min)/(Max-Min))*61) | AccData],
	      Max, Min, _Type).

encode(Data) ->
    encode(Data, []).

encode([], AccData) ->
    lists:reverse(AccData);
encode([Val | Data], AccData) ->
    AccVal = 
	if 
	    (Val >= 0) and (Val =< 25) -> 
		Val + 65;
	    (Val >= 26) and (Val =< 51)  ->
		Val -26 + 97;
	    (Val >= 52) and (Val =< 61) ->
		Val - 52 + 48
	end,
    encode(Data, [AccVal | AccData]).
    
strip_data(DataSet, Max) when Max < length(DataSet) ->
    Steps = trunc(length(DataSet)/Max),
    {_, Result, _} = 
	lists:foldr(
	  fun({Date, Value}, {1, AccList, MaxVal}) ->
		  {MaxVal, [{Date, Value} |AccList], MaxVal};
	     ({_Date, _Value}, {Step, AccList, MaxVal}) ->
		  {Step-1, AccList, MaxVal}
	  end, {Steps, [], Steps}, DataSet),
    Result.
