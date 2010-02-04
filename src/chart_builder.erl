%%%-------------------------------------------------------------------
%%% File    : chart_builder.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created :  3 Feb 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(chart_builder).

%% API
-export([build_chart/6]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
build_chart(Title, DataSets, DateAxis, Days, XSize, YSize) ->
    {ValueSets, Titles, Styles} = lists:unzip3(DataSets),
    {EncodedData, Max, Min} = prepare_data(ValueSets, Days, XSize),
    {Colors, LineStyles} = lists:unzip(Styles),
    FirstDate = hd(DateAxis),
    LastDate = lists:last(DateAxis),
    MiddleDate = lists:nth(round(length(DateAxis)/2), DateAxis),
    Base = "http://chart.apis.google.com/chart?",
    ModTitle = lists:concat(["chtt=",yaws_api:url_encode(Title),":+last+", Days, "+days"]),
    Size = lists:concat(["chs=",XSize,"x",YSize]),
    ChartType = "cht=lc",
    Data = lists:flatten(["chd=s:", string:join(EncodedData, ",")]),
    Axes = "chxt=x,y",
    Grid = lists:concat(["chg=", 30, ",", 20, ",", 2, ",", 4]),
    YAxis = lists:concat(["chxr=", 1, ",", io_lib:format("~.2f", [Min]), ",", 
			  io_lib:format("~.2f", [Max])]),
    XAxis = lists:concat(["chdlp=b&amp;chxl=", 0, ":|", date_lib:convert_date_e_s(FirstDate), "|",
			  date_lib:convert_date_e_s(MiddleDate), "|",
			  date_lib:convert_date_e_s(LastDate)]),
    Legend = lists:flatten(["chdl=",yaws_api:url_encode(string:join(Titles, "|"))]),
    LineStyle = lists:concat(["chls=", 
			      string:join([lists:concat([Thickness, ",", LengthLine, ",", LengthDash]) || 
					      {Thickness, LengthLine, LengthDash} <- LineStyles], "|")]),
    ChartColors = lists:flatten(["chco=", string:join(Colors, ",")]),    
    Base ++ string:join([ChartType, ModTitle, Size, Data, Axes, XAxis, YAxis, Grid, LineStyle, Legend, ChartColors], 
			"&amp;").
      

%%====================================================================
%% Internal functions
%%====================================================================
prepare_data(DataSets, Days, XSize) ->
    Max = lists:max(
	    lists:map(
	      fun(DataSet) ->
		      lists:max(DataSet)
	      end, DataSets)),
    Min = lists:min(
	    lists:map(
	      fun(DataSet) ->
		      lists:min(DataSet)
	      end, DataSets)),
    {lists:map(
      fun(DataSet) ->
	      PeriodSet = lists:nthtail(length(DataSet)-Days, DataSet),    
	      StrippedDataSet =
		  if 
		      length(PeriodSet) > round(XSize/5) ->
			  strip_data(PeriodSet, round(XSize/5));
		      true ->
			  PeriodSet
		  end,
	      ScaledData = scale(StrippedDataSet, Max, Min, boxed),
	      encode(ScaledData)
      end, DataSets), Max, Min}.

scale(Data) ->
    scale(Data, boxed).

scale(Data, Type) ->
    Max = lists:max(Data),
    Min = lists:min(Data),    	      
    scale(Data, [], Max, Min, Type).

scale(Data, Max, Min, Type) ->
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
	  fun(Value, {1, AccList, MaxVal}) ->
		  {MaxVal, [Value | AccList], MaxVal};
	     (_Value, {Step, AccList, MaxVal}) ->
		  {Step-1, AccList, MaxVal}
	  end, {Steps, [], Steps}, DataSet),
    Result.
