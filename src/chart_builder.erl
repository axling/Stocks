%%%-------------------------------------------------------------------
%%% File    : chart_builder.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : 
%%%
%%% Created :  3 Feb 2010 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(chart_builder).

%% API
-export([render/1]).

-include("chart.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
prepare_data([], _) ->
    {[], 100, 0};
prepare_data(DataSets, XSize) ->
    MaxList = lists:map(
		fun(DataSet) ->
			get_max(DataSet)
		end, DataSets),
    Max = case lists:filter(fun(no_max) -> false; (_E) -> true end, MaxList) of
	      [] ->
		  100;
	      AList ->
		  lists:max(AList)
	  end,
    MinList = lists:map(
		fun(DataSet) ->
			get_min(DataSet)
		end, DataSets),
    Min = case lists:filter(fun(no_min) -> false; (_E) -> true end, MinList) of
	      [] ->
		  0;
	      Else ->
		  lists:min(Else)
	  end,
    {lists:map(
       fun(DataSet) ->
	       StrippedDataSet =
		   if 
		       length(DataSet) > round(XSize/5) ->
			   strip_data(DataSet, round(XSize/5));
		       true ->
			   DataSet
		   end,
	       ScaledData = scale(StrippedDataSet, Max, Min, boxed),
	       encode(ScaledData)
       end, DataSets), Max, Min}.

scale([], _, _, _) ->
    [];
scale(Data, Max, Min, Type) ->
    scale(Data, [], Max, Min, Type).

scale([], AccData, _Max, _Min, _Type) ->
    lists:reverse(AccData);
scale([no_value | Data], AccData, Max, Min, Type) ->
    scale(Data, [no_value | AccData], Max, Min, Type);
scale([Val | Data], AccData, Max, Min, from_zero) ->
    scale(Data, [round((Val/Max) * 61) | AccData],
	  Max, Min, from_zero);
scale([Val | Data], AccData, Max, Min, Type) ->
    scale(Data, [round(((Val-Min)/(Max-Min))*61) | AccData],
	  Max, Min, Type).

encode([]) ->
    [];
encode(Data) ->
    encode(Data, []).

encode([], AccData) ->
    lists:reverse(AccData);
encode([no_value | Data], AccData) ->
    encode(Data, ["_" | AccData]);
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

strip_data([], _) ->
    [];
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

render(#google_chart{}=Record) ->
    %% This is mostly borrowed from nitrogen
    Path = "http://chart.apis.google.com/chart?",
						% Chart Type...
    Type = ["&cht=",
	    case Record#google_chart.type of
		line -> "lc";
		sparkline -> "ls";
		stacked_horizontal_bar -> "bhs";
		stacked_vertical_bar -> "bvs";
		grouped_horizontal_bar -> "bhg";
		grouped_vertical_bar -> "bvg";
		pie -> "p";
		pie3d -> "p3";
		OtherType -> erlang:error({unknown_chart_type, OtherType})
	    end
	   ],

						% Title...
    Title = case Record#google_chart.title of
		undefined -> [];
		[] -> [];
		OtherTitle -> ["&chtt=", yaws_api:url_encode(OtherTitle)]
	    end,

						% Title Color and Font Size...
    TitleStyle = yaws_api:f("&chts=~s,~b", [to_list(Record#google_chart.color), 
					    Record#google_chart.font_size]),

						% Size...
    Size = yaws_api:f("&chs=~bx~b", [Record#google_chart.width, Record#google_chart.height]),

						% Grid...
    Grid = yaws_api:f("&chg=~s,~s,~b,~b", [
					   to_list(coalesce([Record#google_chart.grid_x, 0])),
					   to_list(coalesce([Record#google_chart.grid_y, 0])),
					   Record#google_chart.grid_line_length,
					   Record#google_chart.grid_blank_length
					  ]),

						% Background Colors...
    BGColors = yaws_api:f("&chf=bg,s,~s|c,s,~s", 
			  [to_list(Record#google_chart.background_color), 
			   to_list(Record#google_chart.chart_color)]),    
						% Legend Location...
    LegendLocation = "&chdlp=" ++ 
	case Record#google_chart.legend_location of
	    top -> "t";
	    left -> "l";
	    bottom -> "b";
	    right -> "r"
	end,

    %% Data...
    {Data, YMax, YMin, XAxis} = 
	case Record#google_chart.data of
	    undefined -> MaxValueLength=0, [];
	    [] -> MaxValueLength=0, [];
	    ChartData ->
		NewData =
		    case Record#google_chart.date_alignment of
			undefined ->
			    safe_return(ChartData);
			[] ->
			    safe_return(ChartData);
			Dates ->
			    align_data(Dates, ChartData)
		    end,
				
		{StrippedScaled, Max, Min} = prepare_data(NewData, Record#google_chart.width),
		ProcessedData = [process_data(lists:nth(N, StrippedScaled), 
					      lists:nth(N, ChartData)) 
				 || N <- lists:seq(1, length(StrippedScaled))],
		DataColors  = "&chco="  ++ string:join([X || [X, _, _, _, _, _] <- ProcessedData], ","),
		DataLegends = "&chdl="  ++ string:join([X || [_, X, _, _, _, _] <- ProcessedData], "|"),
		DataScales  = "&chds="  ++ string:join([X || [_, _, X, _, _, _] <- ProcessedData], ","),
		DataStyles  = "&chls="  ++ string:join([X || [_, _, _, X, _, _] <- ProcessedData], "|"),
		DataValues  = "&chd=s:" ++ string:join([X || [_, _, _, _, X, _] <- ProcessedData], ","),
		MaxValueLength = lists:max([X || [_, _, _, _, _, X] <- ProcessedData]),
		DataLegends1 = case string:strip(DataLegends, both, $|) of
				   "&chdl=" -> [];
				   _ -> DataLegends
			       end,				


		{DataColors ++ DataLegends1 ++ DataScales ++ DataValues ++ DataStyles, Max, Min, 
		 strip_data(Record#google_chart.date_alignment, 4)}
	end,

    %% Axes...
    Axes = case Record#google_chart.axes of 
	       undefined -> 
		   Positions = "&chxt=x,y",
		   StringLabels = [date_lib:convert_date_e_s(D) || D <- XAxis],
		   XLabels = lists:concat(["&chxl=0:|", string:join(StringLabels, "|")]),
		   YRange = lists:concat(["&chxr=1,", round(YMin), ",", round(YMax), ",", round((YMax-YMin)/4)]),
		   Positions ++ XLabels ++ YRange;
	       [] ->
		   Positions = "&chxt=x,y",
		   StringLabels = [date_lib:convert_date_e_s(D) || D <- XAxis],
		   XLabels = lists:concat(["&chxl=0:|", string:join(StringLabels, "|")]),
		   YRange = lists:concat(["&chxr=1,", round(YMin), ",", round(YMax), ",", round((YMax-YMin)/4)]),
		   Positions ++ XLabels ++ YRange;
	       AxesRecords ->			
		   ProcessedAxes = [process_axis(N - 1, lists:nth(N, AxesRecords)) || N <- lists:seq(1, length(AxesRecords))],
		   AxesPositions = "&chxt=" ++ string:join([X || [X, _, _] <- ProcessedAxes], ","),
		   AxesLabels    = "&chxl=" ++ string:join([X || [_, X, _] <- ProcessedAxes], "|"),
		   AxesColors    = "&chxs=" ++ string:join([X || [_, _, X] <- ProcessedAxes], "|"),
		   AxesPositions ++ AxesLabels ++ AxesColors
	   end,

    %% Calculate bar size...
    BarSize = case MaxValueLength of 
		  0 -> [];
		  _ -> 
		      DataGroupsLength = length(Record#google_chart.data),
		      BarGroupSpace = Record#google_chart.bar_group_space,
		      BarSpace = Record#google_chart.bar_space,
		      GroupSpacerPixels = MaxValueLength * BarGroupSpace,
		      BarSpacerPixels = MaxValueLength * (DataGroupsLength * BarSpace),
		      AvailablePixels = case Record#google_chart.type of 
					    stacked_horizontal_bar -> Record#google_chart.height;
					    grouped_horizontal_bar -> Record#google_chart.height;
					    stacked_vertical_bar -> Record#google_chart.width;
					    grouped_vertical_bar -> Record#google_chart.width;
					    _ -> 0
					end,
		      IndividualBarSize = (AvailablePixels - GroupSpacerPixels - BarSpacerPixels) / (DataGroupsLength * MaxValueLength),
		      yaws_api:f("&chbh=~b,~b,~b", [trunc(IndividualBarSize), BarSpace, BarGroupSpace])
	      end,
    lists:flatten([Path, Type, Title, TitleStyle, Size, Grid, BGColors, LegendLocation, BarSize, Axes, Data]).

align_data(_Dates, []) ->
    [];
align_data(Dates, SecData) ->    
    lists:map(
      fun(SecList) ->
	      Filtered = lists:dropwhile(
			   fun({_, Date}) ->
				   not lists:member(Date, Dates)
			   end, SecList#chart_data.values),
	      {NewValues, _} =
		  lists:mapfoldl(
		    fun(Date, N) when N =< length(Filtered) ->
			    case lists:nth(N, Filtered) of
				{Val, Date} ->
				    {Val, N+1};
				_Else ->
				    {no_value, N+1}
			    end;
		       (_Date, N) ->
			    {no_value, N+1}
		    end, 1, Dates),
	      NewValues
      end, SecData).

process_data(Values, Data) ->    
    Color = to_list(Data#chart_data.color),
    Legend = to_list(Data#chart_data.legend),
    Scale = yaws_api:f("~b,~b", [Data#chart_data.min_value, Data#chart_data.max_value]),     
    Styles = yaws_api:f("~b,~b,~b", [Data#chart_data.line_width, Data#chart_data.line_length, Data#chart_data.blank_length]),
    [Color, Legend, Scale, Styles, Values, length(Values)].

process_axis(N, Axis) ->
    Position = case Axis#chart_axis.position of
		   top -> "t";
		   right -> "r";
		   bottom -> "x";
		   left -> "y";
		   OtherPosition -> erlang:error({unknown_axis_position, OtherPosition})
	       end,
    StringLabels = [to_list(X) || X <- Axis#chart_axis.labels],
    Labels = integer_to_list(N) ++ ":|" ++ string:join(StringLabels, "|"),
    Style = yaws_api:f("~b,~s,~b", [N, to_list(Axis#chart_axis.color), Axis#chart_axis.font_size]),
    [Position, Labels, Style].

get_max([]) ->
    no_max;
get_max(List) ->
    case lists:filter(fun(no_value) -> false; (Else) when is_number(Else) -> true end, List) of
	[] ->
	    no_max;
	List2 ->	    
	    lists:max(List2)
    end.

get_max1([], no_value) ->
    0;
get_max1([], Max) when is_number(Max) ->
    Max;
get_max1([no_value | Rest], no_value) ->
    get_max1(Rest, 0);
get_max1([First | Rest], no_value) when is_number(First) ->
    get_max1(Rest, First);
get_max1([First | Rest], Max) when First > Max ->
    get_max1(Rest, First);
get_max1([_First | Rest], Max) ->
    get_max1(Rest, Max).

get_min([]) ->
    no_min;
get_min(List) ->
    case lists:filter(fun(no_value) -> false; (Else) when is_number(Else) -> true end, List) of
	[] ->
	    no_min;
	List2 ->	    
	    lists:min(List2)		
    end.

get_min1([], no_value) ->
    0;
get_min1([], Min) when is_number(Min) ->
    Min;
get_min1([no_value | Rest], no_value) ->
    get_min1(Rest, 1000);
get_min1([First | Rest], no_value) when is_number(First) ->
    get_min1(Rest, First);
get_min1([First | Rest], Min) when First < Min ->
    get_min1(Rest, First);
get_min1([_First | Rest], Min) ->
    get_min1(Rest, Min).

coalesce([]) -> undefined;
coalesce([H]) -> H;
coalesce([undefined|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.

-define (IS_STRING(Term), (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).

to_list(undefined) -> [];
to_list(L) when ?IS_STRING(L) -> L;
to_list(L) when is_list(L) ->
    SubLists = [inner_to_list(X) || X <- L],
    lists:flatten(SubLists);
to_list(A) -> inner_to_list(A).
inner_to_list(A) when is_atom(A) -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I) -> integer_to_list(I);
inner_to_list(L) when is_list(L) -> L.

safe_return([]) ->
    [];
safe_return(ChartData) ->
    lists:map(
      fun(#chart_data{}=C) ->
	     C#chart_data.values;
	 (_Else) ->
	     []
      end, ChartData).
    
