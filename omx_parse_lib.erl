-module(omx_parse_lib).

-export([parse_page/1, split/2, row_re/0]).

-define(FLOAT, "[0-9]+\\,[0-9]+").
-define(NUMBER, "(?:" ++ ?FLOAT ++ "|\\d+)").
-define(AVGNUMBER, "(?:" ++ ?FLOAT ++ "|\\d+|\\s*)").
-define(NAME, "\\S+").

-define(STOCK_TYPE1, "<span\\s*class=\"[YC]\">\\s*(?<diff>" ++ ?NUMBER ++
	")\\s+(?<buy1>" ++ ?NUMBER ++ ")\\s+(?<sell1>" ++ ?NUMBER ++ 
	")\\s+(?<name1>" ++ ?NAME ++ ")\\s+(?<latest1>" ++ ?NUMBER ++ 
	")\\s+(?<number1>" ++ ?NUMBER ++ ")\\s*</span>").

-define(DATE, "\\d{4}-\\d{2}-\\d{2}").
-define(ROW, 
	"<tr\\s+id=\"historicalTable-\">\\s*<td>(?<date>" ++ ?DATE ++
	")</td>\\s+<td>(?<max>"++ ?NUMBER ++ ")</td>\\s+" ++
	"<td>(?<min>" ++ ?NUMBER ++ ")</td>\\s*<td>(?<closing>" ++ ?NUMBER ++
	")</td>\\s+<td>(?<average>" ++ ?AVGNUMBER ++ ")</td>\\s+" ++ 
	"<td>(?<volume>[^<]+)</td>\\s+<td>(?<turnover>[^<]+)</td>\\s+<td>"
	"(?<completions>[^<]+)</td>").

row_re()->
    ?ROW.

split(String, RegExp) ->
    re:split(String, RegExp,[{return, list}]).

parse_page(Page) ->
    StringList = split(Page, "</tr>"),
    NewStringList = lists:filter(
 		      fun(String) ->
 			      is_match(String, ?ROW)
 		      end, StringList),
    ResultList = 
	lists:map(
	  fun(String) ->
		  case match_type(String) of
		      nomatch ->
			  nomatch;
		      Entry ->
			  Entry
		  end
	  end, NewStringList).

match(String, RegExp) ->
    re:run(String, RegExp, [{capture, all_but_first, list}]).

match_type(String) ->
    case match(String, ?ROW) of
	{match, [Date, Max, Min, Closing, Average, Volume, TurnOver, Completions]} ->
	    {Date, Max, Min, Closing, Average, Volume, TurnOver, Completions};
	nomatch ->
	   undefined
    end.

is_match(String, RegExp) ->
    case re:run(String, RegExp) of
	nomatch ->
	    false;
	_Else ->
	    true
    end.
