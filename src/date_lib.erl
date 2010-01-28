-module(date_lib).

-export([convert_date_e_s/1, convert_date_s_e/1,
	 is_greater/2, get_latest_date/1,
	 is_less_or_equal/2]).


convert_date_e_s({Year, Month, Day}) 
  when Month < 10, Day < 10 ->
    integer_to_list(Year) ++ "-0" ++ integer_to_list(Month) ++ "-0" ++
	integer_to_list(Day);
convert_date_e_s({Year, Month, Day}) 
  when Month < 10, Day >= 10 ->
    integer_to_list(Year) ++ "-0" ++ integer_to_list(Month) ++ "-" ++
	integer_to_list(Day);
convert_date_e_s({Year, Month, Day}) 
  when Month >= 10, Day < 10 ->
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-0" ++
	integer_to_list(Day);
convert_date_e_s({Year, Month, Day}) ->
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++
	integer_to_list(Day).


convert_date_s_e(DateString) ->
    Year = list_to_integer(lists:sublist(DateString, 1, 4)),
    Month = list_to_integer(lists:sublist(DateString, 6, 2)),
    Day = list_to_integer(lists:sublist(DateString, 9, 2)),
    {Year, Month, Day}.

is_greater(Date1, Date2) ->
    Date1ToGregorianDays = calendar:date_to_gregorian_days(Date1),
    Date2ToGregorianDays = calendar:date_to_gregorian_days(Date2),
    Date1ToGregorianDays > Date2ToGregorianDays.

is_less_or_equal(Date1, Date2) ->
    if Date1 == Date2 ->
	    true;
       true ->
	    is_greater(Date2, Date1)
    end.

get_latest_date([]) ->
    [];
get_latest_date(Dates) ->
    get_latest_date(Dates, hd(Dates)).

get_latest_date([], CompDate) ->
    CompDate;
get_latest_date([FirstDate | Dates], CompDate) ->
    case is_less_or_equal(CompDate, FirstDate) of
	true ->
	    get_latest_date(Dates, FirstDate);
	false ->
	    get_latest_date(Dates, CompDate)
    end.
    
    
    
