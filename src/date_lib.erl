-module(date_lib).

-export([convert_date_e_s/1, convert_date_s_e/1, convert_time_e_s/1,
	 is_greater/2, is_greater_or_equal/2, get_latest_date/1,
	 is_less_or_equal/2, today/0, today_time/0,
	 tomorrow/0, seconds_until_time/1, date_minus_days/2,
	 last_workday/0, last_workday/1, get_val_of_date/2]).


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

convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute < 10, Second < 10 ->
    "0" ++ integer_to_list(Hour) ++ ":0" ++ integer_to_list(Minute) ++ ":0" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute < 10, Second >= 10 ->
    "0" ++ integer_to_list(Hour) ++ ":0" ++ integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute >= 10, Second < 10 ->
    "0" ++integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":0" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute >= 10, Second >= 10 ->
    "0" ++integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) ->
    integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second).


convert_date_s_e(DateString) ->
    Year = list_to_integer(lists:sublist(DateString, 1, 4)),
    Month = list_to_integer(lists:sublist(DateString, 6, 2)),
    Day = list_to_integer(lists:sublist(DateString, 9, 2)),
    {Year, Month, Day}.

is_greater(Date1, Date2) ->
    Date1ToGregorianDays = calendar:date_to_gregorian_days(Date1),
    Date2ToGregorianDays = calendar:date_to_gregorian_days(Date2),
    Date1ToGregorianDays > Date2ToGregorianDays.

is_greater_or_equal(Date1, Date2) ->
    if Date1 == Date2 ->
	    true;
       true ->
	    is_greater(Date1, Date2)
    end.

is_less_or_equal(Date1, Date2) ->
    if Date1 == Date2 ->
	    true;
       true ->
	    is_greater(Date2, Date1)
    end.

last_workday() ->
    last_workday(today()).

last_workday(Date) ->
    case calendar:day_of_the_week(Date) > 5 of
	true ->
	    Previous = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 1),
	    last_workday(Previous);
	false ->
	    Date
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
    
today() ->   
    {Date, _Time} = today_time(),
    Date.

today_time() ->
    calendar:now_to_local_time(now()).

tomorrow() ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(today()) + 1).

seconds_until_time(DateTime) ->
    Until = calendar:datetime_to_gregorian_seconds(DateTime),
    Now = calendar:datetime_to_gregorian_seconds(today_time()),
    Diff = Until - Now,
    if 
	Diff =< 0 ->
	    erlang:error(until_date_in_the_past);
	true ->
	    Diff
    end.

date_minus_days(Date, Days) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - Days).
    

get_val_of_date([], _Date) ->
    {error, empty_list};
get_val_of_date([{Val, Date} | _DateList], Date) ->
    Val;
get_val_of_date(DateList, Date) ->
    get_val_of_date1(DateList, Date).

get_val_of_date1([], _Date) ->
    {error, not_found};
get_val_of_date1([{Val, Date} | _DateList], Date) ->
    Val;
get_val_of_date1([{Val, CurrentDate} | DateList], Date) ->
    case is_greater(Date, CurrentDate) of
	true ->
	    Val;
	false ->
	    get_val_of_date1(DateList, Date)
    end.
