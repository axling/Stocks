-module(date_lib).

-export([convert_date_e_s/1, convert_date_s_e/1]).


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


    
    
