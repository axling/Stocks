-module(date_lib).

-export([convert_date_e_s/1]).


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

    
    
