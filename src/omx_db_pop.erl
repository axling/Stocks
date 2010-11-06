-module(omx_db_pop).

-include("mnesia_defs.hrl").
-export([save_instrument/3]).

save_instrument(Instrument, StartDate, EndDate) ->
    OmxReqReply = http_lib:download_stock_data(Instrument, StartDate, EndDate),
    StockList = omx_parse_lib:parse_page(OmxReqReply),
    %% mnesia is started and table created
    L = lists:map(
	  fun({Date, Max, Min, Closing, Average, 
	       Volume, TurnOver, Completions}) ->
		  #stock{
	       date=Date,
	       highest=Max,
	       lowest=Min,
	       closing=Closing,
	       average=Average,
	       turnover=TurnOver,
	       volume=Volume,
	       completions=Completions
	      }
	  end, StockList),
    lists:sort(
      fun(StockA, StockB) ->
	      date_lib:is_greater(StockA#stock.date, StockB#stock.date)
      end, L).
	


