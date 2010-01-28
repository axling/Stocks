-module(omx_db_pop).

-include("mnesia_defs.hrl").
-export([save_instrument/4]).

save_instrument(Instrument, Name, StartDate, EndDate) ->
    OmxReqReply = http_lib:download_stock_data(Instrument, StartDate, EndDate),
    StockList = omx_parse_lib:parse_page(OmxReqReply),
    %% mnesia is started and table created
    lists:foreach(
      fun({Date, Max, Min, Closing, Average, 
	   Volume, TurnOver, Completions}) ->
	      Stock = #stocks{
		company=Name,
		date=Date,
		highest=Max,
		lowest=Min,
		closing=Closing,
		average=Average,
		turnover=TurnOver,
		volume=Volume,
		completions=Completions
	       },
	      ok = db_handler:create_entry(Stock)
      end, StockList).


