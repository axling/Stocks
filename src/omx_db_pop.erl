-module(omx_db_pop).

-include("mnesia_defs.hrl").
-export([save_instrument/3]).

save_instrument(Instrument, StartDate, EndDate) ->
    OmxReqReply = http_lib:download_stock_data(Instrument, StartDate, EndDate),
    StockList = omx_parse_lib:parse_page(OmxReqReply),
    %% mnesia is started and table created
    lists:map(
	  fun({Date, Max, Min, Closing, _Average, 
	       Volume, TurnOver, Completions}) ->
		  #stock{
	       date=Date,
	       highest=Max,
	       lowest=Min,
	       closing=Closing,	       
	       turnover=TurnOver,
	       volume=Volume,
	       completions=Completions
	      }
	  end, StockList).
	


