%%%-------------------------------------------------------------------
%%% File    : db.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : The databse handling. The database is currently DETS 
%%% tables
%%%
%%% Created :  8 May 2009 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(db).

-export([get_stocks/1]).

get_stocks(DetsFile) ->
    {ok, stock_db} = dets:open_file(stock_db, 
				    [{file,DetsFile}]),
    StockList = dets:match(stock_db, '$1'),
    dets:close(stock_db),
    sort_stocks(lists:flatten(StockList)).

sort_stocks(StockList) ->
    lists:keysort(1, StockList).
