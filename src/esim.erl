%%%-------------------------------------------------------------------
%%% File    : esim.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : This file will run a stock market simulation
%%% which simulates that you buy a stock when the gradient reaches a 
%%% certain level. The same goes for when you sell a stock.
%%%
%%% Created : 18 May 2009 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(esim).

-export([run_one/4, run/3, run_sim/3]).

-include("mnesia_defs.hrl").

%% Stocks = [{ericb, [{{2000,1,2}, 1.2}, {{2000,1,3}, 2.1}, ....]},...]
%% Params = [{buy, ericb, [{ericb, [{10, 0.5}, {50, 0.2}]},
%%                         {omx, [{10, 0.1},{100, 0.1}]}]},
%%           {sell, ericb, [{ericb, }]}]

run_one(Stocks, BuyList, SellList, Market) ->
    run([{c, Stocks}], [{buy, c, [{c, BuyList}]}, {sell, c, [{c, SellList}]}],
       Market).

run(Stocks, Params, #market{} = MarketParams) ->
    StockStruct = compile_stock_struct(Stocks),	
    run_sim(StockStruct, Params, MarketParams).

run_sim(StockStruct, Params, #market{}=MarketParams) ->
    BuyNSell = compile_params(Params),
    StartDate = get_earliest_date(StockStruct),
    EndDate = get_latest_date(StockStruct),
    loop(StockStruct, BuyNSell, MarketParams, StartDate, EndDate).

loop(StockStruct, _BuyNSell, MarketParams, EndDate, EndDate) ->
    lists:foldl(fun({StockName, _Nr}, Market) ->
			SellDate = element(1, gb_trees:largest(
						proplists:get_value(StockName,
								    StockStruct))),
			sell(StockStruct, StockName, 
			     SellDate, Market)		   
		end, MarketParams, MarketParams#market.holdings);
		
loop(StockStruct, BuyNSell, MarketParams, Date, EndDate) ->
    NewMarketParams = 
	lists:foldl(fun({sell, StockName, SellFun}, Market) ->
			    case SellFun(StockStruct, Date) of
				true ->
				    sell(StockStruct, StockName, 
					 Date, Market);
				false ->
				    Market
			    end;
		       ({buy, _, _}, Market)->
			    Market
		    end, MarketParams, BuyNSell),
    NewerMarketParams = 	
	lists:foldl(fun({buy, StockName, BuyFun}, Market) ->
			    case BuyFun(StockStruct, Date) of
				true ->
				    buy(StockStruct, StockName, 
					Date, Market);
				false ->
				    Market
			    end;
		       ({sell, _, _}, Market) ->
			    Market
		    end, NewMarketParams, BuyNSell),
    NextDate = get_next_date(Date),
    NewDate = get_next_valid_date(StockStruct, NextDate, EndDate),
    loop(StockStruct, BuyNSell, NewerMarketParams, NewDate, EndDate).

buy(StockStruct, StockName, Date, Market) ->  
    Money = Market#market.money,
    case Money < Market#market.min of
	false ->
	    Risk = Market#market.risk,
	    StockVal = get_value_from_date(StockStruct, StockName, Date, 0),
	    {NrOfStocks, MoneyLeft} =calculate_purchase(StockVal, Money, Risk),
	    case lists:keyfind(StockName, 1, Market#market.holdings) of
		{Stockname, OldNr} ->
		    Market#market{money=MoneyLeft, 
				  holdings = lists:keyreplace(
					       StockName, 
					       1, Market#market.holdings,
					       {StockName, 
						OldNr+NrOfStocks}),
				  history = [{buy, Stockname, Date, StockVal,
					      NrOfStocks*StockVal,
					      NrOfStocks} |
					     Market#market.history]};
		false ->
		    Market#market{money=MoneyLeft, 
				  holdings = [{StockName,NrOfStocks} | 
					      Market#market.holdings],
				  history = [{buy, StockName, Date, StockVal, 
					      NrOfStocks*StockVal,
					      NrOfStocks} |
					     Market#market.history]}
	    end;
	true ->
	    Market
    end.

sell(StockStruct, StockName, Date, Market) ->  
    Money = Market#market.money,
    TaxFactor = Market#market.tax_factor,
    Courtage = Market#market.courtage,
    StockVal = get_value_from_date(StockStruct, StockName, Date, 0),
    case lists:keyfind(StockName, 1, Market#market.holdings) of
	{StockName, Nr} ->
	    BuyAmount = calculate_buy_amount(StockName, Market#market.history, 0),
	    SellMoney = calculate_sell(Nr, StockVal, TaxFactor, Courtage, BuyAmount),
	    Market#market{money = SellMoney + Money,
			  holdings = lists:keydelete(StockName, 1, 
						     Market#market.holdings),
			  history = [{sell, StockName, Date, StockVal, 
				      SellMoney, Nr} | Market#market.history]};
	false ->
	    Market
    end.		

calculate_purchase(StockVal, Money, Risk) ->
    NrOfStocks = trunc((Money*Risk)/StockVal),
    NewMoney = Money - (NrOfStocks * StockVal),
    {NrOfStocks, NewMoney}.

calculate_sell(NrOfStocks, Stockval, TaxFactor, Courtage, BuyAmount) ->
    SellAmount = ((NrOfStocks * Stockval) - Courtage),
    case (SellAmount - BuyAmount) < 0 of
	true -> 
	    SellAmount;
	false ->
	    SellAmount - (SellAmount - BuyAmount)*TaxFactor
    end.

calculate_buy_amount(_StockName, [], AccAmount) ->
    AccAmount;
calculate_buy_amount(_StockName, [{sell, _Stockname, _, _, _} | _Rest], AccAmount) ->
    AccAmount;
calculate_buy_amount(StockName, [{buy, StockName, 
				  _Date, StockVal, Nr} | Rest], 
		     AccAmount) ->
    calculate_buy_amount(StockName, Rest, AccAmount + StockVal*Nr);
calculate_buy_amount(StockName, [_First | Rest], AccAmount) ->
    calculate_buy_amount(StockName, Rest, AccAmount).


%%%%%%%%%%%%% Managing StockValues %%%%%%%%%%%%%%%%%%%%%%%%%
compile_stock_struct(Stocks) ->
    lists:map(fun({StockName, StockValues}) ->
		      {StockName, store_stock_vals(StockValues, 
						   gb_trees:empty())}
	      end, Stocks).

store_stock_vals([], Tree) ->
    Tree;
store_stock_vals([{Date, Val} | Rest], Tree) ->
    store_stock_vals(Rest, gb_trees:enter(Date, Val, Tree)).

get_val(StockTree, Date) ->
    gb_trees:lookup(Date, StockTree).
    
%% get_value_from_date, will try and get the date specified
%% but when there doesn't exist a value for the date it will always go 
%% forward in time to find a date!!!!!
get_value_from_date(StockStruct, StockName, Date, 0) ->
    GetDate = get_old_date_from_days(Date, 0),
    StockTree = proplists:get_value(StockName, StockStruct),
    case get_val(StockTree, GetDate) of
	{value, Val} ->
	    Val;
	none ->
	    no_val
    end;
get_value_from_date(StockStruct, StockName, Date, Days) ->
    GetDate = get_old_date_from_days(Date, Days),
    StockTree = proplists:get_value(StockName, StockStruct),
    case get_val(StockTree, GetDate) of
	{value, Val} ->
	    Val;
	none ->
	    get_value_from_date(StockStruct, StockName, Date, Days-1)
    end.

get_earliest_date(StockStruct) ->
    Results = lists:map(fun({_StockName, Tree}) ->
				gb_trees:smallest(Tree)
			end, StockStruct),
    element(1, hd(lists:sort(Results))).

get_latest_date(StockStruct) ->
    Results = lists:map(fun({_StockName, Tree}) ->
				gb_trees:largest(Tree)
			end, StockStruct),
    element(1, hd(lists:reverse(lists:sort(Results)))).

get_next_valid_date(_StockStruct, EndDate, EndDate) ->
    EndDate;
get_next_valid_date(StockStruct, Date, EndDate) ->
    case is_valid_date(StockStruct, Date) of
	true ->
	    Date;
	false ->
	    NextDate = get_next_date(Date),
	    get_next_valid_date(StockStruct, NextDate, EndDate)
    end.

get_next_date(Date) ->
    calendar:gregorian_days_to_date(
      calendar:date_to_gregorian_days(Date) + 1).

is_valid_date(StockStruct, Date) ->
    DateResult = 
	lists:map(fun({_StockName, Tree}) ->
			  gb_trees:is_defined(Date, Tree)
		  end, StockStruct),
    or_list(DateResult).

%%%%%%%%%%%%% Managing Parameters for Buy and Sell %%%%%%%%% 
compile_params([]) ->
    [];
compile_params([{BuyOrSell, StockName, Criteria} | Rest]) ->
    Fun = compile_criteria(BuyOrSell, Criteria),
    [{BuyOrSell, StockName, Fun} | compile_params(Rest)].


%% Criteria =  [ericb, [{10, 0.6}, {20, 0.8}....]]
compile_criteria(BuyOrSell, Criteria) ->    
    fun(StockStruct, Date) ->
	    Results = 
		lists:map(
		  fun({StockName, CriteriaList}) ->		      
			  case get_value_from_date(StockStruct, StockName, Date, 0) of
			      no_val ->
				  false;
			      Today ->
				  AllResults = 
				      lists:map(
					fun({Days, Param}) ->			  
						%% This value might not be from
						%% the date that we think.
						OldVal = get_value_from_date(
							   StockStruct, 
							   StockName,
							   Date, 
							   Days),
						%% The days factor might be wrong
						buy_or_sell(BuyOrSell, Today, OldVal,
							    Days, Param)			      
					end, CriteriaList),
				  and_list(AllResults)
			  end
		  end, Criteria),
	    and_list(Results)    
    end.

buy_or_sell(buy, Today, OldVal,Days, Param) 
  when Param >= 0 ->
    (((Today - OldVal) / Days) > Param);
buy_or_sell(sell, Today, OldVal,Days, Param)
  when Param >= 0 ->
    (((Today - OldVal) / Days) < -Param).
    
    
%%%%%%%%%%%%% Date functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_old_date_from_days({Year, Month, Days}, MinusDays) ->
    NrDays = calendar:date_to_gregorian_days({Year, Month, Days}),
    calendar:gregorian_days_to_date(NrDays - MinusDays).

and_list([]) ->
    true;
and_list([true | Rest]) ->
    and_list(Rest); 
and_list([false | _Rest]) ->
    false.

or_list([]) ->
    false;
or_list([true | _Rest]) ->
    true;
or_list([false | Rest]) ->
    or_list(Rest).
