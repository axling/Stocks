%%%-------------------------------------------------------------------
%%% File    : ga.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : This module will optimize according to a Genetic Algorithms
%%% a stock scenario using the esim module. 
%%% 
%%%
%%% Created :  8 May 2009 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(ga).

-export([run/5]).
%%-export([run/3]).
-include("stocks.hrl").

%% Stocks = [{ericb, "ericsson.dets"},...]
%% Params = [{buy, ericb, [{ericb, [10,50]},
%%                         {omx, [10,100]}]},
%%           {sell, ericb, [{ericb, [10,15]}]}]
run(Stocks, Params, Market, Mutation, Generations) ->
    StockStruct = esim:compile_stock_struct(Stocks),
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    NrParams = calculate_nr_params(Params, 0),
    ParamsList = generate_params(NrParams, 12),
    ChromosomeList = encode_params(ParamsList),
    loop(StockStruct, ChromosomeList, Params, Market, Mutation, Generations).

loop(Stocks, ChromosomeList, Params, Market, _Mutation, 0) ->
    Pid = self(),
    lists:foreach(fun(Nr) ->
			  ParamsList = 
			      decode_chromosome(
				lists:nth(Nr, ChromosomeList)),
			  RealParams = compile_real_params(ParamsList, Params, []),
			  Fun = 
			      fun() ->
				      ReturnMarket = 
					  esim:run_sim(Stocks, RealParams, Market),
				      Pid ! {end_of_sim, Nr, ReturnMarket}
			      end,
			  spawn(Fun)
		  end, lists:seq(1, length(ChromosomeList))),
    ValueList = receive_values(length(ChromosomeList), []),
    DecodedChromosomes = lists:map(fun(El) ->
					    decode_chromosome(El)
				    end, ChromosomeList),    
    ResultList = lists:zip(ValueList, DecodedChromosomes),
    lists:max(lists:map(fun({{_OrderNr, Return}, DecodedChromosome}) ->
				{Return#market.money, Return, DecodedChromosome}
			end, ResultList));

loop(Stocks, ChromosomeList, Params, Market, Mutation, Generations) ->
    %%io:format("Generations: ~p~n", [Generations]),
    Pid = self(),
    lists:foreach(fun(Nr) ->
			  ParamsList = 
			      decode_chromosome(
				lists:nth(Nr, ChromosomeList)),
			  RealParams = compile_real_params(ParamsList, Params, []),
			  Fun = 
			      fun() ->
				      ReturnMarket = 
					  esim:run_sim(Stocks, RealParams, Market),
				      Pid ! {end_of_sim, Nr, ReturnMarket#market.money}
			      end,
			  spawn(Fun)
		  end, lists:seq(1, length(ChromosomeList))),
    ValueList = receive_values(length(ChromosomeList), []),
    SortedVals = lists:reverse(lists:keysort(2, ValueList)),
    NewChromosomeList = do_crossover(ChromosomeList, SortedVals, Mutation),
    loop(Stocks, NewChromosomeList, Params, Market, Mutation, Generations-1).

calculate_nr_params([], AccNr) ->
    AccNr;
calculate_nr_params([{_BuyOrSell, _StockName, ParamList} | Rest], AccNr) ->
    Sum = 
	lists:sum(lists:map(fun({_Name, List}) ->
				    length(List)
			    end, ParamList)),
    calculate_nr_params(Rest, AccNr + Sum).

compile_real_params(_ParamsList, [], AccParams) ->
    AccParams;
compile_real_params(ParamsList, [{BuyOrSell, StockName, Criterias} | Params], 
		    AccParams) ->
    %% Map params to the real param structure
    {NewCriterias, NewParamsList} = build_criterias(Criterias, ParamsList),
    compile_real_params(NewParamsList, Params, 
			[{BuyOrSell, StockName, NewCriterias} | AccParams]).

build_criterias(Criterias, ParamsList) ->
    lists:mapfoldl(fun({StockName, List}, RestParamsList) ->
		      {Relevant, Rest} = lists:split(length(List), RestParamsList),
		      {{StockName, lists:zip(List, Relevant)}, Rest}
	      end, ParamsList, Criterias).

do_crossover(ChromosomeList, SortedVals, Mutation) ->
    ParentNr = lists:sublist([Nr || {Nr,_} <- SortedVals],
			     trunc(length(SortedVals)/2)),
    Parents = [lists:nth(Nr, ChromosomeList) || Nr <- ParentNr],
    Offspring = cross_parents(Parents, [], Mutation),
    MutatedParents = lists:map(fun(X) ->
				       mutate(X, [], Mutation)
			       end, Parents),
    lists:append([MutatedParents, Offspring]).

cross_parents([], AccOffspring, _Mutation) ->
    AccOffspring;
cross_parents([First, Second | Rest], AccOffspring, Mutation) ->
    SplitPoint = trunc(length(binary_to_list(First))/2),
    {First1, First2} = lists:split(SplitPoint, binary_to_list(First)),
    {Second1, Second2} = lists:split(SplitPoint, binary_to_list(Second)),
    NewOffspring1 =  mutate(list_to_binary(lists:append([First1, Second2])), 
			    [], Mutation),
    NewOffspring2 =  mutate(list_to_binary(lists:append([Second1, First2])), 
			    [], Mutation),
    
    cross_parents(Rest, [NewOffspring1, NewOffspring2 | AccOffspring], 
		  Mutation).

mutate(<<>>, BitList, _Mutation) ->
    << <<Bit:1>> || Bit <- BitList>>;
mutate(<<X:1, BitString/bitstring>>, AccList, Mutation) ->
    Rand = random:uniform(100),
    if
	Rand > Mutation ->
	    case X of
		1 ->
		    mutate(BitString, [0 | AccList], Mutation);
		0 ->
		    mutate(BitString, [1 | AccList], Mutation)
	    end;
	true ->
	    mutate(BitString, [X | AccList], Mutation)
    end.

generate_params(_, 0) ->
    [];
generate_params(NrOfParams, Population) ->
    [lists:map(fun(_X) -> random:uniform(100) end, 
	       lists:seq(1, NrOfParams)) | generate_params(NrOfParams, Population-1)].

receive_values(0, AccValue) ->
    AccValue;
receive_values(NrOfProcLeft, AccValue) ->
    receive
	{end_of_sim, Nr, Value} ->
	    receive_values(NrOfProcLeft-1, [{Nr,Value} | AccValue])
    after
	10000 ->
	    receive_values(NrOfProcLeft-1, [{0,0} | AccValue])
    end.

encode_params(AllParams) ->
    lists:map(fun(ParamList) when is_list(ParamList) ->
		      list_to_binary(ParamList)
	      end, AllParams).

decode_chromosome(BinaryList) ->
    lists:map(fun(Element) ->
		      adjust_val(Element)
	      end, binary_to_list(BinaryList)).

adjust_val(Value) ->
    Adjusted = Value rem 100,
    Adjusted / 100.
    

    



