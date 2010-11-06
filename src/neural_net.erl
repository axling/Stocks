%%%-------------------------------------------------------------------
%%% File    : neural_net.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%%
%%% Created : 15 Oct 2010 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(neural_net).

-include("neural_net.hrl").

-export([five_day_ann/3]).

five_day_ann(TrainingInput, TestInput, AnnConfig) 
  when length(TrainingInput) > 7, length(TestInput) > 7 ->
    NetConfig = #ann{learning_rate=AnnConfig#ann_config.learning_rate,
		     trainer=self()},
    LayerOnePids = [spawn(ann, perceptron, 
			  [[], [], [], NetConfig]) ||
		       _X <- lists:seq(1, 5)],
    LayerTwoPids = [spawn(ann, perceptron, 
			  [[], [], [], NetConfig]) ||
		       _X <- lists:seq(1, AnnConfig#ann_config.hidden_layer)],
    ClassLayerPids = [spawn(ann, perceptron, 
			    [[], [], [], NetConfig]) ||
			 _X <- lists:seq(1, 2)],
    lists:foreach(
      fun(PerceptronPid) ->
	      [ann:connect(PerceptronPid, HiddenLayerPid) ||
		 HiddenLayerPid <-LayerTwoPids]
      end, LayerOnePids),
    lists:foreach(
      fun(PerceptronPid) ->
	      [ann:connect(PerceptronPid, ClassLayerPid) ||
		  ClassLayerPid <- ClassLayerPids]
      end, LayerTwoPids),    
    [train(TrainingInput, LayerOnePids, ClassLayerPids) || 
	_X <- lists:seq(1, AnnConfig#ann_config.iterations)],
    test(TestInput, LayerOnePids, ClassLayerPids, []).

train(TrainingInput, LayerOnePids, [Output1, Output2]=ClassLayerPids)
  when length(TrainingInput) > 7 ->
    Inputs = lists:sublist(TrainingInput, 5),
    [Pid ! {pass, Input} || {Input, Pid} <- lists:zip(Inputs, LayerOnePids)],
    TrainValue1 =
	case lists:nth(7,TrainingInput) > lists:nth(5, TrainingInput) of
	    true ->
		1;
	    false ->
		0
	end,		
    TrainValue2 =
	case (lists:nth(7,TrainingInput) - 
	      lists:nth(5, TrainingInput))/lists:nth(5,TrainingInput) > 0.02 of
	    true ->
		1;
	    false ->
		0
	end,		
    receive
	{get_train_val, Output1, _} ->
	    Output1 ! {train_val, TrainValue1}
    end,    
    receive
	{get_train_val, Output2, _} ->
	    Output2 ! {train_val, TrainValue2}
    end,
    train(lists:nthtail(1,TrainingInput), LayerOnePids, ClassLayerPids);
train(_, _, _) ->
    ok.

test(TestInput, LayerOnePids, [Output1, Output2]=ClassLayerPids, Results) 
  when length(TestInput) > 7 ->
    Inputs = lists:sublist(TestInput, 5),
    [Pid ! {pass, Input} || {Input, Pid} <- lists:zip(Inputs, LayerOnePids)],
    TestValue1 =
	case lists:nth(7,TestInput) > lists:nth(5, TestInput) of
	    true ->
		1;
	    false ->
		0
	end,		
    TestValue2 =
	case (lists:nth(7,TestInput) - 
	      lists:nth(5, TestInput))/lists:nth(5,TestInput) > 0.02 of
	    true ->
		1;
	    false ->
		0
	end,		
    Res1 = 
	receive
	    {get_train_val, Output1, COutput1} ->
		Output1 ! no_training,
		{COutput1, TestValue1}
	end,
    Res2 = 
	receive
	    {get_train_val, Output2, COutput2} ->
		Output2 ! no_training,
		{COutput2, TestValue2}
	end,
    test(lists:nthtail(1, TestInput), LayerOnePids, ClassLayerPids,
	 [{Res1, Res2} | Results]);
test(_, _, _, Res) ->
    {calculate_efficiency(lists:reverse(Res), 0, 0, length(Res)), Res}.

calculate_efficiency([], Corr1, Corr2, Total) ->
    {(Corr1/Total)*100, (Corr2/Total)*100};
calculate_efficiency([{{Output1, TestOutput1},{Output2, TestOutput2}}
		      | Rest], Corr1, Corr2, Total) ->
    True1 = 
	case round(Output1) == TestOutput1 of
	    true ->
		1;
	    false ->
		0
	end,
    True2 = 
	case round(Output2) == TestOutput2 of
	    true ->
		1;
	    false ->
		0
	end,
    calculate_efficiency(Rest, Corr1+True1, Corr2+True2, Total).
