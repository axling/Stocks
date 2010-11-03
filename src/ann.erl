%%% File    : ann.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : 
%%% Created : 12 May 2010 by  <ecka@ECKAX>
-module(ann).

-include("neural_net.hrl").

-export([perceptron/4, connect/2]).

sigmoid(X) ->
    1 / (1 + math:exp(-X)).
sigmoid_deriv(X) ->
    math:exp(-X) / (1 + math:exp(-2 * X)).

dot_prod(X, Y) ->
    lists:sum(vector_map(fun(Ex, Ey) -> Ex * Ey end, X, Y)).

feed_forward(Func, Weights, Inputs) ->
    Func(dot_prod(Weights, Inputs)).

perceptron(Weights, Inputs, Sensitivities, NetworkConfig) ->
    random:seed(now()),
    receive
	{learn, Backprop} ->
	    LearningRate = NetworkConfig#ann.learning_rate,
	    %% Calculate the correct sensitivities
	    NewSensitivities = add_sensitivity(Sensitivities, Backprop),
	    OutputValue = feed_forward(fun sigmoid/1, Weights, convert_to_values(Inputs)),
	    DervValue = feed_forward(fun sigmoid_deriv/1, Weights, convert_to_values(Inputs)),
	    Sensitivity = calculate_sensitivity(Backprop, Inputs, NewSensitivities,
						OutputValue, DervValue),

	    %% Adjust all the weights
	    WeightAdjustments = lists:map(fun(Input) -> 
						  LearningRate * Sensitivity * Input 
					  end, convert_to_values(Inputs)),
	    NewWeights = vector_map(fun(W, D) -> W + D end, Weights, WeightAdjustments),

	    %% propagate sensitivities and associated weights back to the previous layer
	    vector_map(fun(Weight, InputPid) ->
			       InputPid ! {learn, {self(), Sensitivity * Weight}}
		       end, NewWeights, convert_to_keys(Inputs)),            
	    perceptron(NewWeights, Inputs, NewSensitivities, NetworkConfig);

	{stimulate, Input} ->
	    %% add Input to Inputs to get NewInputs...
	    NewInputs = replace_input(Inputs, Input),	 
	    %% calculate output of perceptron...
	    Output = feed_forward(fun sigmoid/1, Weights, convert_to_values(NewInputs)),
	    if Sensitivities =/= [] ->
						% My output's connected to at least one perceptron:
                    lists:foreach(fun(OutputPid) -> 
                                          OutputPid ! {stimulate, {self(), Output}}
                                  end,
                                  convert_to_keys(Sensitivities));
               Sensitivities =:= [] ->
		    %% My output's connected to no one:
		    %% Call a trainer here instead and 
		    Trainer = NetworkConfig#ann.trainer,
		    Trainer ! {get_train_val, self(), Output},
		    receive 
			{train_val, Val} ->
			    self() ! {learn, {self(), Val}};
			no_training ->
			    ok			 
		    end
	    end,
	    perceptron(Weights, NewInputs, Sensitivities, NetworkConfig);
	{connect_to_output, ReceiverPid} ->
	    perceptron(Weights, Inputs, 
		       [{ReceiverPid, random:uniform()} | Sensitivities],
		       NetworkConfig);
	{connect_to_input, SenderPid} ->
	    perceptron([random:uniform() | Weights], 
		       [{SenderPid, random:uniform()} | Inputs], 
		       Sensitivities, NetworkConfig);
	{pass, InputValue} ->
	    lists:foreach(fun(OutputPid) ->
				  OutputPid ! {stimulate, {self(), InputValue}}
			  end,
			  convert_to_keys(Sensitivities)),
	    perceptron(Weights, Inputs, Sensitivities, NetworkConfig)
    end.

replace_input(Inputs, Input) ->
    {InputPid, _} = Input,
    lists:keyreplace(InputPid, 1, Inputs, Input).

convert_to_values(Inputs) ->
    lists:map(
      fun({_, Val}) ->
	      Val
      end, Inputs).

convert_to_keys(TupleList) ->
    lists:map(
      fun({Key, _}) ->
	      Key
      end, TupleList).

connect(SenderPid,  ReceiverPid) ->
    SenderPid ! {connect_to_output, ReceiverPid},
    ReceiverPid ! {connect_to_input, SenderPid}.

						% like map, but with two lists instead.
vector_map(Func, X, Y) ->
    vector_map1(Func, X, Y, []).

vector_map1(_Func, [], [], Acc) ->
    lists:reverse(Acc);
vector_map1(Func, [Xh | Xt], [Yh | Yt], Acc) ->
    vector_map1(Func, Xt, Yt, [Func(Xh, Yh) | Acc]).

add_sensitivity(Sensitivities, Backprop) when Sensitivities =/= [] ->
    replace_input(Sensitivities, Backprop);
add_sensitivity(Sensitivities, _Backprop) when Sensitivities =:= [] ->    
    [].

calculate_sensitivity(_Backprop, Inputs, Sensitivities, _OutputValue, _DervValue) 
  when Sensitivities =/= [], Inputs =:= [] -> % When the node is an input node:
    null;
calculate_sensitivity({_, TrainingValue}, Inputs, Sensitivities, OutputValue, DervValue) 
  when Sensitivities =:= [], Inputs =/= [] -> % When the node is an output node:
    (TrainingValue - OutputValue) * DervValue;
calculate_sensitivity(_Backprop, Inputs, Sensitivities, _OutputValue, DervValue) 
  when Sensitivities =/= [], Inputs =/= [] -> % When the node is a hidden node:
    DervValue * lists:foldl(fun(E, T) -> E + T end, 0, convert_to_values(Sensitivities)).
