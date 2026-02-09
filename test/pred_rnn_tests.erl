%% -*- erlang -*-
-module(pred_rnn_tests).
-include_lib("eunit/include/eunit.hrl").

%% Define the record locally for testing
-record(rnn_cell, {
    input_size :: pos_integer(),
    hidden_size :: pos_integer(),
    output_size :: pos_integer(),
    wxh :: [[float()]],
    whh :: [[float()]],
    why :: [[float()]],
    bh :: [float()],
    by :: [float()]
}).

%% Test matrix_mult
matrix_mult_test() ->
    A = [[1, 2], [3, 4]],
    B = [[5, 6], [7, 8]],
    Result = pred_rnn:matrix_mult(A, B),
    ?assertEqual([[19, 22], [43, 50]], Result).

%% Test tanh_list
tanh_list_test() ->
    ?assertEqual([math:tanh(1.0), math:tanh(2.0)], pred_rnn:tanh_list([1.0, 2.0])).

%% Test softmax
softmax_test() ->
    Input = [1.0, 2.0, 3.0],
    Output = pred_rnn:softmax(Input),
    ?assert(abs(lists:sum(Output) - 1.0) < 0.0001).

%% Test new_rnn
new_rnn_test() ->
    RNN = pred_rnn:new_rnn(2, 3, 2),
    ?assertEqual(2, RNN#rnn_cell.input_size),
    ?assertEqual(3, RNN#rnn_cell.hidden_size),
    ?assertEqual(2, RNN#rnn_cell.output_size).

%% Test forward_step
forward_step_test() ->
    RNN = pred_rnn:new_rnn(2, 2, 1),
    Input = [1.0, 0.5],
    Hidden = [0.0, 0.0],
    {NewHidden, Output} = pred_rnn:forward_step({Input, Hidden}, RNN),
    ?assertEqual(2, length(NewHidden)),
    ?assertEqual(1, length(Output)).

%% Test forward_sequence
forward_sequence_test() ->
    RNN = pred_rnn:new_rnn(1, 2, 1),
    Sequence = [[0.5], [0.8]],
    {FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN),
    ?assertEqual(2, length(FinalHidden)),
    ?assertEqual(2, length(Outputs)).
