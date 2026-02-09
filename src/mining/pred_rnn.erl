%% -*- erlang -*-
%% @doc Lightweight RNN in Pure Erlang
%%
%% Simple recurrent neural network for sequence prediction.
%%
%% @end

-module(pred_rnn).

%% Core API
-export([new_rnn/3, forward_step/2, forward_sequence/2, predict/2]).
-export([get_weights/1, set_weights/2]).

%% Matrix operations
-export([matrix_mult/2, matrix_add/2, tanh_list/1, softmax/1]).

-define(RAND_INIT_SCALE, 0.1).

%%====================================================================
%% Records
%%====================================================================

-record(rnn_cell, {
    input_size :: pos_integer(),
    hidden_size :: pos_integer(),
    output_size :: pos_integer(),
    wxh :: [[float()]],  %% Input-to-hidden weights
    whh :: [[float()]],  %% Hidden-to-hidden weights
    why :: [[float()]],  %% Hidden-to-output weights
    bh :: [float()],     %% Hidden bias
    by :: [float()]      %% Output bias
}).

-type rnn_cell() :: #rnn_cell{}.
-type input_vector() :: [float()].
-type hidden_state() :: [float()].
-type sequence() :: [input_vector()].

-export_type([rnn_cell/0, sequence/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new RNN with random initialization.
-spec new_rnn(pos_integer(), pos_integer(), pos_integer()) -> rnn_cell().
new_rnn(InputSize, HiddenSize, OutputSize) ->
    %% Use default algorithm - no need to manually seed
    Wxh = random_matrix(InputSize, HiddenSize, ?RAND_INIT_SCALE),
    Whh = random_matrix(HiddenSize, HiddenSize, ?RAND_INIT_SCALE),
    Why = random_matrix(HiddenSize, OutputSize, ?RAND_INIT_SCALE),
    Bh = lists:duplicate(HiddenSize, 0.0),
    By = lists:duplicate(OutputSize, 0.0),
    #rnn_cell{
        input_size = InputSize,
        hidden_size = HiddenSize,
        output_size = OutputSize,
        wxh = Wxh,
        whh = Whh,
        why = Why,
        bh = Bh,
        by = By
    }.

%% @doc Single forward step.
-spec forward_step({input_vector(), hidden_state()}, rnn_cell()) ->
    {hidden_state(), [float()]}.
forward_step({Input, Hidden}, #rnn_cell{wxh = Wxh, whh = Whh, why = Why, bh = Bh, by = By}) ->
    %% Input to hidden
    HiddenInput = matrix_mult([Input], Wxh),
    %% Hidden to hidden
    HiddenPrev = matrix_mult([Hidden], Whh),
    %% Combine and activate - flatten the matrices and add element-wise
    HiddenInputFlat = lists:flatten(HiddenInput),
    HiddenPrevFlat = lists:flatten(HiddenPrev),
    HiddenTotal = lists:zipwith(fun(X, Y) -> X + Y end, HiddenInputFlat, HiddenPrevFlat),
    HiddenWithBias = lists:zipwith(fun(H, B) -> H + B end, HiddenTotal, Bh),
    NewHidden = tanh_list(HiddenWithBias),
    %% Output
    OutputInput = matrix_mult([NewHidden], Why),
    OutputFlat = lists:flatten(OutputInput),
    OutputWithBias = lists:zipwith(fun(O, B) -> O + B end, OutputFlat, By),
    Output = softmax(OutputWithBias),
    {NewHidden, Output}.

%% @doc Forward pass through sequence.
-spec forward_sequence(sequence(), rnn_cell()) -> {hidden_state(), [[float()]]}.
forward_sequence(Sequence, RNN) ->
    Hidden = lists:duplicate(RNN#rnn_cell.hidden_size, 0.0),
    forward_sequence(Sequence, Hidden, RNN, []).

forward_sequence([], Hidden, _RNN, Outputs) ->
    {Hidden, lists:reverse(Outputs)};
forward_sequence([Input|Rest], Hidden, RNN, Outputs) ->
    {NewHidden, Output} = forward_step({Input, Hidden}, RNN),
    forward_sequence(Rest, NewHidden, RNN, [Output|Outputs]).

%% @doc Predict next activity from sequence.
-spec predict(sequence(), rnn_cell()) -> [float()].
predict(Sequence, RNN) ->
    {_, [Output|_]} = forward_sequence(Sequence, RNN),
    Output.

%% @doc Get weights.
-spec get_weights(rnn_cell()) -> map().
get_weights(#rnn_cell{wxh = Wxh, whh = Whh, why = Why, bh = Bh, by = By}) ->
    #{
        wxh => Wxh,
        whh => Whh,
        why => Why,
        bh => Bh,
        by => By
    }.

%% @doc Set weights.
-spec set_weights(rnn_cell(), map()) -> rnn_cell().
set_weights(RNN, Weights) ->
    RNN#rnn_cell{
        wxh = maps:get(wxh, Weights, RNN#rnn_cell.wxh),
        whh = maps:get(whh, Weights, RNN#rnn_cell.whh),
        why = maps:get(why, Weights, RNN#rnn_cell.why),
        bh = maps:get(bh, Weights, RNN#rnn_cell.bh),
        by = maps:get(by, Weights, RNN#rnn_cell.by)
    }.

%%====================================================================
%% Matrix Operations
%%====================================================================

%% @doc Matrix multiplication.
-spec matrix_mult([[float()]], [[float()]]) -> [[float()]].
matrix_mult(A, B) ->
    BT = transpose(B),
    [[lists:sum([Aij * Bji || {Aij, Bji} <- lists:zip(ARow, BCol)])
      || BCol <- BT] || ARow <- A].

%% @doc Element-wise matrix addition.
-spec matrix_add([[float()]], [[float()]]) -> [[float()]].
matrix_add(A, B) ->
    lists:zipwith(fun(ARow, BRow) ->
        lists:zipwith(fun(X, Y) -> X + Y end, ARow, BRow)
    end, A, B).

%% @doc Hyperbolic tangent activation.
-spec tanh_list([float()]) -> [float()].
tanh_list(List) ->
    [math:tanh(X) || X <- List].

%% @doc Softmax normalization.
-spec softmax([float()]) -> [float()].
softmax(List) ->
    Max = lists:max(List),
    ExpList = [math:exp(X - Max) || X <- List],
    SumExp = lists:sum(ExpList),
    [X / SumExp || X <- ExpList].

%% @private
random_matrix(Rows, Cols, Scale) ->
    [[(rand:uniform() * 2 - 1) * Scale || _ <- lists:seq(1, Cols)]
     || _ <- lists:seq(1, Rows)].

%% @private
transpose([]) -> [];
transpose([[]|_]) -> [];
transpose(M) ->
    %% Get the number of columns (length of first row)
    NumCols = case M of
        [FirstRow|_] -> length(FirstRow);
        [] -> 0
    end,
    %% Extract each column as a row by getting nth element from each row
    transpose_cols(M, NumCols, 1, []).

%% @private
transpose_cols(_M, NumCols, Col, Acc) when Col > NumCols ->
    lists:reverse(Acc);
transpose_cols(M, NumCols, Col, Acc) ->
    ColVals = [get_nth(Row, Col, 0.0) || Row <- M],
    transpose_cols(M, NumCols, Col + 1, [ColVals | Acc]).

%% @private
get_nth(List, N, Default) ->
    case lists:nth(N, List) of
        Val when Val =:= undefined -> Default;
        Val -> Val
    end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

matrix_mult_test() ->
    A = [[1, 2], [3, 4]],
    B = [[5, 6], [7, 8]],
    Result = matrix_mult(A, B),
    [[19, 22], [43, 50]] = Result,
    ?assertEqual([[19, 22], [43, 50]], Result).

tanh_list_test() ->
    ?assertEqual([math:tanh(1.0), math:tanh(2.0)], tanh_list([1.0, 2.0])).

softmax_test() ->
    Input = [1.0, 2.0, 3.0],
    Output = softmax(Input),
    ?assert(abs(lists:sum(Output) - 1.0) < 0.0001).

new_rnn_test() ->
    RNN = new_rnn(2, 3, 2),
    ?assertEqual(2, RNN#rnn_cell.input_size),
    ?assertEqual(3, RNN#rnn_cell.hidden_size),
    ?assertEqual(2, RNN#rnn_cell.output_size).

forward_step_test() ->
    RNN = new_rnn(2, 2, 1),
    Input = [1.0, 0.5],
    Hidden = [0.0, 0.0],
    {NewHidden, Output} = forward_step({Input, Hidden}, RNN),
    ?assertEqual(2, length(NewHidden)),
    ?assertEqual(1, length(Output)).

forward_sequence_test() ->
    RNN = new_rnn(1, 2, 1),
    Sequence = [[0.5], [0.8]],
    {FinalHidden, Outputs} = forward_sequence(Sequence, RNN),
    ?assertEqual(2, length(FinalHidden)),
    ?assertEqual(2, length(Outputs)).

-endif.
