%% -*- erlang -*-
%% @doc EUnit Tests for Predictive Mining Modules
%%
%% Comprehensive test suite covering:
%% - predictive_mining: Main gen_server API for predictions
%% - pred_rnn: RNN forward pass and matrix operations
%% - pred_stats: Markov chains, EMA, linear regression
%% - pred_training: Data collection and training set preparation
%%
%% @end

-module(predictive_mining_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%% Define records locally for testing
%% From predictive_mining.erl
-record(state, {
    mode :: realtime | batch,
    models :: map(),
    markov_models :: map()
}).

-record(prediction, {
    model_id :: binary(),
    model_type :: statistical | markov | ensemble,
    prediction_type :: next_activity | remaining_time | outcome,
    result :: term(),
    confidence :: float(),
    timestamp :: integer()
}).

%% From pred_rnn.erl
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

%% From pred_stats.erl
-record(markov_model, {
    transitions :: map(),
    activities :: [atom()]
}).

-record(ema_model, {
    alpha :: float(),
    values :: [float()],
    last_ema :: float()
}).

-record(linear_model, {
    slope :: float(),
    intercept :: float(),
    r_squared :: float()
}).

%% From pred_training.erl
-record(training_example, {
    input :: [float()],
    target :: float(),
    metadata :: map()
}).

%%====================================================================
%% predictive_mining Tests
%%====================================================================

%% Test: predict_next_activity with valid trace
predict_next_activity_valid_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_001">>,
    Trace = [submit, review, approve],
    Result = predictive_mining:predict_next_activity(CaseId, Trace),
    ?assertMatch({ok, _Predictions}, Result),
    {ok, Predictions} = Result,
    ?assert(is_list(Predictions)),
    stop_server().

%% Test: predict_next_activity with empty trace
predict_next_activity_empty_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_empty">>,
    Trace = [],
    Result = predictive_mining:predict_next_activity(CaseId, Trace),
    ?assertEqual({ok, []}, Result),
    stop_server().

%% Test: predict_next_activity with single activity
predict_next_activity_single_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_single">>,
    Trace = [submit],
    Result = predictive_mining:predict_next_activity(CaseId, Trace),
    ?assertMatch({ok, _Predictions}, Result),
    {ok, Predictions} = Result,
    ?assert(is_list(Predictions)),
    stop_server().

%% Test: predict_next_activity with non-atom elements (filtered)
predict_next_activity_mixed_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_mixed">>,
    Trace = [submit, 123, <<"binary">>, review],
    Result = predictive_mining:predict_next_activity(CaseId, Trace),
    ?assertMatch({ok, _Predictions}, Result),
    stop_server().

%% Test: predict_remaining_time with valid trace
predict_remaining_time_valid_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_001">>,
    Trace = [submit, review, approve],
    Result = predictive_mining:predict_remaining_time(CaseId, Trace),
    ?assertMatch({ok, _Time}, Result),
    {ok, Time} = Result,
    ?assert(is_integer(Time)),
    ?assert(Time > 0),
    stop_server().

%% Test: predict_remaining_time with empty trace
predict_remaining_time_empty_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_empty">>,
    Trace = [],
    Result = predictive_mining:predict_remaining_time(CaseId, Trace),
    ?assertEqual({ok, 0}, Result),
    stop_server().

%% Test: predict_remaining_time scales with trace length
predict_remaining_time_scaling_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ShortTrace = [a, b],
    LongTrace = [a, b, c, d, e, f, g, h],
    {ok, ShortTime} = predictive_mining:predict_remaining_time(<<"case_short">>, ShortTrace),
    {ok, LongTime} = predictive_mining:predict_remaining_time(<<"case_long">>, LongTrace),
    ?assert(LongTime > ShortTime),
    stop_server().

%% Test: predict_outcome with short trace (success)
predict_outcome_short_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_001">>,
    Trace = [submit, approve],
    Result = predictive_mining:predict_outcome(CaseId, Trace),
    ?assertMatch({ok, success, _Confidence}, Result),
    {ok, success, Confidence} = Result,
    ?assert(is_float(Confidence)),
    ?assert(Confidence > 0),
    stop_server().

%% Test: predict_outcome with long trace (failure prediction)
predict_outcome_long_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    CaseId = <<"case_long">>,
    Trace = [a, b, c, d, e, f, g, h, i, j, k],
    Result = predictive_mining:predict_outcome(CaseId, Trace),
    ?assertMatch({ok, failure, _Confidence}, Result),
    {ok, failure, Confidence} = Result,
    ?assert(is_float(Confidence)),
    stop_server().

%% Test: predict_outcome edge case (exactly 10 activities)
predict_outcome_boundary_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    Trace = lists:seq(1, 10),
    Result = predictive_mining:predict_outcome(<<"case_boundary">>, Trace),
    ?assertMatch({ok, success, _Confidence}, Result),
    stop_server().

%% Test: load_model
load_model_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ModelId = <<"model_test_1">>,
    ?assertEqual(ok, predictive_mining:load_model(ModelId)),
    stop_server().

%% Test: unload_model
unload_model_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ModelId = <<"model_test_2">>,
    predictive_mining:load_model(ModelId),
    ?assertEqual(ok, predictive_mining:unload_model(ModelId)),
    stop_server().

%% Test: list_loaded_models empty
list_loaded_models_empty_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    Models = predictive_mining:list_loaded_models(),
    ?assertEqual([], Models),
    stop_server().

%% Test: list_loaded_models with models
list_loaded_models_with_data_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ModelIds = [<<"model_a">>, <<"model_b">>, <<"model_c">>],
    lists:foreach(fun(M) -> predictive_mining:load_model(M) end, ModelIds),
    Models = predictive_mining:list_loaded_models(),
    ?assertEqual(3, length(Models)),
    lists:foreach(fun(M) -> ?assert(lists:member(M, Models)) end, ModelIds),
    stop_server().

%% Test: load then unload then list
load_unload_list_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ModelId = <<"model_temp">>,
    predictive_mining:load_model(ModelId),
    ?assert(lists:member(ModelId, predictive_mining:list_loaded_models())),
    predictive_mining:unload_model(ModelId),
    ?assertNot(lists:member(ModelId, predictive_mining:list_loaded_models())),
    stop_server().

%% Test: set_prediction_mode to realtime
set_mode_realtime_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ?assertEqual(ok, predictive_mining:set_prediction_mode(realtime)),
    stop_server().

%% Test: set_prediction_mode to batch
set_mode_batch_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ?assertEqual(ok, predictive_mining:set_prediction_mode(batch)),
    stop_server().

%% Test: stop API
stop_api_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    ?assertEqual(ok, predictive_mining:stop()).

%%====================================================================
%% pred_rnn Tests
%%====================================================================

%% Test: new_rnn creates valid RNN cell
new_rnn_creates_valid_cell_test() ->
    RNN = pred_rnn:new_rnn(3, 5, 2),
    ?assertEqual(3, element(2, RNN)),  %% input_size is at position 2
    ?assertEqual(5, element(3, RNN)),  %% hidden_size is at position 3
    ?assertEqual(2, element(4, RNN)).  %% output_size is at position 4

%% Test: new_rnn with minimum sizes
new_rnn_minimum_sizes_test() ->
    RNN = pred_rnn:new_rnn(1, 1, 1),
    ?assertEqual(1, element(2, RNN)),
    ?assertEqual(1, element(3, RNN)),
    ?assertEqual(1, element(4, RNN)).

%% Test: new_rnn weight initialization dimensions
new_rnn_weight_dimensions_test() ->
    RNN = pred_rnn:new_rnn(2, 3, 4),
    Wxh = element(5, RNN),  %% wxh is at position 5
    Whh = element(6, RNN),  %% whh is at position 6
    Why = element(7, RNN),  %% why is at position 7
    Bh = element(8, RNN),   %% bh is at position 8
    By = element(9, RNN),   %% by is at position 9
    ?assertEqual(2, length(Wxh)),
    ?assertEqual(3, length(hd(Wxh))),
    ?assertEqual(3, length(Whh)),
    ?assertEqual(3, length(hd(Whh))),
    ?assertEqual(3, length(Why)),
    ?assertEqual(4, length(hd(Why))),
    ?assertEqual(3, length(Bh)),
    ?assertEqual(4, length(By)).

%% Test: forward_step returns correct dimensions
forward_step_dimensions_test() ->
    RNN = pred_rnn:new_rnn(2, 3, 2),
    Input = [1.0, 0.5],
    Hidden = [0.0, 0.0, 0.0],
    {NewHidden, Output} = pred_rnn:forward_step({Input, Hidden}, RNN),
    ?assertEqual(3, length(NewHidden)),
    ?assertEqual(2, length(Output)).

%% Test: forward_step output sums to ~1 (softmax)
forward_step_softmax_test() ->
    RNN = pred_rnn:new_rnn(2, 3, 5),
    Input = [1.0, 0.5],
    Hidden = [0.0, 0.0, 0.0],
    {_NewHidden, Output} = pred_rnn:forward_step({Input, Hidden}, RNN),
    Sum = lists:sum(Output),
    ?assert(abs(Sum - 1.0) < 0.0001).

%% Test: forward_sequence with single element
forward_sequence_single_test() ->
    RNN = pred_rnn:new_rnn(1, 2, 1),
    Sequence = [[0.5]],
    {FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN),
    ?assertEqual(2, length(FinalHidden)),
    ?assertEqual(1, length(Outputs)).

%% Test: forward_sequence with multiple elements
forward_sequence_multiple_test() ->
    RNN = pred_rnn:new_rnn(1, 3, 2),
    Sequence = [[0.1], [0.5], [0.9]],
    {FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN),
    ?assertEqual(3, length(FinalHidden)),
    ?assertEqual(3, length(Outputs)).

%% Test: forward_sequence with empty sequence
forward_sequence_empty_test() ->
    RNN = pred_rnn:new_rnn(1, 2, 1),
    Sequence = [],
    {FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN),
    ?assertEqual(2, length(FinalHidden)),
    ?assertEqual([], Outputs).

%% Test: predict returns valid output
predict_returns_output_test() ->
    RNN = pred_rnn:new_rnn(1, 2, 3),
    Sequence = [[0.5], [0.8]],
    Output = pred_rnn:predict(Sequence, RNN),
    ?assertEqual(3, length(Output)),
    ?assert(abs(lists:sum(Output) - 1.0) < 0.0001).

%% Test: get_weights returns all weight fields
get_weights_complete_test() ->
    RNN = pred_rnn:new_rnn(2, 3, 2),
    Weights = pred_rnn:get_weights(RNN),
    ?assert(maps:is_key(wxh, Weights)),
    ?assert(maps:is_key(whh, Weights)),
    ?assert(maps:is_key(why, Weights)),
    ?assert(maps:is_key(bh, Weights)),
    ?assert(maps:is_key(by, Weights)).

%% Test: set_weights updates RNN
set_weights_updates_rnn_test() ->
    RNN = pred_rnn:new_rnn(2, 2, 1),
    NewWeights = #{
        wxh => [[0.5, 0.5], [0.5, 0.5]],
        whh => [[0.3, 0.3], [0.3, 0.3]],
        why => [[0.1], [0.1]],
        bh => [0.0, 0.0],
        by => [0.0]
    },
    UpdatedRNN = pred_rnn:set_weights(RNN, NewWeights),
    ?assertEqual([[0.5, 0.5], [0.5, 0.5]], element(5, UpdatedRNN)).

%% Test: set_weights with partial weights (keeps original)
set_weights_partial_test() ->
    RNN = pred_rnn:new_rnn(2, 2, 1),
    OriginalWhh = element(6, RNN),
    PartialWeights = #{wxh => [[1.0, 0.0], [0.0, 1.0]]},
    UpdatedRNN = pred_rnn:set_weights(RNN, PartialWeights),
    ?assertEqual([[1.0, 0.0], [0.0, 1.0]], element(5, UpdatedRNN)),
    ?assertEqual(OriginalWhh, element(6, UpdatedRNN)).

%% Test: matrix_mult correctness
matrix_mult_correctness_test() ->
    A = [[1, 2, 3],
         [4, 5, 6]],
    B = [[7, 8],
         [9, 10],
         [11, 12]],
    Result = pred_rnn:matrix_mult(A, B),
    ?assertEqual([[58, 64], [139, 154]], Result).

%% Test: matrix_mult identity
matrix_mult_identity_test() ->
    A = [[1, 2], [3, 4]],
    I = [[1, 0], [0, 1]],
    Result = pred_rnn:matrix_mult(A, I),
    ?assertEqual(A, Result).

%% Test: matrix_mult rectangular
matrix_mult_rectangular_test() ->
    A = [[1, 2, 3]],
    B = [[4], [5], [6]],
    Result = pred_rnn:matrix_mult(A, B),
    ?assertEqual([[32]], Result).

%% Test: matrix_add element-wise
matrix_add_test() ->
    A = [[1, 2], [3, 4]],
    B = [[5, 6], [7, 8]],
    Result = pred_rnn:matrix_add(A, B),
    ?assertEqual([[6, 8], [10, 12]], Result).

%% Test: matrix_add with zeros
matrix_add_zeros_test() ->
    A = [[1, 2], [3, 4]],
    Zeros = [[0, 0], [0, 0]],
    Result = pred_rnn:matrix_add(A, Zeros),
    ?assertEqual(A, Result).

%% Test: tanh_list known values
tanh_list_values_test() ->
    ?assertEqual([0.0, 1.0, -1.0], round_list(pred_rnn:tanh_list([0.0, 10.0, -10.0]))).

%% Test: tanh_list empty
tanh_list_empty_test() ->
    ?assertEqual([], pred_rnn:tanh_list([])).

%% Test: softmax normalizes to 1
softmax_normalization_test() ->
    ?assert(abs(lists:sum(pred_rnn:softmax([1, 2, 3, 4, 5])) - 1.0) < 0.0001).

%% Test: softmax with equal values
softmax_equal_test() ->
    Input = [1.0, 1.0, 1.0, 1.0],
    Output = pred_rnn:softmax(Input),
    ?assert(lists:all(fun(V) -> abs(V - 0.25) < 0.0001 end, Output)).

%% Test: softmax with extreme values (numerical stability)
softmax_extreme_test() ->
    Input = [1000.0, 1001.0, 1002.0],
    Output = pred_rnn:softmax(Input),
    ?assert(abs(lists:sum(Output) - 1.0) < 0.0001).

%% Test: softmax empty
softmax_empty_test() ->
    Output = pred_rnn:softmax([]),
    ?assertEqual([], Output).

%%====================================================================
%% pred_stats Tests
%%====================================================================

%% Test: fit_markov with simple traces
fit_markov_simple_test() ->
    Traces = [[a, b, c], [a, b, d]],
    Model = pred_stats:fit_markov(Traces),
    ?assert(is_map(element(2, Model))),  %% transitions at position 2
    ?assert(is_list(element(3, Model))).  %% activities at position 3

%% Test: fit_markov extracts all activities
fit_markov_activities_test() ->
    Traces = [[a, b, c], [d, e, f]],
    Model = pred_stats:fit_markov(Traces),
    Activities = element(3, Model),
    ?assertEqual(6, length(Activities)),
    lists:foreach(fun(A) -> ?assert(lists:member(A, Activities)) end, [a, b, c, d, e, f]).

%% Test: fit_markov single trace
fit_markov_single_trace_test() ->
    Traces = [[a, b, c]],
    Model = pred_stats:fit_markov(Traces),
    ?assert(is_map(element(2, Model))).

%% Test: fit_markov empty traces
fit_markov_empty_test() ->
    Model = pred_stats:fit_markov([]),
    ?assertEqual([], element(3, Model)).

%% Test: predict_markov with traces
predict_markov_from_traces_test() ->
    Traces = [[a, b, c], [a, b, c, d]],
    Predictions = pred_stats:predict_markov(Traces, c),
    ?assert(is_list(Predictions)).

%% Test: predict_markov with model
predict_markov_with_model_test() ->
    Traces = [[a, b, c], [a, b, d]],
    Model = pred_stats:fit_markov(Traces),
    Predictions = pred_stats:predict_markov(Model, b),
    ?assert(is_list(Predictions)).

%% Test: predict_markov unknown activity
predict_markov_unknown_test() ->
    Traces = [[a, b, c]],
    Predictions = pred_stats:predict_markov(Traces, unknown_activity),
    ?assertEqual([], Predictions).

%% Test: get_transitions
get_transitions_test() ->
    Traces = [[a, b, c]],
    Model = pred_stats:fit_markov(Traces),
    Transitions = pred_stats:get_transitions(Model),
    ?assert(is_map(Transitions)).

%% Test: fit_ema with valid alpha
fit_ema_valid_test() ->
    Values = [1.0, 2.0, 3.0, 4.0, 5.0],
    Model = pred_stats:fit_ema(Values, 0.5),
    ?assertEqual(0.5, element(2, Model)),  %% alpha at position 2
    ?assertEqual(Values, element(3, Model)).  %% values at position 3

%% Test: fit_ema edge alpha values
fit_ema_alpha_boundary_test() ->
    Values = [1.0, 2.0],
    Model1 = pred_stats:fit_ema(Values, 0.01),
    Model2 = pred_stats:fit_ema(Values, 1.0),
    ?assertEqual(0.01, element(2, Model1)),
    ?assertEqual(1.0, element(2, Model2)).

%% Test: predict_ema returns last EMA
predict_ema_test() ->
    Values = [1.0, 2.0, 3.0],
    Model = pred_stats:fit_ema(Values, 0.5),
    Prediction = pred_stats:predict_ema(Model, 1),
    ?assertEqual(element(4, Model), Prediction).  %% last_ema at position 4

%% Test: fit_linear regression
fit_linear_test() ->
    Values = [1.0, 2.0, 3.0, 4.0, 5.0],
    Model = pred_stats:fit_linear(Values),
    ?assert(is_float(element(2, Model))),  %% slope at position 2
    ?assert(is_float(element(3, Model))),  %% intercept at position 3
    ?assert(is_float(element(4, Model))).  %% r_squared at position 4

%% Test: fit_linear with perfect line
fit_linear_perfect_test() ->
    Values = [2.0, 4.0, 6.0, 8.0],
    Model = pred_stats:fit_linear(Values),
    ?assert(abs(element(2, Model) - 2.0) < 0.001),
    ?assert(element(4, Model) > 0.99).

%% Test: fit_linear single value
fit_linear_single_test() ->
    Values = [5.0],
    Model = pred_stats:fit_linear(Values),
    ?assert(is_float(element(2, Model))),
    ?assert(is_float(element(3, Model))).

%% Test: fit_linear constant values
fit_linear_constant_test() ->
    Values = [3.0, 3.0, 3.0, 3.0],
    Model = pred_stats:fit_linear(Values),
    ?assert(is_float(element(2, Model))),
    ?assert(is_float(element(4, Model))).

%% Test: predict_linear
predict_linear_test() ->
    Values = [1.0, 2.0, 3.0, 4.0, 5.0],
    Model = pred_stats:fit_linear(Values),
    Prediction = pred_stats:predict_linear(Model, 6),
    ?assert(is_float(Prediction)).

%%====================================================================
%% pred_training Tests
%%====================================================================

%% Test: extract_sequences from event log
extract_sequences_test() ->
    EventLog = #{
        cases => #{
            <<"case1">> => #{
                events => [
                    #{activity => a, timestamp => 1},
                    #{activity => b, timestamp => 2}
                ]
            },
            <<"case2">> => #{
                events => [
                    #{activity => c, timestamp => 3}
                ]
            }
        }
    },
    Sequences = pred_training:extract_sequences(EventLog),
    ?assertEqual(2, length(Sequences)),
    ?assertEqual([a, b], lists:nth(1, Sequences)),
    ?assertEqual([c], lists:nth(2, Sequences)).

%% Test: extract_sequences from empty log
extract_sequences_empty_test() ->
    EventLog = #{cases => #{}},
    Sequences = pred_training:extract_sequences(EventLog),
    ?assertEqual([], Sequences).

%% Test: extract_sequences missing cases key
extract_sequences_missing_cases_test() ->
    EventLog = #{},
    Sequences = pred_training:extract_sequences(EventLog),
    ?assertEqual([], Sequences).

%% Test: extract_features from event
extract_features_test() ->
    Event = #{
        activity => submit,
        timestamp => 12345,
        resource => <<"user1">>,
        duration => 100
    },
    Features = pred_training:extract_features(Event),
    ?assertEqual(4, length(Features)),
    ?assert(lists:all(fun(F) -> is_float(F) end, Features)).

%% Test: extract_features with missing fields
extract_features_missing_fields_test() ->
    Event = #{},
    Features = pred_training:extract_features(Event),
    ?assertEqual(4, length(Features)).

%% Test: extract_features with binary activity
extract_features_binary_activity_test() ->
    Event = #{activity => <<"submit">>},
    Features = pred_training:extract_features(Event),
    ?assert(is_float(hd(Features))).

%% Test: build_training_set
build_training_set_test() ->
    Sequences = [[a, b, c, d], [a, b, e, f]],
    WindowSize = 2,
    {Examples, Config} = pred_training:build_training_set(Sequences, WindowSize),
    ?assert(is_list(Examples)),
    ?assert(is_map(Config)),
    ?assertEqual(2, maps:get(window_size, Config)).

%% Test: build_training_set with window size 1
build_training_set_window1_test() ->
    Sequences = [[a, b, c]],
    WindowSize = 1,
    {Examples, _Config} = pred_training:build_training_set(Sequences, WindowSize),
    ?assert(length(Examples) >= 2).

%% Test: build_training_set empty sequences
build_training_set_empty_test() ->
    Sequences = [],
    WindowSize = 2,
    {Examples, Config} = pred_training:build_training_set(Sequences, WindowSize),
    ?assertEqual([], Examples),
    ?assertEqual(2, maps:get(window_size, Config)).

%% Test: build_training_set with sequence shorter than window
build_training_set_short_sequence_test() ->
    Sequences = [[a]],
    WindowSize = 3,
    {Examples, _Config} = pred_training:build_training_set(Sequences, WindowSize),
    ?assertEqual([], Examples).

%% Test: split_train_test random
split_train_test_random_test() ->
    %% Create dummy examples
    Examples = lists:map(fun(I) ->
        {training_example, [float(I)], 0.5, #{id => I}}
    end, lists:seq(1, 100)),
    {Train, Test} = pred_training:split_train_test(Examples, 0.7, random),
    ?assertEqual(70, length(Train)),
    ?assertEqual(30, length(Test)).

%% Test: split_train_test sequential
split_train_test_sequential_test() ->
    Examples = lists:map(fun(I) ->
        {training_example, [float(I)], 0.5, #{id => I}}
    end, lists:seq(1, 100)),
    {Train, Test} = pred_training:split_train_test(Examples, 0.8, sequential),
    ?assertEqual(80, length(Train)),
    ?assertEqual(20, length(Test)).

%% Test: split_train_test edge ratios
split_train_test_edge_ratios_test() ->
    Examples = lists:map(fun(I) ->
        {training_example, [float(I)], 0.5, #{id => I}}
    end, lists:seq(1, 10)),
    {Train1, _Test1} = pred_training:split_train_test(Examples, 0.1, sequential),
    ?assertEqual(1, length(Train1)),
    {Train2, _Test2} = pred_training:split_train_test(Examples, 0.9, sequential),
    ?assertEqual(9, length(Train2)).

%%====================================================================
%% pred_training gen_server Tests
%%====================================================================

%% Test: record_training_event
record_training_event_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_001">>,
    ?assertEqual(ok, pred_training:record_training_event(
        WorkflowId,
        activity_a,
        #{resource => <<"user1">>},
        12345
    )),
    gen_server:stop(Pid).

%% Test: get_training_data not found
get_training_data_not_found_test() ->
    {ok, Pid} = pred_training:start_link(),
    Result = pred_training:get_training_data(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result),
    gen_server:stop(Pid).

%% Test: record then get training data
record_then_get_training_data_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_002">>,
    pred_training:record_training_event(
        WorkflowId,
        activity_a,
        #{outcome => success},
        1000
    ),
    {ok, Data} = pred_training:get_training_data(WorkflowId),
    ?assert(length(Data) > 0),
    gen_server:stop(Pid).

%% Test: record_training_event with success outcome
record_event_success_outcome_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_003">>,
    pred_training:record_training_event(
        WorkflowId,
        activity_a,
        #{outcome => success, resource => <<"system">>},
        1000
    ),
    {ok, [Example | _]} = pred_training:get_training_data(WorkflowId),
    ?assertEqual(1.0, element(3, Example)),  %% target at position 3
    gen_server:stop(Pid).

%% Test: record_training_event with failure outcome
record_event_failure_outcome_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_004">>,
    pred_training:record_training_event(
        WorkflowId,
        activity_a,
        #{outcome => failure},
        1000
    ),
    {ok, [Example | _]} = pred_training:get_training_data(WorkflowId),
    ?assertEqual(0.0, element(3, Example)),
    gen_server:stop(Pid).

%% Test: record_training_event with float outcome
record_event_float_outcome_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_005">>,
    pred_training:record_training_event(
        WorkflowId,
        activity_a,
        #{outcome => 0.75},
        1000
    ),
    {ok, [Example | _]} = pred_training:get_training_data(WorkflowId),
    ?assertEqual(0.75, element(3, Example)),
    gen_server:stop(Pid).

%% Test: multiple events accumulate
multiple_events_accumulate_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_006">>,
    lists:foreach(fun(I) ->
        pred_training:record_training_event(
            WorkflowId,
            list_to_atom("activity_" ++ integer_to_list(I)),
            #{},
            I * 1000
        )
    end, lists:seq(1, 5)),
    {ok, Data} = pred_training:get_training_data(WorkflowId),
    ?assertEqual(5, length(Data)),
    gen_server:stop(Pid).

%% Test: metadata includes workflow_id and event_name
record_event_metadata_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_007">>,
    EventName = test_event,
    pred_training:record_training_event(
        WorkflowId,
        EventName,
        #{},
        1234
    ),
    {ok, [Example | _]} = pred_training:get_training_data(WorkflowId),
    Metadata = element(4, Example),  %% metadata at position 4
    ?assertEqual(WorkflowId, maps:get(workflow_id, Metadata)),
    ?assertEqual(EventName, maps:get(event_name, Metadata)),
    ?assertEqual(1234, maps:get(timestamp, Metadata)),
    gen_server:stop(Pid).

%% Test: buffer size limit
buffer_size_limit_test() ->
    {ok, Pid} = pred_training:start_link(),
    WorkflowId = <<"wf_buffer">>,
    %% Add more events than default buffer size
    lists:foreach(fun(I) ->
        pred_training:record_training_event(
            WorkflowId,
            list_to_atom("a" ++ integer_to_list(I)),
            #{},
            I
        )
    end, lists:seq(1, 10100)),
    {ok, Data} = pred_training:get_training_data(WorkflowId),
    %% Should be limited to max_buffer_size (10000)
    ?assert(length(Data) =< 10000),
    gen_server:stop(Pid).

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test: end-to-end prediction flow
end_to_end_prediction_test() ->
    {ok, _PredPid} = predictive_mining:start_link(),
    {ok, _TrainPid} = pred_training:start_link(),

    %% Record some training events
    WorkflowId = <<"wf_e2e">>,
    lists:foreach(fun(I) ->
        pred_training:record_training_event(
            WorkflowId,
            list_to_atom("activity_" ++ integer_to_list(I)),
            #{outcome => success},
            I * 1000
        )
    end, lists:seq(1, 10)),

    %% Make predictions
    CaseId = <<"case_e2e">>,
    Trace = [activity_1, activity_2, activity_3],

    {ok, NextPred} = predictive_mining:predict_next_activity(CaseId, Trace),
    ?assertMatch({ok, _}, {ok, NextPred}),

    {ok, TimePred} = predictive_mining:predict_remaining_time(CaseId, Trace),
    ?assert(is_integer(TimePred)),

    {ok, Outcome, Conf} = predictive_mining:predict_outcome(CaseId, Trace),
    ?assert(is_atom(Outcome)),
    ?assert(is_float(Conf)),

    gen_server:stop(pred_training),
    stop_server().

%% Test: model management workflow
model_management_workflow_test() ->
    {ok, _Pid} = predictive_mining:start_link(),

    %% Initially empty
    ?assertEqual([], predictive_mining:list_loaded_models()),

    %% Load models
    ModelIds = [<<"m1">>, <<"m2">>, <<"m3">>],
    lists:foreach(fun(M) -> predictive_mining:load_model(M) end, ModelIds),

    %% Verify loaded
    Loaded = predictive_mining:list_loaded_models(),
    ?assertEqual(3, length(Loaded)),

    %% Unload one
    predictive_mining:unload_model(<<"m2">>),

    %% Verify state
    Loaded2 = predictive_mining:list_loaded_models(),
    ?assertEqual(2, length(Loaded2)),
    ?assertNot(lists:member(<<"m2">>, Loaded2)),

    stop_server().

%%====================================================================
%% Error Handling and Edge Cases
%%====================================================================

%% Test: predict_remaining_time with single activity
predict_remaining_time_single_activity_test() ->
    {ok, _Pid} = predictive_mining:start_link(),
    {ok, Time} = predictive_mining:predict_remaining_time(<<"case">>, [only_activity]),
    ?assert(Time >= 0),
    stop_server().

%% Test: RNN with various dimensions
rnn_various_dimensions_test_() ->
    Dimensions = [
        {1, 1, 1},
        {1, 10, 1},
        {10, 1, 1},
        {5, 5, 5},
        {2, 3, 4}
    ],
    [{In, Hid, Out,
      fun() ->
          RNN = pred_rnn:new_rnn(In, Hid, Out),
          ?assertEqual(In, element(2, RNN)),
          ?assertEqual(Hid, element(3, RNN)),
          ?assertEqual(Out, element(4, RNN))
      end}
     || {In, Hid, Out} <- Dimensions].

%% Test: Markov model with duplicate transitions
markov_duplicate_transitions_test() ->
    Traces = [[a, b, c], [a, b, c], [a, b, c]],
    Model = pred_stats:fit_markov(Traces),
    Transitions = pred_stats:get_transitions(Model),
    %% a->b should have higher probability due to repetition
    case maps:get(a, Transitions, undefined) of
        undefined -> ok;
        Probs ->
            b_prob = maps:get(b, Probs, 0.0),
            ?assert(b_prob > 0)
    end.

%% Test: EMA with different alpha values
ema_different_alpha_test_() ->
    Alphas = [0.1, 0.5, 0.9],
    Values = [1.0, 2.0, 3.0],
    [{Alpha,
      fun() ->
          Model = pred_stats:fit_ema(Values, Alpha),
          ?assertEqual(Alpha, element(2, Model))
      end}
     || Alpha <- Alphas].

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Stop the predictive_mining server
stop_server() ->
    case whereis(predictive_mining) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%% @doc Round list of floats for comparison
round_list(List) ->
    [round_float(X) || X <- List].

%% @doc Round float to reasonable precision
round_float(X) ->
    round(X * 100000) / 100000.
