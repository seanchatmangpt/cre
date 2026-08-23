%% -*- erlang -*-
%% @doc RNN Predictor Tests

-module(rnn_predict_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, Pid} = rnn_predict:start_link(),
    Pid.

cleanup(_Pid) ->
    rnn_predict:stop(_Pid).

%%====================================================================
%% Test Generators
%%====================================================================

rnn_predict_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun predict_next_empty_trace/1,
        fun predict_next_single_activity/1,
        fun predict_next_sequence/1,
        fun predict_sequence_length/1,
        fun reset_state/1,
        fun get_state/1,
        fun get_weights/1,
        fun set_learning_rate/1,
        fun train_step/1
     ]}.

%%====================================================================
%% Test Cases
%%====================================================================

predict_next_empty_trace(_Pid) ->
    fun() ->
        {ok, Result} = rnn_predict:predict_next(_Pid, []),
        ?assert(maps:is_key(activity, Result)),
        ?assert(maps:is_key(probabilities, Result)),
        ?assert(maps:is_key(confidence, Result))
    end end.

predict_next_single_activity(_Pid) ->
    fun() ->
        {ok, Result} = rnn_predict:predict_next(_Pid, [activity_a]),
        ?assert(maps:is_key(activity, Result)),
        ?assert(maps:is_key(confidence, Result)),
        ?assert(maps:get(confidence, Result) >= 0.0),
        ?assert(maps:get(confidence, Result) =< 1.0)
    end end.

predict_next_sequence(_Pid) ->
    fun() ->
        Trace = [a, b, c, d],
        {ok, Result} = rnn_predict:predict_next(_Pid, Trace),
        ?assert(is_map(Result)),
        ?assert(maps:is_key(activity, Result)),
        ?assert(maps:is_key(probabilities, Result))
    end end.

predict_sequence_length(_Pid) ->
    fun() ->
        Trace = [a, b],
        {ok, Results} = rnn_predict:predict_sequence(_Pid, Trace, 3),
        ?assertEqual(3, length(Results)),
        ?assert(lists:all(fun(R) -> is_map(R) end, Results))
    end end.

reset_state(_Pid) ->
    fun() ->
        ok = rnn_predict:reset_state(_Pid),
        ?assertPass(ok)
    end end.

get_state(_Pid) ->
    fun() ->
        {ok, State} = rnn_predict:get_state(_Pid),
        ?assert(maps:is_key(hidden, State)),
        ?assert(maps:is_key(cell, State))
    end end.

get_weights(_Pid) ->
    fun() ->
        {ok, Weights} = rnn_predict:get_weights(_Pid),
        ?assert(is_record(Weights, rnn_predict.weights))
    end end.

set_learning_rate(_Pid) ->
    fun() ->
        ok = rnn_predict:set_learning_rate(_Pid, 0.05),
        ?assertPass(ok)
    end end.

train_step(_Pid) ->
    fun() ->
        Input = [a, b],
        Target = c,
        {ok, Loss} = rnn_predict:train_step(_Pid, Input, Target),
        ?assert(is_float(Loss)),
        ?assert(Loss >= 0.0)
    end end.

%%====================================================================
%% Additional Unit Tests
%%====================================================================

vector_operations_test() ->
    V1 = #rnn_predict.vector{data = [1.0, 2.0, 3.0], size = 3},
    V2 = #rnn_predict.vector{data = [0.5, 0.5, 0.5], size = 3},

    %% Test add_vec
    Sum = rnn_predict:add_vec(V1, V2),
    ?assertEqual([1.5, 2.5, 3.5], Sum#rnn_predict.vector.data),

    %% Test mul_vec
    Product = rnn_predict:mul_vec(V1, V2),
    ?assertEqual([0.5, 1.0, 1.5], Product#rnn_predict.vector.data).

zero_vector_test() ->
    Vec = rnn_predict:zero_vector(5),
    ?assertEqual(5, Vec#rnn_predict.vector.size),
    ?assertEqual([0.0, 0.0, 0.0, 0.0, 0.0], Vec#rnn_predict.vector.data).

random_matrix_test() ->
    Matrix = rnn_predict:random_matrix(3, 4, 0.1),
    ?assertEqual(3, Matrix#rnn_predict.matrix.rows),
    ?assertEqual(4, Matrix#rnn_predict.matrix.cols),
    ?assertEqual(12, length(Matrix#rnn_predict.matrix.data)).

sigmoid_test() ->
    Vec = #rnn_predict.vector{data = [0.0, 1.0, -1.0, 100.0], size = 4},
    Result = rnn_predict:sigmoid(Vec),
    [S1, S2, S3, S4] = Result#rnn_predict.vector.data,
    ?assert(S1 > 0.4 andalso S1 < 0.6),  %% sigmoid(0) ~= 0.5
    ?assert(S2 > 0.5),                  %% sigmoid(1) > 0.5
    ?assert(S3 < 0.5),                  %% sigmoid(-1) < 0.5
    ?assert(S4 > 0.99).                 %% sigmoid(100) ~= 1

tanh_vec_test() ->
    Vec = #rnn_predict.vector{data = [0.0, 1.0, -1.0], size = 3},
    Result = rnn_predict:tanh_vec(Vec),
    ?assertEqual(3, length(Result#rnn_predict.vector.data)).

encode_trace_test() ->
    %% Empty trace
    EmptyVec = rnn_predict:encode_trace([], 10),
    ?assertEqual(10, EmptyVec#rnn_predict.vector.size),
    ?assert(lists:all(fun(X) -> X =:= 0.0 end, EmptyVec#rnn_predict.vector.data)),

    %% Non-empty trace
    Vec = rnn_predict:encode_trace([a, b, c], 10),
    ?assertEqual(10, Vec#rnn_predict.vector.size).

activity_to_index_test() ->
    Idx1 = rnn_predict:activity_to_index(activity_a, 20),
    Idx2 = rnn_predict:activity_to_index(activity_b, 20),
    ?assert(Idx1 >= 1),
    ?assert(Idx1 =< 20),
    ?assert(Idx2 >= 1),
    ?assert(Idx2 =< 20).

softmax_test() ->
    Vec = #rnn_predict.vector{data = [1.0, 2.0, 3.0], size = 3},
    Result = rnn_predict:softmax(Vec),
    ?assertEqual(3, length(Result)),
    Sum = lists:sum(Result),
    ?assert(Sum > 0.99 andalso Sum < 1.01).

cross_entropy_loss_test() ->
    Output = #rnn_predict.vector{data = [0.1, 0.2, 0.7], size = 3},
    Loss1 = rnn_predict:cross_entropy_loss(Output, 3),
    Loss2 = rnn_predict:cross_entropy_loss(Output, 1),
    ?assert(Loss1 < Loss2),  %% High probability at index 3 should have lower loss
    ?assert(Loss1 >= 0.0).

init_weights_test() ->
    Weights = rnn_predict:init_weights(10, 20, 5),
    ?assert(is_record(Weights, rnn_predict.weights)),
    ?assert(is_record(Weights#rnn_predict.weights.wii, rnn_predict.matrix)),
    ?assertEqual(20, (Weights#rnn_predict.weights.wii)#rnn_predict.matrix.rows),
    ?assertEqual(10, (Weights#rnn_predict.weights.wii)#rnn_predict.matrix.cols).

matvec_test() ->
    Matrix = #rnn_predict.matrix{
        data = [1.0, 2.0, 3.0, 4.0],
        rows = 2,
        cols = 2
    },
    Vec = #rnn_predict.vector{data = [1.0, 1.0], size = 2},
    Result = rnn_predict:matvec(Matrix, Vec),
    ?assertEqual(2, length(Result)),
    ?assertEqual(3.0, lists:nth(1, Result)),  %% 1*1 + 2*1
    ?assertEqual(7.0, lists:nth(2, Result)).  %% 3*1 + 4*1

get_top_predictions_test() ->
    Probs = [0.1, 0.6, 0.2, 0.05, 0.05],
    Top = rnn_predict:get_top_predictions(Probs, 3),
    ?assertEqual(3, length(Top)),
    [{_, Prob1} | _] = Top,
    ?assert(Prob1 >= 0.6).

%%--------------------------------------------------------------------
%% Record tests
%%----------------------------------------------------------------====

state_record_test() ->
    HiddenState = rnn_predict:zero_vector(64),
    CellState = rnn_predict:zero_vector(64),
    Weights = rnn_predict:init_weights(50, 64, 20),
    State = #rnn_predict.state{
        agent_id = <<"test">>,
        hidden_state = HiddenState,
        cell_state = CellState,
        weights = Weights,
        learning_rate = 0.01,
        input_size = 50,
        hidden_size = 64,
        output_size = 20,
        sequence_history = queue:new()
    },
    ?assertEqual(<<"test">>, State#rnn_predict.state.agent_id),
    ?assertEqual(0.01, State#rnn_predict.state.learning_rate),
    ?assertEqual(50, State#rnn_predict.state.input_size),
    ?assertEqual(64, State#rnn_predict.state.hidden_size),
    ?assertEqual(20, State#rnn_predict.state.output_size).

vector_record_test() ->
    Vec = #rnn_predict.vector{data = [1.0, 2.0, 3.0], size = 3},
    ?assertEqual([1.0, 2.0, 3.0], Vec#rnn_predict.vector.data),
    ?assertEqual(3, Vec#rnn_predict.vector.size).

matrix_record_test() ->
    Mat = #rnn_predict.matrix{data = [1.0, 2.0, 3.0, 4.0], rows = 2, cols = 2},
    ?assertEqual([1.0, 2.0, 3.0, 4.0], Mat#rnn_predict.matrix.data),
    ?assertEqual(2, Mat#rnn_predict.matrix.rows),
    ?assertEqual(2, Mat#rnn_predict.matrix.cols).

weights_record_test() ->
    Wii = rnn_predict:random_matrix(10, 10, 0.1),
    Bi = rnn_predict:zero_vector(10),
    Weights = #rnn_predict.weights{
        wii = Wii,
        whi = Wii,
        wif = Wii,
        whf = Wii,
        wio = Wii,
        who = Wii,
        wig = Wii,
        whg = Wii,
        bi = Bi,
        bf = Bi,
        bo = Bi,
        bg = Bi
    },
    ?assert(is_record(Weights#rnn_predict.weights.wii, rnn_predict.matrix)),
    ?assert(is_record(Weights#rnn_predict.weights.bi, rnn_predict.vector)).
