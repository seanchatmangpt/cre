%% -*- erlang -*-
%% @doc Tests for Transformer-based Prediction

-module(transformer_predict_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
;; Test Fixtures
%%====================================================================

simple_log() ->
    [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]].

empty_log() ->
    [].

complex_log() ->
    [
        [a, b, c, d, e],
        [a, c, b, d, e],
        [a, b, c, e, d],
        [a, b, d, c, e]
    ].

%%====================================================================
;; Model Creation Tests
%%====================================================================

new_model_test() ->
    Model = transformer_predict:new_model(10, 64, #{}),
    ?assert(maps:is_key(config, Model)),
    ?assert(maps:is_key(embeddings, Model)),
    ?assertEqual(false, maps:get(trained, Model)).

new_model_with_options_test() ->
    Model = transformer_predict:new_model(10, 64, #{num_heads => 8, num_layers => 4}),
    Config = maps:get(config, Model),
    ?assertEqual(8, maps:get(num_heads, Config)),
    ?assertEqual(4, maps:get(num_layers, Config)).

new_model_different_dims_test() ->
    Model = transformer_predict:new_model(20, 128, #{d_ff => 256}),
    Config = maps:get(config, Model),
    ?assertEqual(20, maps:get(vocab_size, Config)),
    ?assertEqual(128, maps:get(d_model, Config)),
    ?assertEqual(256, maps:get(d_ff, Config)).

%%====================================================================
;; Training Tests
%%====================================================================

train_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    ?assertEqual(true, maps:get(trained, TrainedModel)).

train_empty_log_test() ->
    Log = empty_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    ?assertEqual(true, maps:get(trained, TrainedModel)).

train_complex_log_test() ->
    Log = complex_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    ?assert(maps:is_key(vocab, TrainedModel)).

train_multiple_times_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, Model1} = transformer_predict:train(Model, Log),
    {ok, Model2} = transformer_predict:train(Model1, Log),
    ?assertEqual(true, maps:get(trained, Model2)).

%%====================================================================
;; Prediction Tests
%%====================================================================

predict_attention_untrained_test() ->
    Model = transformer_predict:new_model(10, 64, #{}),
    Result = transformer_predict:predict_attention([a, b], Model),
    ?assertEqual({error, model_not_trained}, Result).

predict_attention_trained_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    Result = transformer_predict:predict_attention([a, b, c], TrainedModel),
    ?assertMatch({ok, _}, Result).

predict_attention_result_structure_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Result} = transformer_predict:predict_attention([a, b], TrainedModel),
    ?assert(maps:is_key(activity, Result)),
    ?assert(maps:is_key(confidence, Result)),
    ?assert(maps:is_key(probabilities, Result)),
    ?assert(maps:is_key(attention_weights, Result)).

predict_next_activity_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    Result = transformer_predict:predict_next_activity([a, b], TrainedModel),
    ?assertMatch({ok, _}, Result).

predict_next_activity_untrained_test() ->
    Model = transformer_predict:new_model(10, 64, #{}),
    Result = transformer_predict:predict_next_activity([a, b], Model),
    ?assertEqual({error, model_not_trained}, Result).

predict_attention_long_sequence_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{max_seq_len => 5}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Result} = transformer_predict:predict_attention([a, b, c, d, e, f], TrainedModel),
    ?assertMatch({ok, _}, Result).

%%====================================================================
;; Attention Weights Tests
%%====================================================================

get_attention_weights_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Weights} = transformer_predict:get_attention_weights([a, b, c], TrainedModel),
    ?assert(is_list(Weights)).

get_attention_weights_dimensions_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Weights} = transformer_predict:get_attention_weights([a, b], TrainedModel),
    ?assertEqual(2, length(Weights)),
    lists:foreach(fun(Row) ->
        ?assertEqual(2, length(Row))
    end, Weights).

compute_attention_test() ->
    Input = [[0.1, 0.2], [0.3, 0.4]],
    Model = transformer_predict:new_model(2, 2, #{}),
    Attention = transformer_predict:compute_attention(Input, Model),
    ?assert(is_list(Attention)),
    ?assertEqual(2, length(Attention)).

compute_attention_square_matrix_test() ->
    Input = [[0.1, 0.2, 0.3], [0.4, 0.5, 0.6], [0.7, 0.8, 0.9]],
    Model = transformer_predict:new_model(3, 3, #{}),
    Attention = transformer_predict:compute_attention(Input, Model),
    ?assertEqual(3, length(Attention)),
    lists:foreach(fun(Row) ->
        ?assertEqual(3, length(Row))
    end, Attention).

%%====================================================================
;; Model Persistence Tests
;;====================================================================

save_load_model_test() ->
    Model = transformer_predict:new_model(5, 32, #{}),
    TempFile = "/tmp/transformer_model_test.term",
    ?assertEqual(ok, transformer_predict:save_model(Model, TempFile)),
    ?assertMatch({ok, _}, transformer_predict:load_model(TempFile)),
    file:delete(TempFile).

save_trained_model_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(5, 32, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    TempFile = "/tmp/transformer_trained_model_test.term",
    ?assertEqual(ok, transformer_predict:save_model(TrainedModel, TempFile)),
    {ok, Loaded} = transformer_predict:load_model(TempFile),
    ?assertEqual(true, maps:get(trained, Loaded)),
    file:delete(TempFile).

load_invalid_file_test() ->
    Result = transformer_predict:load_model("/nonexistent/file.term"),
    ?assertMatch({error, _}, Result).

%%====================================================================
;; Positional Encoding Tests
;;====================================================================

generate_positional_embeddings_test() ->
    Embeddings = transformer_predict:generate_positional_embeddings(10, 8),
    ?assertEqual(10, length(Embeddings)),
    ?assertEqual(8, length(hd(Embeddings))).

generate_positional_embeddings_single_test() ->
    Embeddings = transformer_predict:generate_positional_embeddings(1, 4),
    ?assertEqual(1, length(Embeddings)),
    ?assertEqual(4, length(hd(Embeddings))).

generate_positional_embeddings_large_test() ->
    Embeddings = transformer_predict:generate_positional_embeddings(100, 64),
    ?assertEqual(100, length(Embeddings)),
    ?assertEqual(64, length(hd(Embeddings))).

%%====================================================================
;; Confidence Tests
;;====================================================================

get_confidence_found_test() ->
    Probs = [{a, 0.7}, {b, 0.2}, {c, 0.1}],
    Conf = transformer_predict:get_confidence(a, Probs),
    ?assertEqual(0.7, Conf).

get_confidence_not_found_test() ->
    Probs = [{a, 0.7}, {b, 0.2}, {c, 0.1}],
    Conf = transformer_predict:get_confidence(d, Probs),
    ?assertEqual(0.0, Conf).

get_confidence_empty_test() ->
    Conf = transformer_predict:get_confidence(a, []),
    ?assertEqual(0.0, Conf).

%%====================================================================
;; Integration Tests
;;====================================================================

predict_full_pipeline_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Result} = transformer_predict:predict_attention([a, b, c], TrainedModel),
    ?assert(maps:is_key(activity, Result)),
    ?assert(maps:is_key(confidence, Result)),
    ?assert(maps:is_key(probabilities, Result)),
    ?assert(maps:is_key(attention_weights, Result)),
    ?assert(maps:get(confidence, Result) >= 0.0),
    ?assert(maps:get(confidence, Result) =< 1.0).

train_predict_multiple_test() ->
    Log = complex_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),

    %% Multiple predictions
    {ok, Result1} = transformer_predict:predict_attention([a, b], TrainedModel),
    {ok, Result2} = transformer_predict:predict_attention([a, c], TrainedModel),
    {ok, Result3} = transformer_predict:predict_attention([b, c], TrainedModel),

    ?assert(is_map(Result1)),
    ?assert(is_map(Result2)),
    ?assert(is_map(Result3)).

predict_with_different_sequence_lengths_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),

    Sequences = [[a], [a, b], [a, b, c], [a, b, c, d]],
    lists:foreach(fun(Seq) ->
        Result = transformer_predict:predict_attention(Seq, TrainedModel),
        ?assertMatch({ok, _}, Result)
    end, Sequences).

%%====================================================================
;; Edge Cases Tests
;;====================================================================

train_with_single_activity_test() ->
    Log = [[a]],
    Model = transformer_predict:new_model(1, 32, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    ?assertEqual(true, maps:get(trained, TrainedModel)).

predict_with_empty_trace_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    Result = transformer_predict:predict_attention([], TrainedModel),
    ?assertMatch({ok, _}, Result).

predict_with_unknown_activity_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    Result = transformer_predict:predict_attention([unknown_activity, a, b], TrainedModel),
    ?assertMatch({ok, _}, Result).

%%====================================================================
;; Attention Visualization Tests
;;====================================================================

attention_weights_sum_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Weights} = transformer_predict:get_attention_weights([a, b, c], TrainedModel),
    lists:foreach(fun(Row) ->
        Sum = lists:sum(Row),
        ?assert(Sum >= 0.0)
    end, Weights).

attention_weights_range_test() ->
    Log = simple_log(),
    Model = transformer_predict:new_model(10, 64, #{}),
    {ok, TrainedModel} = transformer_predict:train(Model, Log),
    {ok, Weights} = transformer_predict:get_attention_weights([a, b], TrainedModel),
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Val) ->
            ?assert(Val >= 0.0),
            ?assert(Val =< 1.0)
        end, Row)
    end, Weights).
