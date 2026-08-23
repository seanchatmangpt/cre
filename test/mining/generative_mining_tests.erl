%% -*- erlang -*-
%% @doc Tests for Generative Process Mining (VAE)

-module(generative_mining_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

simple_log() ->
    [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]].

empty_log() ->
    [].

%%====================================================================
%% VAE Creation Tests
%%====================================================================

new_vae_test() ->
    Model = generative_mining:new_vae(5, 8, 10, 32),
    ?assertEqual(5, maps:get(vocab_size, Model)),
    ?assertEqual(8, maps:get(latent_dim, Model)),
    ?assertEqual(false, maps:get(trained, Model)).

new_vae_different_params_test() ->
    Model = generative_mining:new_vae(10, 16, 20, 64),
    ?assertEqual(10, maps:get(vocab_size, Model)),
    ?assertEqual(16, maps:get(latent_dim, Model)).

%%====================================================================
%% Training Tests
%%====================================================================

train_simple_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, Result} = generative_mining:train(Model, Log),
    ?assert(maps:is_key(loss, Result)),
    ?assert(maps:is_key(model, Result)),
    Trained = maps:get(model, Result),
    ?assertEqual(true, maps:get(trained, Trained)).

train_empty_log_test() ->
    Log = empty_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, Result} = generative_mining:train(Model, Log),
    ?assert(is_map(Result)).

train_multiple_times_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, Result1} = generative_mining:train(Model, Log),
    Trained1 = maps:get(model, Result1),
    {ok, Result2} = generative_mining:train(Trained1, Log),
    ?assert(is_map(Result2)).

%%====================================================================
%% Encoding/Decoding Tests
%%====================================================================

encode_untrained_test() ->
    Model = generative_mining:new_vae(5, 8, 10, 32),
    Result = generative_mining:encode([a, b, c], Model),
    ?assertEqual({error, model_not_trained}, Result).

encode_trained_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    {ok, Result} = generative_mining:encode([a, b, c], TrainedModel),
    ?assert(is_list(Result)).

decode_trained_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    {ok, Result} = generative_mining:decode([0.1, 0.2, 0.3], TrainedModel),
    ?assertMatch({ok, _}, Result).

encode_decode_roundtrip_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    {ok, Latent} = generative_mining:encode([a, b, c], TrainedModel),
    {ok, Decoded} = generative_mining:decode(Latent, TrainedModel),
    ?assert(is_list(Decoded)).

%%====================================================================
%% Generation Tests
%%====================================================================

generate_traces_untrained_test() ->
    Model = generative_mining:new_vae(5, 8, 10, 32),
    Result = generative_mining:generate_traces(Model, 5),
    ?assertEqual([], Result).

generate_traces_trained_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    Traces = generative_mining:generate_traces(TrainedModel, 3),
    ?assert(is_list(Traces)).

generate_traces_with_options_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    Traces = generative_mining:generate_traces(TrainedModel, 2, #{temperature => 0.5, max_length => 5}),
    ?assert(is_list(Traces)).

generate_traces_zero_count_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    Traces = generative_mining:generate_traces(TrainedModel, 0),
    ?assertEqual([], Traces).

%%====================================================================
%% Model Persistence Tests
%%====================================================================

save_load_model_test() ->
    Model = generative_mining:new_vae(5, 8, 10, 32),
    TempFile = "/tmp/vae_model_test.term",
    ?assertEqual(ok, generative_mining:save_model(Model, TempFile)),
    ?assertMatch({ok, _}, generative_mining:load_model(TempFile)),
    file:delete(TempFile).

save_trained_model_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    TempFile = "/tmp/vae_trained_model_test.term",
    ?assertEqual(ok, generative_mining:save_model(TrainedModel, TempFile)),
    {ok, Loaded} = generative_mining:load_model(TempFile),
    ?assertEqual(true, maps:get(trained, Loaded)),
    file:delete(TempFile).

load_invalid_file_test() ->
    Result = generative_mining:load_model("/nonexistent/file.term"),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Likelihood Evaluation Tests
%%====================================================================

evaluate_likelihood_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    Result = generative_mining:evaluate_likelihood([a, b, c], TrainedModel),
    ?assertMatch({ok, _}, Result).

evaluate_likelihood_untrained_test() ->
    Model = generative_mining:new_vae(5, 8, 10, 32),
    Result = generative_mining:evaluate_likelihood([a, b, c], Model),
    ?assertMatch({ok, _}, Result).  %% Returns 0.0 for untrained

%%====================================================================
%% Integration Tests
%%====================================================================

full_pipeline_test() ->
    Log = simple_log(),
    Model = generative_mining:new_vae(5, 8, 10, 32),

    %% Train
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),

    %% Encode
    {ok, Latent} = generative_mining:encode([a, b, c], TrainedModel),

    %% Decode
    {ok, Decoded} = generative_mining:decode(Latent, TrainedModel),

    %% Generate
    Generated = generative_mining:generate_traces(TrainedModel, 5),

    ?assert(is_list(Latent)),
    ?assert(is_list(Decoded)),
    ?assert(is_list(Generated)),
    ?assertEqual(5, length(Generated)).

generate_realistic_traces_test() ->
    Log = [[a, b, c], [a, c, b], [a, b, c]],
    Model = generative_mining:new_vae(3, 8, 10, 32),
    {ok, TrainResult} = generative_mining:train(Model, Log),
    TrainedModel = maps:get(model, TrainResult),
    Traces = generative_mining:generate_traces(TrainedModel, 10),

    %% Check that generated traces have reasonable length
    lists:foreach(fun(Trace) ->
        ?assert(length(Trace) =< 10)
    end, Traces).
