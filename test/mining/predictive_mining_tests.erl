%% -*- erlang -*-
%% @doc Test suite for predictive_mining module

-module(predictive_mining_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    % Make sure the server is not running
    case whereis(predictive_mining) of
        undefined -> ok;
        Pid when is_pid(Pid) -> 
            predictive_mining:stop(),
            wait_for_process(Pid, 1000)
    end,
    % Start the predictive_mining server
    {ok, _} = predictive_mining:start_link(),
    % Load some test models
    predictive_mining:load_model(<<"model1">>),
    predictive_mining:load_model(<<"model2">>),
    ok.

cleanup() ->
    % Clean up
    case whereis(predictive_mining) of
        undefined -> ok;
        Pid when is_pid(Pid) -> 
            predictive_mining:stop(),
            wait_for_process(Pid, 1000)
    end,
    ok.

wait_for_process(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after Timeout ->
        ok
    end.

%%====================================================================
%% Model Management Tests
%%====================================================================

predictive_mining_load_model_test() ->
    setup(),
    try
        ?assertEqual(ok, predictive_mining:load_model(<<"test_model">>)),
        ?assert(lists:member(<<"test_model">>, predictive_mining:list_loaded_models()))
    after
        cleanup()
    end.

predictive_mining_unload_model_test() ->
    setup(),
    try
        predictive_mining:load_model(<<"model_to_remove">>),
        ?assert(lists:member(<<"model_to_remove">>, predictive_mining:list_loaded_models())),
        ?assertEqual(ok, predictive_mining:unload_model(<<"model_to_remove">>)),
        ?assertNot(lists:member(<<"model_to_remove">>, predictive_mining:list_loaded_models()))
    after
        cleanup()
    end.

predictive_mining_list_loaded_models_test() ->
    setup(),
    try
        % Initial setup loads model1 and model2
        Models = predictive_mining:list_loaded_models(),
        ?assert(lists:member(<<"model1">>, Models)),
        ?assert(lists:member(<<"model2">>, Models)),
        % Add another model
        predictive_mining:load_model(<<"new_model">>),
        Models2 = predictive_mining:list_loaded_models(),
        ?assert(lists:member(<<"new_model">>, Models2))
    after
        cleanup()
    end.

%%====================================================================
%% Prediction Tests
%%====================================================================

predictive_mining_predict_next_activity_empty_test() ->
    setup(),
    try
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"case1">>, []),
        ?assertEqual([], Predictions)
    after
        cleanup()
    end.

predictive_mining_predict_next_activity_atoms_test() ->
    setup(),
    try
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"case2">>, [start, process, review]),
        ?assert(is_list(Predictions)),
        ?assert(length(Predictions) > 0),
        % Check format of predictions
        lists:foreach(fun({Activity, Prob}) ->
            ?assert(is_atom(Activity)),
            ?assert(is_float(Prob)),
            ?assert(Prob >= 0.0 andalso Prob =< 1.0)
        end, Predictions)
    after
        cleanup()
    end.

predictive_mining_predict_next_activity_mixed_test() ->
    setup(),
    try
        % extract_activities filters for atoms
        Trace = [start, 123, "string", process, 456, review],
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"case4">>, Trace),
        ?assert(length(Predictions) =:= 4),  % Default predictions
        ?assert(lists:member({complete, 0.4}, Predictions))
    after
        cleanup()
    end.

predictive_mining_predict_next_activity_probability_test() ->
    setup(),
    try
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"case6">>, [activity]),
        Total = lists:sum([P || {_, P} <- Predictions]),
        % Allow some tolerance for floating point
        ?assert(Total >= 0.99 andalso Total =< 1.01)
    after
        cleanup()
    end.

predictive_mining_predict_remaining_time_empty_test() ->
    setup(),
    try
        {ok, Time} = predictive_mining:predict_remaining_time(<<"case7">>, []),
        ?assertEqual(0, Time)
    after
        cleanup()
    end.

predictive_mining_predict_remaining_time_atoms_test() ->
    setup(),
    try
        {ok, Time} = predictive_mining:predict_remaining_time(<<"case8">>, [start, process]),
        % Should be 2 minutes (2 * 60000 ms)
        ?assertEqual(2 * 60000, Time)
    after
        cleanup()
    end.

predictive_mining_predict_remaining_time_mixed_test() ->
    setup(),
    try
        Trace = [start, 123, "string", process, review],
        {ok, Time} = predictive_mining:predict_remaining_time(<<"case10">>, Trace),
        % extract_activities filters for atoms: [start, process, review] = 3 activities
        ?assertEqual(3 * 60000, Time)
    after
        cleanup()
    end.

predictive_mining_predict_remaining_time_large_test() ->
    setup(),
    try
        Trace = lists:duplicate(50, activity),
        {ok, Time} = predictive_mining:predict_remaining_time(<<"case11">>, Trace),
        ?assertEqual(50 * 60000, Time)
    after
        cleanup()
    end.

predictive_mining_predict_outcome_short_test() ->
    setup(),
    try
        {ok, Outcome, Confidence} = predictive_mining:predict_outcome(<<"case12">>, [start]),
        ?assertEqual(success, Outcome),
        ?assert(is_float(Confidence)),
        ?assert(Confidence >= 0.0 andalso Confidence =< 1.0)
    after
        cleanup()
    end.

predictive_mining_predict_outcome_long_test() ->
    setup(),
    try
        % Create trace with > 10 activities
        Trace = lists:duplicate(15, activity),
        {ok, Outcome, Confidence} = predictive_mining:predict_outcome(<<"case13">>, Trace),
        ?assertEqual(failure, Outcome),
        ?assert(is_float(Confidence))
    after
        cleanup()
    end.

predictive_mining_predict_outcome_medium_test() ->
    setup(),
    try
        Trace = lists:duplicate(10, activity),
        {ok, Outcome, _} = predictive_mining:predict_outcome(<<"case14">>, Trace),
        % At 10 activities, should still be success
        ?assertEqual(success, Outcome)
    after
        cleanup()
    end.

predictive_mining_predict_outcome_empty_test() ->
    setup(),
    try
        {ok, Outcome, Confidence} = predictive_mining:predict_outcome(<<"case15">>, []),
        ?assertEqual(success, Outcome),
        ?assert(is_float(Confidence))
    after
        cleanup()
    end.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

predictive_mining_single_atom_test() ->
    setup(),
    try
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"single">>, [only]),
        ?assert(is_list(Predictions))
    after
        cleanup()
    end.

predictive_mining_duplicate_activities_test() ->
    setup(),
    try
        Trace = [a, b, b, c],
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"dups">>, Trace),
        % Should work with duplicates
        ?assert(length(Predictions) > 0)
    after
        cleanup()
    end.

predictive_mining_special_atoms_test() ->
    setup(),
    try
        Trace = [start, 'end', 'case', 'if'],
        {ok, Predictions} = predictive_mining:predict_next_activity(<<"special">>, Trace),
        ?assert(length(Predictions) > 0)
    after
        cleanup()
    end.

predictive_mining_mode_switching_test() ->
    setup(),
    try
        ?assertEqual(ok, predictive_mining:set_prediction_mode(realtime)),
        ?assertEqual(ok, predictive_mining:set_prediction_mode(batch))
    after
        cleanup()
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

predictive_mining_error_case_id_test() ->
    % These tests should not need the server running
    ?assertException(error, function_clause,
        predictive_mining:predict_next_activity("not_binary", [start])),
    ?assertException(error, function_clause,
        predictive_mining:predict_remaining_time(123, [start])),
    ?assertException(error, function_clause,
        predictive_mining:predict_outcome(atom, [start])),
    ?assertException(error, function_clause,
        predictive_mining:load_model(123)),
    ?assertException(error, function_clause,
        predictive_mining:unload_model("not_binary")),
    ?assertException(error, function_clause,
        predictive_mining:set_prediction_mode(invalid)).

predictive_mining_error_unload_test() ->
    % Should not crash even if model doesn't exist
    setup(),
    try
        ?assertEqual(ok, predictive_mining:unload_model(<<"non_existent">>))
    after
        cleanup()
    end.

%%====================================================================
%% Test Suite
%%====================================================================

predictive_mining_test_() ->
    [
    %% Model Management Tests
    ?_assert(predictive_mining_load_model_test()),
    ?_assert(predictive_mining_unload_model_test()),
    ?_assert(predictive_mining_list_loaded_models_test()),
    
    %% Prediction Tests
    ?_assert(predictive_mining_predict_next_activity_empty_test()),
    ?_assert(predictive_mining_predict_next_activity_atoms_test()),
    ?_assert(predictive_mining_predict_next_activity_mixed_test()),
    ?_assert(predictive_mining_predict_next_activity_probability_test()),
    ?_assert(predictive_mining_predict_remaining_time_empty_test()),
    ?_assert(predictive_mining_predict_remaining_time_atoms_test()),
    ?_assert(predictive_mining_predict_remaining_time_mixed_test()),
    ?_assert(predictive_mining_predict_remaining_time_large_test()),
    ?_assert(predictive_mining_predict_outcome_short_test()),
    ?_assert(predictive_mining_predict_outcome_long_test()),
    ?_assert(predictive_mining_predict_outcome_medium_test()),
    ?_assert(predictive_mining_predict_outcome_empty_test()),
    
    %% Edge Cases Tests
    ?_assert(predictive_mining_single_atom_test()),
    ?_assert(predictive_mining_duplicate_activities_test()),
    ?_assert(predictive_mining_special_atoms_test()),
    ?_assert(predictive_mining_mode_switching_test()),
    
    %% Error Handling Tests
    ?_assert(predictive_mining_error_case_id_test()),
    ?_assert(predictive_mining_error_unload_test())
    ].
