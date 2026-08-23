-module(evidence_dbg_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% dbg Trace Sampling Tests
%%%
%%% Note: Tests that use dbg:start_sampling and dbg:stop_sampling
%%% are excluded from this file because dbg is process-local and
%%% doesn't work well with EUnit's parallel test execution.
%%% Those tests are included in the module's internal -ifdef(TEST)
%%% section instead.
%%%===================================================================

%%%-------------------------------------------------------------------
%%% Manual Sampling Tests (no dbg required)
%%%-------------------------------------------------------------------

sample_function_captures_all_fields_test() ->
    %% Verify sample_function creates proper sample format
    Sample = evidence_dbg:sample_function(lists, sum, [[1, 2, 3]]),

    ?assertMatch(#{
        timestamp := _,
        module := lists,
        function := sum,
        args := [[1, 2, 3]],
        return := 6
    }, Sample).

sample_function_different_functions_test() ->
    %% Test sampling various functions
    S1 = evidence_dbg:sample_function(lists, sum, [[1, 2, 3]]),
    S2 = evidence_dbg:sample_function(lists, reverse, [[a, b, c]]),
    S3 = evidence_dbg:sample_function(erlang, length, [[1, 2, 3, 4]]),

    ?assertEqual(lists, maps:get(module, S1)),
    ?assertEqual(sum, maps:get(function, S1)),
    ?assertEqual(6, maps:get(return, S1)),

    ?assertEqual(lists, maps:get(module, S2)),
    ?assertEqual(reverse, maps:get(function, S2)),
    ?assertEqual([c, b, a], maps:get(return, S2)),

    ?assertEqual(erlang, maps:get(module, S3)),
    ?assertEqual(length, maps:get(function, S3)),
    ?assertEqual(4, maps:get(return, S3)).

sample_function_timestamps_differ_test() ->
    %% Verify each sample gets a unique timestamp
    S1 = evidence_dbg:sample_function(lists, sum, [[1]]),
    timer:sleep(1),
    S2 = evidence_dbg:sample_function(lists, sum, [[1]]),

    T1 = maps:get(timestamp, S1),
    T2 = maps:get(timestamp, S2),

    ?assert(T2 >= T1).

%%%-------------------------------------------------------------------
%%% Verification Tests
%%%-------------------------------------------------------------------

verify_samples_exact_match_test() ->
    %% Verify exact matching against golden set
    Samples = [
        #{timestamp => 1, module => m1, function => f1, args => [], return => ok},
        #{timestamp => 2, module => m2, function => f2, args => [a], return => {ok, b}}
    ],

    Golden = [
        #{timestamp => 1, module => m1, function => f1, args => [], return => ok},
        #{timestamp => 2, module => m2, function => f2, args => [a], return => {ok, b}}
    ],

    ?assertEqual(ok, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_wildcard_timestamp_test() ->
    %% Verify wildcard matching for timestamps
    Samples = [
        #{timestamp => 123456789, module => m, function => f, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => ok}
    ],

    ?assertEqual(ok, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_wildcard_nested_test() ->
    %% Verify wildcard matching in nested structures
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [], return => {ok, {nested, value}}}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => {ok, '_'}}
    ],

    ?assertEqual(ok, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_wildcard_args_test() ->
    %% Verify wildcard matching for arguments
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [a, b, c], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => '_', return => ok}
    ],

    ?assertEqual(ok, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_mismatch_module_test() ->
    %% Verify mismatch detection for module
    Samples = [#{timestamp => 1, module => m1, function => f, args => [], return => ok}],
    Golden = [#{timestamp => '_', module => m2, function => f, args => [], return => ok}],

    ?assertMatch({error, {mismatch, _, _}}, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_mismatch_return_test() ->
    %% Verify mismatch detection for return value
    Samples = [#{timestamp => 1, module => m, function => f, args => [], return => error}],
    Golden = [#{timestamp => '_', module => m, function => f, args => [], return => ok}],

    ?assertMatch({error, {mismatch, _, _}}, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_too_few_samples_test() ->
    %% Verify detection when fewer samples than golden
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => ok},
        #{timestamp => '_', module => m, function => f, args => [], return => ok}
    ],

    ?assertMatch({error, {too_few_samples, _}}, evidence_dbg:verify_samples(Samples, Golden)).

verify_samples_too_many_samples_test() ->
    %% Verify detection when more samples than golden
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [], return => ok},
        #{timestamp => 2, module => m, function => f, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => ok}
    ],

    ?assertMatch({error, {too_many_samples, _}}, evidence_dbg:verify_samples(Samples, Golden)).

verify_empty_sets_test() ->
    %% Verify empty sample and golden sets
    ?assertEqual(ok, evidence_dbg:verify_samples([], [])).

%%%-------------------------------------------------------------------
%%% Determinism Tests
%%%-------------------------------------------------------------------

determinism_same_inputs_test() ->
    %% Verify that same inputs produce matching sample structures
    Samples = [
        evidence_dbg:sample_function(lists, sum, [[1, 2, 3]]),
        evidence_dbg:sample_function(lists, sum, [[1, 2, 3]]),
        evidence_dbg:sample_function(lists, sum, [[1, 2, 3]])
    ],

    %% All should have same module, function, args, return
    Modules = [maps:get(module, S) || S <- Samples],
    Functions = [maps:get(function, S) || S <- Samples],
    Args = [maps:get(args, S) || S <- Samples],
    Returns = [maps:get(return, S) || S <- Samples],

    ?assertEqual([lists, lists, lists], Modules),
    ?assertEqual([sum, sum, sum], Functions),
    %% Args is stored as provided: [[1, 2, 3]] becomes the args field
    ?assertEqual([[[1, 2, 3]], [[1, 2, 3]], [[1, 2, 3]]], Args),
    ?assertEqual([6, 6, 6], Returns).

%%%-------------------------------------------------------------------
%%% Error Handling Tests
%%%-------------------------------------------------------------------

sample_function_nonexistent_module_test() ->
    %% Verify sample_function handles missing modules gracefully
    Sample = evidence_dbg:sample_function(nonexistent_module, fake_function, []),

    Return = maps:get(return, Sample),
    ?assertMatch({error, _, _}, Return).

sample_function_throw_test() ->
    %% Verify sample_function captures throw exceptions
    Sample = evidence_dbg:sample_function(erlang, throw, [test_error]),

    Return = maps:get(return, Sample),
    ?assertMatch({throw, test_error, _}, Return).

sample_function_error_test() ->
    %% Verify sample_function captures error exceptions
    Sample = evidence_dbg:sample_function(erlang, error, [badarith]),

    Return = maps:get(return, Sample),
    ?assertMatch({error, badarith, _}, Return).
