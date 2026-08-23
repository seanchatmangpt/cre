%%%-------------------------------------------------------------------
%%% @doc f5_proof_budget test suite
%%%
%%% Tests budget proof verification ensuring execution within budget.
%%% @end
%%%-------------------------------------------------------------------
-module(f5_proof_budget_test).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Data
%%%-------------------------------------------------------------------

%% Helper to create a budget spec
budget_spec(MaxEffects, MaxSteps, MaxTime) ->
    #{
        max_effects => MaxEffects,
        max_steps => MaxSteps,
        max_time => MaxTime
    }.

%% Helper to create an execution result
execution_result(Effects, Steps, Time, Normal, ExitReason) ->
    #{
        effects_executed => Effects,
        reductions_executed => Steps,
        duration_ms => Time,
        terminated_normally => Normal,
        exit_reason => ExitReason
    }.

%% Helper to create unlimited budget spec
unlimited_budget() ->
    budget_spec(unlimited, unlimited, unlimited).

%%%-------------------------------------------------------------------
%%% Verification Tests
%%%-------------------------------------------------------------------

%% @doc Test successful verification - execution within all limits
verify_budget_within_all_limits_test() ->
    BudgetSpec = budget_spec(100, 10000, 5000),
    ExecutionResult = execution_result(50, 5000, 2500, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{
        proof_type := budget,
        exceeded := false,
        within_limits := true,
        hard_stop_triggered := false
    }}, Result).

%% @doc Test verification failure - effects exceeded
verify_budget_effects_exceeded_test() ->
    BudgetSpec = budget_spec(10, unlimited, unlimited),
    ExecutionResult = execution_result(15, 100, 100, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{
        proof_type := budget,
        exceeded := true,
        within_limits := false,
        actual := #{effects := 15}
    }}, Result).

%% @doc Test verification failure - steps exceeded
verify_budget_steps_exceeded_test() ->
    BudgetSpec = budget_spec(unlimited, 1000, unlimited),
    ExecutionResult = execution_result(5, 1500, 100, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{
        proof_type := budget,
        exceeded := true,
        within_limits := false,
        actual := #{steps := 1500}
    }}, Result).

%% @doc Test verification failure - time exceeded
verify_budget_time_exceeded_test() ->
    BudgetSpec = budget_spec(unlimited, unlimited, 1000),
    ExecutionResult = execution_result(5, 100, 1500, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{
        proof_type := budget,
        exceeded := true,
        within_limits := false,
        actual := #{time_ms := 1500}
    }}, Result).

%% @doc Test verification with unlimited budget always passes
verify_budget_unlimited_always_passes_test() ->
    BudgetSpec = unlimited_budget(),
    ExecutionResult = execution_result(999999, 999999, 999999, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{exceeded := false, within_limits := true}}, Result).

%% @doc Test verification with exact limit values (boundary test)
verify_budget_at_exact_limits_test() ->
    BudgetSpec = budget_spec(100, 5000, 2000),
    %% Exactly at limits - should pass (not exceeded)
    ExecutionResult = execution_result(100, 5000, 2000, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{exceeded := false, within_limits := true}}, Result).

%% @doc Test verification with one over limit
verify_budget_one_over_limit_test() ->
    BudgetSpec = budget_spec(100, 5000, 2000),
    ExecutionResult = execution_result(101, 5001, 2001, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{exceeded := true}}, Result).

%%%-------------------------------------------------------------------
%%% Hard Stop Detection Tests
%%%-------------------------------------------------------------------

%% @doc Test hard stop detection with budget_exceeded exit reason
hard_stop_budget_exceeded_reason_test() ->
    BudgetSpec = budget_spec(10, unlimited, unlimited),
    ExitReason = {budget_exceeded, [{effects_exceeded, 15, 10}]},
    ExecutionResult = execution_result(15, 100, 100, false, ExitReason),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{hard_stop_triggered := true}}, Result).

%% @doc Test hard stop detection with simple budget_exceeded atom
hard_stop_simple_budget_exceeded_test() ->
    BudgetSpec = unlimited_budget(),
    ExecutionResult = execution_result(5, 100, 100, false, budget_exceeded),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{hard_stop_triggered := true}}, Result).

%% @doc Test hard stop detection with timeout
hard_stop_timeout_reason_test() ->
    BudgetSpec = unlimited_budget(),
    %% Use a 2-tuple timeout format which is recognized as hard stop
    ExecutionResult = execution_result(5, 100, 100, false, {timeout, call}),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{hard_stop_triggered := true}}, Result).

%% @doc Test hard stop detection with killed reason
hard_stop_killed_reason_test() ->
    BudgetSpec = unlimited_budget(),
    ExecutionResult = execution_result(5, 100, 100, false, killed),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{hard_stop_triggered := true}}, Result).

%% @doc Test normal termination is not hard stop
hard_stop_normal_termination_test() ->
    BudgetSpec = unlimited_budget(),
    ExecutionResult = execution_result(5, 100, 100, true, normal),

    Result = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertMatch({ok, #{hard_stop_triggered := false}}, Result).

%% @doc Test abnormal but not hard stop reasons
hard_stop_other_reasons_test() ->
    BudgetSpec = unlimited_budget(),
    OtherReasons = [
        {badarith, []},
        {{badmatch, 5}, []},
        noproc,
        {badarg, []}
    ],

    Results = [f5_proof_budget:verify_budget(BudgetSpec,
        execution_result(5, 100, 100, false, Reason), 1000)
        || Reason <- OtherReasons],

    %% None should trigger hard stop
    [?assertMatch({ok, #{hard_stop_triggered := false}}, R) || R <- Results].

%%%-------------------------------------------------------------------
%%% Enforce Budget Tests
%%%-------------------------------------------------------------------

%% @doc Test enforce_budget creates initial state
enforce_budget_creates_state_test() ->
    BudgetSpec = budget_spec(100, 10000, 5000),
    StartTime = 1000,

    State = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    ?assertEqual(BudgetSpec, maps_get(spec, State)),
    ?assertEqual(0, maps_get(effects_used, State)),
    ?assertEqual(0, maps_get(steps_used, State)),
    ?assertEqual(StartTime, maps_get(start_time, State)),
    ?assertEqual(false, maps_get(exceeded, State)),
    ?assertEqual(false, maps_get(hard_stop_triggered, State)).

%% @doc Test enforce_budget with unlimited budget
enforce_budget_unlimited_test() ->
    BudgetSpec = unlimited_budget(),
    StartTime = erlang:monotonic_time(millisecond),

    State = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    ?assertEqual(unlimited, maps_get(max_effects, maps_get(spec, State))),
    ?assertEqual(unlimited, maps_get(max_steps, maps_get(spec, State))),
    ?assertEqual(unlimited, maps_get(max_time, maps_get(spec, State))).

%%%-------------------------------------------------------------------
%%% Track Effect Tests
%%%-------------------------------------------------------------------

%% @doc Test track_effect increments counter
track_effect_increments_test() ->
    BudgetSpec = budget_spec(100, unlimited, unlimited),
    StartTime = 1000,
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    State1 = f5_proof_budget:track_effect(State0, 1),
    State2 = f5_proof_budget:track_effect(State1, 1),
    State3 = f5_proof_budget:track_effect(State2, 5),  %% Add 5 at once

    ?assertEqual(1, maps_get(effects_used, State1)),
    ?assertEqual(2, maps_get(effects_used, State2)),
    ?assertEqual(7, maps_get(effects_used, State3)).

%% @doc Test track_effect with cost > 1
track_effect_with_cost_test() ->
    BudgetSpec = unlimited_budget(),
    State = f5_proof_budget:enforce_budget(BudgetSpec, 1000),

    State1 = f5_proof_budget:track_effect(State, 10),

    ?assertEqual(10, maps_get(effects_used, State1)).

%%%-------------------------------------------------------------------
%%% Track Step Tests
%%%-------------------------------------------------------------------

%% @doc Test track_step increments counter
track_step_increments_test() ->
    BudgetSpec = unlimited_budget(),
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, 1000),

    State1 = f5_proof_budget:track_step(State0),
    State2 = f5_proof_budget:track_step(State1),
    State3 = f5_proof_budget:track_step(State2),

    ?assertEqual(1, maps_get(steps_used, State1)),
    ?assertEqual(2, maps_get(steps_used, State2)),
    ?assertEqual(3, maps_get(steps_used, State3)).

%%%-------------------------------------------------------------------
%%% Track Time Tests
%%%-------------------------------------------------------------------

%% @doc Test track_time updates elapsed time
track_time_updates_elapsed_test() ->
    BudgetSpec = unlimited_budget(),
    StartTime = erlang:monotonic_time(millisecond) - 100,  %% Started 100ms ago
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    State1 = f5_proof_budget:track_time(State0),

    Elapsed = maps_get(elapsed_ms, State1),
    ?assert(Elapsed >= 100),  %% At least 100ms elapsed
    ?assert(Elapsed < 200).   %% Less than 200ms (allowing for test time)

%%%-------------------------------------------------------------------
%%% Check Budget Exceeded Tests
%%%-------------------------------------------------------------------

%% @doc Test check_budget_exceeded returns updated state when within limits
check_budget_exceeded_within_limits_test() ->
    BudgetSpec = budget_spec(100, 10000, 5000),
    StartTime = erlang:monotonic_time(millisecond),
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),
    State1 = f5_proof_budget:track_effect(State0, 50),
    State2 = f5_proof_budget:track_step(State1),

    UpdatedState = f5_proof_budget:check_budget_exceeded(State2),

    ?assertEqual(false, maps_get(exceeded, UpdatedState)),
    ?assertEqual(false, maps_get(hard_stop_triggered, UpdatedState)).

%% @doc Test check_budget_exceeded exits when effects exceeded
check_budget_exceeded_effects_exits_test() ->
    BudgetSpec = budget_spec(5, unlimited, unlimited),
    StartTime = erlang:monotonic_time(millisecond),
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),
    State1 = f5_proof_budget:track_effect(State0, 10),  %% Over limit

    ?assertExit({budget_exceeded, _}, f5_proof_budget:check_budget_exceeded(State1)).

%% @doc Test check_budget_exceeded exits when steps exceeded
check_budget_exceeded_steps_exits_test() ->
    BudgetSpec = budget_spec(unlimited, 100, unlimited),
    StartTime = erlang:monotonic_time(millisecond),
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    %% Simulate many steps
    StateN = lists:foldl(fun(_, S) ->
        f5_proof_budget:track_step(S)
    end, State0, lists:seq(1, 101)),

    ?assertExit({budget_exceeded, _}, f5_proof_budget:check_budget_exceeded(StateN)).

%% @doc Test check_budget_exceeded exits when time exceeded
check_budget_exceeded_time_exits_test() ->
    %% Very short time budget
    BudgetSpec = budget_spec(unlimited, unlimited, 1),
    StartTime = erlang:monotonic_time(millisecond) - 10,  %% Started 10ms ago
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    ?assertExit({budget_exceeded, _}, f5_proof_budget:check_budget_exceeded(State0)).

%% @doc Test check_budget_exceeded updates elapsed_ms
check_budget_exceeded_updates_elapsed_test() ->
    BudgetSpec = unlimited_budget(),
    StartTime = erlang:monotonic_time(millisecond) - 50,
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    State1 = f5_proof_budget:check_budget_exceeded(State0),

    Elapsed = maps_get(elapsed_ms, State1),
    ?assert(Elapsed >= 50).

%%%-------------------------------------------------------------------
%%% Generate Proof Tests
%%%-------------------------------------------------------------------

%% @doc Test generate_budget_proof creates valid JSON artifact
generate_budget_proof_valid_test() ->
    BudgetSpec = budget_spec(100, 10000, 5000),
    ExecutionResult = execution_result(50, 5000, 2500, true, normal),
    Metadata = #{case_id => <<"test_case">>},

    Result = f5_proof_budget:generate_budget_proof(BudgetSpec, ExecutionResult, Metadata),

    ?assertMatch({ok, #{
        proof_type := budget,
        status := verified,
        exceeded := false,
        within_limits := true,
        hard_stop_triggered := false,
        proof_hash := <<_:512>>  %% Hex-encoded = 64 bytes = 512 bits
    }}, Result).

%% @doc Test generate_budget_proof with exceeded budget
generate_budget_proof_exceeded_test() ->
    BudgetSpec = budget_spec(10, unlimited, unlimited),
    ExecutionResult = execution_result(15, 100, 100, false, {budget_exceeded, []}),
    Metadata = #{},

    {ok, Proof} = f5_proof_budget:generate_budget_proof(
        BudgetSpec, ExecutionResult, Metadata
    ),

    ?assertEqual(true, maps_get(exceeded, Proof)),
    ?assertEqual(true, maps_get(hard_stop_triggered, Proof)),
    ?assertNotEqual(<<"verified">>, maps_get(status, Proof)).

%% @doc Test generate_budget_proof includes hex-encoded hash
generate_budget_proof_hex_hash_test() ->
    BudgetSpec = unlimited_budget(),
    ExecutionResult = execution_result(5, 100, 100, true, normal),
    Metadata = #{},

    {ok, Proof} = f5_proof_budget:generate_budget_proof(
        BudgetSpec, ExecutionResult, Metadata
    ),

    HashHex = maps_get(proof_hash, Proof),
    ?assertEqual(64, byte_size(HashHex)).  %% 32 bytes = 64 hex chars

%%%-------------------------------------------------------------------
%%% Compute Proof Hash Tests
%%%-------------------------------------------------------------------

%% @doc Test compute_proof_hash generates consistent hashes
compute_proof_hash_consistent_test() ->
    Hash1 = f5_proof_budget:compute_proof_hash(100, 5000, 2500),
    Hash2 = f5_proof_budget:compute_proof_hash(100, 5000, 2500),

    ?assertEqual(Hash1, Hash2),
    ?assertEqual(32, byte_size(Hash1)).

%% @doc Test compute_proof_hash varies with inputs
compute_proof_hash_varies_test() ->
    Hash1 = f5_proof_budget:compute_proof_hash(100, 5000, 2500),
    Hash2 = f5_proof_budget:compute_proof_hash(101, 5000, 2500),  %% Different effects
    Hash3 = f5_proof_budget:compute_proof_hash(100, 5001, 2500),  %% Different steps
    Hash4 = f5_proof_budget:compute_proof_hash(100, 5000, 2501),  %% Different time

    ?assertNotEqual(Hash1, Hash2),
    ?assertNotEqual(Hash1, Hash3),
    ?assertNotEqual(Hash1, Hash4).

%%%-------------------------------------------------------------------
%%% Evidence Sources Tests
%%%-------------------------------------------------------------------

%% @doc Test evidence sources with all data present
evidence_sources_all_present_test() ->
    BudgetSpec = unlimited_budget(),
    ExecutionResult = execution_result(5, 100, 100, false, budget_exceeded),

    {ok, Proof} = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    Sources = maps_get(evidence_sources, Proof),

    ?assert(lists:member(effects, Sources)),
    ?assert(lists:member(reductions, Sources)),
    ?assert(lists:member(timing, Sources)),
    ?assert(lists:member(exit_signal, Sources)).

%% @doc Test evidence sources with minimal data
evidence_sources_minimal_test() ->
    BudgetSpec = unlimited_budget(),
    %% Minimal result with only effects
    ExecutionResult = #{effects_executed => 5},

    {ok, Proof} = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    Sources = maps_get(evidence_sources, Proof),

    ?assert(lists:member(effects, Sources)),
    ?assertNot(lists:member(reductions, Sources)).

%%%-------------------------------------------------------------------
%%% Complex Scenario Tests
%%%-------------------------------------------------------------------

%% @doc Test full budget enforcement workflow
full_enforcement_workflow_test() ->
    %% Setup budget with strict limits
    BudgetSpec = budget_spec(10, 100, 1000),
    StartTime = erlang:monotonic_time(millisecond),

    %% Simulate execution within budget
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),
    State1 = f5_proof_budget:track_effect(State0, 1),
    State2 = lists:foldl(fun(_, S) ->
        f5_proof_budget:track_step(S)
    end, State1, lists:seq(1, 50)),

    %% Check budget (should pass)
    State3 = f5_proof_budget:check_budget_exceeded(State2),

    ?assertEqual(false, maps_get(exceeded, State3)),
    ?assertEqual(1, maps_get(effects_used, State3)),
    ?assertEqual(50, maps_get(steps_used, State3)).

%% @doc Test budget exceeded during execution
budget_exceeded_during_execution_test() ->
    %% Setup budget with strict effects limit
    BudgetSpec = budget_spec(5, unlimited, unlimited),
    StartTime = erlang:monotonic_time(millisecond),

    %% Simulate execution that exceeds budget
    State0 = f5_proof_budget:enforce_budget(BudgetSpec, StartTime),

    %% Stay within limit
    State1 = f5_proof_budget:track_effect(State0, 3),
    ?assertEqual(false, maps_get(exceeded,
        f5_proof_budget:check_budget_exceeded(State1))),

    %% Exceed limit - should cause exit on next check
    State2 = f5_proof_budget:track_effect(State1, 3),
    ?assertExit({budget_exceeded, _}, f5_proof_budget:check_budget_exceeded(State2)).

%% @doc Test multi-limit exceeded detection
multi_limit_exceeded_test() ->
    BudgetSpec = budget_spec(10, 100, 1000),
    ExecutionResult = execution_result(15, 150, 1500, false, {budget_exceeded, []}),

    {ok, Proof} = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    ?assertEqual(true, maps_get(exceeded, Proof)),

    %% Check exceeded details
    ExceededDetails = maps_get(exceeded_details, Proof),
    ?assertEqual(true, maps_get(effects_exceeded, ExceededDetails)),
    ?assertEqual(true, maps_get(steps_exceeded, ExceededDetails)),
    ?assertEqual(true, maps_get(time_exceeded, ExceededDetails)).

%% @doc Test proof hash is deterministic
proof_hash_deterministic_test() ->
    BudgetSpec = budget_spec(100, 10000, 5000),
    ExecutionResult = execution_result(50, 5000, 2500, true, normal),

    {ok, Proof1} = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),
    {ok, Proof2} = f5_proof_budget:verify_budget(BudgetSpec, ExecutionResult, 1000),

    Hash1 = maps_get(proof_hash, Proof1),
    Hash2 = maps_get(proof_hash, Proof2),

    ?assertEqual(Hash1, Hash2).

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

maps_get(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> undefined
    end.
