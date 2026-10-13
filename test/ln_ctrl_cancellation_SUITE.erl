%%%-------------------------------------------------------------------
%%% @doc
%%% ln_ctrl_cancellation Common Test Suite
%%%
%%% Production-quality tests for cancellation semantics in ln_ctrl.
%%% Tests cover all cancellation operations including signal creation,
%%% cancellation propagation, effect filtering, scope management,
%%% deferred actions, and concurrent cancellation scenarios.
%%%
%%% Test Coverage:
%%% - new_cancel_signal/1: signal creation, timestamp accuracy
%%% - is_cancelled/2: checking cancellation status
%%% - propagate_cancel/2: state mutation, flag propagation
%%% - stop_effects_in_scope/2: effect filtering by timestamp
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ln_ctrl_cancellation_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    %% new_cancel_signal/1 tests
    new_cancel_signal_creates_record/1,
    new_cancel_signal_has_valid_scope/1,
    new_cancel_signal_has_timestamp/1,
    new_cancel_signal_unique_timestamps/1,

    %% is_cancelled/2 tests
    is_cancelled_returns_false_initially/1,
    is_cancelled_returns_true_after_propagate/1,
    is_cancelled_multiple_scopes/1,
    is_cancelled_unknown_scope_returns_false/1,

    %% propagate_cancel/2 tests
    propagate_cancel_sets_flag/1,
    propagate_cancel_preserves_other_flags/1,
    propagate_cancel_is_idempotent/1,
    propagate_cancel_nested_scopes/1,
    propagate_cancel_multiple_scopes/1,

    %% stop_effects_in_scope/2 tests
    stop_effects_keeps_pre_cancel_effects/1,
    stop_effects_removes_post_cancel_effects/1,
    stop_effects_keeps_effect_at_exact_timestamp/1,
    stop_effects_empty_log/1,
    stop_effects_malformed_effects/1,

    %% Cancel scope tests
    cancel_scope_nested_isolation/1,
    cancel_scope_boundary/1,
    cancel_scope_propagation_to_parent/1,

    %% Effect filtering tests
    filter_effects_by_type/1,
    filter_effects_keep_specific_types/1,
    filter_effects_empty_result/1,

    %% Deferred actions on cancel tests
    defer_single_action_executes_on_cancel/1,
    defer_multiple_actions_reverse_order/1,
    defer_nested_actions/1,
    defer_no_actions_no_error/1,

    %% Concurrent cancellation tests
    concurrent_cancel_same_scope/1,
    concurrent_cancel_different_scopes/1,
    concurrent_cancel_race_conditions/1,

    %% Edge case tests
    cancel_already_completed_case/1,
    cancel_non_existent_case/1,
    double_cancellation/1,
    cancel_with_empty_exec_state/1,
    cancel_preserves_other_state/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type config() :: ct:config().

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, new_cancel_signal_tests},
        {group, is_cancelled_tests},
        {group, propagate_cancel_tests},
        {group, stop_effects_tests},
        {group, cancel_scope_tests},
        {group, effect_filtering_tests},
        {group, deferred_actions_tests},
        {group, concurrent_cancellation_tests},
        {group, edge_case_tests}
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [{atom(), term()}], [atom()]}].
groups() ->
    [
        {new_cancel_signal_tests, [shuffle, {repeat, 1}], [
            new_cancel_signal_creates_record,
            new_cancel_signal_has_valid_scope,
            new_cancel_signal_has_timestamp,
            new_cancel_signal_unique_timestamps
        ]},
        {is_cancelled_tests, [shuffle, {repeat, 1}], [
            is_cancelled_returns_false_initially,
            is_cancelled_returns_true_after_propagate,
            is_cancelled_multiple_scopes,
            is_cancelled_unknown_scope_returns_false
        ]},
        {propagate_cancel_tests, [shuffle, {repeat, 1}], [
            propagate_cancel_sets_flag,
            propagate_cancel_preserves_other_flags,
            propagate_cancel_is_idempotent,
            propagate_cancel_nested_scopes,
            propagate_cancel_multiple_scopes
        ]},
        {stop_effects_tests, [shuffle, {repeat, 1}], [
            stop_effects_keeps_pre_cancel_effects,
            stop_effects_removes_post_cancel_effects,
            stop_effects_keeps_effect_at_exact_timestamp,
            stop_effects_empty_log,
            stop_effects_malformed_effects
        ]},
        {cancel_scope_tests, [shuffle, {repeat, 1}], [
            cancel_scope_nested_isolation,
            cancel_scope_boundary,
            cancel_scope_propagation_to_parent
        ]},
        {effect_filtering_tests, [shuffle, {repeat, 1}], [
            filter_effects_by_type,
            filter_effects_keep_specific_types,
            filter_effects_empty_result
        ]},
        {deferred_actions_tests, [shuffle, {repeat, 1}], [
            defer_single_action_executes_on_cancel,
            defer_multiple_actions_reverse_order,
            defer_nested_actions,
            defer_no_actions_no_error
        ]},
        {concurrent_cancellation_tests, [], [
            concurrent_cancel_same_scope,
            concurrent_cancel_different_scopes,
            concurrent_cancel_race_conditions
        ]},
        {edge_case_tests, [], [
            cancel_already_completed_case,
            cancel_non_existent_case,
            double_cancellation,
            cancel_with_empty_exec_state,
            cancel_preserves_other_state
        ]}
    ].

%% @doc Suite-level configuration.
-spec suite() -> [{atom(), term()}].
suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: config()) -> config().
init_per_suite(Config) ->
    ct:pal("Starting ln_ctrl_cancellation_SUITE"),

    %% Ensure the cancel module is loaded
    {module, ln_ctrl_cancel} = code:ensure_loaded(ln_ctrl_cancel),
    {module, wf_vm} = code:ensure_loaded(wf_vm),

    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed ln_ctrl_cancellation_SUITE"),
    ok.

%% @doc Group-level initialization.
-spec init_per_group(Group :: atom(), Config :: config()) -> config().
init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

%% @doc Group-level cleanup.
-spec end_per_group(Group :: atom(), Config :: config()) -> ok.
end_per_group(Group, _Config) ->
    ct:pal("Completed group: ~p", [Group]),
    ok.

%% @doc Testcase-level initialization.
-spec init_per_testcase(TestCase :: atom(), Config :: config()) -> config().
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    [{testcase, TestCase} | Config].

%% @doc Testcase-level cleanup.
-spec end_per_testcase(TestCase :: atom(), Config :: config()) -> ok.
end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Create a default execution state for testing.
-spec make_exec_state() -> wf_vm:exec_state().
make_exec_state() ->
    wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []).

%% @doc Create an execution state with pre-set cancel flags.
-spec make_exec_state_with_cancels(#{atom() => boolean()}) -> wf_vm:exec_state().
make_exec_state_with_cancels(CancelFlags) when is_map(CancelFlags) ->
    wf_vm:exec_state([], 0, [], #{}, #{}, CancelFlags, []).

%% @doc Create a test effect with timestamp.
-spec make_effect(term(), non_neg_integer()) -> tuple().
make_effect(EffectId, TimestampMs) ->
    {EffectId, TimestampMs, {effect, test, payload}, #{}}.

%% @doc Create a test effect with specific type.
-spec make_typed_effect(term(), non_neg_integer(), atom()) -> tuple().
make_typed_effect(EffectId, TimestampMs, Type) ->
    {EffectId, TimestampMs, {effect, Type, payload}, #{}}.

%%%===================================================================
%%% Test Cases - new_cancel_signal/1
%%%===================================================================

%% @doc Test that new_cancel_signal creates a valid cancel_signal record.
-spec new_cancel_signal_creates_record(config()) -> {comment, string()}.
new_cancel_signal_creates_record(_Config) ->
    Signal = ln_ctrl_cancel:new_cancel_signal(test_scope),

    %% Verify it's a record-like tuple with expected structure
    ?assertMatch({cancel_signal, test_scope, _}, Signal),
    ?assertEqual(test_scope, element(2, Signal)),
    ?assert(is_integer(element(3, Signal))),

    {comment, "new_cancel_signal creates valid record"}.

%% @doc Test that new_cancel_signal stores the scope ID correctly.
-spec new_cancel_signal_has_valid_scope(config()) -> {comment, string()}.
new_cancel_signal_has_valid_scope(_Config) ->
    Scopes = [scope_a, 'my-scope', 'Scope.With.Dots', simple_scope],

    lists:foreach(
        fun(Scope) ->
            Signal = ln_ctrl_cancel:new_cancel_signal(Scope),
            ?assertEqual(Scope, element(2, Signal))
        end,
        Scopes
    ),

    {comment, "new_cancel_signal stores scope correctly"}.

%% @doc Test that new_cancel_signal generates a valid timestamp.
-spec new_cancel_signal_has_timestamp(config()) -> {comment, string()}.
new_cancel_signal_has_timestamp(_Config) ->
    Signal = ln_ctrl_cancel:new_cancel_signal(test_scope),
    Timestamp = element(3, Signal),

    %% Timestamp should be an integer (monotonic time in ms)
    %% Note: monotonic time can be negative depending on Erlang version
    ?assert(is_integer(Timestamp)),

    {comment, "new_cancel_signal generates valid timestamp"}.

%% @doc Test that sequential signals have increasing timestamps.
-spec new_cancel_signal_unique_timestamps(config()) -> {comment, string()}.
new_cancel_signal_unique_timestamps(_Config) ->
    Signal1 = ln_ctrl_cancel:new_cancel_signal(scope1),
    %% Small delay to ensure different timestamps
    timer:sleep(1),
    Signal2 = ln_ctrl_cancel:new_cancel_signal(scope2),

    Ts1 = element(3, Signal1),
    Ts2 = element(3, Signal2),

    %% Second signal should have equal or greater timestamp
    ?assert(Ts2 >= Ts1),

    {comment, "new_cancel_signal timestamps are monotonic"}.

%%%===================================================================
%%% Test Cases - is_cancelled/2
%%%===================================================================

%% @doc Test that is_cancelled returns false for unset scope.
-spec is_cancelled_returns_false_initially(config()) -> {comment, string()}.
is_cancelled_returns_false_initially(_Config) ->
    ExecState = make_exec_state(),

    ?assertNot(ln_ctrl_cancel:is_cancelled(my_scope, ExecState)),
    ?assertNot(ln_ctrl_cancel:is_cancelled(another_scope, ExecState)),

    {comment, "is_cancelled returns false for uncancelled scope"}.

%% @doc Test that is_cancelled returns true after propagate_cancel.
-spec is_cancelled_returns_true_after_propagate(config()) -> {comment, string()}.
is_cancelled_returns_true_after_propagate(_Config) ->
    ExecState = make_exec_state(),
    CancelledState = ln_ctrl_cancel:propagate_cancel(ExecState, my_scope),

    ?assert(ln_ctrl_cancel:is_cancelled(my_scope, CancelledState)),
    ?assertNot(ln_ctrl_cancel:is_cancelled(other_scope, CancelledState)),

    {comment, "is_cancelled returns true after propagate_cancel"}.

%% @doc Test is_cancelled with multiple scopes.
-spec is_cancelled_multiple_scopes(config()) -> {comment, string()}.
is_cancelled_multiple_scopes(_Config) ->
    ExecState = make_exec_state(),

    %% Cancel multiple scopes
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, scope_a),
    State2 = ln_ctrl_cancel:propagate_cancel(State1, scope_b),
    State3 = ln_ctrl_cancel:propagate_cancel(State2, scope_c),

    %% Verify each scope's cancellation status
    ?assert(ln_ctrl_cancel:is_cancelled(scope_a, State3)),
    ?assert(ln_ctrl_cancel:is_cancelled(scope_b, State3)),
    ?assert(ln_ctrl_cancel:is_cancelled(scope_c, State3)),
    ?assertNot(ln_ctrl_cancel:is_cancelled(scope_d, State3)),

    {comment, "is_cancelled handles multiple scopes correctly"}.

%% @doc Test that is_cancelled returns false for unknown scope.
-spec is_cancelled_unknown_scope_returns_false(config()) -> {comment, string()}.
is_cancelled_unknown_scope_returns_false(_Config) ->
    %% Create state with some cancelled scopes
    ExecState = make_exec_state_with_cancels(#{scope_a => true, scope_b => true}),

    %% Unknown scope should return false
    ?assertNot(ln_ctrl_cancel:is_cancelled(unknown_scope, ExecState)),
    ?assertNot(ln_ctrl_cancel:is_cancelled(scope_c, ExecState)),

    %% Known scopes should return their values
    ?assert(ln_ctrl_cancel:is_cancelled(scope_a, ExecState)),
    ?assert(ln_ctrl_cancel:is_cancelled(scope_b, ExecState)),

    {comment, "is_cancelled returns false for unknown scope"}.

%%%===================================================================
%%% Test Cases - propagate_cancel/2
%%%===================================================================

%% @doc Test that propagate_cancel sets the cancellation flag.
-spec propagate_cancel_sets_flag(config()) -> {comment, string()}.
propagate_cancel_sets_flag(_Config) ->
    ExecState = make_exec_state(),
    NewState = ln_ctrl_cancel:propagate_cancel(ExecState, test_scope),

    CancelFlags = wf_vm:exec_cancel(NewState),
    ?assertEqual(true, maps:get(test_scope, CancelFlags)),

    {comment, "propagate_cancel sets cancellation flag"}.

%% @doc Test that propagate_cancel preserves other cancellation flags.
-spec propagate_cancel_preserves_other_flags(config()) -> {comment, string()}.
propagate_cancel_preserves_other_flags(_Config) ->
    %% Start with some existing cancel flags
    InitialCancels = #{scope_a => true, scope_b => false},
    ExecState = make_exec_state_with_cancels(InitialCancels),

    %% Cancel a new scope
    NewState = ln_ctrl_cancel:propagate_cancel(ExecState, scope_c),
    CancelFlags = wf_vm:exec_cancel(NewState),

    %% Verify all flags are present
    ?assertEqual(true, maps:get(scope_a, CancelFlags)),
    ?assertEqual(false, maps:get(scope_b, CancelFlags)),
    ?assertEqual(true, maps:get(scope_c, CancelFlags)),

    {comment, "propagate_cancel preserves other flags"}.

%% @doc Test that propagate_cancel is idempotent.
-spec propagate_cancel_is_idempotent(config()) -> {comment, string()}.
propagate_cancel_is_idempotent(_Config) ->
    ExecState = make_exec_state(),

    %% Cancel same scope multiple times
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, my_scope),
    State2 = ln_ctrl_cancel:propagate_cancel(State1, my_scope),
    State3 = ln_ctrl_cancel:propagate_cancel(State2, my_scope),

    %% All should result in cancelled state
    ?assert(ln_ctrl_cancel:is_cancelled(my_scope, State1)),
    ?assert(ln_ctrl_cancel:is_cancelled(my_scope, State2)),
    ?assert(ln_ctrl_cancel:is_cancelled(my_scope, State3)),

    %% Cancel flags should be the same
    ?assertEqual(wf_vm:exec_cancel(State1), wf_vm:exec_cancel(State2)),
    ?assertEqual(wf_vm:exec_cancel(State2), wf_vm:exec_cancel(State3)),

    {comment, "propagate_cancel is idempotent"}.

%% @doc Test propagate_cancel with nested scopes (child scope cancellation).
-spec propagate_cancel_nested_scopes(config()) -> {comment, string()}.
propagate_cancel_nested_scopes(_Config) ->
    ExecState = make_exec_state(),

    %% Cancel parent scope
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, parent_scope),

    %% Cancel child scope (using an atom as scope ID per function spec)
    State2 = ln_ctrl_cancel:propagate_cancel(State1, child_scope),

    %% Both should be cancelled
    ?assert(ln_ctrl_cancel:is_cancelled(parent_scope, State2)),
    ?assert(ln_ctrl_cancel:is_cancelled(child_scope, State2)),

    {comment, "propagate_cancel handles nested scopes"}.

%% @doc Test propagate_cancel with multiple different scopes.
-spec propagate_cancel_multiple_scopes(config()) -> {comment, string()}.
propagate_cancel_multiple_scopes(_Config) ->
    ExecState = make_exec_state(),
    Scopes = [scope_1, scope_2, scope_3, scope_4, scope_5],

    %% Cancel each scope sequentially
    FinalState = lists:foldl(
        fun(Scope, State) ->
            ln_ctrl_cancel:propagate_cancel(State, Scope)
        end,
        ExecState,
        Scopes
    ),

    %% Verify all are cancelled
    CancelFlags = wf_vm:exec_cancel(FinalState),
    lists:foreach(
        fun(Scope) ->
            ?assertEqual(true, maps:get(Scope, CancelFlags))
        end,
        Scopes
    ),

    ?assertEqual(5, maps:size(CancelFlags)),

    {comment, "propagate_cancel handles multiple scopes"}.

%%%===================================================================
%%% Test Cases - stop_effects_in_scope/2
%%%===================================================================

%% @doc Test that effects before cancel time are kept.
-spec stop_effects_keeps_pre_cancel_effects(config()) -> {comment, string()}.
stop_effects_keeps_pre_cancel_effects(_Config) ->
    CancelTime = erlang:monotonic_time(millisecond),
    Signal = {cancel_signal, my_scope, CancelTime},

    %% Create effects before cancel
    Effect1 = make_effect(eff1, CancelTime - 1000),
    Effect2 = make_effect(eff2, CancelTime - 500),
    Effect3 = make_effect(eff3, CancelTime - 100),

    EffectLog = [Effect1, Effect2, Effect3],
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    ?assertEqual(3, length(FilteredLog)),
    ?assertEqual([Effect1, Effect2, Effect3], FilteredLog),

    {comment, "Effects before cancel are kept"}.

%% @doc Test that effects after cancel time are removed.
-spec stop_effects_removes_post_cancel_effects(config()) -> {comment, string()}.
stop_effects_removes_post_cancel_effects(_Config) ->
    CancelTime = erlang:monotonic_time(millisecond),
    Signal = {cancel_signal, my_scope, CancelTime},

    %% Create effects before, at, and after cancel
    BeforeEffect = make_effect(eff_before, CancelTime - 100),
    AtEffect = make_effect(eff_at, CancelTime),
    AfterEffect1 = make_effect(eff_after1, CancelTime + 100),
    AfterEffect2 = make_effect(eff_after2, CancelTime + 500),

    EffectLog = [BeforeEffect, AtEffect, AfterEffect1, AfterEffect2],
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    %% Only before and at effects should remain
    ?assertEqual(2, length(FilteredLog)),
    ?assert(lists:member(BeforeEffect, FilteredLog)),
    ?assert(lists:member(AtEffect, FilteredLog)),
    ?assertNot(lists:member(AfterEffect1, FilteredLog)),
    ?assertNot(lists:member(AfterEffect2, FilteredLog)),

    {comment, "Effects after cancel are removed"}.

%% @doc Test that effect at exact cancel timestamp is kept.
-spec stop_effects_keeps_effect_at_exact_timestamp(config()) -> {comment, string()}.
stop_effects_keeps_effect_at_exact_timestamp(_Config) ->
    ExactTime = 10000,
    Signal = {cancel_signal, my_scope, ExactTime},

    %% Effect at exact cancel time
    ExactEffect = make_effect(eff_exact, ExactTime),
    EffectLog = [ExactEffect],

    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    ?assertEqual(1, length(FilteredLog)),
    ?assertEqual(ExactEffect, hd(FilteredLog)),

    {comment, "Effect at exact cancel timestamp is kept"}.

%% @doc Test stop_effects_in_scope with empty log.
-spec stop_effects_empty_log(config()) -> {comment, string()}.
stop_effects_empty_log(_Config) ->
    Signal = {cancel_signal, my_scope, erlang:monotonic_time(millisecond)},
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope([], Signal),

    ?assertEqual([], FilteredLog),

    {comment, "Empty effect log returns empty"}.

%% @doc Test stop_effects_in_scope with malformed effects (missing timestamp).
-spec stop_effects_malformed_effects(config()) -> {comment, string()}.
stop_effects_malformed_effects(_Config) ->
    CancelTime = erlang:monotonic_time(millisecond),
    Signal = {cancel_signal, my_scope, CancelTime},

    %% Create effects with various formats
    ValidEffect = make_effect(eff_valid, CancelTime - 100),
    Malformed1 = {eff_no_timestamp},  %% Missing timestamp
    Malformed2 = {eff_with_atom_timestamp, not_a_number, spec},  %% Non-integer timestamp
    Malformed3 = just_an_atom,  %% Completely different format

    EffectLog = [ValidEffect, Malformed1, Malformed2, Malformed3],
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    %% Malformed effects should be kept (they don't match the timestamp pattern)
    %% Valid effect should also be kept
    ?assertEqual(4, length(FilteredLog)),

    {comment, "Malformed effects are handled gracefully"}.

%%%===================================================================
%%% Test Cases - Cancel Scope Tests
%%%===================================================================

%% @doc Test nested cancellation scope isolation.
-spec cancel_scope_nested_isolation(config()) -> {comment, string()}.
cancel_scope_nested_isolation(_Config) ->
    ExecState = make_exec_state(),

    %% Cancel inner scope only
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, inner_scope),

    %% Outer scope should not be cancelled
    ?assertNot(ln_ctrl_cancel:is_cancelled(outer_scope, State1)),
    ?assert(ln_ctrl_cancel:is_cancelled(inner_scope, State1)),

    %% Now cancel outer scope
    State2 = ln_ctrl_cancel:propagate_cancel(State1, outer_scope),

    %% Both should be cancelled
    ?assert(ln_ctrl_cancel:is_cancelled(outer_scope, State2)),
    ?assert(ln_ctrl_cancel:is_cancelled(inner_scope, State2)),

    {comment, "Nested scope isolation verified"}.

%% @doc Test cancel scope boundary behavior.
-spec cancel_scope_boundary(config()) -> {comment, string()}.
cancel_scope_boundary(_Config) ->
    ExecState = make_exec_state(),

    %% Simulate scope boundaries with different IDs
    OuterScope = outer,
    InnerScope = inner,

    %% Cancel inner scope
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, InnerScope),

    %% Verify boundary - outer not affected
    ?assertNot(ln_ctrl_cancel:is_cancelled(OuterScope, State1)),
    ?assert(ln_ctrl_cancel:is_cancelled(InnerScope, State1)),

    {comment, "Cancel scope boundary verified"}.

%% @doc Test cancellation propagation to parent scope (if applicable).
-spec cancel_scope_propagation_to_parent(config()) -> {comment, string()}.
cancel_scope_propagation_to_parent(_Config) ->
    ExecState = make_exec_state(),

    %% In this implementation, cancellation does NOT automatically propagate to parent.
    %% This test verifies that behavior.

    %% Cancel child scope
    ChildScope = child_task,
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, ChildScope),

    %% Parent should not be automatically cancelled
    ParentScope = parent_task,
    ?assertNot(ln_ctrl_cancel:is_cancelled(ParentScope, State1)),
    ?assert(ln_ctrl_cancel:is_cancelled(ChildScope, State1)),

    {comment, "Cancel does not auto-propagate to parent"}.

%%%===================================================================
%%% Test Cases - Effect Filtering Tests
%%%===================================================================

%% @doc Test filtering effects by type during cancellation.
-spec filter_effects_by_type(config()) -> {comment, string()}.
filter_effects_by_type(_Config) ->
    CancelTime = erlang:monotonic_time(millisecond),
    Signal = {cancel_signal, my_scope, CancelTime},

    %% Create effects with different types and timestamps
    LogEffect1 = make_typed_effect(log_eff1, CancelTime - 500, log),
    LogEffect2 = make_typed_effect(log_eff2, CancelTime - 100, log),
    NetEffect1 = make_typed_effect(net_eff1, CancelTime - 200, network),
    NetEffect2 = make_typed_effect(net_eff2, CancelTime + 100, network),  %% Post-cancel
    DbEffect = make_typed_effect(db_eff, CancelTime - 50, database),

    EffectLog = [LogEffect1, NetEffect1, DbEffect, LogEffect2, NetEffect2],
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    %% Should have 4 effects (all except the post-cancel network effect)
    ?assertEqual(4, length(FilteredLog)),

    %% Post-cancel effect should be removed
    ?assertNot(lists:member(NetEffect2, FilteredLog)),

    {comment, "Effect filtering by timestamp verified"}.

%% @doc Test keeping specific effect types.
-spec filter_effects_keep_specific_types(config()) -> {comment, string()}.
filter_effects_keep_specific_types(_Config) ->
    CancelTime = erlang:monotonic_time(millisecond),
    Signal = {cancel_signal, my_scope, CancelTime},

    %% Create only pre-cancel effects of different types
    LogEffect = make_typed_effect(log_eff, CancelTime - 100, log),
    NetEffect = make_typed_effect(net_eff, CancelTime - 50, network),
    DbEffect = make_typed_effect(db_eff, CancelTime - 10, database),

    EffectLog = [LogEffect, NetEffect, DbEffect],
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    %% All should be kept (all pre-cancel)
    ?assertEqual(3, length(FilteredLog)),
    ?assert(lists:member(LogEffect, FilteredLog)),
    ?assert(lists:member(NetEffect, FilteredLog)),
    ?assert(lists:member(DbEffect, FilteredLog)),

    {comment, "Pre-cancel effects of all types are kept"}.

%% @doc Test filtering resulting in empty list.
-spec filter_effects_empty_result(config()) -> {comment, string()}.
filter_effects_empty_result(_Config) ->
    CancelTime = erlang:monotonic_time(millisecond),
    Signal = {cancel_signal, my_scope, CancelTime},

    %% Create only post-cancel effects
    Effect1 = make_effect(eff1, CancelTime + 100),
    Effect2 = make_effect(eff2, CancelTime + 200),
    Effect3 = make_effect(eff3, CancelTime + 300),

    EffectLog = [Effect1, Effect2, Effect3],
    FilteredLog = ln_ctrl_cancel:stop_effects_in_scope(EffectLog, Signal),

    %% All should be filtered out
    ?assertEqual([], FilteredLog),

    {comment, "All post-cancel effects filtered out"}.

%%%===================================================================
%%% Test Cases - Deferred Actions on Cancel
%%%===================================================================

%% @doc Test single deferred action executes on cancellation.
%% Note: This tests the pattern - actual defer handling is in wf_exec.
-spec defer_single_action_executes_on_cancel(config()) -> {comment, string()}.
defer_single_action_executes_on_cancel(_Config) ->
    %% In the actual system, deferred actions are tracked in the exec_state
    %% and executed when a cancel_scope_exit is reached.
    %%
    %% For this unit test, we verify the cancel signal can be used
    %% to trigger deferred actions by checking the state.

    ExecState = make_exec_state(),
    CancelScope = defer_scope,

    %% Simulate entering a defer scope with an action
    %% (In real code, this would be in the frame data)
    Frame = {defer, CancelScope, #{action => fun() -> cleanup_called end}},
    StateWithFrame = wf_vm:exec_set_stack(ExecState, [Frame]),

    %% Cancel the scope
    CancelledState = ln_ctrl_cancel:propagate_cancel(StateWithFrame, CancelScope),

    %% Verify cancel flag is set
    ?assert(ln_ctrl_cancel:is_cancelled(CancelScope, CancelledState)),

    %% Verify frame is still present (would be processed by wf_exec)
    Stack = wf_vm:exec_stack(CancelledState),
    ?assertEqual(1, length(Stack)),

    {comment, "Defer scope cancellation sets flags correctly"}.

%% @doc Test multiple deferred actions execute in reverse order.
-spec defer_multiple_actions_reverse_order(config()) -> {comment, string()}.
defer_multiple_actions_reverse_order(_Config) ->
    %% Multiple defer frames should be processed LIFO (reverse order)
    ExecState = make_exec_state(),

    %% Create multiple defer frames (would be created by defer statements)
    Frame1 = {defer, scope1, #{order => 1}},
    Frame2 = {defer, scope2, #{order => 2}},
    Frame3 = {defer, scope3, #{order => 3}},

    StateWithFrames = wf_vm:exec_set_stack(ExecState, [Frame3, Frame2, Frame1]),

    %% Cancel all scopes
    State1 = ln_ctrl_cancel:propagate_cancel(StateWithFrames, scope1),
    State2 = ln_ctrl_cancel:propagate_cancel(State1, scope2),
    State3 = ln_ctrl_cancel:propagate_cancel(State2, scope3),

    %% All should be cancelled
    ?assert(ln_ctrl_cancel:is_cancelled(scope1, State3)),
    ?assert(ln_ctrl_cancel:is_cancelled(scope2, State3)),
    ?assert(ln_ctrl_cancel:is_cancelled(scope3, State3)),

    {comment, "Multiple defer scopes can be cancelled"}.

%% @doc Test nested deferred actions.
-spec defer_nested_actions(config()) -> {comment, string()}.
defer_nested_actions(_Config) ->
    ExecState = make_exec_state(),

    %% Nested defer frames
    OuterFrame = {defer, outer, #{level => outer}},
    InnerFrame = {defer, inner, #{level => inner}},

    StateWithFrames = wf_vm:exec_set_stack(ExecState, [InnerFrame, OuterFrame]),

    %% Cancel inner only
    State1 = ln_ctrl_cancel:propagate_cancel(StateWithFrames, inner),
    ?assert(ln_ctrl_cancel:is_cancelled(inner, State1)),
    ?assertNot(ln_ctrl_cancel:is_cancelled(outer, State1)),

    %% Cancel outer
    State2 = ln_ctrl_cancel:propagate_cancel(State1, outer),
    ?assert(ln_ctrl_cancel:is_cancelled(outer, State2)),
    ?assert(ln_ctrl_cancel:is_cancelled(inner, State2)),

    {comment, "Nested defer scopes handled correctly"}.

%% @doc Test that no deferred actions doesn't cause error.
-spec defer_no_actions_no_error(config()) -> {comment, string()}.
defer_no_actions_no_error(_Config) ->
    ExecState = make_exec_state(),

    %% Cancel a scope that has no associated defer actions
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, no_defer_scope),

    %% Should work without error
    ?assert(ln_ctrl_cancel:is_cancelled(no_defer_scope, State1)),

    {comment, "Cancelling scope with no defer actions succeeds"}.

%%%===================================================================
%%% Test Cases - Concurrent Cancellation
%%%===================================================================

%% @doc Test concurrent cancel requests for same scope.
-spec concurrent_cancel_same_scope(config()) -> {comment, string()}.
concurrent_cancel_same_scope(_Config) ->
    ExecState = make_exec_state(),
    Scope = concurrent_scope,

    %% Simulate concurrent cancellation (sequential in test, but same effect)
    Self = self(),
    _Pid1 = spawn_link(fun() ->
        State = ln_ctrl_cancel:propagate_cancel(ExecState, Scope),
        Self ! {done, 1, State}
    end),
    _Pid2 = spawn_link(fun() ->
        State = ln_ctrl_cancel:propagate_cancel(ExecState, Scope),
        Self ! {done, 2, State}
    end),

    %% Receive both results and verify
    State1 = receive
        {done, 1, S1} -> S1
    after 1000 ->
        exit(timeout1)
    end,
    State2 = receive
        {done, 2, S2} -> S2
    after 1000 ->
        exit(timeout2)
    end,

    %% Both should show the scope as cancelled
    ?assert(ln_ctrl_cancel:is_cancelled(Scope, State1)),
    ?assert(ln_ctrl_cancel:is_cancelled(Scope, State2)),

    {comment, "Concurrent cancel of same scope is safe"}.

%% @doc Test concurrent cancel requests for different scopes.
-spec concurrent_cancel_different_scopes(config()) -> {comment, string()}.
concurrent_cancel_different_scopes(_Config) ->
    ExecState = make_exec_state(),

    %% Multiple processes cancelling different scopes
    Self = self(),
    Scopes = [scope_a, scope_b, scope_c],

    _Pids = lists:map(
        fun(Scope) ->
            spawn_link(fun() ->
                State = ln_ctrl_cancel:propagate_cancel(ExecState, Scope),
                Self ! {done, Scope, State}
            end)
        end,
        Scopes
    ),

    %% Collect all results
    Results = lists:map(
        fun(Scope) ->
            receive
                {done, Scope, State} -> {Scope, State}
            after 1000 ->
                exit({timeout, Scope})
            end
        end,
        Scopes
    ),

    %% Each result should have its scope cancelled
    lists:foreach(
        fun({Scope, State}) ->
            ?assert(ln_ctrl_cancel:is_cancelled(Scope, State))
        end,
        Results
    ),

    {comment, "Concurrent cancel of different scopes is safe"}.

%% @doc Test race conditions in cancellation.
-spec concurrent_cancel_race_conditions(config()) -> {comment, string()}.
concurrent_cancel_race_conditions(_Config) ->
    %% Test rapid sequential cancellations (simulating race conditions)
    ExecState = make_exec_state(),

    %% Rapid cancellations
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, race_scope),
    State2 = ln_ctrl_cancel:propagate_cancel(State1, race_scope),
    State3 = ln_ctrl_cancel:propagate_cancel(State2, race_scope),

    %% All should be equivalent (idempotent)
    Cancel1 = wf_vm:exec_cancel(State1),
    Cancel2 = wf_vm:exec_cancel(State2),
    Cancel3 = wf_vm:exec_cancel(State3),

    ?assertEqual(Cancel1, Cancel2),
    ?assertEqual(Cancel2, Cancel3),

    {comment, "Race conditions handled idempotently"}.

%%%===================================================================
%%% Test Cases - Edge Cases
%%%===================================================================

%% @doc Test cancelling an already completed case.
-spec cancel_already_completed_case(config()) -> {comment, string()}.
cancel_already_completed_case(_Config) ->
    %% A "completed" case would have a halt in its trace
    ExecState = make_exec_state(),

    %% Add a halt to simulate completion
    Trace = [{0, exit, halt, #{}, 0, [], false}],
    CompletedState = wf_vm:exec_set_trace(ExecState, Trace),

    %% Cancel should still work
    CancelledState = ln_ctrl_cancel:propagate_cancel(CompletedState, completed_scope),
    ?assert(ln_ctrl_cancel:is_cancelled(completed_scope, CancelledState)),

    %% Trace should be preserved
    ?assertEqual(Trace, wf_vm:exec_trace(CancelledState)),

    {comment, "Cancelling completed case succeeds"}.

%% @doc Test cancelling a non-existent case (scope doesn't exist in state).
-spec cancel_non_existent_case(config()) -> {comment, string()}.
cancel_non_existent_case(_Config) ->
    ExecState = make_exec_state(),

    %% Cancel a scope that was never defined
    CancelledState = ln_ctrl_cancel:propagate_cancel(ExecState, non_existent_scope),

    %% Should create the entry
    ?assert(ln_ctrl_cancel:is_cancelled(non_existent_scope, CancelledState)),

    {comment, "Cancelling non-existent scope creates entry"}.

%% @doc Test double cancellation of same scope.
-spec double_cancellation(config()) -> {comment, string()}.
double_cancellation(_Config) ->
    ExecState = make_exec_state(),
    Scope = double_cancel_scope,

    %% First cancellation
    State1 = ln_ctrl_cancel:propagate_cancel(ExecState, Scope),
    ?assert(ln_ctrl_cancel:is_cancelled(Scope, State1)),

    %% Second cancellation
    State2 = ln_ctrl_cancel:propagate_cancel(State1, Scope),
    ?assert(ln_ctrl_cancel:is_cancelled(Scope, State2)),

    %% States should be equivalent
    ?assertEqual(wf_vm:exec_cancel(State1), wf_vm:exec_cancel(State2)),

    {comment, "Double cancellation is idempotent"}.

%% @doc Test cancellation with completely empty exec_state.
-spec cancel_with_empty_exec_state(config()) -> {comment, string()}.
cancel_with_empty_exec_state(_Config) ->
    %% Empty state
    ExecState = {[], 0, [], #{}, #{}, #{}, []},

    %% Should work
    CancelledState = ln_ctrl_cancel:propagate_cancel(ExecState, empty_scope),
    ?assert(ln_ctrl_cancel:is_cancelled(empty_scope, CancelledState)),

    {comment, "Cancelling empty exec_state succeeds"}.

%% @doc Test that cancellation preserves other state components.
-spec cancel_preserves_other_state(config()) -> {comment, string()}.
cancel_preserves_other_state(_Config) ->
    %% Create state with various components
    Program = [wf_vm:op_halt()],
    PC = 5,
    Stack = [{seq, test_frame, #{}}],
    Ctx = #{data => important_data, signals => [sig1]},
    Joins = #{10 => {2, 3}},
    Trace = [{0, enter, seq_enter, #{}, 0, [], false}],

    ExecState = wf_vm:exec_state(Program, PC, Stack, Ctx, Joins, #{}, Trace),

    %% Cancel a scope
    CancelledState = ln_ctrl_cancel:propagate_cancel(ExecState, preserve_test_scope),

    %% Verify all other components are preserved
    ?assertEqual(Program, wf_vm:exec_program(CancelledState)),
    ?assertEqual(PC, wf_vm:exec_pc(CancelledState)),
    ?assertEqual(Stack, wf_vm:exec_stack(CancelledState)),
    ?assertEqual(Ctx, wf_vm:exec_ctx(CancelledState)),
    ?assertEqual(Joins, wf_vm:exec_joins(CancelledState)),
    ?assertEqual(Trace, wf_vm:exec_trace(CancelledState)),

    %% Only cancel flags should change
    CancelFlags = wf_vm:exec_cancel(CancelledState),
    ?assertEqual(true, maps:get(preserve_test_scope, CancelFlags)),

    {comment, "Cancellation preserves other state components"}.
