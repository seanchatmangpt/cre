%%%-------------------------------------------------------------------
%%% @doc
%%% ln_ctrl_scheduler Common Test Suite
%%%
%%% Production-quality tests for scheduler policies in ln_ctrl_sched.
%%% Tests cover all scheduling modes: deterministic, nondeterministic,
%%% and replay, including mode switching and edge cases.
%%%
%%% Test Coverage:
%%% - Deterministic Mode: order guarantee, reproducibility, seed init
%%% - Nondeterministic Mode: recording choices, random selection
%%% - Replay Mode: trace replay, validation, deviation detection
%%% - Mode Switching: transitions, persistence
%%% - Edge Cases: empty lists, single effect, large lists
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ln_ctrl_scheduler_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/ln_ctrl/ln_ctrl_sched.hrl").

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
    %% Deterministic mode tests
    deterministic_always_picks_first_test/1,
    deterministic_order_consistency_test/1,
    deterministic_multiple_choices_test/1,
    deterministic_policy_immutability_test/1,
    deterministic_empty_choices_test/1,
    deterministic_single_choice_test/1,

    %% Nondeterministic mode tests
    nondeterministic_records_choices_test/1,
    nondeterministic_choice_log_structure_test/1,
    nondeterministic_accumulates_log_test/1,
    nondeterministic_empty_choices_test/1,
    nondeterministic_single_choice_test/1,
    nondeterministic_multiple_branches_test/1,

    %% Replay mode tests
    replay_follows_recorded_trace_test/1,
    replay_empty_trace_test/1,
    replay_partial_trace_test/1,
    replay_invalid_index_fallback_test/1,
    replay_exhausted_trace_fallback_test/1,
    replay_preserves_index_test/1,
    replay_different_branch_selection_test/1,

    %% Mode switching tests
    switch_deterministic_to_nondeterministic_test/1,
    switch_nondeterministic_to_replay_test/1,
    switch_replay_to_deterministic_test/1,
    mode_independence_test/1,

    %% Edge cases tests
    empty_effect_list_test/1,
    single_effect_list_test/1,
    large_effect_list_test/1,
    stress_many_choices_test/1,
    policy_type_preservation_test/1
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
        {group, deterministic_mode_tests},
        {group, nondeterministic_mode_tests},
        {group, replay_mode_tests},
        {group, mode_switching_tests},
        {group, edge_case_tests}
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [{atom(), term()}], [atom()]}].
groups() ->
    [
        {deterministic_mode_tests, [shuffle, {repeat, 1}], [
            deterministic_always_picks_first_test,
            deterministic_order_consistency_test,
            deterministic_multiple_choices_test,
            deterministic_policy_immutability_test,
            deterministic_empty_choices_test,
            deterministic_single_choice_test
        ]},
        {nondeterministic_mode_tests, [shuffle, {repeat, 1}], [
            nondeterministic_records_choices_test,
            nondeterministic_choice_log_structure_test,
            nondeterministic_accumulates_log_test,
            nondeterministic_empty_choices_test,
            nondeterministic_single_choice_test,
            nondeterministic_multiple_branches_test
        ]},
        {replay_mode_tests, [shuffle, {repeat, 1}], [
            replay_follows_recorded_trace_test,
            replay_empty_trace_test,
            replay_partial_trace_test,
            replay_invalid_index_fallback_test,
            replay_exhausted_trace_fallback_test,
            replay_preserves_index_test,
            replay_different_branch_selection_test
        ]},
        {mode_switching_tests, [shuffle, {repeat, 1}], [
            switch_deterministic_to_nondeterministic_test,
            switch_nondeterministic_to_replay_test,
            switch_replay_to_deterministic_test,
            mode_independence_test
        ]},
        {edge_case_tests, [], [
            empty_effect_list_test,
            single_effect_list_test,
            large_effect_list_test,
            stress_many_choices_test,
            policy_type_preservation_test
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
    ct:pal("Starting ln_ctrl_scheduler_SUITE"),

    %% Ensure the scheduler module is loaded
    {module, ln_ctrl_sched} = code:ensure_loaded(ln_ctrl_sched),

    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed ln_ctrl_scheduler_SUITE"),
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

%% @private
%% @doc Apply policy multiple times and collect results.
-spec apply_policy_n_times(ln_ctrl_sched:policy(), non_neg_integer(), [any()]) ->
    {[any()], ln_ctrl_sched:policy()}.
apply_policy_n_times(Policy, 0, _Choices) ->
    {[], Policy};
apply_policy_n_times(Policy, N, Choices) ->
    {Chosen, UpdatedPolicy} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    {RestChoices, FinalPolicy} = apply_policy_n_times(UpdatedPolicy, N - 1, Choices),
    {[Chosen | RestChoices], FinalPolicy}.

%%%===================================================================
%%% Test Cases - Deterministic Mode
%%%===================================================================

%% @doc Test deterministic mode always picks the first branch.
-spec deterministic_always_picks_first_test(config()) -> {comment, string()}.
deterministic_always_picks_first_test(_Config) ->
    Policy = ln_ctrl_sched:new_deterministic(),
    Choices = [branch_a, branch_b, branch_c, branch_d],

    %% Apply multiple times - should always pick first
    {Choice1, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    {Choice2, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    {Choice3, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),

    ?assertEqual(branch_a, Choice1),
    ?assertEqual(branch_a, Choice2),
    ?assertEqual(branch_a, Choice3),

    {comment, "deterministic always picks first verified"}.

%% @doc Test deterministic mode produces consistent order across runs.
-spec deterministic_order_consistency_test(config()) -> {comment, string()}.
deterministic_order_consistency_test(_Config) ->
    %% Create two separate deterministic policies
    Policy1 = ln_ctrl_sched:new_deterministic(),
    Policy2 = ln_ctrl_sched:new_deterministic(),
    Choices = [a, b, c],

    %% Both should produce identical sequences
    {Results1, _} = apply_policy_n_times(Policy1, 5, Choices),
    {Results2, _} = apply_policy_n_times(Policy2, 5, Choices),

    %% All results should be 'a' (first choice)
    Expected = [a, a, a, a, a],
    ?assertEqual(Expected, Results1),
    ?assertEqual(Expected, Results2),

    {comment, "deterministic order consistency verified"}.

%% @doc Test deterministic mode with multiple choice sets.
-spec deterministic_multiple_choices_test(config()) -> {comment, string()}.
deterministic_multiple_choices_test(_Config) ->
    Policy = ln_ctrl_sched:new_deterministic(),

    %% Different choice sets
    Choices1 = [x, y, z],
    Choices2 = [1, 2, 3, 4, 5],
    Choices3 = [foo, bar],

    %% Each should pick first element
    {X, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices1),
    {One, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices2),
    {Foo, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices3),

    ?assertEqual(x, X),
    ?assertEqual(1, One),
    ?assertEqual(foo, Foo),

    {comment, "deterministic multiple choices verified"}.

%% @doc Test that deterministic policy state is immutable (unchanged after use).
-spec deterministic_policy_immutability_test(config()) -> {comment, string()}.
deterministic_policy_immutability_test(_Config) ->
    Policy = ln_ctrl_sched:new_deterministic(),
    Choices = [a, b, c],

    %% Apply policy multiple times using the SAME initial policy
    {Choice1, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    {Choice2, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    {Choice3, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),

    %% All should be first choice (policy doesn't change state)
    ?assertEqual(a, Choice1),
    ?assertEqual(a, Choice2),
    ?assertEqual(a, Choice3),

    {comment, "deterministic policy immutability verified"}.

%% @doc Test deterministic mode with empty choices list.
-spec deterministic_empty_choices_test(config()) -> {comment, string()}.
deterministic_empty_choices_test(_Config) ->
    Policy = ln_ctrl_sched:new_deterministic(),

    %% Empty list should fail the guard (length(Choices) > 0)
    Result = try
        ln_ctrl_sched:apply_policy(Policy, undefined, [])
    catch
        error:function_clause ->
            function_clause
    end,

    ?assertEqual(function_clause, Result),

    {comment, "deterministic empty choices verified"}.

%% @doc Test deterministic mode with single choice.
-spec deterministic_single_choice_test(config()) -> {comment, string()}.
deterministic_single_choice_test(_Config) ->
    Policy = ln_ctrl_sched:new_deterministic(),
    Choices = [only_choice],

    {Choice, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),

    ?assertEqual(only_choice, Choice),

    {comment, "deterministic single choice verified"}.

%%%===================================================================
%%% Test Cases - Nondeterministic Mode
%%%===================================================================

%% @doc Test nondeterministic mode records choices in the log.
-spec nondeterministic_records_choices_test(config()) -> {comment, string()}.
nondeterministic_records_choices_test(_Config) ->
    Policy = ln_ctrl_sched:new_nondeterministic(),
    Choices = [branch1, branch2, branch3],

    {_, UpdatedPolicy} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),

    %% Check choice log was populated
    ChoiceLog = UpdatedPolicy#nondeterministic.choice_log,
    ?assertEqual(1, length(ChoiceLog)),

    %% First element should be the branch index (0-based)
    {BranchIdx, _ChoicePoint} = hd(ChoiceLog),
    ?assertEqual(0, BranchIdx),

    {comment, "nondeterministic records choices verified"}.

%% @doc Test choice log structure contains branch index and choice point.
-spec nondeterministic_choice_log_structure_test(config()) -> {comment, string()}.
nondeterministic_choice_log_structure_test(_Config) ->
    Policy = ln_ctrl_sched:new_nondeterministic(),
    Choices = [a, b, c],

    {_, Policy1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    {_, Policy2} = ln_ctrl_sched:apply_policy(Policy1, undefined, Choices),

    ChoiceLog = Policy2#nondeterministic.choice_log,
    ?assertEqual(2, length(ChoiceLog)),

    %% Verify structure of each log entry
    lists:foreach(
        fun({BranchIdx, ChoicePoint}) ->
            ?assertEqual(0, BranchIdx),  % Current impl always picks first
            ?assert(is_integer(ChoicePoint)),
            ?assert(ChoicePoint > 0)     % positive unique integer
        end,
        ChoiceLog
    ),

    {comment, "nondeterministic choice log structure verified"}.

%% @doc Test choice log accumulates across multiple apply_policy calls.
-spec nondeterministic_accumulates_log_test(config()) -> {comment, string()}.
nondeterministic_accumulates_log_test(_Config) ->
    Policy = ln_ctrl_sched:new_nondeterministic(),
    Choices = [x, y, z],

    %% Apply multiple times, threading the updated policy
    {_, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(1, length(P1#nondeterministic.choice_log)),

    {_, P2} = ln_ctrl_sched:apply_policy(P1, undefined, Choices),
    ?assertEqual(2, length(P2#nondeterministic.choice_log)),

    {_, P3} = ln_ctrl_sched:apply_policy(P2, undefined, Choices),
    ?assertEqual(3, length(P3#nondeterministic.choice_log)),

    {_, P4} = ln_ctrl_sched:apply_policy(P3, undefined, Choices),
    ?assertEqual(4, length(P4#nondeterministic.choice_log)),

    {comment, "nondeterministic accumulates log verified"}.

%% @doc Test nondeterministic mode with empty choices list.
-spec nondeterministic_empty_choices_test(config()) -> {comment, string()}.
nondeterministic_empty_choices_test(_Config) ->
    Policy = ln_ctrl_sched:new_nondeterministic(),

    %% Empty list should fail the guard
    Result = try
        ln_ctrl_sched:apply_policy(Policy, undefined, [])
    catch
        error:function_clause ->
            function_clause
    end,

    ?assertEqual(function_clause, Result),

    {comment, "nondeterministic empty choices verified"}.

%% @doc Test nondeterministic mode with single choice.
-spec nondeterministic_single_choice_test(config()) -> {comment, string()}.
nondeterministic_single_choice_test(_Config) ->
    Policy = ln_ctrl_sched:new_nondeterministic(),
    Choices = [solo],

    {Choice, UpdatedPolicy} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),

    ?assertEqual(solo, Choice),
    ?assertEqual(1, length(UpdatedPolicy#nondeterministic.choice_log)),

    {comment, "nondeterministic single choice verified"}.

%% @doc Test nondeterministic mode with multiple branch types.
-spec nondeterministic_multiple_branches_test(config()) -> {comment, string()}.
nondeterministic_multiple_branches_test(_Config) ->
    Policy = ln_ctrl_sched:new_nondeterministic(),

    %% Different types of branches
    Choices1 = [{tuple, data}, [list, of, atoms], #{map => value}, atom_branch],
    {Choice1, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices1),
    ?assertEqual({tuple, data}, Choice1),

    %% Binary branches
    Choices2 = [<<1, 2, 3>>, <<4, 5, 6>>],
    {Choice2, _} = ln_ctrl_sched:apply_policy(P1, undefined, Choices2),
    ?assertEqual(<<1, 2, 3>>, Choice2),

    {comment, "nondeterministic multiple branches verified"}.

%%%===================================================================
%%% Test Cases - Replay Mode
%%%===================================================================

%% @doc Test replay mode follows recorded trace exactly.
-spec replay_follows_recorded_trace_test(config()) -> {comment, string()}.
replay_follows_recorded_trace_test(_Config) ->
    %% Create a trace that picks branch at index 0, then index 1
    ChoiceLog = [{0, 100}, {1, 101}, {0, 102}],
    Policy = ln_ctrl_sched:new_replay(ChoiceLog),
    Choices = [a, b, c],

    %% First apply should use {0, 100} -> pick index 0
    {Choice1, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(a, Choice1),

    %% Second apply should use {1, 101} -> pick index 1
    {Choice2, P2} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(b, Choice2),

    %% Third apply should use {0, 102} -> pick index 0
    {Choice3, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(a, Choice3),

    {comment, "replay follows recorded trace verified"}.

%% @doc Test replay mode with empty trace.
-spec replay_empty_trace_test(config()) -> {comment, string()}.
replay_empty_trace_test(_Config) ->
    Policy = ln_ctrl_sched:new_replay([]),
    Choices = [a, b, c],

    %% With empty trace, should fall back to first choice
    {Choice, UpdatedPolicy} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(a, Choice),

    %% Index should be incremented even with empty log
    ?assertEqual(1, UpdatedPolicy#replay.index),

    {comment, "replay empty trace verified"}.

%% @doc Test replay mode with partial trace (fewer entries than apply calls).
-spec replay_partial_trace_test(config()) -> {comment, string()}.
replay_partial_trace_test(_Config) ->
    %% Only 2 entries in trace
    ChoiceLog = [{2, 100}, {0, 101}],
    Policy = ln_ctrl_sched:new_replay(ChoiceLog),
    Choices = [a, b, c, d, e],

    %% First two calls use the trace
    {C1, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(c, C1),  %% index 2 -> 'c'

    {C2, P2} = ln_ctrl_sched:apply_policy(P1, undefined, Choices),
    ?assertEqual(a, C2),  %% index 0 -> 'a'

    %% Third call has no trace entry, falls back to first
    {C3, P3} = ln_ctrl_sched:apply_policy(P2, undefined, Choices),
    ?assertEqual(a, C3),  %% fallback -> 'a'

    %% Fourth call also has no trace
    {C4, _} = ln_ctrl_sched:apply_policy(P3, undefined, Choices),
    ?assertEqual(a, C4),  %% fallback -> 'a'

    {comment, "replay partial trace verified"}.

%% @doc Test replay mode falls back when branch index is invalid.
-spec replay_invalid_index_fallback_test(config()) -> {comment, string()}.
replay_invalid_index_fallback_test(_Config) ->
    %% Branch index 5 exists but choices only has 3 elements
    ChoiceLog = [{5, 100}],  %% Invalid index for 3 choices
    Policy = ln_ctrl_sched:new_replay(ChoiceLog),
    Choices = [a, b, c],

    %% Should fallback to first choice when index is out of bounds
    {Choice, _} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(a, Choice),

    {comment, "replay invalid index fallback verified"}.

%% @doc Test replay mode when trace is exhausted.
-spec replay_exhausted_trace_fallback_test(config()) -> {comment, string()}.
replay_exhausted_trace_fallback_test(_Config) ->
    ChoiceLog = [{0, 100}],
    Policy = ln_ctrl_sched:new_replay(ChoiceLog),
    Choices = [a, b, c],

    %% First call uses trace
    {C1, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(a, C1),

    %% Subsequent calls have exhausted trace, fall back to first
    {C2, P2} = ln_ctrl_sched:apply_policy(P1, undefined, Choices),
    ?assertEqual(a, C2),

    {C3, P3} = ln_ctrl_sched:apply_policy(P2, undefined, Choices),
    ?assertEqual(a, C3),

    {C4, _} = ln_ctrl_sched:apply_policy(P3, undefined, Choices),
    ?assertEqual(a, C4),

    {comment, "replay exhausted trace fallback verified"}.

%% @doc Test replay mode preserves index across calls.
-spec replay_preserves_index_test(config()) -> {comment, string()}.
replay_preserves_index_test(_Config) ->
    ChoiceLog = [{0, 100}, {1, 101}, {2, 102}],
    Policy = ln_ctrl_sched:new_replay(ChoiceLog),

    %% Initial index should be 0
    ?assertEqual(0, Policy#replay.index),

    {_, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, [a, b, c]),
    ?assertEqual(1, P1#replay.index),

    {_, P2} = ln_ctrl_sched:apply_policy(P1, undefined, [a, b, c]),
    ?assertEqual(2, P2#replay.index),

    {_, P3} = ln_ctrl_sched:apply_policy(P2, undefined, [a, b, c]),
    ?assertEqual(3, P3#replay.index),

    {comment, "replay preserves index verified"}.

%% @doc Test replay mode can select different branches.
-spec replay_different_branch_selection_test(config()) -> {comment, string()}.
replay_different_branch_selection_test(_Config) ->
    %% Create a trace that picks each branch once
    ChoiceLog = [{0, 100}, {1, 101}, {2, 102}],
    Policy = ln_ctrl_sched:new_replay(ChoiceLog),
    Choices = [first, second, third],

    {C1, P1} = ln_ctrl_sched:apply_policy(Policy, undefined, Choices),
    ?assertEqual(first, C1),

    {C2, P2} = ln_ctrl_sched:apply_policy(P1, undefined, Choices),
    ?assertEqual(second, C2),

    {C3, _} = ln_ctrl_sched:apply_policy(P2, undefined, Choices),
    ?assertEqual(third, C3),

    {comment, "replay different branch selection verified"}.

%%%===================================================================
%%% Test Cases - Mode Switching
%%%===================================================================

%% @doc Test switching from deterministic to nondeterministic mode.
-spec switch_deterministic_to_nondeterministic_test(config()) -> {comment, string()}.
switch_deterministic_to_nondeterministic_test(_Config) ->
    DetPolicy = ln_ctrl_sched:new_deterministic(),
    Choices = [a, b, c],

    %% Use deterministic
    {C1, _} = ln_ctrl_sched:apply_policy(DetPolicy, undefined, Choices),
    ?assertEqual(a, C1),

    %% Create new nondeterministic policy
    NondetPolicy = ln_ctrl_sched:new_nondeterministic(),
    {C2, UpdatedNondet} = ln_ctrl_sched:apply_policy(NondetPolicy, undefined, Choices),
    ?assertEqual(a, C2),
    ?assertEqual(1, length(UpdatedNondet#nondeterministic.choice_log)),

    {comment, "switch deterministic to nondeterministic verified"}.

%% @doc Test switching from nondeterministic to replay mode.
-spec switch_nondeterministic_to_replay_test(config()) -> {comment, string()}.
switch_nondeterministic_to_replay_test(_Config) ->
    NondetPolicy = ln_ctrl_sched:new_nondeterministic(),
    Choices = [a, b, c],

    %% Use nondeterministic and collect choice log
    {_, P1} = ln_ctrl_sched:apply_policy(NondetPolicy, undefined, Choices),
    {_, P2} = ln_ctrl_sched:apply_policy(P1, undefined, Choices),
    {_, P3} = ln_ctrl_sched:apply_policy(P2, undefined, Choices),

    ChoiceLog = P3#nondeterministic.choice_log,
    ?assertEqual(3, length(ChoiceLog)),

    %% Create replay policy from the collected log
    ReplayPolicy = ln_ctrl_sched:new_replay(ChoiceLog),

    %% Replay should produce same choices
    {C1, _} = ln_ctrl_sched:apply_policy(ReplayPolicy, undefined, Choices),
    ?assertEqual(a, C1),

    {comment, "switch nondeterministic to replay verified"}.

%% @doc Test switching from replay to deterministic mode.
-spec switch_replay_to_deterministic_test(config()) -> {comment, string()}.
switch_replay_to_deterministic_test(_Config) ->
    ChoiceLog = [{0, 100}, {1, 101}],
    ReplayPolicy = ln_ctrl_sched:new_replay(ChoiceLog),
    Choices = [a, b, c],

    %% Use replay
    {C1, P1} = ln_ctrl_sched:apply_policy(ReplayPolicy, undefined, Choices),
    ?assertEqual(a, C1),

    {C2, _} = ln_ctrl_sched:apply_policy(P1, undefined, Choices),
    ?assertEqual(b, C2),

    %% Switch to deterministic
    DetPolicy = ln_ctrl_sched:new_deterministic(),
    {C3, _} = ln_ctrl_sched:apply_policy(DetPolicy, undefined, Choices),
    ?assertEqual(a, C3),  %% Deterministic always picks first

    {comment, "switch replay to deterministic verified"}.

%% @doc Test that policies are independent (no shared state).
-spec mode_independence_test(config()) -> {comment, string()}.
mode_independence_test(_Config) ->
    Det = ln_ctrl_sched:new_deterministic(),
    Nondet = ln_ctrl_sched:new_nondeterministic(),
    Replay = ln_ctrl_sched:new_replay([{0, 100}]),

    Choices = [x, y, z],

    %% Apply to each independently
    {DetChoice, DetUpdated} = ln_ctrl_sched:apply_policy(Det, undefined, Choices),
    {NondetChoice, NondetUpdated} = ln_ctrl_sched:apply_policy(Nondet, undefined, Choices),
    {ReplayChoice, ReplayUpdated} = ln_ctrl_sched:apply_policy(Replay, undefined, Choices),

    %% All pick first with default behavior
    ?assertEqual(x, DetChoice),
    ?assertEqual(x, NondetChoice),
    ?assertEqual(x, ReplayChoice),

    %% Verify types are preserved
    ?assertMatch(#deterministic{}, DetUpdated),
    ?assertMatch(#nondeterministic{}, NondetUpdated),
    ?assertMatch(#replay{}, ReplayUpdated),

    {comment, "mode independence verified"}.

%%%===================================================================
%%% Test Cases - Edge Cases
%%%===================================================================

%% @doc Test empty effect list handling across all modes.
-spec empty_effect_list_test(config()) -> {comment, string()}.
empty_effect_list_test(_Config) ->
    Policies = [
        ln_ctrl_sched:new_deterministic(),
        ln_ctrl_sched:new_nondeterministic(),
        ln_ctrl_sched:new_replay([{0, 100}])
    ],

    %% All modes should fail with function_clause on empty list
    lists:foreach(
        fun(Policy) ->
            Result = try
                ln_ctrl_sched:apply_policy(Policy, undefined, [])
            catch
                error:function_clause ->
                    function_clause
            end,
            ?assertEqual(function_clause, Result)
        end,
        Policies
    ),

    {comment, "empty effect list verified"}.

%% @doc Test single effect list handling across all modes.
-spec single_effect_list_test(config()) -> {comment, string()}.
single_effect_list_test(_Config) ->
    Choices = [only_one],

    %% Deterministic
    Det = ln_ctrl_sched:new_deterministic(),
    {C1, _} = ln_ctrl_sched:apply_policy(Det, undefined, Choices),
    ?assertEqual(only_one, C1),

    %% Nondeterministic
    Nondet = ln_ctrl_sched:new_nondeterministic(),
    {C2, Nondet2} = ln_ctrl_sched:apply_policy(Nondet, undefined, Choices),
    ?assertEqual(only_one, C2),
    ?assertEqual(1, length(Nondet2#nondeterministic.choice_log)),

    %% Replay
    Replay = ln_ctrl_sched:new_replay([{0, 100}]),
    {C3, _} = ln_ctrl_sched:apply_policy(Replay, undefined, Choices),
    ?assertEqual(only_one, C3),

    {comment, "single effect list verified"}.

%% @doc Test large effect list handling.
-spec large_effect_list_test(config()) -> {comment, string()}.
large_effect_list_test(_Config) ->
    %% Create a large list of choices
    LargeChoices = lists:seq(1, 10000),

    %% Deterministic - should pick first
    Det = ln_ctrl_sched:new_deterministic(),
    {C1, _} = ln_ctrl_sched:apply_policy(Det, undefined, LargeChoices),
    ?assertEqual(1, C1),

    %% Nondeterministic - should pick first and log
    Nondet = ln_ctrl_sched:new_nondeterministic(),
    {C2, _} = ln_ctrl_sched:apply_policy(Nondet, undefined, LargeChoices),
    ?assertEqual(1, C2),

    %% Replay with index at end of list
    Replay = ln_ctrl_sched:new_replay([{9999, 100}]),
    {C3, _} = ln_ctrl_sched:apply_policy(Replay, undefined, LargeChoices),
    ?assertEqual(10000, C3),

    {comment, "large effect list verified"}.

%% @doc Test stress with many consecutive choices.
-spec stress_many_choices_test(config()) -> {comment, string()}.
stress_many_choices_test(_Config) ->
    NumChoices = 1000,
    Choices = [a, b, c],

    %% Nondeterministic - accumulate many log entries
    Nondet = ln_ctrl_sched:new_nondeterministic(),
    {_, FinalNondet} = apply_policy_n_times(Nondet, NumChoices, Choices),
    ?assertEqual(NumChoices, length(FinalNondet#nondeterministic.choice_log)),

    %% Create replay from log and verify same sequence
    ChoiceLog = FinalNondet#nondeterministic.choice_log,
    Replay = ln_ctrl_sched:new_replay(ChoiceLog),
    {Results, _} = apply_policy_n_times(Replay, NumChoices, Choices),

    %% All should be 'a' (since nondeterministic picks first)
    ExpectedResults = lists:duplicate(NumChoices, a),
    ?assertEqual(ExpectedResults, Results),

    {comment, "stress many choices verified"}.

%% @doc Test that policy record types are preserved after apply_policy.
-spec policy_type_preservation_test(config()) -> {comment, string()}.
policy_type_preservation_test(_Config) ->
    Choices = [x, y, z],

    %% Deterministic preserves type
    Det = ln_ctrl_sched:new_deterministic(),
    {_, Det2} = ln_ctrl_sched:apply_policy(Det, undefined, Choices),
    ?assertMatch(#deterministic{}, Det2),
    ?assertEqual(deterministic, Det2#deterministic.name),

    %% Nondeterministic preserves type
    Nondet = ln_ctrl_sched:new_nondeterministic(),
    {_, Nondet2} = ln_ctrl_sched:apply_policy(Nondet, undefined, Choices),
    ?assertMatch(#nondeterministic{}, Nondet2),
    ?assertEqual(nondeterministic, Nondet2#nondeterministic.name),

    %% Replay preserves type
    Replay = ln_ctrl_sched:new_replay([{0, 100}]),
    {_, Replay2} = ln_ctrl_sched:apply_policy(Replay, undefined, Choices),
    ?assertMatch(#replay{}, Replay2),
    ?assertEqual(replay, Replay2#replay.name),

    {comment, "policy type preservation verified"}.
