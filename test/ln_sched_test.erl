%%%-------------------------------------------------------------------
%%% @doc EUnit tests for ln_sched module.
%%% @end
%%%-------------------------------------------------------------------
-module(ln_sched_test).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Generators
%%%-------------------------------------------------------------------

%% @doc Test init deterministic.
init_deterministic_test() ->
    State = ln_sched:init(deterministic),
    ?assertEqual(deterministic, ln_sched:get_mode(State)),
    ?assertEqual([], ln_sched:get_log(State)).

%% @doc Test init nondeterministic.
init_nondeterministic_test() ->
    State = ln_sched:init(nondeterministic),
    ?assertEqual(nondeterministic, ln_sched:get_mode(State)).

%% @doc Test init nondeterministic with seed.
init_nondet_with_seed_test() ->
    Seed = rand:seed_s(exrop, 12345),
    State = ln_sched:init(nondeterministic, Seed),
    ?assertEqual(nondeterministic, ln_sched:get_mode(State)).

%% @doc Test init replay.
init_replay_test() ->
    Choices = [#ln_sched.choice{type = xor_selection, value = a, metadata = {}, timestamp = 0}],
    State = ln_sched:init(replay, Choices),
    ?assertEqual(replay, ln_sched:get_mode(State)).

%% @doc Test choose in deterministic mode is repeatable.
choose_deterministic_repeatable_test() ->
    State = ln_sched:init(deterministic),
    Candidates = [{a, 1}, {b, 2}, {c, 3}],
    {Selected1, State1} = ln_sched:choose(Candidates, State),
    {Selected2, _State2} = ln_sched:choose(Candidates, State1),
    ?assertEqual(Selected1, Selected2).

%% @doc Test choose in deterministic mode picks first by term order.
choose_deterministic_order_test() ->
    State = ln_sched:init(deterministic),
    Candidates = [{c, 3}, {a, 1}, {b, 2}],
    {Selected, _} = ln_sched:choose(Candidates, State),
    ?assertEqual({a, 1}, Selected).

%% @doc Test choose with single candidate.
choose_single_test() ->
    State = ln_sched:init(deterministic),
    Candidates = [{only, 1}],
    {Selected, _} = ln_sched:choose(Candidates, State),
    ?assertEqual({only, 1}, Selected).

%% @doc Test choose with empty list returns error.
choose_empty_test() ->
    State = ln_sched:init(deterministic),
    ?assertEqual({error, no_choices}, ln_sched:choose([], State)).

%% @doc Test record choice in nondeterministic mode.
record_choice_nondet_test() ->
    State = ln_sched:init(nondeterministic),
    NewState = ln_sched:record_choice(xor_selection, branch_a, State),
    Log = ln_sched:get_log(NewState),
    ?assert(length(Log) > 0).

%% @doc Test record choice in deterministic mode is no-op.
record_choice_deterministic_test() ->
    State = ln_sched:init(deterministic),
    NewState = ln_sched:record_choice(xor_selection, branch_a, State),
    ?assertEqual([], ln_sched:get_log(NewState)).

%% @doc Test verify choice with matching type and value.
verify_choice_match_test() ->
    Choice = #ln_sched.choice{type = xor_selection, value = a, metadata = {}, timestamp = 0},
    ?assertEqual({ok, a}, ln_sched:verify_choice(xor_selection, [a, b, c], Choice)).

%% @doc Test verify choice with mismatched type.
verify_choice_mismatch_test() ->
    Choice = #ln_sched.choice{type = xor_selection, value = a, metadata = {}, timestamp = 0},
    ?assertEqual({error, mismatch}, ln_sched:verify_choice(defer_race, [a, b, c], Choice)).

%% @doc Test verify choice with value not in candidates.
verify_choice_not_available_test() ->
    Choice = #ln_sched.choice{type = xor_selection, value = d, metadata = {}, timestamp = 0},
    ?assertEqual({error, not_available}, ln_sched:verify_choice(xor_selection, [a, b, c], Choice)).

%% @doc Test replay mode consumes choices.
replay_consumes_choice_test() ->
    Choices = [#ln_sched.choice{type = task_selection, value = a, metadata = {}, timestamp = 0},
                #ln_sched.choice{type = task_selection, value = b, metadata = {}, timestamp = 0}],
    State = ln_sched:init(replay, Choices),
    {Selected, State1} = ln_sched:choose([{a, 1}, {b, 2}, {c, 3}], State),
    ?assertEqual({a, 1}, Selected),
    ?assertEqual(1, State1#ln_sched.position).

%% @doc Test replay with exhausted log returns error.
replay_exhausted_test() ->
    Choices = [#ln_sched.choice{type = task_selection, value = a, metadata = {}, timestamp = 0}],
    State = ln_sched:init(replay, Choices),
    {_, State1} = ln_sched:choose([{a, 1}], State),
    ?assertEqual({error, choice_log_exhausted}, ln_sched:choose([{b, 2}], State1)).

%% @doc Test replay with value not in candidates.
replay_value_not_available_test() ->
    Choices = [#ln_sched.choice{type = task_selection, value = d, metadata = {}, timestamp = 0}],
    State = ln_sched:init(replay, Choices),
    ?assertEqual({error, choice_not_available}, ln_sched:choose([{a, 1}, {b, 2}], State)).

%% @doc Test multiple choices in nondeterministic mode.
multiple_choices_nondet_test() ->
    State = ln_sched:init(nondeterministic),
    State1 = ln_sched:record_choice(xor_selection, branch_a, State),
    State2 = ln_sched:record_choice(defer_race, branch_1, State1),
    Log = ln_sched:get_log(State2),
    ?assertEqual(2, length(Log)).

%% @doc Test log is reversed when retrieved.
log_reversed_test() ->
    State = ln_sched:init(nondeterministic),
    State1 = ln_sched:record_choice(xor_selection, first, State),
    State2 = ln_sched:record_choice(defer_race, second, State1),
    Log = ln_sched:get_log(State2),
    [First, Second] = Log,
    ?assertEqual(defer_race, First#ln_sched.choice.type),
    ?assertEqual(xor_selection, Second#ln_sched.choice.type).
