-module(ln_cancel_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(TEST_SCOPE, <<"test_scope">>).
-define(CHILD_SCOPE_1, <<"child_scope_1">>).
-define(CHILD_SCOPE_2, <<"child_scope_2">>).
-define(UNRELATED_SCOPE, <<"unrelated_scope">>).

%%%===================================================================
%%% Scope Cancellation Tests
%%%===================================================================

cancel_scope_prevents_new_effects_test() ->
    %% Verify that cancelling a scope prevents new effects from being registered
    ScopeId = ?TEST_SCOPE,
    EffectId = <<"effect_1">>,

    %% Initialize a scope
    {ok, ScopeState} = ln_scope:init(ScopeId, #{}),

    %% Register an effect
    {ok, ScopeWithEffect} = ln_scope:register_effect(ScopeState, EffectId, #{type => test}),

    %% Cancel the scope
    {ok, CancelledScope} = ln_scope:cancel(ScopeWithEffect, <<"test_cancellation">>),

    %% Attempt to register a new effect after cancellation
    Result = ln_scope:register_effect(CancelledScope, <<"effect_2">>, #{type => test}),

    %% Verify that new effect registration is rejected
    ?assertEqual({error, scope_cancelled}, Result),
    ok.

cancel_scope_propagates_to_children_test() ->
    %% Verify that cancelling a parent scope propagates cancellation to all children
    ParentScope = ?TEST_SCOPE,
    Child1 = ?CHILD_SCOPE_1,
    Child2 = ?CHILD_SCOPE_2,

    %% Initialize parent scope
    {ok, ParentState} = ln_scope:init(ParentScope, #{}),

    %% Create child scopes
    {ok, ParentWithChild1} = ln_scope:add_child(ParentState, Child1, #{}),
    {ok, ParentWithChildren} = ln_scope:add_child(ParentWithChild1, Child2, #{}),

    %% Cancel parent scope
    {ok, CancelledParent} = ln_scope:cancel(ParentWithChildren, <<"parent_cancel">>),

    %% Verify child scopes are also cancelled
    Child1Status = ln_scope:get_status(CancelledParent, Child1),
    Child2Status = ln_scope:get_status(CancelledParent, Child2),

    ?assertEqual(cancelled, Child1Status),
    ?assertEqual(cancelled, Child2Status),
    ok.

unrelated_scopes_continue_test() ->
    %% Verify that cancelling one scope doesn't affect unrelated scopes
    Scope1 = ?TEST_SCOPE,
    Scope2 = ?UNRELATED_SCOPE,

    %% Initialize two independent scopes
    {ok, Scope1State} = ln_scope:init(Scope1, #{}),
    {ok, Scope2State} = ln_scope:init(Scope2, #{}),

    %% Cancel scope1
    {ok, CancelledScope1} = ln_scope:cancel(Scope1State, <<"cancel_1">>),

    %% Verify scope2 is still active
    Scope2Status = ln_scope:get_status(Scope2State, Scope2),

    ?assertEqual(cancelled, ln_scope:get_status(CancelledScope1, Scope1)),
    ?assertEqual(active, Scope2Status),
    ok.

%%%===================================================================
%%% Case Cancellation Tests
%%%===================================================================

cancel_case_stops_execution_test() ->
    %% Verify that cancelling a case stops all execution within that case
    CaseId = <<"test_case">>,
    Step1 = <<"step_1">>,
    Step2 = <<"step_2">>,

    %% Initialize case
    {ok, CaseState} = ln_case:init(CaseId, #{}),

    %% Start first step
    {ok, CaseWithStep1} = ln_case:start_step(CaseState, Step1, #{}),

    %% Cancel the case
    {ok, CancelledCase} = ln_case:cancel(CaseWithStep1, <<"case_cancel">>),

    %% Attempt to start a new step after cancellation
    Result = ln_case:start_step(CancelledCase, Step2, #{}),

    %% Verify step cannot be started
    ?assertEqual({error, case_cancelled}, Result),
    ok.

await_returns_cancelled_test() ->
    %% Verify that awaiting a cancelled case returns a cancelled result
    CaseId = <<"test_case">>,

    %% Initialize case
    {ok, CaseState} = ln_case:init(CaseId, #{}),

    %% Cancel immediately
    {ok, CancelledCase} = ln_case:cancel(CaseState, <<"immediate_cancel">>),

    %% Await the case
    Result = ln_case:await(CancelledCase, 1000),

    %% Verify await returns cancelled status
    ?assertMatch({cancelled, _}, Result),
    ok.

%%%===================================================================
%%% Join Interaction Tests
%%%===================================================================

cancelled_branch_with_all_policy_test() ->
    %% Verify that with 'all' join policy, a cancelled branch causes failure
    CaseId = <<"join_test_case">>,
    Branch1 = <<"branch_1">>,
    Branch2 = <<"branch_2">>,

    %% Initialize case with all-join policy
    {ok, CaseState} = ln_case:init(CaseId, #{join_policy => all}),

    %% Start both branches
    {ok, CaseWithBranches} = ln_case:start_step(
        ln_case:start_step(CaseState, Branch1, #{}),
        Branch2, #{}
    ),

    %% Complete branch1
    {ok, CaseWithBranch1Done} = ln_case:complete_step(CaseWithBranches, Branch1, #{result => ok}),

    %% Cancel branch2
    {ok, CaseWithBranch2Cancelled} = ln_case:cancel_branch(
        CaseWithBranch1Done,
        Branch2,
        <<"branch_cancel">>
    ),

    %% Check join result - should fail with branch_cancelled error
    JoinResult = ln_case:check_join(CaseWithBranch2Cancelled),

    ?assertEqual({error, branch_cancelled}, JoinResult),
    ok.

cancelled_branch_with_n_of_m_policy_test() ->
    %% Verify that with n_of_m join policy, cancellation may not cause failure
    %% if enough branches complete
    CaseId = <<"n_of_m_test_case">>,
    Branch1 = <<"branch_1">>,
    Branch2 = <<"branch_2">>,
    Branch3 = <<"branch_3">>,

    %% Initialize case with 2_of_3 join policy
    {ok, CaseState} = ln_case:init(CaseId, #{join_policy => {n_of_m, 2, 3}}),

    %% Start all three branches
    {ok, CaseWithBranches} = ln_case:start_step(
        ln_case:start_step(
            ln_case:start_step(CaseState, Branch1, #{}),
            Branch2, #{}
        ),
        Branch3, #{}
    ),

    %% Complete branch1 and branch2
    {ok, CaseWith2Complete} = ln_case:complete_step(
        ln_case:complete_step(CaseWithBranches, Branch1, #{result => ok}),
        Branch2, #{result => ok}
    ),

    %% Cancel branch3 (should not affect outcome since we have 2/3)
    {ok, CaseWithBranch3Cancelled} = ln_case:cancel_branch(
        CaseWith2Complete,
        Branch3,
        <<"branch_3_cancel">>
    ),

    %% Check join result - should succeed with quorum
    JoinResult = ln_case:check_join(CaseWithBranch3Cancelled),

    ?assertMatch({ok, _}, JoinResult),
    ok.
