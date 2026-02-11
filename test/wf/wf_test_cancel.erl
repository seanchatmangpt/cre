%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------

-module(wf_test_cancel).
-moduledoc """
Cancellation testing for WF Substrate bytecode VM.

Tests cancellation semantics according to docs/WF_ARCHITECTURE.md:
- Activity cancellation (single task termination)
- Region cancellation (scope-based multi-activity termination)
- Case cancellation (whole workflow termination)
- Cancel propagation through nested scopes
- Cancellation soundness (cancelled tokens never fire)
""".

%%====================================================================
%% Exports
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Functions - Activity Cancellation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test basic activity cancellation stops task execution.
%%
%% Verifies that:
%% 1. Activity can be cancelled during execution
%% 2. Cancelled activity does not complete
%% 3. Other activities continue normally
%%--------------------------------------------------------------------
activity_cancel_basic_test() ->
    %% Create marking with activity tokens
    Marking = #{
        activity1 => [executing],
        activity2 => [ready],
        activity3 => []
    },

    %% Apply cancellation to activity1
    Result = wf_cancel:apply_cancellation(Marking, [activity1]),

    %% Verify activity1 cancelled
    ?assertEqual([], maps:get(activity1, Result)),

    %% Verify other activities unchanged
    ?assertEqual([ready], maps:get(activity2, Result)),
    ?assertEqual([], maps:get(activity3, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancelling multiple activities independently.
%%--------------------------------------------------------------------
activity_cancel_multiple_test() ->
    Marking = #{
        act_a => [token_a],
        act_b => [token_b],
        act_c => [token_c],
        act_d => [token_d]
    },

    %% Cancel activities a and c
    Result = wf_cancel:apply_cancellation(Marking, [act_a, act_c]),

    ?assertEqual([], maps:get(act_a, Result)),
    ?assertEqual([token_b], maps:get(act_b, Result)),
    ?assertEqual([], maps:get(act_c, Result)),
    ?assertEqual([token_d], maps:get(act_d, Result)).

%%--------------------------------------------------------------------
%% @doc Test activity with multiple tokens all cancelled.
%%--------------------------------------------------------------------
activity_cancel_multiple_tokens_test() ->
    Marking = #{
        activity => [token1, token2, token3],
        other => [preserved]
    },

    Result = wf_cancel:apply_cancellation(Marking, [activity]),

    %% All tokens removed from activity
    ?assertEqual([], maps:get(activity, Result)),
    ?assertEqual([preserved], maps:get(other, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancelling non-existent activity is safe.
%%--------------------------------------------------------------------
activity_cancel_nonexistent_test() ->
    Marking = #{
        existing => [token]
    },

    %% Cancel non-existent activity
    Result = wf_cancel:apply_cancellation(Marking, [nonexistent]),

    %% Existing activity preserved
    ?assertEqual([token], maps:get(existing, Result)),

    %% Non-existent activity added as empty
    ?assertEqual([], maps:get(nonexistent, Result)).

%%====================================================================
%% Test Functions - Region Cancellation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test basic region cancellation removes all region tokens.
%%
%% Region cancellation scope semantics:
%% - All activities in region have tokens cleared
%% - Activities outside region unaffected
%% - Nested scopes within region also cancelled
%%--------------------------------------------------------------------
region_cancel_basic_test() ->
    Marking = #{
        region_place1 => [token1],
        region_place2 => [token2],
        region_place3 => [token3],
        outside => [preserved]
    },

    Region = [region_place1, region_place2, region_place3],
    Result = wf_cancel:cancel_region(Marking, Region),

    %% All region places cleared
    ?assertEqual([], maps:get(region_place1, Result)),
    ?assertEqual([], maps:get(region_place2, Result)),
    ?assertEqual([], maps:get(region_place3, Result)),

    %% Outside place preserved
    ?assertEqual([preserved], maps:get(outside, Result)).

%%--------------------------------------------------------------------
%% @doc Test region with parallel branches all cancelled together.
%%--------------------------------------------------------------------
region_cancel_parallel_branches_test() ->
    %% Parallel execution in region
    Marking = #{
        branch_a => [executing_a],
        branch_b => [executing_b],
        branch_c => [executing_c],
        join_point => [],
        after_region => [waiting]
    },

    %% Region includes all parallel branches
    Region = [branch_a, branch_b, branch_c, join_point],
    Result = wf_cancel:cancel_region(Marking, Region),

    %% All branches cancelled
    ?assertEqual([], maps:get(branch_a, Result)),
    ?assertEqual([], maps:get(branch_b, Result)),
    ?assertEqual([], maps:get(branch_c, Result)),
    ?assertEqual([], maps:get(join_point, Result)),

    %% After region preserved
    ?assertEqual([waiting], maps:get(after_region, Result)).

%%--------------------------------------------------------------------
%% @doc Test region cancellation with loop constructs.
%%--------------------------------------------------------------------
region_cancel_with_loop_test() ->
    Marking = #{
        loop_entry => [token],
        loop_body => [executing],
        loop_condition => [checking],
        loop_exit => [],
        outside_loop => [preserved]
    },

    %% Region includes loop places
    Region = [loop_entry, loop_body, loop_condition, loop_exit],
    Result = wf_cancel:cancel_region(Marking, Region),

    %% Loop places cleared
    ?assertEqual([], maps:get(loop_entry, Result)),
    ?assertEqual([], maps:get(loop_body, Result)),
    ?assertEqual([], maps:get(loop_condition, Result)),
    ?assertEqual([], maps:get(loop_exit, Result)),

    %% Outside preserved
    ?assertEqual([preserved], maps:get(outside_loop, Result)).

%%--------------------------------------------------------------------
%% @doc Test nested region cancellation (inner region only).
%%--------------------------------------------------------------------
region_cancel_nested_inner_test() ->
    Marking = #{
        outer_place1 => [outer1],
        inner_place1 => [inner1],
        inner_place2 => [inner2],
        outer_place2 => [outer2]
    },

    %% Cancel only inner region
    InnerRegion = [inner_place1, inner_place2],
    Result = wf_cancel:cancel_region(Marking, InnerRegion),

    %% Inner region cancelled
    ?assertEqual([], maps:get(inner_place1, Result)),
    ?assertEqual([], maps:get(inner_place2, Result)),

    %% Outer region preserved
    ?assertEqual([outer1], maps:get(outer_place1, Result)),
    ?assertEqual([outer2], maps:get(outer_place2, Result)).

%%--------------------------------------------------------------------
%% @doc Test nested region cancellation (outer region cancels inner).
%%--------------------------------------------------------------------
region_cancel_nested_outer_test() ->
    Marking = #{
        outer_place1 => [outer1],
        inner_place1 => [inner1],
        inner_place2 => [inner2],
        outer_place2 => [outer2],
        outside => [preserved]
    },

    %% Cancel outer region (includes inner)
    OuterRegion = [outer_place1, inner_place1, inner_place2, outer_place2],
    Result = wf_cancel:cancel_region(Marking, OuterRegion),

    %% All outer and inner places cancelled
    ?assertEqual([], maps:get(outer_place1, Result)),
    ?assertEqual([], maps:get(inner_place1, Result)),
    ?assertEqual([], maps:get(inner_place2, Result)),
    ?assertEqual([], maps:get(outer_place2, Result)),

    %% Outside preserved
    ?assertEqual([preserved], maps:get(outside, Result)).

%%--------------------------------------------------------------------
%% @doc Test region cancellation with empty region (no-op).
%%--------------------------------------------------------------------
region_cancel_empty_test() ->
    Marking = #{
        place1 => [token1],
        place2 => [token2]
    },

    %% Empty region cancellation
    Result = wf_cancel:cancel_region(Marking, []),

    %% Marking unchanged
    ?assertEqual([token1], maps:get(place1, Result)),
    ?assertEqual([token2], maps:get(place2, Result)).

%%====================================================================
%% Test Functions - Case Cancellation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test case cancellation terminates entire workflow.
%%
%% Case cancellation semantics:
%% - All places in the case cleared
%% - Equivalent to cancelling root scope
%% - Case enters cancelled state
%%--------------------------------------------------------------------
case_cancel_full_workflow_test() ->
    %% Complete workflow case
    Marking = #{
        start => [],
        task1 => [executing],
        task2 => [ready],
        task3 => [ready],
        join => [],
        end_place => []
    },

    %% Case cancellation = cancel all places
    AllPlaces = [start, task1, task2, task3, join, end_place],
    Result = wf_cancel:apply_cancellation(Marking, AllPlaces),

    %% All places cleared
    lists:foreach(fun(Place) ->
        ?assertEqual([], maps:get(Place, Result))
    end, AllPlaces).

%%--------------------------------------------------------------------
%% @doc Test case cancellation with active parallel branches.
%%--------------------------------------------------------------------
case_cancel_parallel_execution_test() ->
    Marking = #{
        fork => [],
        branch1 => [active],
        branch2 => [active],
        branch3 => [active],
        join => []
    },

    AllPlaces = [fork, branch1, branch2, branch3, join],
    Result = wf_cancel:apply_cancellation(Marking, AllPlaces),

    %% All parallel branches cancelled
    ?assertEqual([], maps:get(branch1, Result)),
    ?assertEqual([], maps:get(branch2, Result)),
    ?assertEqual([], maps:get(branch3, Result)).

%%--------------------------------------------------------------------
%% @doc Test case cancellation with multiple instance tasks.
%%--------------------------------------------------------------------
case_cancel_multiple_instances_test() ->
    Marking = #{
        mi_start => [],
        mi_instance1 => [exec1],
        mi_instance2 => [exec2],
        mi_instance3 => [exec3],
        mi_join => []
    },

    AllPlaces = [mi_start, mi_instance1, mi_instance2, mi_instance3, mi_join],
    Result = wf_cancel:apply_cancellation(Marking, AllPlaces),

    %% All instances cancelled
    ?assertEqual([], maps:get(mi_instance1, Result)),
    ?assertEqual([], maps:get(mi_instance2, Result)),
    ?assertEqual([], maps:get(mi_instance3, Result)).

%%====================================================================
%% Test Functions - Cancel Propagation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cancel propagation from outer to inner scopes.
%%
%% Cancel propagation semantics:
%% - Cancelling outer scope cancels all nested scopes
%% - Cancel flag propagates down scope hierarchy
%% - No token firing occurs in cancelled scopes
%%--------------------------------------------------------------------
cancel_propagation_outer_to_inner_test() ->
    Marking = #{
        outer_scope => [token],
        level1_scope => [token],
        level2_scope => [token],
        level3_scope => [token]
    },

    %% Cancel outer scope propagates to all nested
    OuterScope = [outer_scope, level1_scope, level2_scope, level3_scope],
    Result = wf_cancel:cancel_region(Marking, OuterScope),

    %% All levels cancelled
    ?assertEqual([], maps:get(outer_scope, Result)),
    ?assertEqual([], maps:get(level1_scope, Result)),
    ?assertEqual([], maps:get(level2_scope, Result)),
    ?assertEqual([], maps:get(level3_scope, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancel propagation stops at scope boundary.
%%--------------------------------------------------------------------
cancel_propagation_boundary_test() ->
    Marking = #{
        scope_a_outer => [token_a],
        scope_a_inner => [token_a_inner],
        scope_b_outer => [token_b],
        scope_b_inner => [token_b_inner]
    },

    %% Cancel only scope_a
    ScopeA = [scope_a_outer, scope_a_inner],
    Result = wf_cancel:cancel_region(Marking, ScopeA),

    %% Scope A cancelled
    ?assertEqual([], maps:get(scope_a_outer, Result)),
    ?assertEqual([], maps:get(scope_a_inner, Result)),

    %% Scope B preserved
    ?assertEqual([token_b], maps:get(scope_b_outer, Result)),
    ?assertEqual([token_b_inner], maps:get(scope_b_inner, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancel propagation in parallel scopes.
%%--------------------------------------------------------------------
cancel_propagation_parallel_scopes_test() ->
    Marking = #{
        parent => [token],
        child_a => [token_a],
        child_b => [token_b],
        child_c => [token_c],
        grandchild_a1 => [token_a1],
        grandchild_a2 => [token_a2]
    },

    %% Cancel parent and child_a (includes grandchildren)
    CancelScope = [parent, child_a, grandchild_a1, grandchild_a2],
    Result = wf_cancel:cancel_region(Marking, CancelScope),

    %% Parent and child_a hierarchy cancelled
    ?assertEqual([], maps:get(parent, Result)),
    ?assertEqual([], maps:get(child_a, Result)),
    ?assertEqual([], maps:get(grandchild_a1, Result)),
    ?assertEqual([], maps:get(grandchild_a2, Result)),

    %% Sibling scopes preserved
    ?assertEqual([token_b], maps:get(child_b, Result)),
    ?assertEqual([token_c], maps:get(child_c, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancel propagation through XOR branches.
%%--------------------------------------------------------------------
cancel_propagation_xor_branches_test() ->
    Marking = #{
        xor_split => [],
        branch_a => [selected],
        branch_b => [],
        branch_c => [],
        xor_merge => []
    },

    %% Cancel entire XOR structure
    XorScope = [xor_split, branch_a, branch_b, branch_c, xor_merge],
    Result = wf_cancel:cancel_region(Marking, XorScope),

    %% All branches cancelled (even inactive ones)
    ?assertEqual([], maps:get(xor_split, Result)),
    ?assertEqual([], maps:get(branch_a, Result)),
    ?assertEqual([], maps:get(branch_b, Result)),
    ?assertEqual([], maps:get(branch_c, Result)),
    ?assertEqual([], maps:get(xor_merge, Result)).

%%====================================================================
%% Test Functions - Cancellation Soundness
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cancelled tokens never fire transitions.
%%
%% Soundness property: After cancellation, no tokens exist in
%% cancelled places to enable any transitions.
%%--------------------------------------------------------------------
cancel_soundness_no_token_firing_test() ->
    Marking = #{
        place1 => [token1, token2],
        place2 => [token3],
        place3 => []
    },

    %% Cancel place1 and place2
    Result = wf_cancel:apply_cancellation(Marking, [place1, place2]),

    %% Verify no tokens remain
    ?assertEqual([], maps:get(place1, Result)),
    ?assertEqual([], maps:get(place2, Result)),

    %% Verify empty places cannot enable transitions
    ?assertNot(has_tokens(maps:get(place1, Result))),
    ?assertNot(has_tokens(maps:get(place2, Result))).

%%--------------------------------------------------------------------
%% @doc Test cancelled region has no enabled transitions.
%%--------------------------------------------------------------------
cancel_soundness_region_no_enabled_test() ->
    %% Region with potential transition
    Marking = #{
        pre_place => [token],
        transition_place => [enabling_token],
        post_place => []
    },

    %% Cancel region including transition place
    Region = [pre_place, transition_place, post_place],
    Result = wf_cancel:cancel_region(Marking, Region),

    %% All places empty - no transitions enabled
    ?assertEqual([], maps:get(pre_place, Result)),
    ?assertEqual([], maps:get(transition_place, Result)),
    ?assertEqual([], maps:get(post_place, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancellation soundness with join points.
%%
%% Cancelled branches never contribute to join synchronization.
%%--------------------------------------------------------------------
cancel_soundness_join_no_sync_test() ->
    Marking = #{
        fork => [],
        branch1 => [token1],
        branch2 => [token2],
        branch3 => [token3],
        join => []
    },

    %% Cancel branch2
    Result = wf_cancel:apply_cancellation(Marking, [branch2]),

    %% Branch2 cancelled - cannot contribute to join
    ?assertEqual([], maps:get(branch2, Result)),

    %% Other branches still active
    ?assertEqual([token1], maps:get(branch1, Result)),
    ?assertEqual([token3], maps:get(branch3, Result)),

    %% Join point has no tokens from cancelled branch
    ?assertEqual([], maps:get(join, Result)).

%%--------------------------------------------------------------------
%% @doc Test soundness: cancelled scope prevents token creation.
%%
%% New tokens cannot be added to cancelled places.
%%--------------------------------------------------------------------
cancel_soundness_no_token_creation_test() ->
    Marking = #{
        source => [token],
        cancelled_target => [old_token]
    },

    %% Cancel target
    Result = wf_cancel:apply_cancellation(Marking, [cancelled_target]),

    %% Target cleared
    ?assertEqual([], maps:get(cancelled_target, Result)),

    %% Source preserved (not cancelled)
    ?assertEqual([token], maps:get(source, Result)),

    %% Verify cancelled place empty
    ?assertEqual(0, length(maps:get(cancelled_target, Result))).

%%--------------------------------------------------------------------
%% @doc Test soundness: cancel idempotence.
%%
%% Cancelling already cancelled scope is safe no-op.
%%--------------------------------------------------------------------
cancel_soundness_idempotent_test() ->
    Marking = #{
        place1 => [token1],
        place2 => [token2]
    },

    %% First cancellation
    Result1 = wf_cancel:apply_cancellation(Marking, [place1]),
    ?assertEqual([], maps:get(place1, Result1)),

    %% Second cancellation (idempotent)
    Result2 = wf_cancel:apply_cancellation(Result1, [place1]),
    ?assertEqual([], maps:get(place1, Result2)),

    %% Results identical
    ?assertEqual(Result1, Result2).

%%--------------------------------------------------------------------
%% @doc Test soundness: cancel order independence.
%%
%% Cancellation order does not affect final state.
%%--------------------------------------------------------------------
cancel_soundness_order_independence_test() ->
    Marking = #{
        place1 => [token1],
        place2 => [token2],
        place3 => [token3]
    },

    %% Cancel in order: place1, place2, place3
    Result1 = wf_cancel:apply_cancellation(Marking, [place1]),
    Result1b = wf_cancel:apply_cancellation(Result1, [place2]),
    Result1c = wf_cancel:apply_cancellation(Result1b, [place3]),

    %% Cancel in reverse order: place3, place2, place1
    Result2 = wf_cancel:apply_cancellation(Marking, [place3]),
    Result2b = wf_cancel:apply_cancellation(Result2, [place2]),
    Result2c = wf_cancel:apply_cancellation(Result2b, [place1]),

    %% Results identical regardless of order
    ?assertEqual(Result1c, Result2c),

    %% All cancelled
    ?assertEqual([], maps:get(place1, Result1c)),
    ?assertEqual([], maps:get(place2, Result1c)),
    ?assertEqual([], maps:get(place3, Result1c)).

%%====================================================================
%% Test Functions - Cancel Token Handling
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cancel token creation and validation.
%%--------------------------------------------------------------------
cancel_token_creation_test() ->
    %% Create cancel token
    Token = wf_cancel:create_cancel_token([place1, place2]),

    %% Validate structure
    ?assert(wf_cancel:is_cancel_token(Token)),
    ?assertEqual([place1, place2], wf_cancel:cancel_targets(Token)).

%%--------------------------------------------------------------------
%% @doc Test cancel token with single target.
%%--------------------------------------------------------------------
cancel_token_single_target_test() ->
    Token = wf_cancel:create_cancel_token(single_place),

    ?assert(wf_cancel:is_cancel_token(Token)),
    ?assertEqual([single_place], wf_cancel:cancel_targets(Token)).

%%--------------------------------------------------------------------
%% @doc Test invalid cancel tokens rejected.
%%--------------------------------------------------------------------
cancel_token_invalid_test() ->
    ?assertNot(wf_cancel:is_cancel_token({other, [place1]})),
    ?assertNot(wf_cancel:is_cancel_token(not_a_token)),
    ?assertNot(wf_cancel:is_cancel_token({cancel, not_a_list})),
    ?assertNot(wf_cancel:is_cancel_token({cancel, [place1, "invalid"]})).

%%--------------------------------------------------------------------
%% @doc Test extracting targets from cancel tokens.
%%--------------------------------------------------------------------
cancel_token_extract_targets_test() ->
    Token = wf_cancel:create_cancel_token([p1, p2, p3]),
    Targets = wf_cancel:cancel_targets(Token),

    ?assertEqual([p1, p2, p3], Targets),
    ?assertEqual(3, length(Targets)).

%%--------------------------------------------------------------------
%% @doc Test cancel token with empty target list.
%%--------------------------------------------------------------------
cancel_token_empty_targets_test() ->
    Token = wf_cancel:create_cancel_token([]),

    ?assert(wf_cancel:is_cancel_token(Token)),
    ?assertEqual([], wf_cancel:cancel_targets(Token)).

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Check if a place has any tokens.
%%--------------------------------------------------------------------
-spec has_tokens(TokenList :: [term()]) -> boolean().
has_tokens([]) -> false;
has_tokens([_|_]) -> true.
