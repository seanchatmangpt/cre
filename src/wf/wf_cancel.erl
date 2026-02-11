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

-module(wf_cancel).
-moduledoc """
Cancellation semantics for WF Substrate bytecode VM.

Implements cancellation flag propagation, scope cleanup, and soundness
guarantees for workflow cancellation. Cancellation is hierarchical:
when a scope is cancelled, all nested scopes are also cancelled.

According to WF_ARCHITECTURE.md:

- Activity cancellation: Cancels a single activity/task
- Region cancellation: Cancels all activities in a region (cascading)
- Case cancellation: Cancels the entire workflow case (root scope)
- Soundness: Cancelled tokens never fire transitions

```erlang
%% Cancel a specific scope
State = wf_vm:exec_state(Program, PC, Stack, Ctx, #{}, #{}, []).
State1 = wf_cancel:cancel_scope(State, scope_id).
CancelFlags = wf_vm:exec_cancel(State1).
true = wf_cancel:is_cancelled(scope_id, CancelFlags).

%% Check if any scope in path is cancelled
Stack = [{seq, seq1, _}, {cancel_scope, region1, _}].
Scope = [seq1, region1].
true = wf_cancel:is_scope_cancelled(Scope, #{region1 => true}).

%% Cancel case (root scope)
State2 = wf_cancel:cancel_case(State).
true = wf_cancel:is_case_cancelled(State2).

%% Cascade cancellation to nested scopes
CancelFlags = #{parent => true}.
Stack = [{cancel_scope, child, _}, {cancel_scope, parent, _}].
true = wf_cancel:should_cancel_scope(child, Stack, CancelFlags).
```

<h3>Cancellation Semantics</h3>

1. **Activity Cancellation**: Sets cancel flag for single scope ID
2. **Region Cancellation**: Cascades to all scopes in region hierarchy
3. **Case Cancellation**: Sets root case cancel flag
4. **Scope Cleanup**: Removes cancelled scope frames from stack
5. **Soundness**: Cancelled tokens never enable transitions

<h3>Cancel Flag Propagation</h3>

Cancel flags are stored in exec_state.cancel as a map #{ScopeId => bool}.
When a scope is cancelled, all child scopes in the stack hierarchy
inherit the cancellation flag.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Scope cancellation
-export([
    cancel_scope/2,
    cancel_activity/2,
    cancel_region/3,
    cancel_case/1
]).

%% Cancellation queries
-export([
    is_cancelled/2,
    is_scope_cancelled/2,
    is_case_cancelled/1,
    should_cancel_scope/3
]).

%% Scope cleanup
-export([
    cleanup_cancelled_scopes/1,
    remove_scope_flag/2
]).

%% Stack introspection
-export([
    find_scope_in_stack/2,
    get_nested_scopes/2,
    get_parent_scopes/2
]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A scope identifier (activity, region, or case ID).
%%
%% Scope IDs are atoms that uniquely identify cancellation boundaries.
%%--------------------------------------------------------------------
-type scope_id() :: atom().

%%--------------------------------------------------------------------
%% @doc A scope path is a list of scope IDs from innermost to outermost.
%%
%% Used to check if any ancestor scope is cancelled.
%%--------------------------------------------------------------------
-type scope_path() :: [scope_id()].

%%--------------------------------------------------------------------
%% @doc Cancel flags map scope IDs to cancellation status.
%%
%% This is the cancel field in exec_state: #{ScopeId => boolean()}.
%% true = scope is cancelled, false or missing = not cancelled.
%%--------------------------------------------------------------------
-type cancel_flags() :: #{scope_id() => boolean()}.

%%--------------------------------------------------------------------
%% @doc Region specification for region cancellation.
%%
%% Identifies a named region that may contain multiple scopes.
%%--------------------------------------------------------------------
-type region_spec() :: {region, atom(), [scope_id()]}.

%% Export types
-export_type([scope_id/0, scope_path/0, cancel_flags/0, region_spec/0]).

%%====================================================================
%% Scope Cancellation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cancel a specific scope by setting its cancel flag.
%%
%% Sets the cancel flag for the specified scope ID in the exec_state.
%% The scope will be checked for cancellation on the next reduction step.
%%
%% @param State The current execution state
%% @param ScopeId The scope identifier to cancel
%% @return Updated execution state with cancel flag set
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_scope(State :: wf_vm:exec_state(), ScopeId :: scope_id()) ->
    wf_vm:exec_state().

cancel_scope(State, ScopeId) when is_atom(ScopeId) ->
    CancelFlags = wf_vm:exec_cancel(State),
    NewCancelFlags = CancelFlags#{ScopeId => true},
    wf_vm:exec_set_cancel(State, NewCancelFlags).

%%--------------------------------------------------------------------
%% @doc Cancel a specific activity (alias for cancel_scope).
%%
%% Activities are the finest-grained cancellation unit. This is
%% a semantic alias for cancel_scope/2.
%%
%% @param State The current execution state
%% @param ActivityId The activity identifier to cancel
%% @return Updated execution state with cancel flag set
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_activity(State :: wf_vm:exec_state(), ActivityId :: scope_id()) ->
    wf_vm:exec_state().

cancel_activity(State, ActivityId) ->
    cancel_scope(State, ActivityId).

%%--------------------------------------------------------------------
%% @doc Cancel a region and all scopes within it (cascading).
%%
%% Region cancellation cascades to all scopes contained in the region.
%% All scope IDs in the region's scope list are marked as cancelled.
%%
%% @param State The current execution state
%% @param RegionId The region identifier
%% @param ScopeIds List of scope IDs in the region
%% @return Updated execution state with all region scopes cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_region(
    State :: wf_vm:exec_state(),
    RegionId :: atom(),
    ScopeIds :: [scope_id()]
) -> wf_vm:exec_state().

cancel_region(State, RegionId, ScopeIds) when is_atom(RegionId), is_list(ScopeIds) ->
    CancelFlags = wf_vm:exec_cancel(State),
    %% Mark region itself as cancelled
    CancelFlags1 = CancelFlags#{RegionId => true},
    %% Mark all contained scopes as cancelled
    NewCancelFlags = lists:foldl(fun(ScopeId, Acc) ->
        Acc#{ScopeId => true}
    end, CancelFlags1, ScopeIds),
    wf_vm:exec_set_cancel(State, NewCancelFlags).

%%--------------------------------------------------------------------
%% @doc Cancel the entire case (root scope cancellation).
%%
%% Case cancellation is the highest-level cancellation. Sets the
%% special 'root_case' flag which causes the entire execution to halt.
%%
%% @param State The current execution state
%% @return Updated execution state with root case cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_case(State :: wf_vm:exec_state()) -> wf_vm:exec_state().

cancel_case(State) ->
    cancel_scope(State, root_case).

%%====================================================================
%% Cancellation Query Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Check if a specific scope is cancelled.
%%
%% Returns true if the scope ID is present in cancel flags and set to true.
%%
%% @param ScopeId The scope identifier to check
%% @param CancelFlags The cancel flags map from exec_state
%% @return true if scope is cancelled, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cancelled(ScopeId :: scope_id(), CancelFlags :: cancel_flags()) ->
    boolean().

is_cancelled(ScopeId, CancelFlags) when is_atom(ScopeId), is_map(CancelFlags) ->
    maps:get(ScopeId, CancelFlags, false) =:= true.

%%--------------------------------------------------------------------
%% @doc Check if any scope in a scope path is cancelled.
%%
%% Checks the cancellation status of all scopes in the path (innermost
%% to outermost). Returns true if any scope in the path is cancelled,
%% implementing hierarchical cancellation semantics.
%%
%% @param ScopePath List of scope IDs from innermost to outermost
%% @param CancelFlags The cancel flags map from exec_state
%% @return true if any scope in path is cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec is_scope_cancelled(
    ScopePath :: scope_path(),
    CancelFlags :: cancel_flags()
) -> boolean().

is_scope_cancelled([], _CancelFlags) ->
    false;
is_scope_cancelled([ScopeId | Rest], CancelFlags) ->
    case is_cancelled(ScopeId, CancelFlags) of
        true -> true;
        false -> is_scope_cancelled(Rest, CancelFlags)
    end.

%%--------------------------------------------------------------------
%% @doc Check if the entire case is cancelled.
%%
%% Returns true if the root_case cancel flag is set.
%%
%% @param State The current execution state
%% @return true if case is cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec is_case_cancelled(State :: wf_vm:exec_state()) -> boolean().

is_case_cancelled(State) ->
    CancelFlags = wf_vm:exec_cancel(State),
    is_cancelled(root_case, CancelFlags).

%%--------------------------------------------------------------------
%% @doc Determine if a scope should be cancelled based on parent hierarchy.
%%
%% Checks if the scope or any of its parent scopes are cancelled.
%% This implements cascading cancellation: if a parent is cancelled,
%% all children are implicitly cancelled.
%%
%% @param ScopeId The scope to check
%% @param Stack The execution stack (used to find parent scopes)
%% @param CancelFlags The cancel flags map
%% @return true if scope should be cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec should_cancel_scope(
    ScopeId :: scope_id(),
    Stack :: [wf_vm:stack_frame()],
    CancelFlags :: cancel_flags()
) -> boolean().

should_cancel_scope(ScopeId, Stack, CancelFlags) ->
    %% Check if this scope is directly cancelled
    case is_cancelled(ScopeId, CancelFlags) of
        true -> true;
        false ->
            %% Check if any parent scope is cancelled
            ParentScopes = get_parent_scopes(ScopeId, Stack),
            is_scope_cancelled(ParentScopes, CancelFlags)
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit test runner for the module.
%% Tests the doctest examples from the moduledoc.
%%--------------------------------------------------------------------
doctest_test() ->
    %% Test is_cancel_token/1
    ?assertEqual(true, is_cancel_token({cancel, [p1, p2]})),
    ?assertEqual(false, is_cancel_token({other, tuple})),

    %% Test create_cancel_token/1
    Token = create_cancel_token([region_place1, region_place2]),
    ?assertEqual({cancel, [region_place1, region_place2]}, Token),

    %% Test cancel_targets/1
    ?assertEqual([region_place1, region_place2], cancel_targets(Token)),

    %% Test apply_cancellation/2
    Marking1 = #{p1 => [a], p2 => [b], p3 => [c]},
    ?assertEqual(#{p1 => [], p2 => [], p3 => [c]}, apply_cancellation(Marking1, [p1, p2])),

    %% Test cancel_region/2
    Marking2 = #{p1 => [a], p2 => [b], p3 => [c], p4 => [d]},
    Region = [p2, p3],
    ?assertEqual(#{p1 => [a], p2 => [], p3 => [], p4 => [d]}, cancel_region(Marking2, Region)),

    %% Test is_cancellation_set/1
    ?assertEqual(true, is_cancellation_set([p1, p2, p3])),
    ?assertEqual(false, is_cancellation_set(not_a_list)),

    ok.

%%--------------------------------------------------------------------
%% @doc Test is_cancel_token/1 with various inputs.
%%--------------------------------------------------------------------
is_cancel_token_valid_test() ->
    ?assertEqual(true, is_cancel_token({cancel, []})),
    ?assertEqual(true, is_cancel_token({cancel, [p1]})),
    ?assertEqual(true, is_cancel_token({cancel, [p1, p2, p3]})),
    ?assertEqual(false, is_cancel_token({cancel, "not_a_list"})),
    ?assertEqual(false, is_cancel_token({other, [p1]})),
    ?assertEqual(false, is_cancel_token(not_a_tuple)),
    ?assertEqual(false, is_cancel_token({cancel, [p1, "not_atom"]})).

%%--------------------------------------------------------------------
%% @doc Test create_cancel_token/1 with various inputs.
%%--------------------------------------------------------------------
create_cancel_token_test() ->
    ?assertEqual({cancel, [p1]}, create_cancel_token(p1)),
    ?assertEqual({cancel, [p1, p2]}, create_cancel_token([p1, p2])),
    ?assertEqual({cancel, []}, create_cancel_token([])).

%%--------------------------------------------------------------------
%% @doc Test cancel_targets/1 with various inputs.
%%--------------------------------------------------------------------
cancel_targets_test() ->
    ?assertEqual([p1, p2], cancel_targets({cancel, [p1, p2]})),
    ?assertEqual([], cancel_targets({cancel, []})),
    ?assertEqual([], cancel_targets(not_a_token)),
    ?assertEqual([], cancel_targets({other, [p1]})).

%%--------------------------------------------------------------------
%% @doc Test apply_cancellation/2 with various inputs.
%%--------------------------------------------------------------------
apply_cancellation_test() ->
    %% Empty cancellation set
    Marking = #{p1 => [a], p2 => [b]},
    ?assertEqual(Marking, apply_cancellation(Marking, [])),

    %% Single place cancellation
    ?assertEqual(#{p1 => [], p2 => [b]}, apply_cancellation(Marking, [p1])),

    %% Multiple place cancellation
    ?assertEqual(#{p1 => [], p2 => []}, apply_cancellation(Marking, [p1, p2])),

    %% Non-existent places in set
    ?assertEqual(#{p1 => [], p2 => [b], p3 => []}, apply_cancellation(Marking, [p1, p3])),

    %% Empty marking - cancellation adds new places with empty tokens
    ?assertEqual(#{p1 => []}, apply_cancellation(#{}, [p1])),

    %% Cancellation with multiple new places
    ?assertEqual(#{p1 => [], p2 => []}, apply_cancellation(#{}, [p1, p2])).

%%--------------------------------------------------------------------
%% @doc Test cancel_region/2 is an alias for apply_cancellation/2.
%%--------------------------------------------------------------------
cancel_region_test() ->
    Marking = #{a => [1], b => [2], c => [3]},
    Region = [b, c],
    ?assertEqual(#{a => [1], b => [], c => []}, cancel_region(Marking, Region)),
    ?assertEqual(apply_cancellation(Marking, Region), cancel_region(Marking, Region)).

%%--------------------------------------------------------------------
%% @doc Test is_cancellation_set/1 with various inputs.
%%--------------------------------------------------------------------
is_cancellation_set_test() ->
    %% Valid sets
    ?assertEqual(true, is_cancellation_set([p1])),
    ?assertEqual(true, is_cancellation_set([p1, p2, p3])),
    ?assertEqual(true, is_cancellation_set([])),  % Empty list is valid

    %% Invalid sets
    ?assertEqual(false, is_cancellation_set(not_a_list)),
    ?assertEqual(false, is_cancellation_set([p1, "not_atom"])),
    ?assertEqual(false, is_cancellation_set([p1, 123])),
    ?assertEqual(false, is_cancellation_set([p1, {tuple, here}])).

%%--------------------------------------------------------------------
%% @doc Test that cancellation preserves non-target places.
%%--------------------------------------------------------------------
cancellation_preservation_test() ->
    Marking = #{
        p1 => [a, b, c],
        p2 => [d],
        p3 => [],
        p4 => [e, f]
    },
    ?assertEqual(
        #{p1 => [], p2 => [d], p3 => [], p4 => [e, f]},
        apply_cancellation(Marking, [p1, p3])
    ).

%%--------------------------------------------------------------------
%% @doc test cancel token with complex place names.
%%--------------------------------------------------------------------
complex_place_names_test() ->
    %% Test with atoms that have different forms
    Token = create_cancel_token(['place-1', 'place_2', 'place.3']),
    ?assertEqual(true, is_cancel_token(Token)),
    ?assertEqual(['place-1', 'place_2', 'place.3'], cancel_targets(Token)).

-endif.
