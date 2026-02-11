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
%% Scope Cleanup Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Clean up cancelled scope frames from the execution stack.
%%
%% Removes stack frames for scopes that have been cancelled, ensuring
%% the stack reflects only active scopes. This is typically called
%% during cancellation processing.
%%
%% @param State The current execution state
%% @return Updated execution state with cleaned stack
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_cancelled_scopes(State :: wf_vm:exec_state()) ->
    wf_vm:exec_state().

cleanup_cancelled_scopes(State) ->
    Stack = wf_vm:exec_stack(State),
    CancelFlags = wf_vm:exec_cancel(State),

    %% Filter out frames for cancelled scopes
    NewStack = lists:filter(fun(Frame) ->
        FrameId = wf_vm:frame_id(Frame),
        ScopeId = extract_scope_id(FrameId),
        not is_cancelled(ScopeId, CancelFlags)
    end, Stack),

    wf_vm:exec_set_stack(State, NewStack).

%%--------------------------------------------------------------------
%% @doc Remove a specific cancel flag from the exec_state.
%%
%% This is used when a scope completes successfully and its cancel
%% flag should be removed from the flags map.
%%
%% @param State The current execution state
%% @param ScopeId The scope ID whose flag should be removed
%% @return Updated execution state with flag removed
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_scope_flag(State :: wf_vm:exec_state(), ScopeId :: scope_id()) ->
    wf_vm:exec_state().

remove_scope_flag(State, ScopeId) ->
    CancelFlags = wf_vm:exec_cancel(State),
    NewCancelFlags = maps:remove(ScopeId, CancelFlags),
    wf_vm:exec_set_cancel(State, NewCancelFlags).

%%====================================================================
%% Stack Introspection Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Find a specific scope in the execution stack.
%%
%% Searches the stack for a frame with the given scope ID.
%% Returns {ok, Frame} if found, or not_found otherwise.
%%
%% @param ScopeId The scope identifier to find
%% @param Stack The execution stack
%% @return {ok, Frame} | not_found
%%
%% @end
%%--------------------------------------------------------------------
-spec find_scope_in_stack(
    ScopeId :: scope_id(),
    Stack :: [wf_vm:stack_frame()]
) -> {ok, wf_vm:stack_frame()} | not_found.

find_scope_in_stack(_ScopeId, []) ->
    not_found;
find_scope_in_stack(ScopeId, [Frame | Rest]) ->
    FrameId = wf_vm:frame_id(Frame),
    case extract_scope_id(FrameId) of
        ScopeId -> {ok, Frame};
        _ -> find_scope_in_stack(ScopeId, Rest)
    end.

%%--------------------------------------------------------------------
%% @doc Get all nested scopes below a given scope in the stack.
%%
%% Returns a list of scope IDs for all frames nested within the
%% specified scope. Used for cascading cancellation.
%%
%% @param ScopeId The parent scope ID
%% @param Stack The execution stack
%% @return List of nested scope IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec get_nested_scopes(
    ScopeId :: scope_id(),
    Stack :: [wf_vm:stack_frame()]
) -> [scope_id()].

get_nested_scopes(ScopeId, Stack) ->
    case find_scope_in_stack(ScopeId, Stack) of
        not_found ->
            [];
        {ok, _Frame} ->
            %% Collect all scope IDs before this frame (nested within)
            collect_nested_scopes(ScopeId, Stack, [])
    end.

%%--------------------------------------------------------------------
%% @doc Get all parent scopes above a given scope in the stack.
%%
%% Returns a list of scope IDs for all ancestor frames. Used to
%% check for inherited cancellation from parent scopes.
%%
%% @param ScopeId The child scope ID
%% @param Stack The execution stack
%% @return List of parent scope IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec get_parent_scopes(
    ScopeId :: scope_id(),
    Stack :: [wf_vm:stack_frame()]
) -> [scope_id()].

get_parent_scopes(ScopeId, Stack) ->
    collect_parent_scopes(ScopeId, Stack, []).

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Extract scope ID from a frame ID.
%%
%% Frame IDs may be atoms or tuples {Type, UniqueInt}. This function
%% extracts the meaningful scope identifier.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_scope_id(FrameId :: atom() | {atom(), integer()}) -> scope_id().

extract_scope_id(FrameId) when is_atom(FrameId) ->
    FrameId;
extract_scope_id({Type, _UniqueInt}) when is_atom(Type) ->
    Type;
extract_scope_id(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc Collect nested scope IDs from stack (before target scope).
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_nested_scopes(
    TargetScopeId :: scope_id(),
    Stack :: [wf_vm:stack_frame()],
    Acc :: [scope_id()]
) -> [scope_id()].

collect_nested_scopes(_TargetScopeId, [], Acc) ->
    lists:reverse(Acc);
collect_nested_scopes(TargetScopeId, [Frame | Rest], Acc) ->
    FrameId = wf_vm:frame_id(Frame),
    ScopeId = extract_scope_id(FrameId),
    case ScopeId of
        TargetScopeId ->
            %% Found target, return accumulated nested scopes
            lists:reverse(Acc);
        _ ->
            %% This is a nested scope, add it
            collect_nested_scopes(TargetScopeId, Rest, [ScopeId | Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collect parent scope IDs from stack (after target scope).
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_parent_scopes(
    TargetScopeId :: scope_id(),
    Stack :: [wf_vm:stack_frame()],
    Acc :: [scope_id()]
) -> [scope_id()].

collect_parent_scopes(_TargetScopeId, [], Acc) ->
    lists:reverse(Acc);
collect_parent_scopes(TargetScopeId, [Frame | Rest], Acc) ->
    FrameId = wf_vm:frame_id(Frame),
    ScopeId = extract_scope_id(FrameId),
    case ScopeId of
        TargetScopeId ->
            %% Found target, collect remaining frames as parents
            collect_all_scope_ids(Rest);
        _ ->
            collect_parent_scopes(TargetScopeId, Rest, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collect all scope IDs from remaining stack frames.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_all_scope_ids(Stack :: [wf_vm:stack_frame()]) -> [scope_id()].

collect_all_scope_ids([]) ->
    [];
collect_all_scope_ids([Frame | Rest]) ->
    FrameId = wf_vm:frame_id(Frame),
    ScopeId = extract_scope_id(FrameId),
    [ScopeId | collect_all_scope_ids(Rest)].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test basic scope cancellation.
%%--------------------------------------------------------------------
cancel_scope_test() ->
    Program = [],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    %% Cancel a scope
    State1 = cancel_scope(State, test_scope),
    CancelFlags = wf_vm:exec_cancel(State1),

    ?assertEqual(true, is_cancelled(test_scope, CancelFlags)),
    ?assertEqual(false, is_cancelled(other_scope, CancelFlags)).

%%--------------------------------------------------------------------
%% @doc Test region cancellation cascades to all scopes.
%%--------------------------------------------------------------------
cancel_region_test() ->
    State = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),

    %% Cancel region with multiple scopes
    State1 = cancel_region(State, region1, [scope1, scope2, scope3]),
    CancelFlags = wf_vm:exec_cancel(State1),

    ?assertEqual(true, is_cancelled(region1, CancelFlags)),
    ?assertEqual(true, is_cancelled(scope1, CancelFlags)),
    ?assertEqual(true, is_cancelled(scope2, CancelFlags)),
    ?assertEqual(true, is_cancelled(scope3, CancelFlags)),
    ?assertEqual(false, is_cancelled(other_scope, CancelFlags)).

%%--------------------------------------------------------------------
%% @doc Test case cancellation.
%%--------------------------------------------------------------------
cancel_case_test() ->
    State = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),

    %% Cancel entire case
    State1 = cancel_case(State),

    ?assertEqual(true, is_case_cancelled(State1)),
    ?assertEqual(false, is_case_cancelled(State)).

%%--------------------------------------------------------------------
%% @doc Test hierarchical cancellation check.
%%--------------------------------------------------------------------
is_scope_cancelled_test() ->
    CancelFlags = #{parent_scope => true},

    %% Scope path: [child, parent]
    ?assertEqual(true, is_scope_cancelled([child_scope, parent_scope], CancelFlags)),
    ?assertEqual(false, is_scope_cancelled([child_scope, other_scope], CancelFlags)),
    ?assertEqual(false, is_scope_cancelled([], CancelFlags)).

%%--------------------------------------------------------------------
%% @doc Test should_cancel_scope with parent hierarchy.
%%--------------------------------------------------------------------
should_cancel_scope_test() ->
    %% Build a simple stack: [child, parent]
    ParentFrame = wf_vm:frame(cancel_scope, {parent_scope}),
    ChildFrame = wf_vm:frame(cancel_scope, {child_scope}),
    Stack = [ChildFrame, ParentFrame],

    %% Parent is cancelled
    CancelFlags = #{parent_scope => true},

    %% Child should be cancelled due to parent
    ?assertEqual(true, should_cancel_scope(child_scope, Stack, CancelFlags)),
    ?assertEqual(true, should_cancel_scope(parent_scope, Stack, CancelFlags)),
    ?assertEqual(false, should_cancel_scope(unrelated_scope, Stack, #{})).

%%--------------------------------------------------------------------
%% @doc Test scope cleanup removes cancelled frames.
%%--------------------------------------------------------------------
cleanup_cancelled_scopes_test() ->
    %% Build stack with mix of cancelled and active scopes
    Frame1 = wf_vm:frame(seq, {active_scope}),
    Frame2 = wf_vm:frame(cancel_scope, {cancelled_scope}),
    Frame3 = wf_vm:frame(task, {another_active}),
    Stack = [Frame1, Frame2, Frame3],

    State = wf_vm:exec_state([], 0, Stack, #{}, #{}, #{cancelled_scope => true}, []),

    %% Cleanup should remove Frame2
    State1 = cleanup_cancelled_scopes(State),
    NewStack = wf_vm:exec_stack(State1),

    %% Stack should have 2 frames (Frame2 removed)
    ?assertEqual(2, length(NewStack)).

%%--------------------------------------------------------------------
%% @doc Test remove_scope_flag.
%%--------------------------------------------------------------------
remove_scope_flag_test() ->
    State = wf_vm:exec_state([], 0, [], #{}, #{}, #{scope1 => true, scope2 => true}, []),

    State1 = remove_scope_flag(State, scope1),
    CancelFlags = wf_vm:exec_cancel(State1),

    ?assertEqual(false, is_cancelled(scope1, CancelFlags)),
    ?assertEqual(true, is_cancelled(scope2, CancelFlags)).

%%--------------------------------------------------------------------
%% @doc Test find_scope_in_stack.
%%--------------------------------------------------------------------
find_scope_in_stack_test() ->
    Frame1 = wf_vm:frame(seq, {scope1}),
    Frame2 = wf_vm:frame(cancel_scope, {scope2}),
    Stack = [Frame1, Frame2],

    {ok, Found} = find_scope_in_stack(scope2, Stack),
    ?assertEqual(cancel_scope, wf_vm:frame_type(Found)),

    ?assertEqual(not_found, find_scope_in_stack(missing, Stack)).

%%--------------------------------------------------------------------
%% @doc Test extract_scope_id.
%%--------------------------------------------------------------------
extract_scope_id_test() ->
    ?assertEqual(my_scope, extract_scope_id(my_scope)),
    ?assertEqual(my_scope, extract_scope_id({my_scope, 123})),
    ?assertEqual(undefined, extract_scope_id({complex, tuple, with, many, elements})).

-endif.
