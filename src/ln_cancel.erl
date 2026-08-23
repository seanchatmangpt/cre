%% -*- erlang -*-
%%%% @doc Linear Nesting Cancellation Manager
%%
%% This module provides hierarchical cancellation state management for workflow
%% execution. Scopes are organized in a tree structure where cancelling a parent
%% scope automatically cancels all descendant scopes.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Hierarchical scope cancellation with parent-child relationships</li>
%%   <li>Efficient descendant lookup for propagation</li>
%%   <li>Pure functional state management</li>
%%   <li>Integration with refusal guard system</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Initializing cancellation state:
%% ```erlang
%% > Cancel0 = ln_cancel:init().
%% {ln_cancel, #{}, #{}}
%%
%% > Cancel1 = ln_cancel:init(#{root => undefined, child1 => root, child2 => root}).
%% {ln_cancel, #{root => undefined, child1 => root, child2 => root}, #{}}
%% ```
%%
%% Cancelling a scope:
%% ```erlang
%% > {ok, Cancel2} = ln_cancel:cancel_scope(child1, Cancel1).
%% > ln_cancel:is_cancelled(child1, Cancel2).
%% true
%% '''
%%
%% Checking if a scope is cancelled (for guard evaluation):
%% ```erlang
%% > ln_cancel:check_cancel(child1, Cancel2).
%% {refused, forbidden_action, <<"Scope child1 is cancelled">>}
%%
%% > ln_cancel:check_cancel(child2, Cancel2).
%% pass
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_cancel).

%%====================================================================
%% Exports
%%====================================================================

%% Lifecycle
-export([init/0]).
-export([init/1]).

%% Cancellation operations
-export([cancel_scope/2]).

%% Guard integration
-export([check_cancel/2]).
-export([is_cancelled/2]).

%% Propagation
-export([propagate_cancel/2]).

%% Queries
-export([get_descendants/2]).

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cancellation state record.
%%
%% Tracks the hierarchical structure of scopes and their cancellation status.
%%
%% <ul>
%%   <li><b>scopes:</b> Map of scope_id => parent_id (undefined for root)</li>
%%   <li><b>scope_parents:</b> Inverse map for efficient parent lookup</li>
%%   <li><b>statuses:</b> Map of scope_id => status (active | cancelled)</li>
%% </ul>
%%--------------------------------------------------------------------
-record(ln_cancel, {
    scopes :: #{term() => term() | undefined},
    scope_parents :: #{term() => [term()]},
    statuses :: #{term() => status()}
}).

%%--------------------------------------------------------------------
%% @doc Cancellation status.
%%
%% Scopes are either active (can execute) or cancelled (forbidden).
%%--------------------------------------------------------------------
-type status() :: active | cancelled.

%%--------------------------------------------------------------------
%% @doc Cancellation state handle.
%%
%% Opaque record tracking hierarchical cancellation state.
%%--------------------------------------------------------------------
-opaque ln_cancel() :: #ln_cancel{}.

%%--------------------------------------------------------------------
%% @doc Scope identifier.
%%
%% Can be any Erlang term used to identify a scope.
%%--------------------------------------------------------------------
-type scope_id() :: term().

%%--------------------------------------------------------------------
%% @doc Check result for guard integration.
%%
%% Returns pass if scope is active, or refused tuple if cancelled.
%%--------------------------------------------------------------------
-type check_result() :: pass | {refused, forbidden_action, binary()}.

%% Export types
-export_type([ln_cancel/0, scope_id/0, status/0, check_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes a new empty cancellation state.
%%
%% Creates a cancellation tracker with no scopes. Use this when
%% scopes will be added dynamically during execution.
%%
%% ```erlang
%% > Cancel = ln_cancel:init().
%% #ln_cancel{scopes => #{}, scope_parents => #{}, statuses => #{}}
%% '''
%%
%% @returns New cancellation state
%%
%% @end
%%--------------------------------------------------------------------
-spec init() -> ln_cancel().

init() ->
    #ln_cancel{
        scopes = #{},
        scope_parents = #{},
        statuses = #{}
    }.

%%--------------------------------------------------------------------
%% @doc Initializes cancellation state with a scope hierarchy.
%%
%% Creates a cancellation tracker from a map of scope_id => parent_id.
%% The parent_id is undefined for root scopes. This function also builds
%% the inverse parent map for efficient descendant lookup.
%%
%% ```erlang
%% > Scopes = #{
%% ..     workflow => undefined,
%% ..     task1 => workflow,
%% ..     task2 => workflow,
%% ..     subtask => task1
%% .. },
%% > Cancel = ln_cancel:init(Scopes).
%% #ln_cancel{...}
%% '''
%%
%% @param Scopes Map of scope_id to parent_id (undefined for root)
%% @returns New cancellation state with hierarchy
%%
%% @end
%%--------------------------------------------------------------------
-spec init(#{scope_id() => scope_id() | undefined}) -> ln_cancel().

init(Scopes) when is_map(Scopes) ->
    %% Build inverse map: parent => [children]
    ScopeParents = build_parent_map(Scopes),
    #ln_cancel{
        scopes = Scopes,
        scope_parents = ScopeParents,
        statuses = initialize_statuses(maps:keys(Scopes))
    }.

%%--------------------------------------------------------------------
%% @doc Cancels a scope and all its descendants.
%%
%% Marks the specified scope as cancelled and propagates the cancellation
%% to all descendant scopes. Returns an error tuple if the scope doesn't exist.
%%
%% This is the primary cancellation operation. After calling this function,
%% check_cancel/2 will return {refused, forbidden_action, ...} for the
%% cancelled scope and all its descendants.
%%
%% ```erlang
%% > Cancel0 = ln_cancel:init(#{
%% ..     root => undefined,
%% ..     child1 => root,
%% ..     child2 => root,
%% ..     grandchild => child1
%% .. }),
%% > {ok, Cancel1} = ln_cancel:cancel_scope(child1, Cancel0).
%% > ln_cancel:is_cancelled(child1, Cancel1).
%% true
%% > ln_cancel:is_cancelled(grandchild, Cancel1).
%% true
%% > ln_cancel:is_cancelled(child2, Cancel1).
%% false
%% '''
%%
%% @param ScopeId The scope to cancel
%% @param Cancel Current cancellation state
%% @returns {ok, UpdatedCancel} or {error, unknown_scope}
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_scope(scope_id(), ln_cancel()) ->
          {ok, ln_cancel()} | {error, unknown_scope}.

cancel_scope(ScopeId, #ln_cancel{scopes = Scopes, statuses = Statuses} = Cancel) ->
    case maps:is_key(ScopeId, Scopes) of
        false ->
            {error, unknown_scope};
        true ->
            %% Get all descendants to cancel
            Descendants = get_descendants(ScopeId, Cancel),
            AllToCancel = [ScopeId | Descendants],

            %% Update statuses for all affected scopes
            NewStatuses = lists:foldl(fun(SId, Acc) ->
                Acc#{SId => cancelled}
            end, Statuses, AllToCancel),

            {ok, Cancel#ln_cancel{statuses = NewStatuses}}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a scope is cancelled (guard integration).
%%
%% Returns pass if the scope is active and can execute transitions.
%% Returns {refused, forbidden_action, Reason} if the scope or any
%% ancestor is cancelled.
%%
%% This function integrates with the yawl_refusal_guard system for
%% preventing transition execution in cancelled scopes.
%%
%% ```erlang
%% > Cancel0 = ln_cancel:init(#{root => undefined, child => root}),
%% > {ok, Cancel1} = ln_cancel:cancel_scope(child, Cancel0),
%% > ln_cancel:check_cancel(child, Cancel1).
%% {refused, forbidden_action, <<"Scope child is cancelled">>}
%%
%% > ln_cancel:check_cancel(root, Cancel1).
%% pass
%% '''
%%
%% @param ScopeId The scope to check
%% @param Cancel Current cancellation state
%% @returns pass | {refused, forbidden_action, binary()}
%%
%% @end
%%--------------------------------------------------------------------
-spec check_cancel(scope_id(), ln_cancel()) -> check_result().

check_cancel(ScopeId, #ln_cancel{statuses = Statuses} = Cancel) ->
    case maps:get(ScopeId, Statuses, undefined) of
        cancelled ->
            Reason = <<"Scope ", (scope_to_binary(ScopeId))/binary, " is cancelled">>,
            {refused, forbidden_action, Reason};
        active ->
            %% Check if any ancestor is cancelled
            Ancestors = get_ancestors(ScopeId, Cancel),
            case has_cancelled_ancestor(Ancestors, Statuses) of
                {true, CancelledAncestor} ->
                    Reason = <<"Scope ", (scope_to_binary(CancelledAncestor))/binary,
                               " (ancestor) is cancelled">>,
                    {refused, forbidden_action, Reason};
                false ->
                    pass
            end;
        undefined ->
            %% Scope doesn't exist - treat as not cancelled (fail open)
            pass
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a scope is cancelled.
%%
%% Returns true if the scope has been cancelled, false otherwise.
%% This is a simpler check than check_cancel/2 and doesn't consider
%% ancestors - use check_cancel/2 for full guard semantics.
%%
%% ```erlang
%% > Cancel0 = ln_cancel:init(#{s => undefined}),
%% > {ok, Cancel1} = ln_cancel:cancel_scope(s, Cancel0),
%% > ln_cancel:is_cancelled(s, Cancel1).
%% true
%% '''
%%
%% @param ScopeId The scope to check
%% @param Cancel Current cancellation state
%% @returns true if cancelled, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cancelled(scope_id(), ln_cancel()) -> boolean().

is_cancelled(ScopeId, #ln_cancel{statuses = Statuses}) ->
    maps:get(ScopeId, Statuses, active) =:= cancelled.

%%--------------------------------------------------------------------
%% @doc Propagates cancellation from a parent to all descendants.
%%
%% This is equivalent to cancel_scope/2 but can be used when you
%% explicitly want to propagate from an already-cancelled parent.
%% Useful for batch operations or state synchronization.
%%
%% ```erlang
%% > Cancel0 = ln_cancel:init(#{
%% ..     root => undefined,
%% ..     a => root,
%% ..     b => root,
%% ..     c => a
%% .. }),
%% > %% Manually mark root as cancelled
%% > Cancel1 = Cancel0#ln_cancel{statuses = #{root => cancelled, ...}},
%% > Cancel2 = ln_cancel:propagate_cancel(root, Cancel1).
%% > ln_cancel:is_cancelled(a, Cancel2).
%% true
%% > ln_cancel:is_cancelled(b, Cancel2).
%% true
%% '''
%%
%% @param ScopeId The cancelled scope to propagate from
%% @param Cancel Current cancellation state
%% @returns Updated cancellation state with descendants cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec propagate_cancel(scope_id(), ln_cancel()) -> ln_cancel().

propagate_cancel(ScopeId, #ln_cancel{statuses = Statuses} = Cancel) ->
    case maps:get(ScopeId, Statuses, active) of
        active ->
            Cancel;  %% Not cancelled - nothing to propagate
        cancelled ->
            Descendants = get_descendants(ScopeId, Cancel),
            NewStatuses = lists:foldl(fun(D, Acc) ->
                Acc#{D => cancelled}
            end, Statuses, Descendants),
            Cancel#ln_cancel{statuses = NewStatuses}
    end.

%%--------------------------------------------------------------------
%% @doc Gets all descendant scopes of a given scope.
%%
%% Returns a list of all descendant scope IDs in the hierarchy,
%% including nested descendants at any depth. The order is breadth-first.
%%
%% ```erlang
%% > Cancel = ln_cancel:init(#{
%% ..     root => undefined,
%% ..     a => root,
%% ..     b => root,
%% ..     a1 => a,
%% ..     a2 => a,
%% ..     a1_1 => a1
%% .. }),
%% > ln_cancel:get_descendants(root, Cancel).
%% [a, b, a1, a2, a1_1]
%%
%% > ln_cancel:get_descendants(a, Cancel).
%% [a1, a2, a1_1]
%% '''
%%
%% @param ScopeId The parent scope
%% @param Cancel Current cancellation state
%% @returns List of descendant scope IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec get_descendants(scope_id(), ln_cancel()) -> [scope_id()].

get_descendants(ScopeId, #ln_cancel{scope_parents = ScopeParents}) ->
    %% Breadth-first traversal of descendants
    Children = maps:get(ScopeId, ScopeParents, []),
    get_descendants_bfs(Children, ScopeParents, []).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Builds the inverse parent map (parent => [children]).
-spec build_parent_map(#{term() => term() | undefined}) -> #{term() => [term()]}.

build_parent_map(Scopes) ->
    maps:fold(fun
        (Child, undefined, Acc) ->
            %% Root scope - no parent
            Acc;
        (Child, Parent, Acc) ->
            %% Add child to parent's list
            Children = maps:get(Parent, Acc, []),
            Acc#{Parent => [Child | Children]}
    end, #{}, Scopes).

%% @private
%% @doc Initializes all scopes as active.
-spec initialize_statuses([term()]) -> #{term() => status()}.

initialize_statuses(ScopeIds) ->
    maps:from_list([{S, active} || S <- ScopeIds]).

%% @private
%% @doc Converts a scope ID to binary for error messages.
-spec scope_to_binary(term()) -> binary().

scope_to_binary(ScopeId) when is_binary(ScopeId) ->
    ScopeId;
scope_to_binary(ScopeId) when is_atom(ScopeId) ->
    atom_to_binary(ScopeId);
scope_to_binary(ScopeId) ->
    iolist_to_binary(io_lib:format("~p", [ScopeId])).

%% @private
%% @doc Gets all ancestors of a scope (from immediate parent to root).
-spec get_ancestors(term(), ln_cancel()) -> [term()].

get_ancestors(ScopeId, #ln_cancel{scopes = Scopes}) ->
    get_ancestors_recursive(ScopeId, Scopes, []).

%% @private
-spec get_ancestors_recursive(term(), #{term() => term() | undefined}, [term()]) -> [term()].

get_ancestors_recursive(ScopeId, Scopes, Acc) ->
    case maps:get(ScopeId, Scopes, undefined) of
        undefined ->
            %% Reached root or unknown scope
            lists:reverse(Acc);
        Parent ->
            get_ancestors_recursive(Parent, Scopes, [Parent | Acc])
    end.

%% @private
%% @doc Checks if any ancestor in the list is cancelled.
%% Returns {true, CancelledAncestor} or false.
-spec has_cancelled_ancestor([term()], #{term() => status()}) ->
          {true, term()} | false.

has_cancelled_ancestor([], _Statuses) ->
    false;
has_cancelled_ancestor([Ancestor | Rest], Statuses) ->
    case maps:get(Ancestor, Statuses, active) of
        cancelled -> {true, Ancestor};
        active -> has_cancelled_ancestor(Rest, Statuses)
    end.

%% @private
%% @doc Breadth-first traversal to collect all descendants.
-spec get_descendants_bfs([term()], #{term() => [term()]}, [term()]) -> [term()].

get_descendants_bfs([], _ScopeParents, Acc) ->
    lists:reverse(Acc);
get_descendants_bfs([Current | Rest], ScopeParents, Acc) ->
    Children = maps:get(Current, ScopeParents, []),
    get_descendants_bfs(Rest ++ Children, ScopeParents, [Current | Acc]).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test init/0 creates empty state
%%--------------------------------------------------------------------
init_empty_test() ->
    Cancel = init(),
    ?assertEqual(#{}, Cancel#ln_cancel.scopes),
    ?assertEqual(#{}, Cancel#ln_cancel.scope_parents),
    ?assertEqual(#{}, Cancel#ln_cancel.statuses).

%%--------------------------------------------------------------------
%% @doc Test init/1 builds correct hierarchy
%%--------------------------------------------------------------------
init_hierarchy_test() ->
    Scopes = #{root => undefined, a => root, b => root, c => a},
    Cancel = init(Scopes),
    ?assertEqual(Scopes, Cancel#ln_cancel.scopes),
    ?assertEqual([a, b], lists:sort(maps:get(root, Cancel#ln_cancel.scope_parents, []))),
    ?assertEqual([c], maps:get(a, Cancel#ln_cancel.scope_parents, [])),
    ?assertEqual(active, maps:get(root, Cancel#ln_cancel.statuses)),
    ?assertEqual(active, maps:get(a, Cancel#ln_cancel.statuses)),
    ?assertEqual(active, maps:get(c, Cancel#ln_cancel.statuses)).

%%--------------------------------------------------------------------
%% @doc Test cancel_scope on single scope
%%--------------------------------------------------------------------
cancel_scope_single_test() ->
    Cancel0 = init(#{root => undefined}),
    {ok, Cancel1} = cancel_scope(root, Cancel0),
    ?assertEqual(true, is_cancelled(root, Cancel1)),
    ?assertEqual(cancelled, maps:get(root, Cancel1#ln_cancel.statuses)).

%%--------------------------------------------------------------------
%% @doc Test cancel_scope propagates to descendants
%%--------------------------------------------------------------------
cancel_scope_propagates_test() ->
    Cancel0 = init(#{root => undefined, a => root, b => root, c => a}),
    {ok, Cancel1} = cancel_scope(root, Cancel0),
    ?assertEqual(true, is_cancelled(root, Cancel1)),
    ?assertEqual(true, is_cancelled(a, Cancel1)),
    ?assertEqual(true, is_cancelled(b, Cancel1)),
    ?assertEqual(true, is_cancelled(c, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test cancel_scope only affects subtree
%%--------------------------------------------------------------------
cancel_scope_subtree_test() ->
    Cancel0 = init(#{root => undefined, a => root, b => root, c => a}),
    {ok, Cancel1} = cancel_scope(a, Cancel0),
    ?assertEqual(false, is_cancelled(root, Cancel1)),
    ?assertEqual(false, is_cancelled(b, Cancel1)),
    ?assertEqual(true, is_cancelled(a, Cancel1)),
    ?assertEqual(true, is_cancelled(c, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test cancel_scope returns error for unknown scope
%%--------------------------------------------------------------------
cancel_scope_unknown_test() ->
    Cancel0 = init(),
    ?assertEqual({error, unknown_scope}, cancel_scope(unknown, Cancel0)).

%%--------------------------------------------------------------------
%% @doc Test check_cancel returns pass for active scope
%%--------------------------------------------------------------------
check_cancel_pass_test() ->
    Cancel = init(#{root => undefined}),
    ?assertEqual(pass, check_cancel(root, Cancel)).

%%--------------------------------------------------------------------
%% @doc Test check_cancel returns forbidden for cancelled scope
%%--------------------------------------------------------------------
check_cancel_forbidden_test() ->
    Cancel0 = init(#{root => undefined}),
    {ok, Cancel1} = cancel_scope(root, Cancel0),
    Result = check_cancel(root, Cancel1),
    ?assertMatch({refused, forbidden_action, _}, Result).

%%--------------------------------------------------------------------
%% @doc Test check_cancel returns forbidden for descendant of cancelled scope
%%--------------------------------------------------------------------
check_cancel_ancestor_cancelled_test() ->
    Cancel0 = init(#{root => undefined, child => root}),
    {ok, Cancel1} = cancel_scope(root, Cancel0),
    ?assertEqual(pass, check_cancel(root, Cancel1)),
    Result = check_cancel(child, Cancel1),
    ?assertMatch({refused, forbidden_action, _}, Result),
    {refused, _, Reason} = Result,
    ?assert(binary:match(Reason, <<"ancestor">>) =/= nomatch).

%%--------------------------------------------------------------------
%% @doc Test check_cancel passes for unknown scope (fail open)
%%--------------------------------------------------------------------
check_cancel_unknown_test() ->
    Cancel = init(),
    ?assertEqual(pass, check_cancel(unknown, Cancel)).

%%--------------------------------------------------------------------
%% @doc Test is_cancelled after cancel
%%--------------------------------------------------------------------
is_cancelled_test() ->
    Cancel0 = init(#{s => undefined}),
    ?assertEqual(false, is_cancelled(s, Cancel0)),
    {ok, Cancel1} = cancel_scope(s, Cancel0),
    ?assertEqual(true, is_cancelled(s, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test is_cancelled for unknown scope returns false
%%--------------------------------------------------------------------
is_cancelled_unknown_test() ->
    Cancel = init(),
    ?assertEqual(false, is_cancelled(unknown, Cancel)).

%%--------------------------------------------------------------------
%% @doc Test propagate_cancel from cancelled parent
%%--------------------------------------------------------------------
propagate_cancel_test() ->
    Cancel0 = init(#{root => undefined, a => root, b => root}),
    Cancel1 = Cancel0#ln_cancel{statuses = #{root => cancelled, a => active, b => active}},
    Cancel2 = propagate_cancel(root, Cancel1),
    ?assertEqual(true, is_cancelled(root, Cancel2)),
    ?assertEqual(true, is_cancelled(a, Cancel2)),
    ?assertEqual(true, is_cancelled(b, Cancel2)).

%%--------------------------------------------------------------------
%% @doc Test propagate_cancel is idempotent
%%--------------------------------------------------------------------
propagate_cancel_idempotent_test() ->
    Cancel0 = init(#{root => undefined, a => root}),
    {ok, Cancel1} = cancel_scope(root, Cancel0),
    Cancel2 = propagate_cancel(root, Cancel1),
    ?assertEqual(Cancel1#ln_cancel.statuses, Cancel2#ln_cancel.statuses).

%%--------------------------------------------------------------------
%% @doc Test get_descendants returns all descendants
%%--------------------------------------------------------------------
get_descendants_test() ->
    Cancel = init(#{root => undefined, a => root, b => root, c => a, d => c}),
    Descendants = get_descendants(root, Cancel),
    ?assertEqual(true, lists:member(a, Descendants)),
    ?assertEqual(true, lists:member(b, Descendants)),
    ?assertEqual(true, lists:member(c, Descendants)),
    ?assertEqual(true, lists:member(d, Descendants)),
    ?assertEqual(4, length(Descendants)).

%%--------------------------------------------------------------------
%% @doc Test get_descendants for leaf scope returns empty
%%--------------------------------------------------------------------
get_descendants_leaf_test() ->
    Cancel = init(#{root => undefined, leaf => root}),
    ?assertEqual([], get_descendants(leaf, Cancel)).

%%--------------------------------------------------------------------
%% @doc Test get_descendants for unknown scope returns empty
%%--------------------------------------------------------------------
get_descendants_unknown_test() ->
    Cancel = init(),
    ?assertEqual([], get_descendants(unknown, Cancel)).

%%--------------------------------------------------------------------
%% @doc Test get_ancestors returns full path to root
%%--------------------------------------------------------------------
get_ancestors_test() ->
    Cancel = init(#{root => undefined, a => root, b => a, c => b}),
    ?assertEqual([], get_ancestors(root, Cancel)),
    ?assertEqual([root], get_ancestors(a, Cancel)),
    ?assertEqual([a, root], get_ancestors(b, Cancel)),
    ?assertEqual([b, a, root], get_ancestors(c, Cancel)).

%%--------------------------------------------------------------------
%% @doc Test guarantee: after cancel_scope, check_cancel returns forbidden
%%--------------------------------------------------------------------
guarantee_cancel_forbids_test() ->
    Cancel0 = init(#{root => undefined, child => root}),
    {ok, Cancel1} = cancel_scope(child, Cancel0),
    ?assertMatch({refused, forbidden_action, _}, check_cancel(child, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test guarantee: cancel affects entire subtree
%%--------------------------------------------------------------------
guarantee_subtree_cancelled_test() ->
    Cancel0 = init(#{root => undefined, a => root, b => a, c => b, d => root}),
    {ok, Cancel1} = cancel_scope(a, Cancel0),
    ?assertEqual(false, is_cancelled(root, Cancel1)),
    ?assertEqual(false, is_cancelled(d, Cancel1)),
    ?assertEqual(true, is_cancelled(a, Cancel1)),
    ?assertEqual(true, is_cancelled(b, Cancel1)),
    ?assertEqual(true, is_cancelled(c, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test cancellation with multiple roots
%%--------------------------------------------------------------------
multiple_roots_test() ->
    Cancel0 = init(#{root1 => undefined, root2 => undefined, c1 => root1, c2 => root2}),
    {ok, Cancel1} = cancel_scope(root1, Cancel0),
    ?assertEqual(true, is_cancelled(root1, Cancel1)),
    ?assertEqual(true, is_cancelled(c1, Cancel1)),
    ?assertEqual(false, is_cancelled(root2, Cancel1)),
    ?assertEqual(false, is_cancelled(c2, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test complex hierarchy cancellation
%%--------------------------------------------------------------------
complex_hierarchy_test() ->
    Scopes = #{
        workflow => undefined,
        stage1 => workflow,
        stage2 => workflow,
        task1_1 => stage1,
        task1_2 => stage1,
        task2_1 => stage2,
        subtask => task1_1
    },
    Cancel0 = init(Scopes),
    {ok, Cancel1} = cancel_scope(stage1, Cancel0),

    ?assertEqual(false, is_cancelled(workflow, Cancel1)),
    ?assertEqual(true, is_cancelled(stage1, Cancel1)),
    ?assertEqual(false, is_cancelled(stage2, Cancel1)),
    ?assertEqual(true, is_cancelled(task1_1, Cancel1)),
    ?assertEqual(true, is_cancelled(task1_2, Cancel1)),
    ?assertEqual(false, is_cancelled(task2_1, Cancel1)),
    ?assertEqual(true, is_cancelled(subtask, Cancel1)).

%%--------------------------------------------------------------------
%% @doc Test cancel is irreversible
%%--------------------------------------------------------------------
cancel_irreversible_test() ->
    Cancel0 = init(#{s => undefined}),
    {ok, Cancel1} = cancel_scope(s, Cancel0),
    ?assertEqual(true, is_cancelled(s, Cancel1)),
    %% Cancel again - no change
    {ok, Cancel2} = cancel_scope(s, Cancel1),
    ?assertEqual(true, is_cancelled(s, Cancel2)),
    ?assertEqual(Cancel1#ln_cancel.statuses, Cancel2#ln_cancel.statuses).

%%--------------------------------------------------------------------
%% @doc Test deep hierarchy propagation
%%--------------------------------------------------------------------
deep_hierarchy_test() ->
    Scopes = lists:foldl(fun(N, Acc) ->
        Parent = if N > 0 -> N - 1; true -> undefined end,
        Acc#{N => Parent}
    end, #{}, lists:seq(0, 100)),
    Cancel0 = init(Scopes),
    {ok, Cancel1} = cancel_scope(0, Cancel0),
    %% All scopes should be cancelled
    lists:foreach(fun(N) ->
        ?assertEqual(true, is_cancelled(N, Cancel1), {scope, N})
    end, lists:seq(0, 100)).

%%--------------------------------------------------------------------
%% @doc Test breadth-first descendant order
%%--------------------------------------------------------------------
descendant_bfs_order_test() ->
    Cancel = init(#{root => undefined, a => root, b => root, a1 => a, a2 => a, b1 => b}),
    Descendants = get_descendants(root, Cancel),
    %% BFS should visit a and b before their children
    ?assertEqual([a, b, a1, a2, b1], Descendants).

-endif.
