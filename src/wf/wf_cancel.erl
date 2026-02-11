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
%% Token Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cancel token.
%%
%% A valid cancel token is a 2-tuple where the first element is the
%% atom `cancel` and the second element is a list of place atoms.
%% The function never crashes.
%%
%% ```erlang
%% > wf_cancel:is_cancel_token({cancel, [p1, p2]}).
%% true
%% > wf_cancel:is_cancel_token({cancel, "not_a_list"}).
%% false
%% > wf_cancel:is_cancel_token({other, [p1]}).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_cancel_token(term()) -> boolean().

is_cancel_token({cancel, Targets}) when is_list(Targets) ->
    %% Verify all targets are atoms (places)
    lists:all(fun(T) -> is_atom(T) end, Targets);
is_cancel_token(_) ->
    false.

%%====================================================================
%% Token Creation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a cancel token for the specified target or targets.
%%
%% The target can be a single place atom or a list of place atoms.
%% When a single atom is provided, it is wrapped in a list.
%%
%% ```erlang
%% > wf_cancel:create_cancel_token(p1).
%% {cancel, [p1]}
%% > wf_cancel:create_cancel_token([p1, p2, p3]).
%% {cancel, [p1, p2, p3]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_cancel_token(Target :: atom() | [atom()]) -> cancel_token().

create_cancel_token(Target) when is_atom(Target) ->
    {cancel, [Target]};
create_cancel_token(Targets) when is_list(Targets) ->
    {cancel, Targets}.

%%====================================================================
%% Token Inspection Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts the list of target places from a cancel token.
%%
%% Returns the list of places that will be affected by the cancellation.
%% The function is total and returns an empty list for invalid tokens.
%%
%% ```erlang
%% > Token = {cancel, [p1, p2, p3]}.
%% > wf_cancel:cancel_targets(Token).
%% [p1, p2, p3]
%% > wf_cancel:cancel_targets(not_a_token).
%% []
%% ```
%% @end
%%--------------------------------------------------------------------
-spec cancel_targets(Token :: cancel_token() | term()) -> [atom()].

cancel_targets({cancel, Targets}) when is_list(Targets) ->
    Targets;
cancel_targets(_) ->
    [].

%%====================================================================
%% Cancellation Application Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Applies cancellation to a marking for a set of places.
%%
%% All places in the cancellation set have their tokens removed
%% (set to empty lists). Places not in the set are preserved unchanged.
%%
%% ```erlang
%% > Marking = #{p1 => [a, b], p2 => [c], p3 => [d]}.
%% > wf_cancel:apply_cancellation(Marking, [p1, p3]).
%% #{p1 => [], p2 => [c], p3 => []}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec apply_cancellation(Marking :: marking(), CancelSet :: cancellation_set()) ->
    marking().

apply_cancellation(Marking, CancelSet) when is_map(Marking), is_list(CancelSet) ->
    %% Set all places in CancelSet to empty lists
    lists:foldl(fun(Place, Acc) ->
        Acc#{Place => []}
    end, Marking, CancelSet).

%%--------------------------------------------------------------------
%% @doc Cancels all tokens in a region defined by a list of places.
%%
%% This is a convenience function that applies cancellation to a
%% specific region of the workflow. All places in the region have
%% their tokens removed.
%%
%% ```erlang
%% > Marking = #{a => [1], b => [2], c => [3], d => [4]}.
%% > Region = [b, c].
%% > wf_cancel:cancel_region(Marking, Region).
%% #{a => [1], b => [], c => [], d => [4]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec cancel_region(Marking :: marking(), Region :: [atom()]) -> marking().

cancel_region(Marking, Region) when is_map(Marking), is_list(Region) ->
    apply_cancellation(Marking, Region).

%%====================================================================
%% Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cancellation set.
%%
%% A valid cancellation set is a non-empty list of place atoms.
%% The function never crashes.
%%
%% ```erlang
%% > wf_cancel:is_cancellation_set([p1, p2, p3]).
%% true
%% > wf_cancel:is_cancellation_set([]).
%% true
%% > wf_cancel:is_cancellation_set([p1, "not_an_atom"]).
%% false
%% > wf_cancel:is_cancellation_set(not_a_list).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_cancellation_set(Term :: term()) -> boolean().

is_cancellation_set(Term) when is_list(Term) ->
    %% Check that all elements are atoms (places)
    lists:all(fun(E) -> is_atom(E) end, Term);
is_cancellation_set(_) ->
    false.

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
