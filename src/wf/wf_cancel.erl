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
Cancellation token handling for YAWL cancellation regions.

Manages cancellation tokens that terminate workflow regions when
specific conditions are met. Cancellation regions are used in YAWL
workflows to model scenarios where the occurrence of a specific event
(e.g., cancellation, timeout, or error) should terminate all activities
within a designated region.

## Legacy Token API

```erlang
> wf_cancel:is_cancel_token({cancel, [p1, p2]}).
true
> wf_cancel:is_cancel_token({other, tuple}).
false

> Token = wf_cancel:create_cancel_token([region_place1, region_place2]).
{cancel, [region_place1, region_place2]}

> wf_cancel:cancel_targets(Token).
[region_place1, region_place2]

> Marking = #{p1 => [a], p2 => [b], p3 => [c]}.
> wf_cancel:apply_cancellation(Marking, [p1, p2]).
#{p1 => [], p2 => [], p3 => [c]}
```

## Scope-Based Cancellation API

```erlang
> wf_cancel:create_activity_cancel(task1).
{cancel, {activity, task1}}

> wf_cancel:create_region_cancel(payment_region).
{cancel, {region, payment_region}}

> wf_cancel:create_case_cancel().
{cancel, {case, all}}

> wf_cancel:is_cancel_token({cancel, {activity, task1}}).
true
```

<h3>Token Types</h3>
<ul>
  <li><strong>cancel_token (legacy):</strong> `{cancel, [atom()]}` - Identifies targets to cancel</li>
  <li><strong>cancel_token (scope):</strong> `{cancel, {ScopeType, ScopeId}}` - Scope-based cancellation</li>
  <li><strong>cancel_region:</strong> `{cancel_region, atom(), [atom()]}` - Named region with places</li>
</ul>

<h3>Scope Types</h3>
<ul>
  <li><strong>{activity, TaskId}:</strong> Cancel single task/activity</li>
  <li><strong>{region, RegionId}:</strong> Cancel all tasks in region</li>
  <li><strong>{case, all}:</strong> Cancel entire workflow case</li>
</ul>

<h3>Cancellation Behavior</h3>
When a cancel token is processed:
<ol>
  <li>Remove all tokens from places in the cancellation set</li>
  <li>Set those places to empty lists</li>
  <li>Return the updated marking</li>
</ol>

<h3>Usage in YAWL Workflows</h3>
Cancellation tokens are typically used in workflow patterns such as:
<ul>
  <li><strong>Cancel Case:</strong> Terminate an entire workflow case</li>
  <li><strong>Cancel Region:</strong> Terminate activities within a specific region</li>
  <li><strong>Cancel Activity:</strong> Terminate a specific task or activity</li>
</ul>
""".

%%====================================================================
%% Exports
%%====================================================================

%% Token validation
-export([is_cancel_token/1]).

%% Token creation
-export([create_cancel_token/1, create_activity_cancel/1,
         create_region_cancel/1, create_case_cancel/0]).

%% Scope resolution
-export([resolve_scope/3]).

%% Token inspection
-export([cancel_targets/1]).

%% Cancellation application
-export([apply_cancellation/2, cancel_region/2]).

%% Validation
-export([is_cancellation_set/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cancellation scope types.
%%
%% Three granularity levels:
%% - `{activity, TaskId}`: Cancel single task/activity
%% - `{region, RegionId}`: Cancel all tasks in region
%% - `{'case', all}`: Cancel entire workflow case
%%--------------------------------------------------------------------
-type cancel_scope() :: {activity, atom()} |
                       {region, atom()} |
                       {'case', all}.

%%--------------------------------------------------------------------
%% @doc A cancel token identifies places to be cleared.
%%
%% Supports both legacy format (list of places) and scope-based format.
%% Legacy: `{cancel, [Place]}`
%% Scope: `{cancel, {ScopeType, ScopeId}}`
%%--------------------------------------------------------------------
-type cancel_token() :: {cancel, [atom()]} |                       % legacy
                       {cancel, cancel_scope()}.                    % scope

%%--------------------------------------------------------------------
%% @doc A cancel region defines a named region with its places.
%%
%% Used for defining cancellation regions in workflow specifications.
%%--------------------------------------------------------------------
-type cancel_region() :: {cancel_region, atom(), [atom()]}.

%%--------------------------------------------------------------------
%% @doc A cancellation set is a list of place atoms to be cancelled.
%%
%% All places in the set will have their tokens removed when the
%% cancellation is applied.
%%--------------------------------------------------------------------
-type cancellation_set() :: [atom()].

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% This is the standard Petri net marking representation used
%% throughout the workflow engine.
%%--------------------------------------------------------------------
-type marking() :: #{atom() => [term()]}.

%% Export types
-export_type([cancel_token/0, cancel_region/0, cancellation_set/0, cancel_scope/0]).

%%====================================================================
%% Token Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cancel token.
%%
%% A valid cancel token is a 2-tuple where the first element is the
%% atom `cancel`. The second element can be:
%% - A list of place atoms (legacy format)
%% - A scope tuple {activity, TaskId}, {region, RegionId}, or {case, all}
%%
%% The function never crashes.
%%
%% ```erlang
%% > wf_cancel:is_cancel_token({cancel, [p1, p2]}).
%% true
%% > wf_cancel:is_cancel_token({cancel, {activity, task1}}).
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
    %% Legacy format - verify all targets are atoms (places)
    lists:all(fun(T) -> is_atom(T) end, Targets);
is_cancel_token({cancel, {activity, TaskId}}) when is_atom(TaskId) ->
    true;
is_cancel_token({cancel, {region, RegionId}}) when is_atom(RegionId) ->
    true;
is_cancel_token({cancel, {'case', all}}) ->
    true;
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

%%--------------------------------------------------------------------
%% @doc Creates a cancellation token for an activity scope.
%%
%% Activity scope cancels a single task's places.
%%
%% ```erlang
%% > wf_cancel:create_activity_cancel(task1).
%% {cancel, {activity, task1}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_activity_cancel(TaskId :: atom()) -> cancel_token().

create_activity_cancel(TaskId) when is_atom(TaskId) ->
    {cancel, {activity, TaskId}}.

%%--------------------------------------------------------------------
%% @doc Creates a cancellation token for a region scope.
%%
%% Region scope cancels all places within a named region.
%%
%% ```erlang
%% > wf_cancel:create_region_cancel(payment_region).
%% {cancel, {region, payment_region}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_region_cancel(RegionId :: atom()) -> cancel_token().

create_region_cancel(RegionId) when is_atom(RegionId) ->
    {cancel, {region, RegionId}}.

%%--------------------------------------------------------------------
%% @doc Creates a cancellation token for case scope.
%%
%% Case scope cancels the entire workflow.
%%
%% ```erlang
%% > wf_cancel:create_case_cancel().
%% {cancel, {'case', all}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec create_case_cancel() -> cancel_token().

create_case_cancel() ->
    {cancel, {'case', all}}.

%%====================================================================
%% Scope Resolution Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Resolves a cancellation scope to a concrete list of places.
%%
%% Uses the binding table (from wf_spec) to map scope identifiers to
%% actual place atoms in the Petri net. Returns empty list for unknown
%% scopes to maintain totality.
%%
%% ```erlang
%% > BT = #{task1 => #{p1_in => child_p1_in, p1_out => child_p1_out}}.
%% > wf_cancel:resolve_scope({activity, task1}, BT, Spec).
%% [child_p1_in, child_p1_out]
%%
%% > wf_cancel:resolve_scope({'case', all}, BT, Spec).
%% [p1, p2, p3, ...]  % all places in workflow
%% ```
%% @end
%%--------------------------------------------------------------------
-spec resolve_scope(Scope :: cancel_scope(),
                   BindingTable :: wf_scope:binding_table(),
                   Spec :: wf_spec:yawl_spec() | undefined) -> [atom()].

resolve_scope({activity, TaskId}, BindingTable, Spec) ->
    %% Get places for this task from binding table
    case maps:get(TaskId, BindingTable, undefined) of
        undefined ->
            %% No binding - try spec fallback
            case Spec of
                undefined -> [];
                _ ->
                    try wf_spec:task_places(Spec, TaskId) of
                        undefined -> [];
                        Places -> Places
                    catch
                        _:_ -> []
                    end
            end;
        Mapping when is_map(Mapping) ->
            %% Extract all child places from the mapping
            maps:values(Mapping)
    end;

resolve_scope({region, RegionId}, BindingTable, Spec) ->
    %% Get cancellation set from spec
    case Spec of
        undefined -> [];
        _ ->
            try wf_spec:cancellation_set(Spec, RegionId) of
                [] -> [];
                TaskIds ->
                    %% Resolve each task to its places
                    lists:flatmap(
                        fun(TaskId) ->
                            resolve_scope({activity, TaskId}, BindingTable, Spec)
                        end,
                        TaskIds
                    )
            catch
                _:_ -> []
            end
    end;

resolve_scope({'case', all}, _BindingTable, Spec) ->
    %% Return all places in the workflow specification
    case Spec of
        undefined -> [];
        _ ->
            try wf_spec:all_places(Spec)
            catch
                _:_ -> []
            end
    end.

%%====================================================================
%% Token Inspection Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts the list of target places from a cancel token.
%%
%% For legacy tokens, returns the place list directly.
%% For scope tokens, returns empty list (must use resolve_scope/3).
%% The function is total and returns an empty list for invalid tokens.
%%
%% ```erlang
%% > Token = {cancel, [p1, p2, p3]}.
%% > wf_cancel:cancel_targets(Token).
%% [p1, p2, p3]
%% > wf_cancel:cancel_targets({cancel, {activity, task1}}).
%% []  % scope token - use resolve_scope/3
%% > wf_cancel:cancel_targets(not_a_token).
%% []
%% ```
%% @end
%%--------------------------------------------------------------------
-spec cancel_targets(Token :: cancel_token() | term()) -> [atom()].

cancel_targets({cancel, Targets}) when is_list(Targets) ->
    %% Legacy token format
    Targets;
cancel_targets({cancel, {_, _}}) ->
    %% Scope token - must be resolved
    [];
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

%%--------------------------------------------------------------------
%% @doc Test scope-based token creation functions.
%%--------------------------------------------------------------------
create_activity_cancel_test() ->
    ?assertEqual({cancel, {activity, task1}}, create_activity_cancel(task1)),
    ?assertEqual({cancel, {activity, my_task}}, create_activity_cancel(my_task)).

create_region_cancel_test() ->
    ?assertEqual({cancel, {region, region1}}, create_region_cancel(region1)),
    ?assertEqual({cancel, {region, payment}}, create_region_cancel(payment)).

create_case_cancel_test() ->
    ?assertEqual({cancel, {'case', all}}, create_case_cancel()),
    ?assertEqual({cancel, {'case', all}}, create_case_cancel()).

%%--------------------------------------------------------------------
%% @doc Test is_cancel_token/1 with scope tokens.
%%--------------------------------------------------------------------
is_cancel_token_scope_test() ->
    %% Activity scope
    ?assertEqual(true, is_cancel_token({cancel, {activity, task1}})),
    ?assertEqual(true, is_cancel_token({cancel, {activity, my_task}})),

    %% Region scope
    ?assertEqual(true, is_cancel_token({cancel, {region, region1}})),
    ?assertEqual(true, is_cancel_token({cancel, {region, payment_region}})),

    %% Case scope
    ?assertEqual(true, is_cancel_token({cancel, {'case', all}})),

    %% Invalid scope tokens
    ?assertEqual(false, is_cancel_token({cancel, {activity, "not_atom"}})),
    ?assertEqual(false, is_cancel_token({cancel, {region, "not_atom"}})),
    ?assertEqual(false, is_cancel_token({cancel, {'case', "not_all"}})),
    ?assertEqual(false, is_cancel_token({cancel, {invalid, type}})).

%%--------------------------------------------------------------------
%% @doc Test cancel_targets/1 with scope tokens.
%%--------------------------------------------------------------------
cancel_targets_scope_test() ->
    %% Legacy tokens return the list
    ?assertEqual([p1, p2], cancel_targets({cancel, [p1, p2]})),

    %% Scope tokens return empty list
    ?assertEqual([], cancel_targets({cancel, {activity, task1}})),
    ?assertEqual([], cancel_targets({cancel, {region, region1}})),
    ?assertEqual([], cancel_targets({cancel, {'case', all}})),

    %% Invalid tokens return empty list
    ?assertEqual([], cancel_targets(not_a_token)),
    ?assertEqual([], cancel_targets({other, [p1]})).

%%--------------------------------------------------------------------
%% @doc Test resolve_scope/3 for activity scope.
%%--------------------------------------------------------------------
resolve_scope_activity_test() ->
    %% Test with binding table (primary use case)
    BT = #{task1 => #{parent_in => child_in, parent_out => child_out}},

    %% Need to create a minimal yawl_spec record for the spec functions to work
    %% For unit testing, just test that the binding table path works
    ?assertEqual([child_in, child_out], resolve_scope({activity, task1}, BT, undefined)),

    %% Unknown task with no binding table returns empty list
    ?assertEqual([], resolve_scope({activity, unknown_task}, #{}, undefined)).

%%--------------------------------------------------------------------
%% @doc Test resolve_scope/3 for region scope.
%%--------------------------------------------------------------------
resolve_scope_region_test() ->
    %% Test with undefined spec - will return empty list from cancellation_set
    ?assertEqual([], resolve_scope({region, unknown_region}, #{}, undefined)).

%%--------------------------------------------------------------------
%% @doc Test resolve_scope/3 for case scope.
%%--------------------------------------------------------------------
resolve_scope_case_test() ->
    %% Test with undefined spec - returns empty list from all_places
    ?assertEqual([], resolve_scope({'case', all}, #{}, undefined)).

-endif.
