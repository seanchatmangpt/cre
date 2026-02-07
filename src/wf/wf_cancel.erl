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

> Marking = #{p1 => [a], p2 => [b], p3 => [c], p4 => [d]}.
> Region = [p2, p3].
> wf_cancel:cancel_region(Marking, Region).
#{p1 => [a], p2 => [], p3 => [], p4 => [d]}

> wf_cancel:is_cancellation_set([p1, p2, p3]).
true
> wf_cancel:is_cancellation_set(not_a_list).
false
> wf_cancel:is_cancellation_set([p1, "not_an_atom"]).
false
```

<h3>Token Types</h3>
<ul>
  <li><strong>cancel_token:</strong> `{cancel, [atom()]}` - Identifies targets to cancel</li>
  <li><strong>cancel_region:</strong> `{cancel_region, atom(), [atom()]}` - Named region with places</li>
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
-export([create_cancel_token/1]).

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
%% @doc A cancel token identifies places to be cleared.
%%
%% The first element is the atom `cancel` and the second is a list
%% of place atoms that should have their tokens removed.
%%--------------------------------------------------------------------
-type cancel_token() :: {cancel, [atom()]}.

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
-export_type([cancel_token/0, cancel_region/0, cancellation_set/0]).

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
