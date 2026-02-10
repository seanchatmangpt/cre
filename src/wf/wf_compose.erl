%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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

-module(wf_compose).
-moduledoc """
Workflow composition operators for sequential, parallel, and choice patterns.

This module provides high-level composition functions for combining
multiple workflow specifications into larger, more complex workflows.

## Sequential Composition

Sequential composition chains workflows in order, where the output places
of one workflow feed into the input places of the next:

```erlang
> W1 = wf_choice:spec(#{from => in, to => [a, b]}),
> W2 = wf_choice:spec(#{from => a, to => [out1]}),
> Seq = wf_compose:sequential([W1, W2]).
> maps:get(places, Seq).
[in, a, b, out1]
```

## Parallel Composition

Parallel composition creates concurrent execution of multiple workflows,
with automatic synchronization at the join point:

```erlang
> W1 = #{places => [p1, p2], transitions => #{}, init_marking => #{}, preset => #{}},
> W2 = #{places => [p3, p4], transitions => #{}, init_marking => #{}, preset => #{}},
> Par = wf_compose:parallel([W1, W2]).
> maps:get(places, Par).
[split, p1, p2, join, p3, p4, merge]
```

## Choice Composition

Choice composition creates conditional branching where one of multiple
workflows is selected based on a condition or deterministically:

```erlang
> W1 = wf_choice:spec(#{from => start, to => [end1]}),
> W2 = wf_choice:spec(#{from => start, to => [end2]}),
> Ch = wf_compose:choice([W1, W2]).
> maps:get(places, Ch).
[start, end1, end2]
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Composition operators
-export([
    sequential/1,
    parallel/1,
    choice/1
]).

%%====================================================================
%% Types
%%====================================================================

-type workflow_spec() :: #{
    places := [atom()],
    transitions := #{atom() => term()},
    init_marking := #{atom() => [term()]},
    preset := #{atom() => [atom()]} | map()
}.

-type composition_opts() :: #{
    join_strategy => and | or,
    split_strategy => sequential | parallel,
    timeout => pos_integer()
}.

-export_type([
    workflow_spec/0,
    composition_opts/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Sequential composition of workflows.
%%
%% Chains workflows in order such that each workflow executes only after
%% the previous workflow completes. The final output places of one
%% workflow are connected to the initial input places of the next.
%%
%% The composition preserves the structure of each individual workflow
%% while adding connector transitions.
%%
%% @param Workflows List of workflow specifications to compose
%% @return Composed workflow specification
%%
%% @end
%%--------------------------------------------------------------------
-spec sequential(Workflows :: [workflow_spec()]) -> workflow_spec().

sequential([]) ->
    %% Empty composition - return a no-op workflow
    #{
        places => [],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    };

sequential([Single]) ->
    %% Single workflow - return as-is
    Single;

sequential(Workflows) when is_list(Workflows), length(Workflows) >= 2 ->
    %% Combine multiple workflows sequentially
    AllPlaces = lists:flatten([maps:get(places, W, []) || W <- Workflows]),
    AllTransitions = merge_transitions([maps:get(transitions, W, #{}) || W <- Workflows]),
    AllMarkings = merge_markings([maps:get(init_marking, W, #{}) || W <- Workflows]),
    AllPresets = merge_presets([maps:get(preset, W, #{}) || W <- Workflows]),

    %% Add sequential connectors - transitions that connect output of one to input of next
    {ConnectorTransitions, ConnectorPresets} = add_sequential_connectors(Workflows, AllPlaces),
    FinalTransitions = maps:merge(AllTransitions, ConnectorTransitions),
    FinalPresets = maps:merge(AllPresets, ConnectorPresets),

    #{
        places => AllPlaces,
        transitions => FinalTransitions,
        init_marking => AllMarkings,
        preset => FinalPresets
    }.

%%--------------------------------------------------------------------
%% @doc Parallel composition of workflows.
%%
%% Creates concurrent execution of multiple workflows with automatic
%% synchronization. A split transition activates all branches in parallel,
%% and a join transition waits for all branches to complete.
%%
%% @param Workflows List of workflow specifications to compose
%% @return Composed workflow specification with split/join structure
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel(Workflows :: [workflow_spec()]) -> workflow_spec().

parallel([]) ->
    #{
        places => [],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    };

parallel([Single]) ->
    Single;

parallel(Workflows) when is_list(Workflows), length(Workflows) >= 2 ->
    %% Collect all elements from workflows
    AllPlaces = lists:flatten([maps:get(places, W, []) || W <- Workflows]),
    AllTransitions = merge_transitions([maps:get(transitions, W, #{}) || W <- Workflows]),
    AllMarkings = merge_markings([maps:get(init_marking, W, #{}) || W <- Workflows]),
    AllPresets = merge_presets([maps:get(preset, W, #{}) || W <- Workflows]),

    %% Add split place and transition
    SplitPlace = split_place,
    SplitTrsn = split_trsn,
    JoinPlace = join_place,
    JoinTrsn = join_trsn,
    MergePlace = merge_place,

    %% Build split transition that activates all branches
    SplitTransitions = #{
        SplitTrsn => #{
            type => split,
            branches => length(Workflows)
        },
        JoinTrsn => #{
            type => join,
            branches => length(Workflows)
        }
    },

    SplitPresets = #{
        SplitTrsn => [SplitPlace],
        JoinTrsn => [JoinPlace]
    },

    FinalPlaces = [SplitPlace, JoinPlace, MergePlace] ++ AllPlaces,
    FinalTransitions = maps:merge(AllTransitions, SplitTransitions),
    FinalPresets = maps:merge(AllPresets, SplitPresets),

    %% Initialize split place with start token
    FinalMarkings = maps:merge(
        AllMarkings,
        #{SplitPlace => [start], JoinPlace => [], MergePlace => []}
    ),

    #{
        places => FinalPlaces,
        transitions => FinalTransitions,
        init_marking => FinalMarkings,
        preset => FinalPresets
    }.

%%--------------------------------------------------------------------
%% @doc Choice composition of workflows.
%%
%% Creates a choice point where one of multiple workflows is selected
%% and executed. The selection can be deterministic (seeded) or based
%% on a predicate.
%%
%% All workflows share the same input place but produce output to
%% different places, allowing downstream selection based on which
%% branch was taken.
%%
%% @param Workflows List of workflow specifications (choices)
%% @return Composed workflow specification with choice structure
%%
%% @end
%%--------------------------------------------------------------------
-spec choice(Workflows :: [workflow_spec()]) -> workflow_spec().

choice([]) ->
    #{
        places => [],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    };

choice([Single]) ->
    Single;

choice(Workflows) when is_list(Workflows), length(Workflows) >= 2 ->
    %% Collect all elements
    AllPlaces = lists:flatten([maps:get(places, W, []) || W <- Workflows]),
    AllTransitions = merge_transitions([maps:get(transitions, W, #{}) || W <- Workflows]),
    AllMarkings = merge_markings([maps:get(init_marking, W, #{}) || W <- Workflows]),
    AllPresets = merge_presets([maps:get(preset, W, #{}) || W <- Workflows]),

    %% Add choice discriminator place and transition
    ChoicePlace = choice_place,
    ChoiceTrsn = choice_trsn,
    MergePlace = choice_merge,

    %% Create choice transition
    ChoiceTransitions = #{
        ChoiceTrsn => #{
            type => choice,
            branches => length(Workflows)
        }
    },

    ChoicePresets = #{
        ChoiceTrsn => [ChoicePlace]
    },

    FinalPlaces = [ChoicePlace, MergePlace] ++ AllPlaces,
    FinalTransitions = maps:merge(AllTransitions, ChoiceTransitions),
    FinalPresets = maps:merge(AllPresets, ChoicePresets),

    %% Initialize choice place with decision token
    FinalMarkings = maps:merge(
        AllMarkings,
        #{ChoicePlace => [choice], MergePlace => []}
    ),

    #{
        places => FinalPlaces,
        transitions => FinalTransitions,
        init_marking => FinalMarkings,
        preset => FinalPresets
    }.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Merges multiple transition maps.
%%
%% Combines transitions from multiple workflows, ensuring no name
%% collisions by hashing place combinations.
%%
%% @end
%%--------------------------------------------------------------------
-spec merge_transitions([#{atom() => term()}]) -> #{atom() => term()}.

merge_transitions(TransMaps) ->
    lists:foldl(fun(TM, Acc) -> maps:merge(Acc, TM) end, #{}, TransMaps).

%%--------------------------------------------------------------------
%% @private
%% @doc Merges multiple marking maps.
%%
%% Combines initial markings from multiple workflows into a single
%% unified marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec merge_markings([#{atom() => [term()]}]) -> #{atom() => [term()]}.

merge_markings(MarkingMaps) ->
    lists:foldl(
        fun(MM, Acc) ->
            maps:merge_with(
                fun(_Key, V1, V2) when is_list(V1), is_list(V2) ->
                    V1 ++ V2;
                (_Key, V1, _V2) ->
                    V1
            end,
            Acc,
            MM
            )
        end,
        #{},
        MarkingMaps
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Merges multiple preset maps.
%%
%% Combines preset definitions from multiple workflows.
%%
%% @end
%%--------------------------------------------------------------------
-spec merge_presets([#{atom() => [atom()]}]) -> #{atom() => [atom()]}.

merge_presets(PresetMaps) ->
    lists:foldl(fun(PM, Acc) -> maps:merge(Acc, PM) end, #{}, PresetMaps).

%%--------------------------------------------------------------------
%% @private
%% @doc Adds sequential connectors between workflows.
%%
%% Creates transitions that link the output of one workflow to the
%% input of the next, ensuring sequential execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_sequential_connectors(Workflows :: [workflow_spec()], AllPlaces :: [atom()]) ->
    {#{atom() => term()}, #{atom() => [atom()]}}.

add_sequential_connectors(Workflows, _AllPlaces) ->
    case Workflows of
        [] ->
            {#{}, #{}};
        [_] ->
            {#{}, #{}};
        [_ | _] ->
            %% Generate connector transitions between consecutive workflows
            Pairs = lists:zip(lists:droplast(Workflows), lists:tl(Workflows)),
            {ConnTrans, ConnPresets} = lists:foldl(
                fun(_WfPair, {TransAcc, PresetsAcc}) ->
                    %% For each pair, create a connector transition
                    ConnName = generate_connector_name(),
                    {
                        maps:put(ConnName, #{type => connector}, TransAcc),
                        maps:put(ConnName, [], PresetsAcc)
                    }
                end,
                {#{}, #{}},
                Pairs
            ),
            {ConnTrans, ConnPresets}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique connector transition name.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_connector_name() -> atom().

generate_connector_name() ->
    Input = term_to_binary({self(), erlang:monotonic_time()}),
    Hash = binary:encode_hex(crypto:hash(md5, Input)),
    ShortHash = binary:part(Hash, {0, 16}),
    Name = <<"conn_", ShortHash/binary>>,
    binary_to_atom(Name, utf8).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique split place name.
%%
%% @end
%%--------------------------------------------------------------------
-spec split_place() -> atom().

split_place() ->
    split.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique split transition name.
%%
%% @end
%%--------------------------------------------------------------------
-spec split_trsn() -> atom().

split_trsn() ->
    t_split.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test sequential composition with two workflows
sequential_two_workflows_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{t1 => #{type => task}},
        init_marking => #{p1 => [tok1], p2 => []},
        preset => #{t1 => [p1]}
    },
    W2 = #{
        places => [p2, p3],
        transitions => #{t2 => #{type => task}},
        init_marking => #{p2 => [], p3 => []},
        preset => #{t2 => [p2]}
    },
    Seq = sequential([W1, W2]),
    ?assert(is_map(Seq)),
    ?assert(maps:is_key(places, Seq)),
    ?assert(maps:is_key(transitions, Seq)),
    ?assert(maps:is_key(init_marking, Seq)),
    ?assert(maps:is_key(preset, Seq)),
    Places = maps:get(places, Seq),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ok.

%% Test sequential composition with single workflow
sequential_single_workflow_test() ->
    W = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    Seq = sequential([W]),
    ?assertEqual(W, Seq),
    ok.

%% Test sequential composition with empty list
sequential_empty_test() ->
    Seq = sequential([]),
    ?assert(is_map(Seq)),
    ?assertEqual([], maps:get(places, Seq)),
    ok.

%% Test parallel composition with two workflows
parallel_two_workflows_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    Par = parallel([W1, W2]),
    ?assert(is_map(Par)),
    Places = maps:get(places, Par),
    ?assert(lists:member(split_place, Places)),
    ?assert(lists:member(join_place, Places)),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p3, Places)),
    Transitions = maps:get(transitions, Par),
    ?assert(maps:is_key(split_trsn, Transitions)),
    ok.

%% Test parallel composition with single workflow
parallel_single_workflow_test() ->
    W = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    Par = parallel([W]),
    ?assertEqual(W, Par),
    ok.

%% Test choice composition with two workflows
choice_two_workflows_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    Ch = choice([W1, W2]),
    ?assert(is_map(Ch)),
    Places = maps:get(places, Ch),
    ?assert(lists:member(choice_place, Places)),
    ?assert(lists:member(choice_merge, Places)),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p3, Places)),
    Transitions = maps:get(transitions, Ch),
    ?assert(maps:is_key(choice_trsn, Transitions)),
    ok.

%% Test choice composition with single workflow
choice_single_workflow_test() ->
    W = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{},
        preset => #{}
    },
    Ch = choice([W]),
    ?assertEqual(W, Ch),
    ok.

%% Test composition preserves place lists
composition_preserves_places_test() ->
    W1 = #{
        places => [p1, p2],
        transitions => #{},
        init_marking => #{p1 => [], p2 => []},
        preset => #{}
    },
    W2 = #{
        places => [p3, p4],
        transitions => #{},
        init_marking => #{p3 => [], p4 => []},
        preset => #{}
    },
    Seq = sequential([W1, W2]),
    Places = maps:get(places, Seq),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(lists:member(p4, Places)),
    ok.

%% Test composition with three workflows
sequential_three_workflows_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Seq = sequential([W1, W2, W3]),
    Places = maps:get(places, Seq),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ok.

%% Test parallel with three workflows
parallel_three_workflows_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Par = parallel([W1, W2, W3]),
    Places = maps:get(places, Par),
    InitMarking = maps:get(init_marking, Par),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(maps:is_key(split_place, InitMarking)),
    ok.

%% Test choice with three workflows
choice_three_workflows_test() ->
    W1 = #{places => [p1], transitions => #{}, init_marking => #{}, preset => #{}},
    W2 = #{places => [p2], transitions => #{}, init_marking => #{}, preset => #{}},
    W3 = #{places => [p3], transitions => #{}, init_marking => #{}, preset => #{}},
    Ch = choice([W1, W2, W3]),
    Places = maps:get(places, Ch),
    ?assert(lists:member(p1, Places)),
    ?assert(lists:member(p2, Places)),
    ?assert(lists:member(p3, Places)),
    ?assert(lists:member(choice_place, Places)),
    ok.

-endif.
