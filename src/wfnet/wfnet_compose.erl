%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @author CRE Team
%% @version 0.3.0
%% @doc Workflow Net Composition Operators
%%
%% Provides high-level composition operators for building complex
%% workflows from simpler components. Operators follow BPMN and
%% YAWL workflow pattern semantics.
%%
%% <h3>Composition Patterns</h3>
%%
%% <ul>
%%   <li><b>sequence/2:</b> Execute A then B</li>
%%   <li><b>parallel/2:</b> Execute A and B concurrently</li>
%%   <li><b>choice/2:</b> Execute A or B (exclusive)</li>
%%   <li><b>loop/2:</b> Repeat A while condition holds</li>
%%   <li><b>merge/1:</b> Merge multiple workflows</li>
%%   <li><b>nest/2:</b> Nest sub-workflow with boundary</li>
%% </ul>
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Build an order processing workflow
%% Validate = wfnet_compose:task(validate_order),
%% Process = wfnet_compose:task(process_payment),
%% Ship = wfnet_compose:task(ship_order),
%%
%% %% Compose: validate -> (process || ship)
%% Workflow = wfnet_compose:sequence(
%%     Validate,
%%     wfnet_compose:parallel(Process, Ship)
%% ).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_compose).

%% API exports
-export([
    %% Basic composition
    sequence/2,
    parallel/2,
    choice/2,
    loop/2,

    %% Multiple input composition
    merge/1,
    split/2,

    %% Structural composition
    nest/2,
    surround/3,

    %% Task wrapper
    task/1,

    %% Validation
    validate_composition/1
]).

%% Types
-type workflow_spec() :: wfnet_types:workflow_spec().
-type place() :: wfnet_types:place().
-type trsn() :: wfnet_types:trsn().

-export_type([workflow_spec/0]).

%%====================================================================
%% Basic Composition Operators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compose two workflows in sequence.
%%
%% The output place of SpecA is connected to the input place of SpecB.
%% Tokens flow from A to B after A completes.
%%
%% === Example ===
%% ```erlang
%% A = task(task_a),
%% B = task(task_b),
%% Sequential = wfnet_compose:sequence(A, B).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec sequence(workflow_spec(), workflow_spec()) -> workflow_spec().
sequence(SpecA, SpecB) ->
    #{places := PlacesA, transitions := TransA, start_place := StartA, end_place := EndA} = SpecA,
    #{places := PlacesB, transitions := TransB, start_place := StartB, end_place := EndB} = SpecB,

    %% Generate unique names by adding prefix
    PrefixA = prefix(SpecA),
    PrefixB = prefix(SpecB),

    %% Rename places and transitions to avoid conflicts
    PlacesARenamed = rename_places(PlacesA, PrefixA),
    PlacesBRenamed = rename_places(PlacesB, PrefixB),
    PlacesSeq = PlacesARenamed ++ PlacesBRenamed,

    TransARenamed = rename_transitions(TransA, PrefixA),
    TransBRenamed = rename_transitions(TransB, PrefixB),
    TransSeq = TransARenamed ++ TransBRenamed,

    %% Connect EndA to StartB
    EndARenamed = rename_place(EndA, PrefixA),
    StartBRenamed = rename_place(StartB, PrefixB),

    %% Merge preset/postset with connection
    PresetSeq = maps:merge(
        rename_preset_keys(maps:get(preset, SpecA, #{}), PrefixA),
        rename_preset_keys(maps:get(preset, SpecB, #{}), PrefixB)
    ),
    PostsetSeq = maps:merge(
        rename_postset_keys(maps:get(postset, SpecA, #{}), PrefixA),
        rename_postset_keys(maps:get(postset, SpecB, #{}), PrefixB)
    ),

    %% Add connection transition from EndA to StartB
    ConnTrans = list_to_atom(atom_to_list(EndARenamed) ++ "_to_" ++ atom_to_list(StartBRenamed)),

    #{
        places => PlacesSeq,
        transitions => [ConnTrans | TransSeq],
        start_place => rename_place(StartA, PrefixA),
        end_place => rename_place(EndB, PrefixB),
        preset => PresetSeq#{ConnTrans => [EndARenamed]},
        postset => PostsetSeq#{ConnTrans => [StartBRenamed]}
    }.

%%--------------------------------------------------------------------
%% @doc Compose two workflows in parallel (AND-split/sync).
%%
%% Both workflows execute concurrently. The composition waits for
%% both to complete before proceeding.
%%
%% === Example ===
%% ```erlang
%% A = task(task_a),
%% B = task(task_b),
%% Parallel = wfnet_compose:parallel(A, B).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel(workflow_spec(), workflow_spec()) -> workflow_spec().
parallel(SpecA, SpecB) ->
    #{start_place := StartA, end_place := EndA, transitions := TransA} = SpecA,
    #{start_place := StartB, end_place := EndB, transitions := TransB} = SpecB,

    PrefixA = prefix(SpecA),
    PrefixB = prefix(SpecB),

    %% Create split and join transitions
    SplitTrans = list_to_atom("split_" ++ PrefixA ++ "_" ++ PrefixB),
    JoinTrans = list_to_atom("join_" ++ PrefixA ++ "_" ++ PrefixB),

    %% Rename places
    StartARenamed = rename_place(StartA, PrefixA),
    StartBRenamed = rename_place(StartB, PrefixB),
    EndARenamed = rename_place(EndA, PrefixA),
    EndBRenamed = rename_place(EndB, PrefixB),

    %% Build parallel structure
    Places = [StartARenamed, StartBRenamed, EndARenamed, EndBRenamed],
    Transitions = [SplitTrans, JoinTrans |
                   rename_transitions(TransA, PrefixA) ++
                   rename_transitions(TransB, PrefixB)],

    %% Split connects to both starts
    Preset = #{
        SplitTrans => [],
        JoinTrans => [EndARenamed, EndBRenamed]
    } ++ rename_preset_keys(maps:get(preset, SpecA, #{}), PrefixA) ++
       rename_preset_keys(maps:get(preset, SpecB, #{}), PrefixB),

    %% Join receives from both ends
    Postset = #{
        SplitTrans => [StartARenamed, StartBRenamed],
        JoinTrans => []
    } ++ rename_postset_keys(maps:get(postset, SpecA, #{}), PrefixA) ++
       rename_postset_keys(maps:get(postset, SpecB, #{}), PrefixB),

    #{
        places => Places,
        transitions => Transitions,
        start_place => SplitTrans,
        end_place => JoinTrans,
        preset => Preset,
        postset => Postset
    }.

%%--------------------------------------------------------------------
%% @doc Compose two workflows with exclusive choice (XOR-split).
%%
%% Exactly one branch will be selected based on runtime conditions.
%%
%% === Example ===
%% ```erlang
%% A = task(task_a),
%% B = task(task_b),
%% Choice = wfnet_compose:choice(A, B).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec choice(workflow_spec(), workflow_spec()) -> workflow_spec().
choice(SpecA, SpecB) ->
    #{start_place := StartA, end_place := EndA, transitions := TransA} = SpecA,
    #{start_place := StartB, end_place := EndB, transitions := TransB} = SpecB,

    PrefixA = prefix(SpecA),
    PrefixB = prefix(SpecB),

    %% Create choice and merge transitions
    ChoiceTrans = list_to_atom("choice_" ++ PrefixA ++ "_" ++ PrefixB),
    MergeTrans = list_to_atom("merge_" ++ PrefixA ++ "_" ++ PrefixB),

    %% Rename places
    StartARenamed = rename_place(StartA, PrefixA),
    StartBRenamed = rename_place(StartB, PrefixB),
    EndARenamed = rename_place(EndA, PrefixA),
    EndBRenamed = rename_place(EndB, PrefixB),

    Places = [StartARenamed, StartBRenamed, EndARenamed, EndBRenamed],
    Transitions = [ChoiceTrans, MergeTrans |
                   rename_transitions(TransA, PrefixA) ++
                   rename_transitions(TransB, PrefixB)],

    %% Choice connects to both starts (XOR)
    Preset = #{
        ChoiceTrans => [],
        MergeTrans => [EndARenamed, EndBRenamed]
    } ++ rename_preset_keys(maps:get(preset, SpecA, #{}), PrefixA) ++
       rename_preset_keys(maps:get(preset, SpecB, #{}), PrefixB),

    %% Merge receives from either end (XOR)
    Postset = #{
        ChoiceTrans => [StartARenamed, StartBRenamed],
        MergeTrans => []
    } ++ rename_postset_keys(maps:get(postset, SpecA, #{}), PrefixA) ++
       rename_postset_keys(maps:get(postset, SpecB, #{}), PrefixB),

    #{
        places => Places,
        transitions => Transitions,
        start_place => ChoiceTrans,
        end_place => MergeTrans,
        preset => Preset,
        postset => Postset
    }.

%%--------------------------------------------------------------------
%% @doc Create a loop (iteration) over a workflow.
%%
%% The body workflow executes repeatedly while a condition holds.
%%
%% === Example ===
%% ```erlang
%% Body = task(process_item),
%% Loop = wfnet_compose:loop(Body).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec loop(workflow_spec()) -> workflow_spec().
loop(Spec) ->
    loop(Spec, 0).

%%--------------------------------------------------------------------
%% @doc Create a loop with maximum iteration count.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop(workflow_spec(), non_neg_integer()) -> workflow_spec().
loop(Spec, MaxIterations) when is_integer(MaxIterations), MaxIterations >= 0 ->
    #{start_place := Start, end_place := End, transitions := Trans} = Spec,
    Prefix = prefix(Spec),

    StartRenamed = rename_place(Start, Prefix),
    EndRenamed = rename_place(End, Prefix),
    TransRenamed = rename_transitions(Trans, Prefix),

    %% Create loop decision and back-edge
    LoopTrans = list_to_atom("loop_check_" ++ Prefix),
    ExitTrans = list_to_atom("loop_exit_" ++ Prefix),

    Places = [StartRenamed, EndRenamed],
    Transitions = [LoopTrans, ExitTrans | TransRenamed],

    Preset = #{
        LoopTrans => [EndRenamed],
        ExitTrans => [EndRenamed]
    } ++ rename_preset_keys(maps:get(preset, Spec, #{}), Prefix),

    Postset = #{
        LoopTrans => [StartRenamed],  %% Back edge
        ExitTrans => []
    } ++ rename_postset_keys(maps:get(postset, Spec, #{}), Prefix),

    #{
        places => Places,
        transitions => Transitions,
        start_place => StartRenamed,
        end_place => ExitTrans,
        preset => Preset,
        postset => Postset,
        optional => #{max_iterations => MaxIterations}
    }.

%%--------------------------------------------------------------------
%% @doc Compose two workflows with loop (body + condition).
%%
%% @end
%%--------------------------------------------------------------------
-spec loop_with_condition(workflow_spec(), workflow_spec()) -> workflow_spec().
loop_with_condition(BodySpec, ConditionSpec) ->
    %% Body executes, then condition decides to continue or exit
    BodyWithLoop = loop(BodySpec),
    sequence(BodyWithLoop, ConditionSpec).

%%--------------------------------------------------------------------
%% @doc Merge multiple workflows into one.
%%
%% All workflows execute independently. The merge creates a combined
%% specification without explicit synchronization.
%%
%% === Example ===
%% ```erlang
%% Merged = wfnet_compose:merge([Spec1, Spec2, Spec3]).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec merge([workflow_spec()]) -> workflow_spec().
merge([]) ->
    error(empty_workflow_list);
merge([Single]) ->
    Single;
merge([First | Rest]) ->
    lists:foldl(fun(Spec, Acc) ->
        parallel(Acc, Spec)
    end, First, Rest).

%%--------------------------------------------------------------------
%% @doc Split a workflow into multiple branches.
%%
%% Creates a split that fans out to multiple workflow branches.
%%
%% === Example ===
%% ```erlang
%% Branches = [Spec1, Spec2, Spec3],
%% Split = wfnet_compose:split(Branches).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec split(workflow_spec(), [workflow_spec()]) -> workflow_spec().
split(Spec, []) ->
    Spec;
split(Spec, Branches) ->
    %% Split the main spec, then parallel with all branches
    lists:foldl(fun(Branch, Acc) ->
        parallel(Acc, Branch)
    end, Spec, Branches).

%%--------------------------------------------------------------------
%% @doc Nest a sub-workflow with a boundary.
%%
%% Creates a scoped boundary around the sub-workflow. Used for
%% cancellation regions and transaction boundaries.
%%
%% === Example ===
%% ```erlang
%% Nested = wfnet_compose:nest(SubWorkflow, OuterWorkflow).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec nest(workflow_spec(), workflow_spec()) -> workflow_spec().
nest(SubSpec, OuterSpec) ->
    %% Entry point enters sub-workflow
    %% Sub-workflow exit returns to outer workflow
    EntryTrans = entry_transition(SubSpec),
    ExitTrans = exit_transition(SubSpec),

    sequence(
        OuterSpec,
        #{
            places => [],
            transitions => [EntryTrans, ExitTrans],
            start_place => EntryTrans,
            end_place => ExitTrans,
            preset => #{EntryTrans => []},
            postset => #{ExitTrans => []},
            optional => #{nested_workflow => SubSpec}
        }
    ).

%%--------------------------------------------------------------------
%% @doc Surround a workflow with pre and post processing.
%%
%% === Example ===
%% ```erlang
%% Processed = wfnet_compose:surround(PreProcess, MainWorkflow, PostProcess).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec surround(workflow_spec(), workflow_spec(), workflow_spec()) -> workflow_spec().
surround(Pre, Main, Post) ->
    sequence(sequence(Pre, Main), Post).

%%--------------------------------------------------------------------
%% @doc Wrap a single task as a workflow spec.
%%
%% === Example ===
%% ```erlang
%% TaskSpec = wfnet_compose:task(my_task).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec task(atom()) -> workflow_spec().
task(TaskName) when is_atom(TaskName) ->
    StartPlace = list_to_atom(atom_to_list(TaskName) ++ "_start"),
    EndPlace = list_to_atom(atom_to_list(TaskName) ++ "_end"),

    #{
        places => [StartPlace, EndPlace],
        transitions => [TaskName],
        start_place => StartPlace,
        end_place => EndPlace,
        preset => #{TaskName => [StartPlace]},
        postset => #{TaskName => [EndPlace]},
        optional => #{task_name => TaskName}
    }.

%%--------------------------------------------------------------------
%% @doc Validate a composed workflow specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_composition(workflow_spec()) -> ok | {error, term()}.
validate_composition(#{places := Places, transitions := Transitions,
                        start_place := Start, end_place := End}) ->
    %% Basic structural validation
    case {lists:member(Start, Places), lists:member(End, Places)} of
        {false, _} -> {error, {invalid_start_place, Start}};
        {_, false} -> {error, {invalid_end_place, End}};
        {true, true} -> ok
    end;
validate_composition(_) ->
    {error, invalid_spec_format}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generate a unique prefix for a workflow spec.
%%--------------------------------------------------------------------
-spec prefix(workflow_spec()) -> string().
prefix(#{optional := #{task_name := TaskName}}) ->
    atom_to_list(TaskName);
prefix(#{transitions := [FirstTrans | _]}) ->
    atom_to_list(FirstTrans);
prefix(_) ->
    integer_to_list(erlang:unique_integer([positive])).

%%--------------------------------------------------------------------
%% @private
%% @doc Rename a place with a prefix.
%%--------------------------------------------------------------------
-spec rename_place(place(), string()) -> place().
rename_place(Place, Prefix) ->
    list_to_atom(Prefix ++ "_" ++ atom_to_list(Place)).

%%--------------------------------------------------------------------
%% @private
%% @doc Rename a transition with a prefix.
%%--------------------------------------------------------------------
-spec rename_transition(trsn(), string()) -> trsn().
rename_transition(Trans, Prefix) ->
    list_to_atom(Prefix ++ "_" ++ atom_to_list(Trans)).

%%--------------------------------------------------------------------
%% @private
%% @doc Rename all places in a list.
%%--------------------------------------------------------------------
-spec rename_places([place()], string()) -> [place()].
rename_places(Places, Prefix) ->
    [rename_place(P, Prefix) || P <- Places].

%%--------------------------------------------------------------------
%% @private
%% @doc Rename all transitions in a list.
%%--------------------------------------------------------------------
-spec rename_transitions([trsn()], string()) -> [trsn()].
rename_transitions(Transitions, Prefix) ->
    [rename_transition(T, Prefix) || T <- Transitions].

%%--------------------------------------------------------------------
%% @private
%% @doc Rename preset map keys.
%%--------------------------------------------------------------------
-spec rename_preset_keys(#{trsn() => [place()]}, string()) -> #{trsn() => [place()]}.
rename_preset_keys(Preset, Prefix) ->
    maps:map(fun(_Trans, Places) ->
        [rename_place(P, Prefix) || P <- Places]
    end, Preset).

%%--------------------------------------------------------------------
%% @private
%% @doc Rename postset map keys.
%%--------------------------------------------------------------------
-spec rename_postset_keys(#{trsn() => [place()]}, string()) -> #{trsn() => [place()]}.
rename_postset_keys(Postset, Prefix) ->
    maps:map(fun(_Trans, Places) ->
        [rename_place(P, Prefix) || P <- Places]
    end, Postset).

%%--------------------------------------------------------------------
%% @private
%% @doc Create entry transition for nested workflow.
%%--------------------------------------------------------------------
-spec entry_transition(workflow_spec()) -> trsn().
entry_transition(#{start_place := Start}) ->
    list_to_atom("enter_" ++ atom_to_list(Start)).

%%--------------------------------------------------------------------
%% @private
%% @doc Create exit transition for nested workflow.
%%--------------------------------------------------------------------
-spec exit_transition(workflow_spec()) -> trsn().
exit_transition(#{end_place := End}) ->
    list_to_atom("exit_" ++ atom_to_list(End)).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Task creation test
task_test() ->
    Spec = task(my_task),
    ?assertMatch(#{places := [_, _]}, Spec),
    ?assertEqual([my_task], maps:get(transitions, Spec)).

%% Sequence composition test
sequence_test() ->
    A = task(task_a),
    B = task(task_b),
    Seq = sequence(A, B),
    ?assertMatch(#{transitions := Transitions} when length(Transitions) >= 3, Seq),
    ?assert(is_valid_start_end(Seq)).

%% Parallel composition test
parallel_test() ->
    A = task(task_a),
    B = task(task_b),
    Par = parallel(A, B),
    ?assertMatch(#{transitions := Transitions} when length(Transitions) >= 4, Par).

%% Choice composition test
choice_test() ->
    A = task(task_a),
    B = task(task_b),
    Ch = choice(A, B),
    ?assertMatch(#{transitions := Transitions} when length(Transitions) >= 4, Ch).

%% Loop test
loop_test() ->
    Body = task(process),
    Loop = loop(Body),
    ?assertMatch(#{transitions := Transitions} when length(Transitions) >= 3, Loop).

%% Validation test
validate_composition_test() ->
    A = task(task_a),
    ?assertEqual(ok, validate_composition(A)),
    ?assertEqual({error, invalid_spec_format}, validate_composition(#{})).

%% Helper for testing start/end places
is_valid_start_end(#{start_place := Start, end_place := End, places := Places}) ->
    lists:member(Start, Places) andalso lists:member(End, Places).

-endif.
