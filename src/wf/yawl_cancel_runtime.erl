%% -*- erlang -*-
%%
-module(yawl_cancel_runtime).
-moduledoc """
Cancellation runtime for YAWL workflows.

Processes cancellation tokens and applies cancellation regions during
workflow execution. This module provides the runtime layer for handling
YAWL cancellation semantics, delegating token operations to wf_cancel
and specification queries to wf_spec.

## Overview

The cancellation runtime is responsible for:

- **Token Detection**: Identifying cancellation tokens in markings
- **Region Resolution**: Mapping cancellation sets to place collections
- **Cancellation Application**: Removing tokens from cancelled regions
- **Cascade Handling**: Managing cascading cancellations

## Basic Usage

```erlang
> Marking = #{
..   p1 => [a],
..   p2 => [b],
..   p3 => [{cancel, [p2, p3]}]
.. }.
> yawl_cancel_runtime:process_cancel_tokens(Marking).
{#{p1 => [a], p2 => [], p3 => [{cancel, [p2, p3]}]}, [[p2, p3]]}
```

## Specification-Based Cancellation

```erlang
> {ok, Spec} = wf_spec:from_xml(Xml),
> Regions = yawl_cancel_runtime:get_cancellation_regions(Spec, main),
> yawl_cancel_runtime:should_cancel(Marking, task1, Regions).
true
```

## Cancellation Types

- **Cancel Case**: Terminate entire workflow (all places cleared)
- **Cancel Region**: Terminate activities within region
- **Cancel Activity**: Terminate specific task
- **Cascade Cancellation**: Propagate cancellation through regions

<h3>Runtime Architecture</h3>
The runtime operates in three phases:
<ol>
  <li><strong>Scan:</strong> Find all cancel tokens in marking</li>
  <li><strong>Resolve:</strong> Map cancel sets to concrete places</li>
  <li><strong>Apply:</strong> Remove tokens from resolved places</li>
</ol>

<h3>Integration Points</h3>
<ul>
  <li><strong>wf_cancel:</strong> Token validation and cancellation application</li>
  <li><strong>wf_spec:</strong> Specification metadata and region definitions</li>
  <li><strong>gen_pnet:</strong> Marking processing during workflow execution</li>
</ul>
""".

%%====================================================================
%% Exports
%%====================================================================

%% Token validation and inspection
-export([is_cancel_token/1, extract_cancel_set/1]).

%% Cancellation application
-export([apply_cancellation/2, process_cancel_tokens/1]).

%% Specification-based operations
-export([get_cancellation_regions/1, should_cancel/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net workflow.
%%
%% Places are locations where tokens reside.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net workflow.
%%
%% Tokens can be any Erlang term, including cancellation tokens.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Each place atom maps to a list of tokens currently in that place.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A cancellation set is a list of places to cancel.
%%
%% These places will have their tokens removed when cancellation
%% is applied.
%%--------------------------------------------------------------------
-type cancellation_set() :: [place()].

%%--------------------------------------------------------------------
%% @doc A task identifier in the workflow.
%%
%% Tasks correspond to YAWL task definitions.
%%--------------------------------------------------------------------
-type task_id() :: atom().

%%--------------------------------------------------------------------
%% @doc A net identifier (decomposition ID).
%%
%% Binary identifier for a YAWL decomposition net.
%%--------------------------------------------------------------------
-type net_id() :: binary().

%%--------------------------------------------------------------------
%% @doc A YAWL specification record.
%%
%% Opaque type representing a parsed YAWL specification.
%%--------------------------------------------------------------------
-type yawl_spec() :: wf_spec:yawl_spec().

%%--------------------------------------------------------------------
%% @doc A cancellation region maps tasks to their cancellation sets.
%%
%% Each tuple contains a task ID and the list of tasks/places
%% that should be cancelled when that task completes.
%%--------------------------------------------------------------------
-type cancellation_region() :: {task_id(), [place()]}.

%%--------------------------------------------------------------------
%% @doc Result of processing cancellation tokens.
%%
%% Returns the updated marking and a list of cancelled regions
%% (each region is a list of places).
%%--------------------------------------------------------------------
-type cancel_result() :: {marking(), [[place()]]}.

%% Export types
-export_type([place/0, token/0, marking/0, cancellation_set/0,
              task_id/0, net_id/0, cancellation_region/0,
              cancel_result/0]).

%%====================================================================
%% Token Validation and Inspection Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cancel token.
%%
%% Delegates to wf_cancel:is_cancel_token/1. A valid cancel token
%% is a 2-tuple {cancel, [Place]} where Place is a list of place atoms.
%%
%% ```erlang
%% > yawl_cancel_runtime:is_cancel_token({cancel, [p1, p2]}).
%% true
%% > yawl_cancel_runtime:is_cancel_token({other, [p1]}).
%% false
%% > yawl_cancel_runtime:is_cancel_token(not_a_token).
%% false
%% ```
%%
%% @param Token The term to check
%% @return true if valid cancellation token
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cancel_token(Token :: term()) -> boolean().

is_cancel_token(Token) ->
    wf_cancel:is_cancel_token(Token).

%%--------------------------------------------------------------------
%% @doc Extracts the cancellation set from a cancel token.
%%
%% Returns the list of places to cancel, or undefined if the token
%% is invalid. Delegates to wf_cancel:cancel_targets/1.
%%
%% ```erlang
%% > yawl_cancel_runtime:extract_cancel_set({cancel, [p1, p2, p3]}).
%% [p1, p2, p3]
%% > yawl_cancel_runtime:extract_cancel_set(not_a_token).
%% []
%% ```
%%
%% @param Token The cancellation token
%% @return List of places to cancel (empty if invalid)
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_cancel_set(Token :: term()) -> cancellation_set() | [].

extract_cancel_set(Token) ->
    wf_cancel:cancel_targets(Token).

%%====================================================================
%% Cancellation Application Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Applies a cancellation set to a marking.
%%
%% All places in the cancellation set have their tokens removed
%% (set to empty lists). Delegates to wf_cancel:apply_cancellation/2.
%%
%% ```erlang
%% > Marking = #{p1 => [a, b], p2 => [c], p3 => [d]}.
%% > CancelSet = [p1, p3].
%% > yawl_cancel_runtime:apply_cancellation(Marking, CancelSet).
%% #{p1 => [], p2 => [c], p3 => []}
%% ```
%%
%% @param Marking The current marking
%% @param CancelSet List of places to cancel
%% @return Updated marking with places cleared
%%
%% @end
%%--------------------------------------------------------------------
-spec apply_cancellation(Marking :: marking(), CancelSet :: cancellation_set()) ->
    marking().

apply_cancellation(Marking, CancelSet) ->
    wf_cancel:apply_cancellation(Marking, CancelSet).

%%--------------------------------------------------------------------
%% @doc Processes all cancellation tokens in a marking.
%%
%% Scans the marking for cancel tokens, extracts their cancellation sets,
%% and applies each cancellation. Returns the updated marking and a list
%% of cancelled regions (each region is a list of places).
%%
%% This is the main runtime entry point for cancellation processing
%% during workflow execution.
%%
%% ```erlang
%% > Marking = #{
%% ..   trigger => [{cancel, [p2, p3]}],
%% ..   p1 => [a],
%% ..   p2 => [b, c],
%% ..   p3 => [d],
%% ..   p4 => [e]
%% .. }.
%% > yawl_cancel_runtime:process_cancel_tokens(Marking).
%% {#{trigger => [{cancel, [p2, p3]}], p1 => [a], p2 => [], p3 => [], p4 => [e]},
%%  [[p2, p3]]}
%%
%% > Marking2 = #{
%% ..   t1 => [{cancel, [p1]}],
%% ..   t2 => [{cancel, [p2, p3]}],
%% ..   p1 => [a],
%% ..   p2 => [b],
%% ..   p3 => [c]
%% .. }.
%% > {Updated, Regions} = yawl_cancel_runtime:process_cancel_tokens(Marking2).
%% > Updated.
%% #{t1 => [{cancel, [p1]}], t2 => [{cancel, [p2, p3]}], p1 => [], p2 => [], p3 => []}
%% > Regions.
%% [[p1], [p2, p3]]
%% ```
%%
%% @param Marking The current marking
%% @return {UpdatedMarking, CancelledRegions} tuple
%%
%% @end
%%--------------------------------------------------------------------
-spec process_cancel_tokens(Marking :: marking()) -> cancel_result().

process_cancel_tokens(Marking) ->
    %% Extract all cancellation sets from tokens in the marking
    CancelSets = extract_all_cancel_sets(Marking),

    %% Apply each cancellation set to the marking
    {UpdatedMarking, CancelledRegions} = lists:foldl(
        fun(CancelSet, {AccMarking, AccRegions}) ->
            %% Apply this cancellation set
            NewMarking = apply_cancellation(AccMarking, CancelSet),
            %% Track the cancelled region
            {NewMarking, [CancelSet | AccRegions]}
        end,
        {Marking, []},
        CancelSets
    ),

    %% Return updated marking and list of cancelled regions (in original order)
    {UpdatedMarking, lists:reverse(CancelledRegions)}.

%%====================================================================
%% Specification-Based Operations Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets all cancellation regions from a YAWL specification.
%%
%% Queries the specification for all tasks that have cancellation sets
%% defined. Returns a list of {TaskId, CancelSet} tuples where CancelSet
%% is the list of tasks/places cancelled when TaskId completes.
%%
%% Delegates to wf_spec:cancellation_regions/1.
%%
%% ```erlang
%% > {ok, Spec} = wf_spec:from_file("order_fulfillment.yawl").
%% > yawl_cancel_runtime:get_cancellation_regions(Spec).
%% [{carrier_timeout, [pending_appointment, awaiting_confirmation]},
%%  {order_cancelled, [shipping, billing]}]
%% ```
%%
%% @param Spec The YAWL specification
%% @return List of {TaskId, CancelSet} tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cancellation_regions(Spec :: yawl_spec()) -> [cancellation_region()].

get_cancellation_regions(Spec) ->
    wf_spec:cancellation_regions(Spec).

%%--------------------------------------------------------------------
%% @doc Checks if a task should trigger cancellation based on marking.
%%
%% Determines if any cancellation region for the specified task has
%% been triggered by examining the current marking. Returns true if
%% cancellation should fire.
%%
%% This is useful for workflow engines to check whether a task's
%% completion should trigger cancellation of other regions.
%%
%% ```erlang
%% > {ok, Spec} = wf_spec:from_file("workflow.yawl").
%% > Regions = yawl_cancel_runtime:get_cancellation_regions(Spec).
%% > Marking = #{timeout => [expired], task1 => [running]}.
%% > yawl_cancel_runtime:should_cancel(Markin, timeout_trigger, Regions).
%% true
%% > yawl_cancel_runtime:should_cancel(Marking, task1, Regions).
%% false
%% ```
%%
%% @param Marking The current marking
%% @param TaskId The task to check
%% @param Regions List of cancellation regions from spec
%% @return true if task's cancellation region is triggered
%%
%% @end
%%--------------------------------------------------------------------
-spec should_cancel(Marking :: marking(), TaskId :: task_id(),
                    Regions :: [cancellation_region()]) -> boolean().

should_cancel(Marking, TaskId, Regions) ->
    %% Find cancellation regions for this task
    TaskRegions = lists:filtermap(
        fun({RegionTaskId, CancelSet}) ->
            case RegionTaskId of
                TaskId -> {true, CancelSet};
                _ -> false
            end
        end,
        Regions
    ),

    %% Check if any place in any of the task's regions has tokens
    lists:any(
        fun(CancelSet) ->
            lists:any(
                fun(Place) ->
                    case maps:get(Place, Marking, undefined) of
                        undefined -> false;
                        [] -> false;
                        _Tokens -> true
                    end
                end,
                CancelSet
            )
        end,
        TaskRegions
    ).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts all cancellation sets from tokens in a marking.
%%
%% Scans every place in the marking, finds all cancellation tokens,
%% and extracts their target lists. Returns a list of unique
%% cancellation sets.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_all_cancel_sets(Marking :: marking()) -> [cancellation_set()].

extract_all_cancel_sets(Marking) ->
    %% Collect all cancellation sets from tokens
    AllSets = maps:fold(
        fun(_Place, Tokens, Acc) ->
            CancelSets = lists:filtermap(
                fun(Token) ->
                    case is_cancel_token(Token) of
                        true ->
                            Set = extract_cancel_set(Token),
                            case Set of
                                [] -> false;
                                _ -> {true, Set}
                            end;
                        false ->
                            false
                    end
                end,
                Tokens
            ),
            CancelSets ++ Acc
        end,
        [],
        Marking
    ),

    %% Return unique sets (preserve order, remove exact duplicates)
    lists:usort(AllSets).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test is_cancel_token delegation
%%--------------------------------------------------------------------
is_cancel_token_test() ->
    ?assertEqual(true, is_cancel_token({cancel, [p1, p2]})),
    ?assertEqual(true, is_cancel_token({cancel, []})),
    ?assertEqual(false, is_cancel_token({other, [p1]})),
    ?assertEqual(false, is_cancel_token(not_a_token)).

%%--------------------------------------------------------------------
%% @doc Test extract_cancel_set delegation
%%--------------------------------------------------------------------
extract_cancel_set_test() ->
    ?assertEqual([p1, p2, p3], extract_cancel_set({cancel, [p1, p2, p3]})),
    ?assertEqual([], extract_cancel_set({cancel, []})),
    ?assertEqual([], extract_cancel_set(not_a_token)).

%%--------------------------------------------------------------------
%% @doc Test apply_cancellation delegation
%%--------------------------------------------------------------------
apply_cancellation_test() ->
    Marking = #{p1 => [a, b], p2 => [c], p3 => [d]},
    ?assertEqual(#{p1 => [], p2 => [c], p3 => []},
                 apply_cancellation(Marking, [p1, p3])).

%%--------------------------------------------------------------------
%% @doc Test process_cancel_tokens with single cancellation
%%--------------------------------------------------------------------
process_cancel_tokens_single_test() ->
    Marking = #{
        trigger => [{cancel, [p2, p3]}],
        p1 => [a],
        p2 => [b, c],
        p3 => [d],
        p4 => [e]
    },
    {Updated, Regions} = process_cancel_tokens(Marking),
    ?assertEqual([], maps:get(p2, Updated)),
    ?assertEqual([], maps:get(p3, Updated)),
    ?assertEqual([a], maps:get(p1, Updated)),
    ?assertEqual([e], maps:get(p4, Updated)),
    ?assertEqual([[p2, p3]], Regions).

%%--------------------------------------------------------------------
%% @doc Test process_cancel_tokens with multiple cancellations
%%--------------------------------------------------------------------
process_cancel_tokens_multiple_test() ->
    Marking = #{
        t1 => [{cancel, [p1]}],
        t2 => [{cancel, [p2, p3]}],
        p1 => [a],
        p2 => [b],
        p3 => [c]
    },
    {Updated, Regions} = process_cancel_tokens(Marking),
    ?assertEqual([], maps:get(p1, Updated)),
    ?assertEqual([], maps:get(p2, Updated)),
    ?assertEqual([], maps:get(p3, Updated)),
    ?assertEqual(2, length(Regions)),
    ?assert(lists:member([p1], Regions)),
    ?assert(lists:member([p2, p3], Regions)).

%%--------------------------------------------------------------------
%% @doc Test process_cancel_tokens with no cancellation tokens
%%--------------------------------------------------------------------
process_cancel_tokens_none_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    {Updated, Regions} = process_cancel_tokens(Marking),
    ?assertEqual(Marking, Updated),
    ?assertEqual([], Regions).

%%--------------------------------------------------------------------
%% @doc Test process_cancel_tokens with empty marking
%%--------------------------------------------------------------------
process_cancel_tokens_empty_test() ->
    Marking = #{},
    {Updated, Regions} = process_cancel_tokens(Marking),
    ?assertEqual(#{}, Updated),
    ?assertEqual([], Regions).

%%--------------------------------------------------------------------
%% @doc Test process_cancel_tokens preserves cancellation tokens
%%--------------------------------------------------------------------
process_cancel_tokens_preserve_test() ->
    Marking = #{
        trigger => [{cancel, [p1]}, other_token],
        p1 => [a]
    },
    {Updated, _Regions} = process_cancel_tokens(Marking),
    %% Cancellation token should remain at trigger place
    ?assertEqual([{cancel, [p1]}, other_token], maps:get(trigger, Updated)),
    %% But p1 should be cancelled
    ?assertEqual([], maps:get(p1, Updated)).

%%--------------------------------------------------------------------
%% @doc Test extract_all_cancel_sets
%%--------------------------------------------------------------------
extract_all_cancel_sets_test() ->
    Marking = #{
        t1 => [{cancel, [p1, p2]}, a],
        t2 => [{cancel, [p3]}],
        p1 => [b]
    },
    Sets = extract_all_cancel_sets(Marking),
    ?assert(lists:member([p1, p2], Sets)),
    ?assert(lists:member([p3], Sets)),
    ?assertEqual(2, length(Sets)).

%%--------------------------------------------------------------------
%% @doc Test extract_all_cancel_sets removes duplicates
%%--------------------------------------------------------------------
extract_all_cancel_sets_dedup_test() ->
    Marking = #{
        t1 => [{cancel, [p1, p2]}],
        t2 => [{cancel, [p1, p2]}]
    },
    Sets = extract_all_cancel_sets(Marking),
    ?assertEqual([[p1, p2]], Sets).

%%--------------------------------------------------------------------
%% @doc Test should_cancel with active region
%%--------------------------------------------------------------------
should_cancel_true_test() ->
    Marking = #{
        trigger => [active],
        p1 => [token1]
    },
    Regions = [{trigger, [p1, p2]}],
    ?assertEqual(true, should_cancel(Marking, trigger, Regions)).

%%--------------------------------------------------------------------
%% @doc Test should_cancel with empty region
%%--------------------------------------------------------------------
should_cancel_false_empty_test() ->
    Marking = #{
        trigger => [active],
        p1 => []
    },
    Regions = [{trigger, [p1, p2]}],
    ?assertEqual(false, should_cancel(Marking, trigger, Regions)).

%%--------------------------------------------------------------------
%% @doc Test should_cancel with no matching regions
%%--------------------------------------------------------------------
should_cancel_no_regions_test() ->
    Marking = #{p1 => [token]},
    Regions = [{other_task, [p2]}],
    ?assertEqual(false, should_cancel(Marking, p1, Regions)).

%%--------------------------------------------------------------------
%% @doc Test should_cancel with multiple regions
%%--------------------------------------------------------------------
should_cancel_multiple_test() ->
    Marking = #{
        t1 => [active],
        t2 => [inactive],
        p1 => [token1],
        p2 => []
    },
    Regions = [
        {t1, [p1]},
        {t2, [p2]},
        {t1, [p3]}  % t1 has two cancellation sets
    ],
    %% t1 should cancel because p1 has tokens
    ?assertEqual(true, should_cancel(Marking, t1, Regions)),
    %% t2 should not cancel because p2 is empty
    ?assertEqual(false, should_cancel(Marking, t2, Regions)).

%%--------------------------------------------------------------------
%% @doc Test should_cancel with non-existent places
%%--------------------------------------------------------------------
should_cancel_missing_place_test() ->
    Marking = #{
        trigger => [active]
        % p1 not in marking
    },
    Regions = [{trigger, [p1, p2]}],
    ?assertEqual(false, should_cancel(Marking, trigger, Regions)).

%%--------------------------------------------------------------------
%% @doc Test cascade cancellation scenario
%%--------------------------------------------------------------------
cascade_cancellation_test() ->
    Marking = #{
        timeout => [{cancel, [region_a]}],
        region_a => [{cancel, [region_b]}],
        region_b => [data],
        region_c => [safe]
    },
    {Updated, Regions} = process_cancel_tokens(Marking),
    %% Both regions should be cancelled
    ?assertEqual([], maps:get(region_a, Updated)),
    ?assertEqual([], maps:get(region_b, Updated)),
    %% region_c should be preserved
    ?assertEqual([safe], maps:get(region_c, Updated)),
    %% Two cancellations occurred
    ?assertEqual(2, length(Regions)).

%%--------------------------------------------------------------------
%% @doc Test cancellation case (all places)
%%--------------------------------------------------------------------
cancel_case_test() ->
    %% Simulate cancel entire case
    AllPlaces = [p1, p2, p3, p4],
    Marking = lists:foldl(
        fun(P, Acc) -> Acc#{P => [some_token]} end,
        #{},
        AllPlaces
    ),
    CancelSet = AllPlaces,
    Updated = apply_cancellation(Marking, CancelSet),
    %% All places should be empty
    lists:foreach(
        fun(P) -> ?assertEqual([], maps:get(P, Updated)) end,
        AllPlaces
    ).

%%--------------------------------------------------------------------
%% @doc Test cancel region (partial places)
%%--------------------------------------------------------------------
cancel_region_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c],
        p4 => [d]
    },
    Region = [p2, p3],
    Updated = apply_cancellation(Marking, Region),
    ?assertEqual([a], maps:get(p1, Updated)),
    ?assertEqual([], maps:get(p2, Updated)),
    ?assertEqual([], maps:get(p3, Updated)),
    ?assertEqual([d], maps:get(p4, Updated)).

%%--------------------------------------------------------------------
%% @doc Test cancel activity (single place)
%%--------------------------------------------------------------------
cancel_activity_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c]
    },
    Activity = [p2],
    Updated = apply_cancellation(Marking, Activity),
    ?assertEqual([a], maps:get(p1, Updated)),
    ?assertEqual([], maps:get(p2, Updated)),
    ?assertEqual([c], maps:get(p3, Updated)).

%%--------------------------------------------------------------------
%% @doc Test cancellation with mixed token types
%%--------------------------------------------------------------------
mixed_tokens_test() ->
    Marking = #{
        trigger => [{cancel, [p1]}, {cancel, [p2]}, other_token],
        p1 => [{complex, tuple}, atom, 42],
        p2 => [<<"binary">>, [list]],
        p3 => [safe]
    },
    {Updated, Regions} = process_cancel_tokens(Marking),
    %% Both cancellations should apply
    ?assertEqual([], maps:get(p1, Updated)),
    ?assertEqual([], maps:get(p2, Updated)),
    ?assertEqual([safe], maps:get(p3, Updated)),
    %% Two regions cancelled
    ?assertEqual(2, length(Regions)).

%%--------------------------------------------------------------------
%% @doc Test idempotent cancellation
%%--------------------------------------------------------------------
idempotent_cancellation_test() ->
    Marking = #{p1 => [a]},
    CancelSet = [p1],
    %% First cancellation
    Updated1 = apply_cancellation(Marking, CancelSet),
    ?assertEqual([], maps:get(p1, Updated1)),
    %% Second cancellation (should be no-op)
    Updated2 = apply_cancellation(Updated1, CancelSet),
    ?assertEqual([], maps:get(p1, Updated2)),
    ?assertEqual(Updated1, Updated2).

%%--------------------------------------------------------------------
%% @doc Test cancellation with empty cancellation set
%%--------------------------------------------------------------------
empty_cancel_set_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    Updated = apply_cancellation(Marking, []),
    ?assertEqual(Marking, Updated).

%%--------------------------------------------------------------------
%% @doc Test cancellation adds new places to marking
%%--------------------------------------------------------------------
cancel_adds_places_test() ->
    Marking = #{p1 => [a]},
    %% Cancellation for non-existent place should add it as empty
    Updated = apply_cancellation(Marking, [p2]),
    ?assertEqual([a], maps:get(p1, Updated)),
    ?assertEqual([], maps:get(p2, Updated)).

-endif.
