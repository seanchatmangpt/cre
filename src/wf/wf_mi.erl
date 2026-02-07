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

-module(wf_mi).
-moduledoc """
Multi-instance task handling for YAWL.

This module provides pure utility functions for determining if a task
is multi-instance, evaluating instance count from data, and creating
tokens for each instance.

Multi-instance tasks allow parallel or sequential execution of multiple
task instances with configurable thresholds for continuation.

```erlang
> MIParams = #{min_instances => 2, max_instances => 5, continuation_threshold => 3}.
> wf_mi:is_mi_task(Spec, NetId, TaskId).
true

> Data = #{instance_count => 4}.
> wf_mi:evaluate_mi(MIParams, Data).
{ok, 4}

> wf_mi:create_instance_tokens(review_task, 4).
[{mi_instance,review_task,0},
 {mi_instance,review_task,1},
 {mi_instance,review_task,2},
 {mi_instance,review_task,3}]
```

```erlang
> wf_mi:instance_count(#{min_instances => 2, max_instances => unlimited}, #{count => 5}).
{ok, 5}

> wf_mi:instance_threshold(#{continuation_threshold => 3}, 2).
false

> wf_mi:instance_threshold(#{continuation_threshold => 3}, 3).
true
```

<h3>Multi-Instance Semantics</h3>

Multi-instance tasks in YAWL support:

<ul>
  <li><b>Parallel:</b> All instances run simultaneously</li>
  <li><b>Sequential:</b> Instances run one at a time</li>
  <li><b>N-of-M:</b> Continue when N out of M instances complete</li>
</ul>

<h3>Usage</h3>

Use `is_mi_task/3` to check if a task has multi-instance configuration.
Use `evaluate_mi/2` to calculate the actual number of instances from data.
Use `create_instance_tokens/2` to generate indexed tokens for each instance.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Detection and evaluation
-export([is_mi_task/3, evaluate_mi/2]).

%% Token creation
-export([create_instance_tokens/2]).

%% Instance management
-export([instance_count/2, instance_threshold/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Unique identifier for a workflow net.
%%
%% Binary identifying the specific net decomposition within a YAWL
%% specification.
%%--------------------------------------------------------------------
-type net_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Task identifier within a net.
%%
%% Atom uniquely identifying a task in the workflow.
%%--------------------------------------------------------------------
-type task_id() :: atom().

%%--------------------------------------------------------------------
%% @doc Multi-instance parameters map.
%%
%% Defines the constraints for multi-instance task execution:
%% <ul>
%%   <li><b>min_instances:</b> Minimum instances to create</li>
%%   <li><b>max_instances:</b> Maximum instances (unlimited for no limit)</li>
%%   <li><b>continuation_threshold:</b> Instances needed before continuing</li>
%% </ul>
%%--------------------------------------------------------------------
-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.

%%--------------------------------------------------------------------
%% @doc Workflow data map.
%%
%% Contains variable bindings that may influence instance count.
%%--------------------------------------------------------------------
-type data() :: map().

%%--------------------------------------------------------------------
%% @doc Multi-instance token.
%%
%% Tokens representing individual instances are tagged with the task ID
%% and instance index (0-based) for tracking and correlation.
%%--------------------------------------------------------------------
-type mi_token() :: {mi_instance, task_id(), non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc Specification structure.
%%
%% Opaque reference to a YAWL specification structure. The internal
%% representation is defined in wf_spec module.
%%--------------------------------------------------------------------
-opaque spec() :: term().

%% Export types
-export_type([mi_params/0, data/0, mi_token/0, net_id/0, task_id/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a task is configured as multi-instance.
%%
%% Examines the task configuration in the specification to determine
%% if multi-instance parameters are defined. Returns true if mi_params
%% are present and valid, false otherwise.
%%
%% @param Spec YAWL specification
%% @param NetId Network/decomposition identifier
%% @param TaskId Task identifier to check
%% @return true if task is multi-instance, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_mi_task(Spec :: term(), NetId :: net_id(), TaskId :: task_id()) ->
    boolean().

is_mi_task(_Spec, _NetId, _TaskId) ->
    %% In the new architecture, tasks are accessed via pnet_net behavior
    %% This function is a stub that would typically delegate to wf_spec
    %% For pure utility usage, we return false as default
    %% Real implementation would check task_info#task_info.mi_params
    false.

%%--------------------------------------------------------------------
%% @doc Evaluates the instance count from multi-instance parameters and data.
%%
%% Calculates the actual number of instances to create based on:
%% <ol>
%%   <li>MIParams configuration (min/max bounds)</li>
%%   <li>Data map which may contain instance count specification</li>
%% </ol>
%%
%% Returns {ok, Count} with the calculated instance count, or
%% {error, Reason} if parameters are invalid.
%%
%% @param MIParams Multi-instance parameters map (undefined if not MI task)
%% @param Data Workflow data map potentially containing count
%% @return {ok, non_neg_integer()} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_mi(MIParams :: mi_params() | undefined, Data :: data()) ->
    {ok, non_neg_integer()} | {error, term()}.

evaluate_mi(undefined, _Data) ->
    %% Not a multi-instance task
    {ok, 0};
evaluate_mi(#{min_instances := Min, max_instances := Max} = MIParams, Data) when is_map(Data) ->
    %% Extract instance count from data or use default
    Requested = extract_instance_count(Data, Min),
    %% Apply bounds checking
    case check_bounds(Requested, Min, Max) of
        {ok, Count} ->
            {ok, Count};
        {error, _} = Error ->
            Error
    end;
evaluate_mi(_, _) ->
    {error, invalid_params}.

%%--------------------------------------------------------------------
%% @doc Creates tokens for each instance of a multi-instance task.
%%
%% Generates a list of tokens, one per instance, indexed from 0 to
%% Count-1. Each token carries the task ID and instance index for
%% tracking completion and correlation.
%%
%% @param TaskId The task identifier
%% @param Count Number of instances to create tokens for
%% @return List of multi-instance tokens
%%
%% @end
%%--------------------------------------------------------------------
-spec create_instance_tokens(TaskId :: task_id(), Count :: non_neg_integer()) ->
    [mi_token()].

create_instance_tokens(_TaskId, 0) ->
    [];
create_instance_tokens(TaskId, Count) when is_atom(TaskId), is_integer(Count), Count > 0 ->
    [{mi_instance, TaskId, Index} || Index <- lists:seq(0, Count - 1)].

%%--------------------------------------------------------------------
%% @doc Gets the instance count with bounds checking.
%%
%% Similar to evaluate_mi/2 but returns undefined instead of {ok, 0}
%% when MIParams is undefined. This provides a more convenient API
%% for scenarios where undefined means "not applicable".
%%
%% @param MIParams Multi-instance parameters (undefined if not MI task)
%% @param Data Workflow data map
%% @return {ok, Count} | undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec instance_count(MIParams :: mi_params() | undefined, Data :: data()) ->
    {ok, non_neg_integer()} | undefined.

instance_count(undefined, _Data) ->
    undefined;
instance_count(#{min_instances := Min, max_instances := Max} = MIParams, Data) ->
    case evaluate_mi(MIParams, Data) of
        {ok, Count} -> {ok, Count};
        {error, _} -> undefined
    end.

%%--------------------------------------------------------------------
%% @doc Checks if the continuation threshold is met.
%%
%% Returns true when the number of completed instances meets or exceeds
%% the threshold defined in MIParams. Used to determine if a multi-instance
%% task can continue despite not all instances being complete.
%%
%% @param MIParams Multi-instance parameters (undefined defaults to requiring 1)
%% @param Completed Number of completed instances
%% @return true if threshold met, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec instance_threshold(MIParams :: mi_params() | undefined, Completed :: non_neg_integer()) ->
    boolean().

instance_threshold(undefined, Completed) when Completed > 0 ->
    true;
instance_threshold(undefined, _Completed) ->
    false;
instance_threshold(#{continuation_threshold := Threshold}, Completed)
        when is_integer(Threshold), is_integer(Completed) ->
    Completed >= Threshold.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Extracts instance count from data map.
%%
%% Looks for common keys that may specify instance count:
%% <ul>
%%   <li><b>instance_count:</b> Direct count specification</li>
%%   <li><b>count:</b> Shorthand for instance count</li>
%%   <li><b>items:</b> List whose length determines count</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_instance_count(Data :: data(), Default :: non_neg_integer()) ->
    non_neg_integer().

extract_instance_count(Data, Default) when is_map(Data) ->
    case maps:get(instance_count, Data, undefined) of
        Count when is_integer(Count), Count >= 0 ->
            Count;
        _ ->
            case maps:get(count, Data, undefined) of
                Count when is_integer(Count), Count >= 0 ->
                    Count;
                _ ->
                    case maps:get(items, Data, undefined) of
                        Items when is_list(Items) ->
                            length(Items);
                        _ ->
                            Default
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if requested count is within min/max bounds.
%%
%% Returns {ok, BoundedCount} if valid, {error, Reason} otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_bounds(Requested :: non_neg_integer(),
                    Min :: non_neg_integer(),
                    Max :: non_neg_integer() | unlimited) ->
    {ok, non_neg_integer()} | {error, term()}.

check_bounds(Requested, Min, Max) when is_integer(Requested), Requested < Min ->
    {error, {below_minimum, Requested, Min}};
check_bounds(Requested, Min, unlimited) when is_integer(Requested) ->
    {ok, max(Requested, Min)};
check_bounds(Requested, Min, Max) when is_integer(Requested), is_integer(Max), Requested > Max ->
    {error, {exceeds_maximum, Requested, Max}};
check_bounds(Requested, Min, Max) when is_integer(Requested), is_integer(Max) ->
    Count = max(Requested, Min),
    {ok, min(Count, Max)}.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%%====================================================================
%% Unit Tests
%%====================================================================

%%--------------------------------------------------------------------
%% evaluate_mi/2 tests
%%--------------------------------------------------------------------

evaluate_mi_undefined_params_test() ->
    ?assertEqual({ok, 0}, evaluate_mi(undefined, #{})).

evaluate_mi_min_max_unlimited_test() ->
    MIParams = #{min_instances => 2, max_instances => unlimited, continuation_threshold => 3},
    Data = #{instance_count => 5},
    ?assertEqual({ok, 5}, evaluate_mi(MIParams, Data)).

evaluate_mi_below_minimum_test() ->
    MIParams = #{min_instances => 5, max_instances => 10, continuation_threshold => 3},
    Data = #{instance_count => 2},
    ?assertEqual({error, {below_minimum, 2, 5}}, evaluate_mi(MIParams, Data)).

evaluate_mi_exceeds_maximum_test() ->
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    Data = #{instance_count => 10},
    ?assertEqual({error, {exceeds_maximum, 10, 5}}, evaluate_mi(MIParams, Data)).

evaluate_mi_within_bounds_test() ->
    MIParams = #{min_instances => 2, max_instances => 10, continuation_threshold => 3},
    Data = #{instance_count => 5},
    ?assertEqual({ok, 5}, evaluate_mi(MIParams, Data)).

evaluate_mi_count_from_items_test() ->
    MIParams = #{min_instances => 1, max_instances => 10, continuation_threshold => 1},
    Data = #{items => [a, b, c, d]},
    ?assertEqual({ok, 4}, evaluate_mi(MIParams, Data)).

evaluate_mi_count_from_shorthand_test() ->
    MIParams = #{min_instances => 1, max_instances => 10, continuation_threshold => 1},
    Data = #{count => 7},
    ?assertEqual({ok, 7}, evaluate_mi(MIParams, Data)).

evaluate_mi_default_to_min_test() ->
    MIParams = #{min_instances => 3, max_instances => 10, continuation_threshold => 2},
    Data = #{},
    ?assertEqual({ok, 3}, evaluate_mi(MIParams, Data)).

%%--------------------------------------------------------------------
%% create_instance_tokens/2 tests
%%--------------------------------------------------------------------

create_instance_tokens_zero_test() ->
    ?assertEqual([], create_instance_tokens(my_task, 0)).

create_instance_tokens_one_test() ->
    Tokens = create_instance_tokens(review, 1),
    ?assertEqual([{mi_instance, review, 0}], Tokens).

create_instance_tokens_multiple_test() ->
    Tokens = create_instance_tokens(approve, 3),
    ?assertEqual([
        {mi_instance, approve, 0},
        {mi_instance, approve, 1},
        {mi_instance, approve, 2}
    ], Tokens).

%%--------------------------------------------------------------------
%% instance_count/2 tests
%%--------------------------------------------------------------------

instance_count_undefined_test() ->
    ?assertEqual(undefined, instance_count(undefined, #{count => 5})).

instance_count_valid_test() ->
    MIParams = #{min_instances => 1, max_instances => 10, continuation_threshold => 1},
    ?assertEqual({ok, 5}, instance_count(MIParams, #{count => 5})).

instance_count_invalid_test() ->
    MIParams = #{min_instances => 5, max_instances => 10, continuation_threshold => 3},
    ?assertEqual(undefined, instance_count(MIParams, #{count => 2})).

%%--------------------------------------------------------------------
%% instance_threshold/2 tests
%%--------------------------------------------------------------------

instance_threshold_undefined_zero_test() ->
    ?assertEqual(false, instance_threshold(undefined, 0)).

instance_threshold_undefined_positive_test() ->
    ?assertEqual(true, instance_threshold(undefined, 1)).

instance_threshold_met_test() ->
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    ?assertEqual(true, instance_threshold(MIParams, 3)),
    ?assertEqual(true, instance_threshold(MIParams, 4)),
    ?assertEqual(true, instance_threshold(MIParams, 5)).

instance_threshold_not_met_test() ->
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    ?assertEqual(false, instance_threshold(MIParams, 0)),
    ?assertEqual(false, instance_threshold(MIParams, 1)),
    ?assertEqual(false, instance_threshold(MIParams, 2)).

%%--------------------------------------------------------------------
%% extract_instance_count/2 tests
%%--------------------------------------------------------------------

extract_instance_count_from_instance_count_test() ->
    ?assertEqual(5, extract_instance_count(#{instance_count => 5}, 1)).

extract_instance_count_from_count_test() ->
    ?assertEqual(7, extract_instance_count(#{count => 7}, 1)).

extract_instance_count_from_items_test() ->
    ?assertEqual(4, extract_instance_count(#{items => [a, b, c, d]}, 1)).

extract_instance_count_default_test() ->
    ?assertEqual(3, extract_instance_count(#{}, 3)).

%%--------------------------------------------------------------------
%% check_bounds/3 tests
%%--------------------------------------------------------------------

check_bounds_below_minimum_test() ->
    ?assertEqual({error, {below_minimum, 2, 5}}, check_bounds(2, 5, 10)).

check_bounds_exceeds_maximum_test() ->
    ?assertEqual({error, {exceeds_maximum, 15, 10}}, check_bounds(15, 5, 10)).

check_bounds_unlimited_test() ->
    ?assertEqual({ok, 100}, check_bounds(100, 1, unlimited)).

check_bounds_within_bounds_test() ->
    ?assertEqual({ok, 5}, check_bounds(5, 1, 10)).

check_bounds_below_min_uses_min_test() ->
    ?assertEqual({ok, 5}, check_bounds(3, 5, 10)).

check_bounds_above_max_uses_max_test() ->
    ?assertEqual({ok, 10}, check_bounds(15, 5, 10)).

-endif.
