%% -*- erlang -*-
%%
-module(yawl_mi_runtime).
-moduledoc """
Multi-instance runtime for YAWL workflow execution.

This module connects `wf_mi` with `gen_pnet` concepts to manage the lifecycle
of multi-instance tasks in workflow execution. It provides pure functional
utilities for creating, tracking, and completing multi-instance task tokens
within Petri net markings.

Multi-instance tasks allow parallel or sequential execution of multiple task
instances with configurable thresholds for continuation. This runtime manages
the token-based representation of instances within the Petri net marking.

<h3>Execution Flow Diagram</h3>

```
YAWL Task Enabled (MI Token Present)
        |
        v
evaluate_mi(MIParams, Data)  ->  Instance Count
        |
        v
create_instance_tokens(TaskId, Count)  ->  [MI Tokens]
        |
        v
Inject MI Tokens into Marking
        |
        v
For Each Instance:
  - Instance token enables in marking
  - Transition fires (per-instance logic)
  - Instance completes -> complete_instance()
        |
        v
is_complete(CompletedCount, Threshold)
        |
        +---> Threshold NOT met -> Wait for more instances
        |
        +---> Threshold met -> Produce continuation token
```

<h3>Key Concepts</h3>

<ul>
  <li><strong>MI Token:</strong> `{mi_instance, TaskId, Index}` - Represents one active instance</li>
  <li><strong>Completion Token:</strong> `{mi_complete, TaskId, Index}` - Represents a finished instance</li>
  <li><strong>Threshold:</strong> Minimum instances required before workflow can continue</li>
  <li><strong>Marking:</strong> Petri net state mapping places to tokens</li>
</ul>

<h3>Usage</h3>

```erlang
% 1. Create instance tokens when MI task is enabled
MIParams = #{min_instances => 2, max_instances => 5, continuation_threshold => 3},
Data = #{instance_count => 4},
Tokens = yawl_mi_runtime:create_instance_tokens(review_task, spec, net1, Data, MIParams).
% [{mi_instance, review_task, 0}, {mi_instance, review_task, 1}, ...]

% 2. Check which instances are still active
Marking = gen_pnet:marking(Pid),
ActiveIndices = yawl_mi_runtime:get_active_instances(Marking, review_task).
% [0, 2, 3]  (instance 1 already complete)

% 3. Complete an instance
{ok, UpdatedMarking} = yawl_mi_runtime:complete_instance(
    {mi_instance, review_task, 2}, Marking, review_output_place).

% 4. Check if enough instances completed
CompletionTokens = [{mi_complete, review_task, I} || I <- [0, 1, 2]],
IsDone = yawl_mi_runtime:is_complete(CompletionTokens, MIParams).
% true (3 >= threshold of 3)
```

<h3>Integration with gen_pnet</h3>

This module works with `gen_pnet` markings:

```erlang
% Inject instance tokens into a running net
Tokens = yawl_mi_runtime:create_instance_tokens(my_task, spec, net1, Data, MIParams),
gen_pnet:inject(Pid, #{mi_task_place => Tokens}).

% Query active instances from current marking
Marking = gen_pnet:marking(Pid),
Active = yawl_mi_runtime:get_active_instances(Marking, my_task).
```

<h3>Pure Functional Design</h3>

All functions are pure (no side effects):
- No process communication
- No global state
- Deterministic results
- Easy to test and reason about

<h3>Type Specifications</h3>

- `task_id()` - Atom identifying the task
- `marking()` - Map from places to token lists
- `mi_token()` - Multi-instance token tuple
- `mi_params()` - Multi-instance configuration parameters
- `data()` - Workflow data map
""".

%%====================================================================
%% Exports
%%====================================================================

%% Instance lifecycle management
-export([create_instance_tokens/5,
         is_complete/2,
         get_active_instances/2,
         complete_instance/3]).

%% Instance counting and tracking utilities
-export([count_active_instances/2,
         count_completed_instances/2,
         extract_instance_tokens/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Task identifier within a workflow net.
%%
%% Atom uniquely identifying a task in the workflow.
%%--------------------------------------------------------------------
-type task_id() :: atom().

%%--------------------------------------------------------------------
%% @doc Workflow specification reference.
%%
%% Opaque reference to a YAWL specification structure.
%%--------------------------------------------------------------------
-type spec() :: term().

%%--------------------------------------------------------------------
%% @doc Network/decomposition identifier.
%%
%% Binary identifying the specific net decomposition within a YAWL
%% specification.
%%--------------------------------------------------------------------
-type net_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Workflow data map.
%%
%% Contains variable bindings that may influence instance count.
%%--------------------------------------------------------------------
-type data() :: map().

%%--------------------------------------------------------------------
%% @doc Multi-instance parameters from wf_mi.
%%
%% Defines constraints for multi-instance execution.
%%--------------------------------------------------------------------
-type mi_params() :: wf_mi:mi_params().

%%--------------------------------------------------------------------
%% @doc Petri net marking.
%%
%% Maps places to their token lists.
%%--------------------------------------------------------------------
-type marking() :: #{atom() => [term()]}.

%%--------------------------------------------------------------------
%% @doc Multi-instance active token.
%%
%% Represents an active instance of a multi-instance task.
%%--------------------------------------------------------------------
-type mi_token() :: {mi_instance, task_id(), non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc Multi-instance completion token.
%%
%% Represents a completed instance of a multi-instance task.
%%--------------------------------------------------------------------
-type mi_complete_token() :: {mi_complete, task_id(), non_neg_integer()}.

%% Export types
-export_type([task_id/0, spec/0, net_id/0, data/0, marking/0,
              mi_token/0, mi_complete_token/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates tokens for each instance of a multi-instance task.
%%
%% Evaluates the instance count from MI parameters and workflow data,
%% then generates indexed tokens for each instance. This is the main
%% entry point for starting multi-instance task execution.
%%
%% Returns a list of `{mi_instance, TaskId, Index}' tokens, where Index
%% ranges from 0 to Count-1.
%%
%% @param TaskId The task identifier
%% @param Spec YAWL specification (unused in pure utility, reserved for future)
%% @param NetId Network identifier (unused in pure utility, reserved for future)
%% @param Data Workflow data map potentially containing instance count
%% @param MIParams Multi-instance parameters configuration
%% @return List of multi-instance tokens
%%
%% @end
%%--------------------------------------------------------------------
-spec create_instance_tokens(TaskId :: task_id(),
                              Spec :: spec(),
                              NetId :: net_id(),
                              Data :: data(),
                              MIParams :: mi_params() | undefined) ->
          [mi_token()].

create_instance_tokens(TaskId, _Spec, _NetId, Data, MIParams)
  when is_atom(TaskId), is_map(Data) ->
    case wf_mi:evaluate_mi(MIParams, Data) of
        {ok, Count} when is_integer(Count), Count >= 0 ->
            wf_mi:create_instance_tokens(TaskId, Count);
        {error, _Reason} ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc Checks if multi-instance task completion threshold is met.
%%
%% Evaluates whether enough instances have completed to satisfy the
%% continuation threshold. Returns true when the number of completed
%% instances meets or exceeds the threshold defined in MIParams.
%%
%% This is used to determine if a multi-instance task can continue
%% to the next workflow step, even if not all instances are complete.
%%
%% @param InstanceTokens List of instance or completion tokens
%% @param MIParams Multi-instance parameters (undefined defaults to threshold of 1)
%% @return true if threshold met, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_complete(InstanceTokens :: [mi_token() | mi_complete_token()],
                  MIParams :: mi_params() | undefined) ->
          boolean().

is_complete(InstanceTokens, MIParams) when is_list(InstanceTokens) ->
    %% Count completion tokens
    CompletedCount = count_completed_instances(InstanceTokens, undefined),
    %% Check against threshold
    wf_mi:instance_threshold(MIParams, CompletedCount).

%%--------------------------------------------------------------------
%% @doc Gets list of active instance indices for a task in the marking.
%%
%% Scans the marking (typically from `gen_pnet:marking/1`) for all
%% active instance tokens matching the given task ID. Returns a list
%% of instance indices that are currently active.
%%
%% This is useful for:
%% - Checking which instances are still running
%% - Displaying progress to users
%% - Detecting stuck or orphaned instances
%%
%% @param Marking Petri net marking to scan
%% @param TaskId Task identifier to filter instances
%% @return List of active instance indices (sorted ascending)
%%
%% @end
%%--------------------------------------------------------------------
-spec get_active_instances(Marking :: marking(), TaskId :: task_id()) ->
          [non_neg_integer()].

get_active_instances(Marking, TaskId) when is_map(Marking), is_atom(TaskId) ->
    %% Scan all places for mi_instance tokens
    AllTokens = lists:append(maps:values(Marking)),
    %% Filter for matching task and extract indices
    ActiveLists = [Index || {mi_instance, T, Index} <- AllTokens, T =:= TaskId],
    %% Return sorted list
    lists:sort(ActiveLists).

%%--------------------------------------------------------------------
%% @doc Completes a single instance and updates the marking.
%%
%% Transitions an active instance token to a completion token in the
%% marking. This represents the lifecycle event of an instance finishing
%% its execution.
%%
%% The function:
%% 1. Removes the active `{mi_instance, TaskId, Index}` token from marking
%% 2. Adds a `{mi_complete, TaskId, Index}` token to the completion place
%% 3. Returns the updated marking
%%
%% @param InstanceToken The active instance token to complete
%% @param Marking Current marking
%% @param CompletionPlace Place to add completion token to
%% @return {ok, UpdatedMarking} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec complete_instance(InstanceToken :: mi_token(),
                        Marking :: marking(),
                        CompletionPlace :: atom()) ->
          {ok, marking()} | {error, term()}.

complete_instance({mi_instance, TaskId, Index}, Marking, CompletionPlace)
  when is_map(Marking), is_atom(TaskId), is_integer(Index), is_atom(CompletionPlace) ->
    %% Remove the active instance token from marking
    ConsumeMap = #{mi_active => [{mi_instance, TaskId, Index}]},
    case pnet_marking:take(Marking, ConsumeMap) of
        {ok, Marking1} ->
            %% Add completion token to completion place
            CompleteToken = {mi_complete, TaskId, Index},
            ProduceMap = #{CompletionPlace => [CompleteToken]},
            Marking2 = pnet_marking:add(Marking1, ProduceMap),
            {ok, Marking2};
        {error, _} = Error ->
            Error
    end;
complete_instance(_InvalidToken, _Marking, _CompletionPlace) ->
    {error, invalid_instance_token}.

%%--------------------------------------------------------------------
%% @doc Counts active instances for a task in the marking.
%%
%% Returns the number of active `{mi_instance, TaskId, _}' tokens
%% found in the marking. This is a convenience wrapper around
%% `get_active_instances/2`.
%%
%% @param Marking Petri net marking
%% @param TaskId Task identifier to count
%% @return Number of active instances
%%
%% @end
%%--------------------------------------------------------------------
-spec count_active_instances(Marking :: marking(), TaskId :: task_id()) ->
          non_neg_integer().

count_active_instances(Marking, TaskId) ->
    length(get_active_instances(Marking, TaskId)).

%%--------------------------------------------------------------------
%% @doc Counts completed instances in a token list.
%%
%% Counts `{mi_complete, TaskId, _}` tokens in the provided list.
%% If TaskId is undefined, counts all completion tokens regardless
%% of task.
%%
%% @param Tokens List of tokens to scan
%% @param TaskId Task identifier to filter (undefined = all tasks)
%% @return Number of completed instances
%%
%% @end
%%--------------------------------------------------------------------
-spec count_completed_instances(Tokens :: [term()], TaskId :: task_id() | undefined) ->
          non_neg_integer().

count_completed_instances(Tokens, TaskId) when is_list(Tokens) ->
    lists:foldl(fun
        ({mi_complete, T, _}, Acc) when TaskId =:= undefined; T =:= TaskId ->
            Acc + 1;
        (_, Acc) ->
            Acc
    end, 0, Tokens).

%%--------------------------------------------------------------------
%% @doc Extracts all multi-instance tokens from a marking.
%%
%% Scans the marking and returns all tokens related to multi-instance
%% tasks (both active and completion). Useful for diagnostics and
%% reporting.
%%
%% @param Marking Petri net marking
%% @param TaskId Task identifier to filter (undefined = all tasks)
%% @return List of MI tokens found in marking
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_instance_tokens(Marking :: marking(), TaskId :: task_id() | undefined) ->
          [mi_token() | mi_complete_token()].

extract_instance_tokens(Marking, TaskId) when is_map(Marking) ->
    AllTokens = lists:append(maps:values(Marking)),
    lists:filter(fun
        ({mi_instance, T, _}) when TaskId =:= undefined; T =:= TaskId -> true;
        ({mi_complete, T, _}) when TaskId =:= undefined; T =:= TaskId -> true;
        (_) -> false
    end, AllTokens).

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
%% create_instance_tokens/5 tests
%%--------------------------------------------------------------------

create_instance_tokens_basic_test() ->
    MIParams = #{min_instances => 3, max_instances => 5, continuation_threshold => 2},
    Data = #{instance_count => 3},
    Tokens = create_instance_tokens(review_task, spec, <<"net1">>, Data, MIParams),
    ?assertEqual(3, length(Tokens)),
    ?assertEqual([
        {mi_instance, review_task, 0},
        {mi_instance, review_task, 1},
        {mi_instance, review_task, 2}
    ], lists:sort(Tokens)).

create_instance_tokens_zero_count_test() ->
    MIParams = #{min_instances => 0, max_instances => 5, continuation_threshold => 0},
    Data = #{instance_count => 0},
    Tokens = create_instance_tokens(approve, spec, <<"net1">>, Data, MIParams),
    ?assertEqual([], Tokens).

create_instance_tokens_error_returns_empty_test() ->
    %% Below minimum -> error -> returns empty list
    MIParams = #{min_instances => 5, max_instances => 10, continuation_threshold => 3},
    Data = #{instance_count => 2},
    Tokens = create_instance_tokens(check, spec, <<"net1">>, Data, MIParams),
    ?assertEqual([], Tokens).

create_instance_tokens_from_items_test() ->
    MIParams = #{min_instances => 1, max_instances => 10, continuation_threshold => 1},
    Data = #{items => [a, b, c, d, e]},
    Tokens = create_instance_tokens(process, spec, <<"net1">>, Data, MIParams),
    ?assertEqual(5, length(Tokens)).

create_instance_tokens_undefined_params_test() ->
    Data = #{instance_count => 3},
    Tokens = create_instance_tokens(task, spec, <<"net1">>, Data, undefined),
    ?assertEqual([], Tokens).

%%--------------------------------------------------------------------
%% is_complete/2 tests
%%--------------------------------------------------------------------

is_complete_threshold_met_test() ->
    Tokens = [
        {mi_complete, review, 0},
        {mi_complete, review, 1},
        {mi_complete, review, 2}
    ],
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    ?assertEqual(true, is_complete(Tokens, MIParams)).

is_complete_threshold_not_met_test() ->
    Tokens = [
        {mi_complete, review, 0},
        {mi_complete, review, 1}
    ],
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    ?assertEqual(false, is_complete(Tokens, MIParams)).

is_complete_threshold_exceeded_test() ->
    Tokens = [
        {mi_complete, review, 0},
        {mi_complete, review, 1},
        {mi_complete, review, 2},
        {mi_complete, review, 3}
    ],
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    ?assertEqual(true, is_complete(Tokens, MIParams)).

is_complete_undefined_params_test() ->
    %% Undefined params defaults to threshold of 1
    Tokens = [{mi_complete, review, 0}],
    ?assertEqual(true, is_complete(Tokens, undefined)),
    ?assertEqual(false, is_complete([], undefined)).

is_complete_ignores_active_tokens_test() ->
    %% Only completion tokens count
    Tokens = [
        {mi_instance, review, 0},
        {mi_instance, review, 1},
        {mi_complete, review, 2}
    ],
    MIParams = #{min_instances => 1, max_instances => 5, continuation_threshold => 3},
    ?assertEqual(false, is_complete(Tokens, MIParams)).

%%--------------------------------------------------------------------
%% get_active_instances/2 tests
%%--------------------------------------------------------------------

get_active_instances_found_test() ->
    Marking = #{
        mi_active => [
            {mi_instance, review, 0},
            {mi_instance, review, 2},
            {mi_instance, review, 1},
            {other_token}
        ],
        other_place => [{mi_instance, other_task, 0}]
    },
    Active = get_active_instances(Marking, review),
    ?assertEqual([0, 1, 2], Active).

get_active_instances_empty_test() ->
    Marking = #{p1 => [], p2 => []},
    Active = get_active_instances(Marking, task1),
    ?assertEqual([], Active).

get_active_instances_not_found_test() ->
    Marking = #{
        p1 => [{mi_instance, other_task, 0}],
        p2 => [data]
    },
    Active = get_active_instances(Marking, missing_task),
    ?assertEqual([], Active).

get_active_instances_multiple_places_test() ->
    Marking = #{
        place1 => [{mi_instance, task, 0}],
        place2 => [{mi_instance, task, 2}],
        place3 => [{mi_instance, task, 1}]
    },
    Active = get_active_instances(Marking, task),
    ?assertEqual([0, 1, 2], Active).

%%--------------------------------------------------------------------
%% complete_instance/3 tests
%%--------------------------------------------------------------------

complete_instance_success_test() ->
    Marking = #{
        mi_active => [{mi_instance, review, 1}, {mi_instance, review, 2}],
        mi_complete => []
    },
    {ok, Marking1} = complete_instance({mi_instance, review, 1}, Marking, mi_complete),
    ?assertEqual([{mi_instance, review, 2}], maps:get(mi_active, Marking1)),
    ?assertEqual([{mi_complete, review, 1}], maps:get(mi_complete, Marking1)).

complete_instance_nonexistent_test() ->
    Marking = #{mi_active => []},
    Result = complete_instance({mi_instance, review, 0}, Marking, mi_complete),
    ?assertEqual({error, insufficient}, Result).

complete_instance_invalid_token_test() ->
    Marking = #{p1 => []},
    Result = complete_instance({invalid, token}, Marking, p2),
    ?assertEqual({error, invalid_instance_token}, Result).

complete_instance_preserves_other_tokens_test() ->
    Marking = #{
        mi_active => [
            {mi_instance, review, 0},
            {other, data},
            {mi_instance, review, 1}
        ],
        output => [existing]
    },
    {ok, Marking1} = complete_instance({mi_instance, review, 0}, Marking, output),
    %% Other tokens preserved
    ?assert(lists:member({other, data}, maps:get(mi_active, Marking1))),
    ?assert(lists:member(existing, maps:get(output, Marking1))).

%%--------------------------------------------------------------------
%% count_active_instances/2 tests
%%--------------------------------------------------------------------

count_active_instances_basic_test() ->
    Marking = #{
        p1 => [{mi_instance, task, 0}, {mi_instance, task, 1}],
        p2 => [{mi_instance, task, 2}]
    },
    ?assertEqual(3, count_active_instances(Marking, task)).

count_active_instances_zero_test() ->
    Marking = #{p1 => []},
    ?assertEqual(0, count_active_instances(Marking, task)).

count_active_instances_different_task_test() ->
    Marking = #{
        p1 => [{mi_instance, other, 0}, {mi_instance, other, 1}]
    },
    ?assertEqual(0, count_active_instances(Marking, task)).

%%--------------------------------------------------------------------
%% count_completed_instances/2 tests
%%--------------------------------------------------------------------

count_completed_instances_all_tasks_test() ->
    Tokens = [
        {mi_complete, task1, 0},
        {mi_complete, task2, 0},
        {mi_complete, task1, 1},
        {other, token}
    ],
    ?assertEqual(3, count_completed_instances(Tokens, undefined)).

count_completed_instances_specific_task_test() ->
    Tokens = [
        {mi_complete, task1, 0},
        {mi_complete, task2, 0},
        {mi_complete, task1, 1}
    ],
    ?assertEqual(2, count_completed_instances(Tokens, task1)).

count_completed_instances_empty_test() ->
    ?assertEqual(0, count_completed_instances([], undefined)).

count_completed_instances_ignores_active_test() ->
    Tasks = [
        {mi_instance, task, 0},
        {mi_complete, task, 0},
        {mi_instance, task, 1}
    ],
    ?assertEqual(1, count_completed_instances(Tasks, undefined)).

%%--------------------------------------------------------------------
%% extract_instance_tokens/2 tests
%%--------------------------------------------------------------------

extract_instance_tokens_all_tasks_test() ->
    Marking = #{
        p1 => [{mi_instance, task1, 0}, {data}, {mi_complete, task2, 0}],
        p2 => [{mi_instance, task2, 1}]
    },
    Tokens = extract_instance_tokens(Marking, undefined),
    ?assertEqual(3, length(Tokens)),
    ?assert(lists:member({mi_instance, task1, 0}, Tokens)),
    ?assert(lists:member({mi_complete, task2, 0}, Tokens)),
    ?assert(lists:member({mi_instance, task2, 1}, Tokens)).

extract_instance_tokens_specific_task_test() ->
    Marking = #{
        p1 => [{mi_instance, task1, 0}, {mi_instance, task2, 0}],
        p2 => [{mi_complete, task1, 1}]
    },
    Tokens = extract_instance_tokens(Marking, task1),
    ?assertEqual(2, length(Tokens)),
    ?assert(lists:member({mi_instance, task1, 0}, Tokens)),
    ?assert(lists:member({mi_complete, task1, 1}, Tokens)).

extract_instance_tokens_empty_test() ->
    Marking = #{p1 => [], p2 => []},
    Tokens = extract_instance_tokens(Marking, undefined),
    ?assertEqual([], Tokens).

%%--------------------------------------------------------------------
%% Integration tests simulating MI workflow execution
%%--------------------------------------------------------------------

mi_workflow_execution_full_test() ->
    %% Simulate full MI workflow lifecycle
    MIParams = #{min_instances => 2, max_instances => 4, continuation_threshold => 3},
    Data = #{instance_count => 3},

    %% 1. Create instance tokens
    InstanceTokens = create_instance_tokens(approve, spec, <<"net1">>, Data, MIParams),
    ?assertEqual(3, length(InstanceTokens)),

    %% 2. Build initial marking
    Marking0 = pnet_marking:new([mi_active, mi_complete]),
    Marking1 = pnet_marking:add(Marking0, #{mi_active => InstanceTokens}),

    %% 3. Check active instances
    Active0 = get_active_instances(Marking1, approve),
    ?assertEqual([0, 1, 2], Active0),
    ?assertEqual(3, count_active_instances(Marking1, approve)),

    %% 4. Complete instances one by one
    {ok, Marking2} = complete_instance({mi_instance, approve, 0}, Marking1, mi_complete),
    {ok, Marking3} = complete_instance({mi_instance, approve, 1}, Marking2, mi_complete),
    {ok, Marking4} = complete_instance({mi_instance, approve, 2}, Marking3, mi_complete),

    %% 5. Check completion status
    Active1 = get_active_instances(Marking4, approve),
    ?assertEqual([], Active1),
    AllTokens = extract_instance_tokens(Marking4, approve),
    ?assertEqual(true, is_complete(AllTokens, MIParams)).

mi_workflow_threshold_early_exit_test() ->
    %% Test that workflow can continue before all instances complete
    MIParams = #{min_instances => 2, max_instances => 5, continuation_threshold => 2},
    Data = #{instance_count => 4},

    %% Create 4 instances
    InstanceTokens = create_instance_tokens(review, spec, <<"net1">>, Data, MIParams),
    ?assertEqual(4, length(InstanceTokens)),

    %% Simulate 2 completions (meets threshold)
    CompletionTokens = [
        {mi_complete, review, 0},
        {mi_complete, review, 1}
    ],

    %% Check if threshold met (should be true)
    ?assertEqual(true, is_complete(CompletionTokens, MIParams)),

    %% But still have 2 active
    Marking = pnet_marking:new([mi_active]),
    Marking1 = pnet_marking:set(Marking, mi_active, [
        {mi_instance, review, 2},
        {mi_instance, review, 3}
    ]),
    Active = get_active_instances(Marking1, review),
    ?assertEqual([2, 3], Active).

mi_workflow_error_handling_test() ->
    %% Test error scenarios
    MIParams = #{min_instances => 5, max_instances => 10, continuation_threshold => 3},
    Data = #{instance_count => 2},  % Below minimum

    %% Should return empty tokens on error
    Tokens = create_instance_tokens(check, spec, <<"net1">>, Data, MIParams),
    ?assertEqual([], Tokens),

    %% Completing non-existent instance returns error
    Marking = pnet_marking:new([mi_active]),
    Result = complete_instance({mi_instance, check, 0}, Marking, mi_complete),
    ?assertEqual({error, insufficient}, Result).

-endif.
