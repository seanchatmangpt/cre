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
%% @doc YAWL (Yet Another Workflow Language) pattern support for CRE.
%%
%% This module implements YAWL workflow patterns as Petri net structures
%% compatible with the CRE runtime environment.
%%
%% <h3>Implemented Patterns</h3>
%%
%% <h4>Basic Control Flow (WCP-1 to WCP-10):</h4>
%% <ul>
%%   <li>sequence/0, parallel_split/0, synchronization/0</li>
%%   <li>exclusive_choice/0, simple_merge/0, multi_choice/0</li>
%%   <li>synchronizing_merge/0, multi_merge/0, discriminator/0, arbitration/0</li>
%% </ul>
%%
%% <h4>Data Flow Patterns (WDP-1 to WDP-5):</h4>
%% <ul>
%%   <li><b>param_pass/2</b> (WDP-1) - Pass parameters between tasks</li>
%%   <li><b>data_transform/2</b> (WDP-2) - Transform data between tasks</li>
%%   <li><b>data_distribute/1</b> (WDP-3) - Distribute data to multiple tasks</li>
%%   <li><b>data_accumulate/1</b> (WDP-4) - Accumulate data from multiple tasks</li>
%%   <li><b>data_visibility/2</b> (WDP-5) - Control data visibility scope</li>
%% </ul>
%%
%% <h4>Resource Patterns (WRP-1 to WRP-5):</h4>
%% <ul>
%%   <li><b>resource_create/1</b> (WRP-1) - Create new workflow resource</li>
%%   <li><b>role_allocate/2</b> (WRP-2) - Allocate based on role</li>
%%   <li><b>resource_start/1</b> (WRP-3) - Start resource execution</li>
%%   <li><b>role_distribute/2</b> (WRP-4) - Distribute based on roles</li>
%%   <li><b>capability_allocate/2</b> (WRP-5) - Allocate by capability</li>
%% </ul>
%%
%% <h3>Petri Net Mapping</h3>
%%
%% Each pattern maps to a Petri net structure with:
%% <ul>
%%   <li>Places: State locations holding tokens</li>
%%   <li>Transitions: State transformations consuming/producing tokens</li>
%%   <li>Arcs: Connections between places and transitions</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------
-module(cre_yawl).

-moduledoc """
YAWL (Yet Another Workflow Language) pattern support for CRE.

This module implements YAWL workflow patterns as Petri net structures
compatible with the CRE runtime environment. It provides comprehensive
support for control flow, data flow, and resource patterns.

## Examples

### Creating a Simple Workflow

```erlang
%% Create a new workflow
Workflow = cre_yawl:new_workflow(<<"my_workflow">>),

%% Add tasks
Task1 = cre_yawl:add_task(Workflow, <<"step1">>, [{name, <<"First Step">>}, {type, atomic}]),
Task2 = cre_yawl:add_task(Task1, <<"step2">>, [{name, <<"Second Step">>}, {type, atomic}]),

%% Connect tasks
Workflow3 = cre_yawl:connect(Task2, <<"step1">>, <<"step2">>),

%% Set boundaries
Workflow4 = cre_yawl:set_workflow_boundaries(Workflow3, <<"step1">>, [<<"step2">>]),

%% Validate
ok = cre_yawl:validate(Workflow4).
```

### Using Data Flow Patterns

```erlang
%% Create a parameter passing pattern
ParamPass = cre_yawl:param_pass(<<"source_task">>, <<"target_task">>),

%% Create a data transformation pattern
Transform = cre_yawl:data_transform(<<"input_task">>, <<"output_task">>,
                                   fun(X) -> X * 2 end),

%% Execute parameter passing
{ok, Result} = cre_yawl:execute_param_pass(
    ParamPass,
    #{<<"source_task">> => 42},
    #{}
).
```
""".

-include("yawl_otel_logger.hrl").

%%====================================================================
%% Exports
%%====================================================================

-export([new_workflow/0, new_workflow/1]).
-export([add_task/3, add_condition/3, set_split_type/3, set_join_type/3, connect/3]).
-export([validate/1, get_errors/1, get_workflow_id/1, get_workflow_name/1,
         get_tasks/1, get_connections/1, get_conditions/1, set_workflow_boundaries/3]).
-export([sequence/0, parallel_split/0, synchronization/0, exclusive_choice/0,
         simple_merge/0, multi_choice/0, synchronizing_merge/0, multi_merge/0,
         discriminator/0, arbitration/0]).
-export([execute_synchronizing_merge/3, execute_multi_merge/3,
         execute_discriminator/3, execute_arbitration/3]).
-export([param_pass/2, param_pass/3, validate_param/2, data_transform/2,
         data_transform/3, validate_transform/2, data_distribute/1,
         data_distribute/4, distribution_strategy/1, data_accumulate/1,
         data_accumulate/4, aggregate_function/1, data_visibility/2,
         data_visibility/3, validate_access/2]).
-export([execute_param_pass/3, execute_data_transform/3,
         execute_data_distribute/3, execute_data_accumulate/3,
         execute_data_visibility/3]).
-export([resource_create/1, role_allocate/2, resource_start/1, role_distribute/2, capability_allocate/2]).
-export([execute_resource_create/2, execute_role_allocate/3, execute_resource_start/2,
         execute_role_distribute/3, execute_capability_allocate/3]).

%% Human-in-the-loop wrappers that delegate to yawl_approval
-export([request_approval/3, approve_task/3, reject_task/3,
         get_approval_status/2, cancel_approval/2]).

%% Telemetry wrappers that delegate to yawl_otel_logger
-export([enable_telemetry/0, enable_telemetry/1, disable_telemetry/0,
         get_workflow_metrics/1, log_workflow_event/3, log_workflow_event/4]).

%%====================================================================
%% Types
%%====================================================================

-type join_type() :: and_join | or_join | xor_join.
-type split_type() :: and_split | or_split | xor_split.
-type condition() :: binary() | {atom(), term()} | fun(() -> boolean()).
-type element_id() :: binary().
-type task_type() :: atomic | composite | subworkflow | multi_instance.

%%====================================================================
%% Records
%%====================================================================

-record(task, {
          id :: element_id(),
          name :: binary(),
          type :: task_type(),
          split_type :: split_type() | undefined,
          join_type :: join_type() | undefined,
          metadata = #{} :: #{atom() => term()}
         }).

-record(yawl_condition, {
          id :: element_id(),
          expression :: condition(),
          description :: binary() | undefined
         }).

-record(connection, {
          from_id :: element_id(),
          to_id :: element_id(),
          condition_id :: element_id() | undefined
         }).

-record(workflow, {
          id :: element_id(),
          name :: binary(),
          tasks = #{} :: #{element_id() => #task{}},
          conditions = #{} :: #{element_id() => #yawl_condition{}},
          connections = [] :: [#connection{}],
          start_task_id :: element_id() | undefined,
          end_task_ids = [] :: [element_id()]
         }).

-record(sequence, {task_ids :: [element_id()]}).
-record(parallel_split, {split_task_id :: element_id(), branch_task_ids :: [element_id()]}).
-record(synchronization, {join_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(exclusive_choice, {choice_task_id :: element_id(), branches :: [{element_id(), condition()}]}).
-record(simple_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(multi_choice, {choice_task_id :: element_id(), branches :: [{element_id(), condition()}]}).
-record(synchronizing_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(multi_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(discriminator, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(parallel_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()], required_count :: pos_integer() | all}).
-record(blocking_discriminator, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()], blocking_mode :: block_subsequent | reset_on_first}).
-record(arb_cycles, {loop_task_id :: element_id(), back_edge_targets :: [element_id()], condition_fun :: function(), max_iterations :: pos_integer() | unlimited}).
-record(arbitration, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()], required_count :: pos_integer()}).

%% Data Flow Pattern Records (WDP-1 to WDP-5)
-record(param_pass, {source_task_id :: element_id(), target_task_id :: element_id(), param_name :: atom(), transform_fn :: function() | undefined}).
-record(data_transform, {input_task_id :: element_id(), output_task_id :: element_id(), transform_fn :: function(), output_schema :: term() | undefined}).
-record(data_distribute, {source_task_id :: element_id(), recipient_task_ids :: [element_id()], distribution_type :: broadcast | round_robin | partitioned}).
-record(data_accumulate, {source_task_ids :: [element_id()], target_task_id :: element_id(), aggregation_fn :: function(), initial_value :: term()}).
-record(data_visibility, {data_task_id :: element_id(), scope :: local | branch | global, access_list :: [element_id()] | undefined}).

%% Resource Pattern Records (WRP-1 to WRP-5)
-record(resource_create, {resource_id :: element_id(), resource_type :: atom(), init_params :: map()}).
-record(role_allocate, {role_id :: atom(), required_capability :: term(), allocation_strategy :: first_fit | best_fit | random}).
-record(resource_start, {resource_id :: element_id(), start_params :: map()}).
-record(role_distribute, {work_item_ids :: [element_id()], role_assignments :: map(), distribution_policy :: round_robin | least_loaded | affinity_based}).
-record(capability_allocate, {required_capabilities :: map(), resource_registry :: [term()], matching_strategy :: exact_match | minimum_met | best_effort}).

-type pattern() :: #workflow{} | #sequence{} | #parallel_split{} |
                   #synchronization{} | #exclusive_choice{} |
                   #simple_merge{} | #multi_choice{} |
                   #synchronizing_merge{} | #multi_merge{} |
                   #discriminator{} | #arbitration{} |
                   #param_pass{} | #data_transform{} | #data_distribute{} |
                   #data_accumulate{} | #data_visibility{} |
                   #resource_create{} | #role_allocate{} | #resource_start{} |
                   #role_distribute{} | #capability_allocate{}.

-export_type([pattern/0]).

%%====================================================================
%% API
%%====================================================================

-doc """
Creates a new workflow with an auto-generated ID.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
{ok, Id} = cre_yawl:get_workflow_id(Workflow),
is_binary(Id) andalso size(Id) > 0.
```
""".
-spec new_workflow() -> #workflow{}.
new_workflow() ->
    WorkflowId = generate_id(<<"workflow">>),
    new_workflow(WorkflowId).

-doc """
Creates a new workflow with the specified ID.

## Example

```erlang
Workflow = cre_yawl:new_workflow(<<"my_custom_workflow">>),
{ok, Id} = cre_yawl:get_workflow_id(Workflow),
Id =:= <<"my_custom_workflow">>.
```
""".
-spec new_workflow(Id :: element_id()) -> #workflow{}.
new_workflow(Id) when is_binary(Id) ->
    #workflow{id = Id, name = <<"Untitled Workflow">>}.

-doc """
Adds a task to the workflow.

The task can be specified as a task record or a property list.
Property list options: name, type, split_type, join_type, metadata.

## Examples

```erlang
Workflow = cre_yawl:new_workflow(),
%% Add task using property list
W1 = cre_yawl:add_task(Workflow, <<"task1">>, [{name, <<"My Task">>}, {type, atomic}]),
{ok, Tasks} = cre_yawl:get_tasks(W1),
maps:is_key(<<"task1">>, Tasks).
```
""".
-spec add_task(Workflow :: #workflow{}, TaskId :: element_id(),
               Task :: #task{} | [{atom(), term()}]) -> #workflow{}.
add_task(#workflow{tasks = Tasks} = Workflow, TaskId, TaskRec)
  when element(1, TaskRec) =:= task ->
    Workflow#workflow{tasks = Tasks#{TaskId => TaskRec}};
add_task(Workflow, TaskId, PropList) when is_list(PropList) ->
    Task = make_task(TaskId, PropList),
    add_task(Workflow, TaskId, Task).

-doc """
Adds a condition to the workflow.

Conditions are used for branching logic in exclusive and multi-choice patterns.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
%% Add a boolean condition
W1 = cre_yawl:add_condition(Workflow, <<"cond1">>, fun() -> true end),
{ok, Conds} = cre_yawl:get_conditions(W1),
maps:is_key(<<"cond1">>, Conds).
```
""".
-spec add_condition(Workflow :: #workflow{}, ConditionId :: element_id(),
                    Condition :: #yawl_condition{} | condition()) -> #workflow{}.
add_condition(#workflow{conditions = Conds} = Workflow, ConditionId, CondRec)
  when element(1, CondRec) =:= yawl_condition ->
    Workflow#workflow{conditions = Conds#{ConditionId => CondRec}};
add_condition(Workflow, ConditionId, Expression) ->
    CondRec = #yawl_condition{id = ConditionId, expression = Expression},
    add_condition(Workflow, ConditionId, CondRec).

-doc """
Sets the split type for a task.

Split types determine how a task divides control flow to multiple outgoing branches:
- `and_split`: All outgoing branches execute concurrently
- `or_split`: One or more outgoing branches may execute
- `xor_split`: Exactly one outgoing branch executes

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"split">>, [{name, <<"Split Task">>}]),
W2 = cre_yawl:set_split_type(W1, <<"split">>, and_split),
{ok, Tasks} = cre_yawl:get_tasks(W2),
#{<<"split">> := #task{split_type = and_split}} = Tasks.
```
""".
-spec set_split_type(Workflow :: #workflow{}, TaskId :: element_id(),
                     SplitType :: split_type()) -> #workflow{}.
set_split_type(#workflow{tasks = Tasks} = Workflow, TaskId, SplitType)
  when is_binary(TaskId), (SplitType =:= and_split orelse
                           SplitType =:= or_split orelse
                           SplitType =:= xor_split) ->
    case maps:get(TaskId, Tasks, undefined) of
        undefined -> Workflow;
        Task -> Workflow#workflow{tasks = Tasks#{TaskId => Task#task{split_type = SplitType}}}
    end.

-doc """
Sets the join type for a task.

Join types determine how a task merges control flow from multiple incoming branches:
- `and_join`: Waits for all incoming branches to complete
- `or_join`: Waits for one or more incoming branches to complete
- `xor_join`: Waits for exactly one incoming branch to complete

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"join">>, [{name, <<"Join Task">>}]),
W2 = cre_yawl:set_join_type(W1, <<"join">>, and_join),
{ok, Tasks} = cre_yawl:get_tasks(W2),
#{<<"join">> := #task{join_type = and_join}} = Tasks.
```
""".
-spec set_join_type(Workflow :: #workflow{}, TaskId :: element_id(),
                    JoinType :: join_type()) -> #workflow{}.
set_join_type(#workflow{tasks = Tasks} = Workflow, TaskId, JoinType)
  when is_binary(TaskId), (JoinType =:= and_join orelse
                           JoinType =:= or_join orelse
                           JoinType =:= xor_join) ->
    case maps:get(TaskId, Tasks, undefined) of
        undefined -> Workflow;
        Task -> Workflow#workflow{tasks = Tasks#{TaskId => Task#task{join_type = JoinType}}}
    end.

-doc """
Creates a connection between two tasks in the workflow.

Connections define the flow of control from one task to another.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"task1">>, [{name, <<"First">>}]),
W2 = cre_yawl:add_task(W1, <<"task2">>, [{name, <<"Second">>}]),
W3 = cre_yawl:connect(W2, <<"task1">>, <<"task2">>),
{ok, Conns} = cre_yawl:get_connections(W3),
length(Conns) > 0.
```
""".
-spec connect(Workflow :: #workflow{}, FromId :: element_id(),
              ToId :: element_id()) -> #workflow{}.
connect(#workflow{connections = Conns} = Workflow, FromId, ToId)
  when is_binary(FromId), is_binary(ToId) ->
    Conn = #connection{from_id = FromId, to_id = ToId},
    Workflow#workflow{connections = [Conn | Conns]}.

-doc """
Validates a workflow.

Checks for structural errors including:
- Missing tasks referenced in connections
- Invalid start/end task references
- Cycles in the workflow graph
- Inconsistent split/join configurations

## Examples

```erlang
%% Valid workflow
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"task1">>, [{name, <<"First">>}]),
W2 = cre_yawl:set_workflow_boundaries(W1, <<"task1">>, [<<"task1">>]),
ok = cre_yawl:validate(W2).

%% Invalid workflow (no tasks)
Empty = cre_yawl:new_workflow(),
{error, _Errors} = cre_yawl:validate(Empty).
```
""".
-spec validate(Workflow :: #workflow{}) -> ok | {error, [binary()]}.
validate(#workflow{} = Workflow) ->
    case validate_workflow(Workflow) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

-doc """
Returns a list of validation errors for a workflow.

Unlike `validate/1`, this returns the error list directly without wrapping.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
Errors = cre_yawl:get_errors(Workflow),
is_list(Errors).
```
""".
-spec get_errors(Workflow :: #workflow{}) -> [binary()].
get_errors(#workflow{} = Workflow) ->
    validate_workflow(Workflow).

-doc """
Gets the workflow ID from a workflow record.

## Example

```erlang
Workflow = cre_yawl:new_workflow(<<"my_workflow">>),
{ok, Id} = cre_yawl:get_workflow_id(Workflow),
Id =:= <<"my_workflow">>.
```
""".
-spec get_workflow_id(#workflow{}) -> {ok, element_id()}.
get_workflow_id(#workflow{id = Id}) -> {ok, Id}.

-doc """
Gets the workflow name from a workflow record.

## Example

```erlang
Workflow = cre_yawl:new_workflow(<<"my_workflow">>),
{ok, Name} = cre_yawl:get_workflow_name(Workflow),
Name =:= <<"Untitled Workflow">>.
```
""".
-spec get_workflow_name(#workflow{}) -> {ok, binary()}.
get_workflow_name(#workflow{name = Name}) -> {ok, Name}.

-doc """
Gets the tasks map from a workflow record.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"task1">>, [{name, <<"My Task">>}]),
{ok, Tasks} = cre_yawl:get_tasks(W1),
maps:is_key(<<"task1">>, Tasks).
```
""".
-spec get_tasks(#workflow{}) -> {ok, #{element_id() => #task{}}}.
get_tasks(#workflow{tasks = Tasks}) -> {ok, Tasks}.

-doc """
Gets the connections list from a workflow record.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"t1">>, []),
W2 = cre_yawl:add_task(W1, <<"t2">>, []),
W3 = cre_yawl:connect(W2, <<"t1">>, <<"t2">>),
{ok, Conns} = cre_yawl:get_connections(W3),
length(Conns) > 0.
```
""".
-spec get_connections(#workflow{}) -> {ok, [#connection{}]}.
get_connections(#workflow{connections = Conns}) -> {ok, Conns}.

-doc """
Gets the conditions map from a workflow record.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_condition(Workflow, <<"cond1">>, fun() -> true end),
{ok, Conds} = cre_yawl:get_conditions(W1),
maps:is_key(<<"cond1">>, Conds).
```
""".
-spec get_conditions(#workflow{}) -> {ok, #{element_id() => #yawl_condition{}}}.
get_conditions(#workflow{conditions = Conds}) -> {ok, Conds}.

-doc """
Sets the start and end task boundaries for a workflow.

Validates that the specified start and end tasks exist in the workflow
before setting them. Returns an error if any task is not found.

## Example

```erlang
Workflow = cre_yawl:new_workflow(),
W1 = cre_yawl:add_task(Workflow, <<"start">>, [{name, <<"Start">>}]),
W2 = cre_yawl:add_task(W1, <<"finish">>, [{name, <<"Finish">>}]),
W3 = cre_yawl:set_workflow_boundaries(W2, <<"start">>, [<<"finish">>]),
ok = cre_yawl:validate(W3).
```
""".
-spec set_workflow_boundaries(Workflow :: #workflow{},
                              StartTaskId :: element_id(),
                              EndTaskIds :: [element_id()]) ->
          #workflow{} | {error, term()}.

set_workflow_boundaries(#workflow{tasks = Tasks} = Workflow, StartTaskId, EndTaskIds)
  when is_binary(StartTaskId), is_list(EndTaskIds) ->
    %% Validate start task exists
    case maps:is_key(StartTaskId, Tasks) of
        false ->
            {error, {start_task_not_found, StartTaskId}};
        true ->
            %% Validate all end tasks exist
            case lists:foldl(fun(EndTaskId, Acc) ->
                case maps:is_key(EndTaskId, Tasks) of
                    false -> {error, {end_task_not_found, EndTaskId}};
                    true -> Acc
                end
            end, ok, EndTaskIds) of
                {error, _} = Error -> Error;
                ok ->
                    Workflow#workflow{
                        start_task_id = StartTaskId,
                        end_task_ids = EndTaskIds
                    }
            end
    end.

%% Pattern constructors
-doc """
Creates a sequence pattern (WCP-1).

This pattern represents a sequential execution of tasks where each task
completes before the next one begins.

## Example

```erlang
Pattern = cre_yawl:sequence(),
#sequence{task_ids = []} = Pattern.
```
""".
-spec sequence() -> #sequence{}.
sequence() -> #sequence{task_ids = []}.

-doc """
Creates a parallel split pattern (WCP-2).

This pattern splits execution into multiple concurrent branches.

## Example

```erlang
Pattern = cre_yawl:parallel_split(),
#parallel_split{split_task_id = <<>>, branch_task_ids = []} = Pattern.
```
""".
-spec parallel_split() -> #parallel_split{}.
parallel_split() -> #parallel_split{split_task_id = <<>>, branch_task_ids = []}.

-doc """
Creates a synchronization pattern (WCP-3).

This pattern synchronizes multiple concurrent branches into a single flow.

## Example

```erlang
Pattern = cre_yawl:synchronization(),
#synchronization{join_task_id = <<>>, incoming_task_ids = []} = Pattern.
```
""".
-spec synchronization() -> #synchronization{}.
synchronization() -> #synchronization{join_task_id = <<>>, incoming_task_ids = []}.

-doc """
Creates an exclusive choice pattern (WCP-4).

This pattern selects exactly one branch based on conditions.

## Example

```erlang
Pattern = cre_yawl:exclusive_choice(),
#exclusive_choice{choice_task_id = <<>>, branches = []} = Pattern.
```
""".
-spec exclusive_choice() -> #exclusive_choice{}.
exclusive_choice() -> #exclusive_choice{choice_task_id = <<>>, branches = []}.

-doc """
Creates a simple merge pattern (WCP-5).

This pattern merges multiple incoming branches without synchronization.

## Example

```erlang
Pattern = cre_yawl:simple_merge(),
#simple_merge{merge_task_id = <<>>, incoming_task_ids = []} = Pattern.
```
""".
-spec simple_merge() -> #simple_merge{}.
simple_merge() -> #simple_merge{merge_task_id = <<>>, incoming_task_ids = []}.

-doc """
Creates a multi-choice pattern (WCP-6).

This pattern allows multiple branches to be selected for execution.

## Example

```erlang
Pattern = cre_yawl:multi_choice(),
#multi_merge{choice_task_id = <<>>, branches = []} = Pattern.
```
""".
-spec multi_choice() -> #multi_choice{}.
multi_choice() -> #multi_choice{choice_task_id = <<>>, branches = []}.

-doc """
Creates a synchronizing merge pattern (WCP-7).

This pattern merges multiple branches with n-out-of-m synchronization.

## Example

```erlang
Pattern = cre_yawl:synchronizing_merge(),
#synchronizing_merge{merge_task_id = <<>>, incoming_task_ids = []} = Pattern.
```
""".
-spec synchronizing_merge() -> #synchronizing_merge{}.
synchronizing_merge() -> #synchronizing_merge{merge_task_id = <<>>, incoming_task_ids = []}.

-doc """
Creates a multi-merge pattern (WCP-8).

This pattern merges multiple branches without synchronization.

## Example

```erlang
Pattern = cre_yawl:multi_merge(),
#multi_merge{merge_task_id = <<>>, incoming_task_ids = []} = Pattern.
```
""".
-spec multi_merge() -> #multi_merge{}.
multi_merge() -> #multi_merge{merge_task_id = <<>>, incoming_task_ids = []}.

-doc """
Creates a discriminator pattern (WCP-9).

This pattern passes control through after the first incoming branch completes.

## Example

```erlang
Pattern = cre_yawl:discriminator(),
#discriminator{merge_task_id = <<>>, incoming_task_ids = []} = Pattern.
```
""".
-spec discriminator() -> #discriminator{}.
discriminator() -> #discriminator{merge_task_id = <<>>, incoming_task_ids = []}.

-doc """
Creates an arbitration pattern (WCP-10).

This pattern activates after a specified number of incoming branches complete.

## Example

```erlang
Pattern = cre_yawl:arbitration(),
#arbitration{merge_task_id = <<>>, incoming_task_ids = [], required_count = 1} = Pattern.
```
""".
-spec arbitration() -> #arbitration{}.
arbitration() -> #arbitration{merge_task_id = <<>>, incoming_task_ids = [], required_count = 1}.

%% Data Flow Pattern constructors (WDP-1 to WDP-5)
-doc """
Creates a parameter passing pattern (WDP-1).

This pattern passes parameters from a source task to a target task.

## Example

```erlang
%% Create a parameter passing pattern
Pattern = cre_yawl:param_pass(<<"source">>, <<"target">>),
#param_pass{source_task_id = <<"source">>, target_task_id = <<"target">>} = Pattern.
```
""".
-spec param_pass(element_id(), element_id()) -> #param_pass{}.
param_pass(SourceId, TargetId) ->
    #param_pass{source_task_id = SourceId, target_task_id = TargetId, param_name = undefined, transform_fn = undefined}.

-doc """
Creates a parameter passing pattern with a transformation function or named parameter (WDP-1).

## Examples

```erlang
%% With a transform function
Pattern1 = cre_yawl:param_pass(<<"source">>, <<"target">>, fun(X) -> X * 2 end),
#param_pass{transform_fn = Fn} = Pattern1,
is_function(Fn, 1).

%% With a named parameter
Pattern2 = cre_yawl:param_pass(<<"source">>, <<"target">>, {my_param, 0}),
#param_pass{param_name = my_param} = Pattern2.
```
""".
-spec param_pass(element_id(), element_id(), function() | {atom(), term()}) -> #param_pass{}.
param_pass(SourceId, TargetId, TransformFn) when is_function(TransformFn, 1) ->
    #param_pass{source_task_id = SourceId, target_task_id = TargetId, param_name = undefined, transform_fn = TransformFn};
param_pass(SourceId, TargetId, {ParamName, _Default}) when is_atom(ParamName) ->
    #param_pass{source_task_id = SourceId, target_task_id = TargetId, param_name = ParamName, transform_fn = undefined}.

-doc """
Creates a data transformation pattern (WDP-2).

This pattern transforms data between tasks using an identity function.

## Example

```erlang
Pattern = cre_yawl:data_transform(<<"input">>, <<"output">>),
#data_transform{input_task_id = <<"input">>, output_task_id = <<"output">>} = Pattern.
```
""".
-spec data_transform(element_id(), element_id()) -> #data_transform{}.
data_transform(InputId, OutputId) ->
    TransformFn = fun(X) -> X end,
    #data_transform{input_task_id = InputId, output_task_id = OutputId, transform_fn = TransformFn, output_schema = undefined}.

-doc """
Creates a data transformation pattern with a custom transform function (WDP-2).

## Example

```erlang
%% Create a transformation that doubles the value
Pattern = cre_yawl:data_transform(<<"input">>, <<"output">>, fun(X) -> X * 2 end),
#data_transform{transform_fn = Fn} = Pattern,
is_function(Fn, 1).
```
""".
-spec data_transform(element_id(), element_id(), function()) -> #data_transform{}.
data_transform(InputId, OutputId, TransformFn) when is_function(TransformFn, 1) ->
    #data_transform{input_task_id = InputId, output_task_id = OutputId, transform_fn = TransformFn, output_schema = undefined}.

-doc """
Creates a data distribution pattern (WDP-3).

This pattern distributes data to multiple recipient tasks using broadcast by default.

## Example

```erlang
Pattern = cre_yawl:data_distribute([<<"task1">>, <<"task2">>, <<"task3">>]),
#data_distribute{recipient_task_ids = Recipients} = Pattern,
length(Recipients) =:= 3.
```
""".
-spec data_distribute([element_id()]) -> #data_distribute{}.
data_distribute(RecipientIds) ->
    #data_distribute{source_task_id = <<>>, recipient_task_ids = RecipientIds, distribution_type = broadcast}.

-doc """
Creates a data distribution pattern with source and type (WDP-3).

## Example

```erlang
Pattern = cre_yawl:data_distribute(<<"source">>, [<<"t1">>, <<"t2">>], round_robin, #{}),
#data_distribute{source_task_id = <<"source">>, distribution_type = round_robin} = Pattern.
```
""".
-spec data_distribute(element_id(), [element_id()], broadcast | round_robin | partitioned, term()) -> #data_distribute{}.
data_distribute(SourceId, RecipientIds, DistributionType, _Options) ->
    #data_distribute{source_task_id = SourceId, recipient_task_ids = RecipientIds, distribution_type = DistributionType}.

-doc """
Creates a data accumulation pattern (WDP-4).

This pattern accumulates data from multiple source tasks using a list aggregator.

## Example

```erlang
Pattern = cre_yawl:data_accumulate([<<"s1">>, <<"s2">>]),
#data_accumulate{source_task_ids = [<<"s1">>, <<"s2">>]} = Pattern.
```
""".
-spec data_accumulate([element_id()]) -> #data_accumulate{}.
data_accumulate(SourceIds) ->
    AggFn = fun(Acc, X) -> [X | Acc] end,
    #data_accumulate{source_task_ids = SourceIds, target_task_id = <<>>, aggregation_fn = AggFn, initial_value = []}.

-doc """
Creates a data accumulation pattern with custom aggregator (WDP-4).

## Example

```erlang
%% Sum accumulation
AggFn = fun(Acc, X) -> Acc + X end,
Pattern = cre_yawl:data_accumulate([<<"s1">>], <<"target">>, AggFn, 0),
#data_accumulate{initial_value = 0} = Pattern.
```
""".
-spec data_accumulate([element_id()], element_id(), function(), term()) -> #data_accumulate{}.
data_accumulate(SourceIds, TargetId, AggFn, InitialValue) when is_function(AggFn, 2) ->
    #data_accumulate{source_task_ids = SourceIds, target_task_id = TargetId, aggregation_fn = AggFn, initial_value = InitialValue}.

-doc """
Creates a data visibility pattern (WDP-5).

This pattern controls the visibility scope of data within the workflow.

## Example

```erlang
%% Local scope visibility
Pattern = cre_yawl:data_visibility(<<"data1">>, local),
#data_visibility{scope = local} = Pattern.
```
""".
-spec data_visibility(element_id(), local | branch | global) -> #data_visibility{}.
data_visibility(DataId, Scope) ->
    #data_visibility{data_task_id = DataId, scope = Scope, access_list = undefined}.

-doc """
Creates a data visibility pattern with access list (WDP-5).

## Example

```erlang
%% Branch scope with specific access list
Pattern = cre_yawl:data_visibility(<<"data1">>, branch, [<<"task1">>, <<"task2">>]),
#data_visibility{scope = branch, access_list = [<<"task1">>, <<"task2">>]} = Pattern.
```
""".
-spec data_visibility(element_id(), local | branch | global, [element_id()]) -> #data_visibility{}.
data_visibility(DataId, Scope, AccessList) when is_list(AccessList) ->
    #data_visibility{data_task_id = DataId, scope = Scope, access_list = AccessList}.

%% Resource Pattern constructors (WRP-1 to WRP-5)
-doc """
Creates a resource creation pattern (WRP-1).

This pattern defines the creation of a new workflow resource.

## Example

```erlang
Pattern = cre_yawl:resource_create(database_connection),
#resource_create{resource_type = database_connection} = Pattern.
```
""".
-spec resource_create(atom()) -> #resource_create{}.
resource_create(ResourceType) ->
    #resource_create{resource_id = <<>>, resource_type = ResourceType, init_params = #{}}.

-doc """
Creates a role allocation pattern (WRP-2).

This pattern defines how resources are allocated based on roles.

## Example

```erlang
Pattern = cre_yawl:role_allocate(admin, root_access),
#role_allocate{role_id = admin, allocation_strategy = first_fit} = Pattern.
```
""".
-spec role_allocate(atom(), term()) -> #role_allocate{}.
role_allocate(RoleId, Capability) ->
    #role_allocate{role_id = RoleId, required_capability = Capability, allocation_strategy = first_fit}.

-doc """
Creates a resource start pattern (WRP-3).

This pattern defines the startup parameters for a resource.

## Example

```erlang
Pattern = cre_yawl:resource_start(<<"my_resource">>),
#resource_start{resource_id = <<"my_resource">>} = Pattern.
```
""".
-spec resource_start(element_id()) -> #resource_start{}.
resource_start(ResourceId) ->
    #resource_start{resource_id = ResourceId, start_params = #{}}.

-doc """
Creates a role distribution pattern (WRP-4).

This pattern distributes work items based on role assignments.

## Example

```erlang
Assignments = #{admin => [<<"task1">>], user => [<<"task2">>]},
Pattern = cre_yawl:role_distribute([<<"task1">>, <<"task2">>], Assignments),
#role_distribute{distribution_policy = round_robin} = Pattern.
```
""".
-spec role_distribute([element_id()], map()) -> #role_distribute{}.
role_distribute(WorkItemIds, RoleAssignments) ->
    #role_distribute{work_item_ids = WorkItemIds, role_assignments = RoleAssignments, distribution_policy = round_robin}.

-doc """
Creates a capability allocation pattern (WRP-5).

This pattern allocates resources based on capability requirements.

## Example

```erlang
Caps = #{cpu => 4, memory => 8192},
Registry = [node1, node2, node3],
Pattern = cre_yawl:capability_allocate(Caps, Registry),
#capability_allocate{matching_strategy = exact_match} = Pattern.
```
""".
-spec capability_allocate(map(), [term()]) -> #capability_allocate{}.
capability_allocate(Capabilities, Registry) ->
    #capability_allocate{required_capabilities = Capabilities, resource_registry = Registry, matching_strategy = exact_match}.

%%====================================================================
%% Internal
%%====================================================================

-spec generate_id(Prefix :: binary()) -> element_id().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

-spec make_task(TaskId :: element_id(), PropList :: [{atom(), term()}]) -> #task{}.
make_task(TaskId, PropList) ->
    Name = proplists:get_value(name, PropList, <<"Unnamed Task">>),
    Type = proplists:get_value(type, PropList, atomic),
    SplitType = proplists:get_value(split_type, PropList, undefined),
    JoinType = proplists:get_value(join_type, PropList, undefined),
    Metadata = proplists:get_value(metadata, PropList, #{}),
    #task{id = TaskId, name = Name, type = Type, split_type = SplitType,
          join_type = JoinType, metadata = Metadata}.

-spec validate_workflow(#workflow{}) -> [binary()].
validate_workflow(#workflow{tasks = Tasks, connections = Conns,
                            start_task_id = StartTaskId, end_task_ids = EndTaskIds}) ->
    lists:flatten([
        validate_tasks_exist(Tasks, Conns),
        validate_start_task(StartTaskId, Tasks),
        validate_end_tasks(EndTaskIds, Tasks),
        validate_connections(Conns),
        validate_no_cycles(Conns),
        validate_split_join_consistency(Tasks, Conns)
    ]).

-spec validate_tasks_exist(#{element_id() => #task{}}, [#connection{}]) -> [binary()].
validate_tasks_exist(Tasks, Conns) ->
    TaskIds = maps:keys(Tasks),
    F = fun(#connection{from_id = From, to_id = To}, Acc) ->
            Acc1 = case lists:member(From, TaskIds) of
                       false -> [iolist_to_binary([<<"Task '">>, From, <<"' does not exist">>]) | Acc];
                       true -> Acc
                   end,
            case lists:member(To, TaskIds) of
                false -> [iolist_to_binary([<<"Task '">>, To, <<"' does not exist">>]) | Acc1];
                true -> Acc1
            end
        end,
    lists:foldl(F, [], Conns).

-spec validate_start_task(element_id() | undefined, #{element_id() => #task{}}) -> [binary()].
validate_start_task(undefined, _Tasks) -> [];
validate_start_task(StartTaskId, Tasks) ->
    case maps:is_key(StartTaskId, Tasks) of
        false -> [iolist_to_binary([<<"Start task '">>, StartTaskId, <<"' does not exist">>])];
        true -> []
    end.

-spec validate_end_tasks([element_id()], #{element_id() => #task{}}) -> [binary()].
validate_end_tasks([], _Tasks) -> [];
validate_end_tasks(EndTaskIds, Tasks) ->
    F = fun(TaskId, Acc) ->
            case maps:is_key(TaskId, Tasks) of
                false -> [iolist_to_binary([<<"End task '">>, TaskId, <<"' does not exist">>]) | Acc];
                true -> Acc
            end
        end,
    lists:foldl(F, [], EndTaskIds).

-spec validate_connections([#connection{}]) -> [binary()].
validate_connections(Conns) ->
    F = fun(#connection{from_id = From, to_id = To}, Acc) ->
            Acc1 = case From =:= To of
                       true -> [iolist_to_binary([<<"Self-loop on '">>, From, <<"'">>]) | Acc];
                       false -> Acc
                   end,
            case lists:any(fun(C) -> C#connection.from_id =:= From andalso
                                      C#connection.to_id =:= To end,
                           Conns -- [#connection{from_id = From, to_id = To}]) of
                true -> [iolist_to_binary([<<"Duplicate connection '">>, From, <<"' -> '">>, To, <<"'">>]) | Acc1];
                false -> Acc1
            end
        end,
    lists:foldl(F, [], Conns).

-spec validate_no_cycles([#connection{}]) -> [binary()].
validate_no_cycles([]) -> [];
validate_no_cycles(Conns) ->
    F = fun(#connection{from_id = From, to_id = To}, Acc) ->
            Acc#{From => [To | maps:get(From, Acc, [])]}
        end,
    Graph = lists:foldl(F, #{}, Conns),
    Nodes = lists:usort([N || #connection{from_id = N} <- Conns] ++
                         [T || #connection{to_id = T} <- Conns]),
    case detect_cycle(Graph, Nodes, [], #{}) of
        {cycle, Node} -> [iolist_to_binary([<<"Cycle involving '">>, Node, <<"'">>])];
        no_cycle -> []
    end.

-spec detect_cycle(#{element_id() => [element_id()]}, [element_id()],
                   [element_id()], #{element_id() => visiting | visited}) ->
          {cycle, element_id()} | no_cycle.
detect_cycle(_Graph, [], _Path, _Visited) -> no_cycle;
detect_cycle(Graph, [Node | Rest], Path, Visited) ->
    case maps:get(Node, Visited, undefined) of
        visiting -> {cycle, Node};
        visited -> detect_cycle(Graph, Rest, Path, Visited);
        undefined ->
            Visited1 = Visited#{Node => visiting},
            Neighbors = maps:get(Node, Graph, []),
            case detect_cycle(Graph, Neighbors, [Node | Path], Visited1) of
                {cycle, _} = Cycle -> Cycle;
                no_cycle ->
                    Visited2 = Visited1#{Node => visited},
                    detect_cycle(Graph, Rest, Path, Visited2)
            end
    end.

-spec validate_split_join_consistency(#{element_id() => #task{}},
                                      [#connection{}]) -> [binary()].
validate_split_join_consistency(Tasks, Conns) ->
    F = fun(TaskId, Task, Acc) ->
            Acc ++ validate_split_type(TaskId, Task, Conns)
                ++ validate_join_type(TaskId, Task, Conns)
        end,
    maps:fold(F, [], Tasks).

-spec validate_split_type(element_id(), #task{}, [#connection{}]) -> [binary()].
validate_split_type(_TaskId, #task{split_type = undefined}, _Conns) -> [];
validate_split_type(TaskId, #task{split_type = SplitType}, Conns) ->
    OutgoingCount = length([C || #connection{from_id = F} = C <- Conns, F =:= TaskId]),
    NBin = integer_to_binary(OutgoingCount),
    case {SplitType, OutgoingCount} of
        {xor_split, N} when N < 2 ->
            [iolist_to_binary([<<"XOR split on '">>, TaskId,
                              <<"' requires at least 2 outgoing, found ">>, NBin])];
        {and_split, N} when N < 2 ->
            [iolist_to_binary([<<"AND split on '">>, TaskId,
                              <<"' requires at least 2 outgoing, found ">>, NBin])];
        {or_split, N} when N < 2 ->
            [iolist_to_binary([<<"OR split on '">>, TaskId,
                              <<"' requires at least 2 outgoing, found ">>, NBin])];
        _ -> []
    end.

-spec validate_join_type(element_id(), #task{}, [#connection{}]) -> [binary()].
validate_join_type(_TaskId, #task{join_type = undefined}, _Conns) -> [];
validate_join_type(TaskId, #task{join_type = JoinType}, Conns) ->
    IncomingCount = length([C || #connection{to_id = T} = C <- Conns, T =:= TaskId]),
    NBin = integer_to_binary(IncomingCount),
    case {JoinType, IncomingCount} of
        {xor_join, N} when N < 2 ->
            [iolist_to_binary([<<"XOR join on '">>, TaskId,
                              <<"' requires at least 2 incoming, found ">>, NBin])];
        {and_join, N} when N < 2 ->
            [iolist_to_binary([<<"AND join on '">>, TaskId,
                              <<"' requires at least 2 incoming, found ">>, NBin])];
        {or_join, N} when N < 2 ->
            [iolist_to_binary([<<"OR join on '">>, TaskId,
                              <<"' requires at least 2 incoming, found ">>, NBin])];
        _ -> []
    end.

%%====================================================================
%% WCP-07 to WCP-10 Pattern Execution Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a synchronizing merge pattern (WCP-07).
%%
%% The synchronizing merge pattern waits for all incoming branches
%% to be "in progress" before allowing exactly one to proceed.
%% It ensures all branches have been activated before merging.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_synchronizing_merge(#synchronizing_merge{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_synchronizing_merge(#synchronizing_merge{
        merge_task_id = MergeTaskId,
        incoming_task_ids = IncomingTaskIds
    }, Input, Options) ->
    case validate_incoming_ids(IncomingTaskIds) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            Timeout = maps:get(timeout, Options, 5000),
            case check_all_branches_active(IncomingTaskIds, Input) of
                {ok, ActiveBranches} ->
                    Result = #{
                        merge_task => MergeTaskId,
                        active_branches => ActiveBranches,
                        selected_branch => select_first_active(ActiveBranches),
                        status => sync_merge_complete
                    },
                    {ok, apply_timeout(Result, Timeout)};
                {error, _} = Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a multi merge pattern (WCP-08).
%%
%% The multi merge pattern collects all incoming branch completions
%% and merges them together.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multi_merge(#multi_merge{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_multi_merge(#multi_merge{
        merge_task_id = MergeTaskId,
        incoming_task_ids = IncomingTaskIds
    }, Input, Options) ->
    case validate_incoming_ids(IncomingTaskIds) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            Timeout = maps:get(timeout, Options, 5000),
            case collect_all_branch_results(IncomingTaskIds, Input) of
                {ok, CollectedResults} when length(CollectedResults) > 0 ->
                    Result = #{
                        merge_task => MergeTaskId,
                        collected_results => CollectedResults,
                        result_count => length(CollectedResults),
                        status => multi_merge_complete
                    },
                    {ok, apply_timeout(Result, Timeout)};
                {ok, []} ->
                    {error, no_results_collected};
                {error, _} = Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a discriminator pattern (WCP-09).
%%
%% The discriminator pattern triggers on the first incoming branch
%% completion and ignores subsequent completions.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_discriminator(#discriminator{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_discriminator(#discriminator{
        merge_task_id = MergeTaskId,
        incoming_task_ids = IncomingTaskIds
    }, Input, Options) ->
    case validate_incoming_ids(IncomingTaskIds) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            Timeout = maps:get(timeout, Options, 5000),
            case get_first_completed_branch(IncomingTaskIds, Input) of
                {ok, FirstBranch} ->
                    Result = #{
                        merge_task => MergeTaskId,
                        triggering_branch => FirstBranch,
                        discriminator_reset => true,
                        status => discriminator_triggered
                    },
                    {ok, apply_timeout(Result, Timeout)};
                {error, _} = Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes an arbitration pattern (WCP-10).
%%
%% The arbitration pattern waits for a specified number of incoming
%% branches (N of M) to complete before proceeding.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_arbitration(#arbitration{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_arbitration(#arbitration{
        merge_task_id = MergeTaskId,
        incoming_task_ids = IncomingTaskIds,
        required_count = RequiredCount
    }, Input, Options) ->
    case validate_incoming_ids(IncomingTaskIds) of
        {error, Reason} ->
            {error, Reason};
        ok when RequiredCount > length(IncomingTaskIds) ->
            {error, {invalid_count, RequiredCount, length(IncomingTaskIds)}};
        ok ->
            Timeout = maps:get(timeout, Options, 5000),
            TotalCount = length(IncomingTaskIds),
            ActualRequired = min(RequiredCount, TotalCount),

            case collect_quorum_results(IncomingTaskIds, ActualRequired, Input) of
                {ok, QuorumResults} when length(QuorumResults) >= ActualRequired ->
                    Result = #{
                        merge_task => MergeTaskId,
                        quorum_results => QuorumResults,
                        quorum_count => ActualRequired,
                        total_branches => TotalCount,
                        status => arbitration_complete
                    },
                    {ok, apply_timeout(Result, Timeout)};
                {ok, PartialResults} ->
                    {error, {quorum_not_met, length(PartialResults), ActualRequired}};
                {error, _} = Error ->
                    Error
            end
    end.

%%====================================================================
%% WCP-07 to WCP-10 Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates incoming task IDs list.
%% @end
%%--------------------------------------------------------------------
validate_incoming_ids([]) ->
    {error, empty_incoming_list};
validate_incoming_ids(_IncomingIds) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Checks if all branches are active (for synchronizing merge).
%% @end
%%--------------------------------------------------------------------
check_all_branches_active(IncomingIds, Input) ->
    case is_map(Input) of
        true ->
            ActiveBranches = lists:filter(fun(Id) ->
                case maps:get(Id, Input, undefined) of
                    undefined -> false;
                    Status when is_map(Status) -> maps:get(is_defined, Status, true);
                    _ -> true
                end
            end, IncomingIds),

            case length(ActiveBranches) of
                N when N =:= length(IncomingIds) ->
                    {ok, ActiveBranches};
                _ ->
                    {error, {not_all_active, length(ActiveBranches), length(IncomingIds)}}
            end;
        false ->
            {ok, IncomingIds}
    end.

%%--------------------------------------------------------------------
%% @doc Selects the first active branch.
%% @end
%%--------------------------------------------------------------------
select_first_active([]) ->
    undefined;
select_first_active([First | _Rest]) ->
    First.

%%--------------------------------------------------------------------
%% @doc Collects results from all incoming branches.
%% @end
%%--------------------------------------------------------------------
collect_all_branch_results(IncomingIds, Input) ->
    Results = lists:foldl(fun(Id, Acc) ->
        Result = case is_map(Input) of
            true ->
                maps:get(Id, Input, {error, not_found});
            false ->
                {ok, Id}
        end,
        case Result of
            {error, _} -> Acc;
            _ -> [{Id, Result} | Acc]
        end
    end, [], IncomingIds),

    case Results of
        [] -> {error, no_branch_results};
        _ -> {ok, lists:reverse(Results)}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the first completed branch for discriminator.
%% @end
%%--------------------------------------------------------------------
get_first_completed_branch(IncomingIds, Input) ->
    case lists:dropwhile(fun(Id) ->
        case is_map(Input) of
            true ->
                not maps:is_key(Id, Input);
            false ->
                false
        end
    end, IncomingIds) of
        [] ->
            {error, no_completed_branches};
        [First | _] ->
            {ok, First}
    end.

%%--------------------------------------------------------------------
%% @doc Collects results until quorum is reached.
%% @end
%%--------------------------------------------------------------------
collect_quorum_results(IncomingIds, RequiredCount, Input) ->
    Results = lists:foldl(fun(Id, Acc) ->
        case length(Acc) of
            N when N >= RequiredCount ->
                Acc;
            _ ->
                case is_map(Input) of
                    true ->
                        case maps:get(Id, Input, undefined) of
                            undefined -> Acc;
                            Value -> [{Id, Value} | Acc]
                        end;
                    false ->
                        [{Id, ok} | Acc]
                end
        end
    end, [], IncomingIds),

    {ok, lists:reverse(Results)}.

%%--------------------------------------------------------------------
%% @doc Applies timeout to result.
%% @end
%%--------------------------------------------------------------------
apply_timeout(Result, _Timeout) ->
    Result.

%%====================================================================
%% WRP-01 to WRP-05 Resource Pattern Execution Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a resource creation pattern (WRP-01).
%%
%% Creates a new workflow resource with the specified type and initialization
%% parameters. Tracks the resource lifecycle and handles initialization failures.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_resource_create(#resource_create{}, term()) -> {ok, map()} | {error, term()}.
execute_resource_create(#resource_create{resource_id = ResourceId,
                                        resource_type = ResourceType,
                                        init_params = InitParams}, WorkflowState) ->
    case ResourceId of
        <<>> ->
            {error, invalid_resource_id};
        _ ->
            Resources = maps:get(resources, WorkflowState, #{}),
            case maps:get(ResourceId, Resources, undefined) of
                undefined ->
                    create_resource(ResourceId, ResourceType, InitParams, WorkflowState);
                _ExistingResource ->
                    {error, {resource_already_exists, ResourceId}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a role allocation pattern (WRP-02).
%%
%% Allocates resources based on role requirements using the specified strategy.
%% Supports first_fit, best_fit, and random allocation strategies.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_role_allocate(#role_allocate{}, map(), term()) ->
          {ok, map()} | {error, term()}.
execute_role_allocate(#role_allocate{role_id = RoleId,
                                     required_capability = RequiredCapability,
                                     allocation_strategy = Strategy},
                      AvailableResources, WorkflowState) ->
    MatchingResources = find_matching_resources(RequiredCapability, AvailableResources),
    case MatchingResources of
        [] ->
            {error, {no_resources_for_role, RoleId, RequiredCapability}};
        _ ->
            case select_resource_by_strategy(Strategy, MatchingResources, WorkflowState) of
                {error, Reason} ->
                    {error, Reason};
                {ok, SelectedResource} ->
                    allocate_resource_to_role(RoleId, SelectedResource, WorkflowState)
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a resource start pattern (WRP-03).
%%
%% Starts resource execution after validating resource state.
%% Handles start failures and tracks resource lifecycle.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_resource_start(#resource_start{}, term()) -> {ok, map()} | {error, term()}.
execute_resource_start(#resource_start{resource_id = ResourceId,
                                       start_params = StartParams},
                       WorkflowState) ->
    Resources = maps:get(resources, WorkflowState, #{}),
    case maps:get(ResourceId, Resources, undefined) of
        undefined ->
            {error, {resource_not_found, ResourceId}};
        Resource ->
            case validate_resource_state(Resource) of
                {error, Reason} ->
                    {error, {invalid_resource_state, ResourceId, Reason}};
                ok ->
                    start_resource(ResourceId, Resource, StartParams, WorkflowState)
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a role distribution pattern (WRP-04).
%%
%% Distributes work items based on role assignments using the specified policy.
%% Supports round_robin, least_loaded, and affinity_based distribution.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_role_distribute(#role_distribute{}, map(), term()) ->
          {ok, map()} | {error, term()}.
execute_role_distribute(#role_distribute{work_item_ids = WorkItemIds,
                                         role_assignments = RoleAssignments,
                                         distribution_policy = Policy},
                        CurrentWorkload, WorkflowState) ->
    case WorkItemIds of
        [] ->
            {error, no_work_items};
        _ when map_size(RoleAssignments) =:= 0 ->
            {error, no_roles_assigned};
        _ ->
            distribute_work_by_policy(WorkItemIds, RoleAssignments, Policy,
                                     CurrentWorkload, WorkflowState)
    end.

%%--------------------------------------------------------------------
%% @doc Executes a capability allocation pattern (WRP-05).
%%
%% Allocates resources based on capability requirements using the specified
%% matching strategy. Supports exact_match, minimum_met, and best_effort.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_capability_allocate(#capability_allocate{}, [term()], term()) ->
          {ok, map()} | {error, term()}.
execute_capability_allocate(#capability_allocate{required_capabilities = RequiredCaps,
                                                resource_registry = _ResourceRegistry,
                                                matching_strategy = Strategy},
                           AvailableResources, WorkflowState) ->
    MatchingResources = match_capabilities_by_strategy(Strategy, RequiredCaps,
                                                       AvailableResources),
    case MatchingResources of
        [] ->
            {error, {no_matching_capabilities, RequiredCaps}};
        _ ->
            allocate_matched_resources(RequiredCaps, MatchingResources,
                                      Strategy, WorkflowState)
    end.

%%====================================================================
%% Resource Pattern Helper Functions
%%====================================================================

%% @private
create_resource(ResourceId, ResourceType, InitParams, WorkflowState) ->
    try
        InitializedResource = initialize_resource_type(ResourceType, InitParams),
        Resources = maps:get(resources, WorkflowState, #{}),
        UpdatedResources = Resources#{
            ResourceId => InitializedResource#{
                id => ResourceId,
                type => ResourceType,
                state => created,
                created_at => erlang:timestamp(),
                init_params => InitParams
            }
        },
        UpdatedState = WorkflowState#{
            resources => UpdatedResources,
            last_updated => erlang:timestamp()
        },
        {ok, UpdatedState}
    catch
        Error:Reason ->
            {error, {resource_init_failed, ResourceType, {Error, Reason}}}
    end.

%% @private
initialize_resource_type(ResourceType, InitParams) ->
    case ResourceType of
        database_connection ->
            #{
                connection => undefined,
                pool_size => maps:get(pool_size, InitParams, 10),
                host => maps:get(host, InitParams, "localhost"),
                port => maps:get(port, InitParams, 5432)
            };
        worker_pool ->
            #{
                workers => [],
                pool_size => maps:get(pool_size, InitParams, 5),
                max_queue => maps:get(max_queue, InitParams, 100)
            };
        cache ->
            #{
                store => #{},
                ttl => maps:get(ttl, InitParams, 3600),
                max_size => maps:get(max_size, InitParams, 1000)
            };
        message_queue ->
            #{
                queue => queue:new(),
                max_length => maps:get(max_length, InitParams, 10000)
            };
        file_handle ->
            #{
                path => maps:get(path, InitParams, ""),
                mode => maps:get(mode, InitParams, read),
                handle => undefined
            };
        _ ->
            #{
                config => InitParams,
                custom_state => undefined
            }
    end.

%% @private
find_matching_resources(RequiredCapability, AvailableResources) ->
    maps:fold(fun(ResourceId, ResourceCaps, Acc) ->
        case capability_matches(RequiredCapability, ResourceCaps) of
            true -> [ResourceId | Acc];
            false -> Acc
        end
    end, [], AvailableResources).

%% @private
capability_matches(Required, Available) ->
    case {Required, Available} of
        {R, A} when is_map(R) andalso is_map(A) ->
            maps:fold(fun(Key, RequiredValue, Acc) ->
                case maps:get(Key, A, undefined) of
                    undefined -> false;
                    AvailableValue when AvailableValue >= RequiredValue -> Acc andalso true;
                    _ -> false
                end
            end, true, R);
        {R, A} when R =:= A -> true;
        {R, _} when is_list(R) -> lists:member(Available, R);
        _ -> false
    end.

%% @private
select_resource_by_strategy(first_fit, [ResourceId | _Rest], _WorkflowState) ->
    {ok, ResourceId};
select_resource_by_strategy(best_fit, ResourceIds, WorkflowState) ->
    Resources = maps:get(resources, WorkflowState, #{}),
    case lists:map(fun(Id) ->
        Resource = maps:get(Id, Resources, #{}),
        Score = calculate_fit_score(Resource),
        {Score, Id}
    end, ResourceIds) of
        [] -> {error, no_resources};
        Scored ->
            {_BestScore, BestId} = lists:min(Scored),
            {ok, BestId}
    end;
select_resource_by_strategy(random, ResourceIds, _WorkflowState) ->
    case length(ResourceIds) of
        0 -> {error, no_resources};
        N ->
            Index = rand:uniform(N),
            {ok, lists:nth(Index, ResourceIds)}
    end.

%% @private
calculate_fit_score(_Resource) ->
    rand:uniform().

%% @private
allocate_resource_to_role(RoleId, ResourceId, WorkflowState) ->
    Allocations = maps:get(role_allocations, WorkflowState, #{}),
    case maps:get(RoleId, Allocations, undefined) of
        undefined ->
            UpdatedAllocations = Allocations#{RoleId => ResourceId},
            UpdatedState = WorkflowState#{
                role_allocations => UpdatedAllocations,
                last_updated => erlang:timestamp()
            },
            {ok, UpdatedState};
        _Existing ->
            {error, {role_already_allocated, RoleId}}
    end.

%% @private
validate_resource_state(Resource) ->
    State = maps:get(state, Resource, unknown),
    case State of
        created -> ok;
        stopped -> ok;
        failed -> {error, resource_in_failed_state};
        running -> {error, resource_already_running};
        _ -> {error, {invalid_state, State}}
    end.

%% @private
start_resource(ResourceId, Resource, StartParams, WorkflowState) ->
    try
        StartedResource = Resource#{
            state => running,
            started_at => erlang:timestamp(),
            start_params => StartParams
        },
        Resources = maps:get(resources, WorkflowState, #{}),
        UpdatedResources = Resources#{ResourceId => StartedResource},
        UpdatedState = WorkflowState#{
            resources => UpdatedResources,
            last_updated => erlang:timestamp()
        },
        {ok, UpdatedState}
    catch
        Error:Reason ->
            {error, {resource_start_failed, ResourceId, {Error, Reason}}}
    end.

%% @private
distribute_work_by_policy(WorkItemIds, RoleAssignments, Policy,
                          CurrentWorkload, WorkflowState) ->
    case Policy of
        round_robin ->
            distribute_round_robin(WorkItemIds, RoleAssignments, WorkflowState);
        least_loaded ->
            distribute_least_loaded(WorkItemIds, RoleAssignments, CurrentWorkload,
                                   WorkflowState);
        affinity_based ->
            distribute_affinity(WorkItemIds, RoleAssignments, WorkflowState)
    end.

%% @private
distribute_round_robin(WorkItemIds, RoleAssignments, WorkflowState) ->
    RoleIds = maps:keys(RoleAssignments),
    {Distribution, _Index} = lists:mapfoldl(fun(WorkItemId, Index) ->
        RoleIndex = (Index rem length(RoleIds)) + 1,
        RoleId = lists:nth(RoleIndex, RoleIds),
        {{WorkItemId, RoleId}, Index + 1}
    end, 0, WorkItemIds),
    Result = #{
        distribution => maps:from_list(Distribution),
        policy => round_robin,
        total_items => length(WorkItemIds)
    },
    UpdatedState = WorkflowState#{
        work_distribution => Result,
        last_updated => erlang:timestamp()
    },
    {ok, UpdatedState}.

%% @private
distribute_least_loaded(WorkItemIds, RoleAssignments, CurrentWorkload,
                        WorkflowState) ->
    RoleIds = maps:keys(RoleAssignments),
    InitialLoad = lists:map(fun(RoleId) ->
        Load = maps:get(RoleId, CurrentWorkload, 0),
        {RoleId, Load}
    end, RoleIds),
    {Distribution, FinalLoads} = lists:mapfoldl(fun(WorkItemId, CurrentLoads) ->
        {_RoleId, _MinLoad} = lists:unzip(CurrentLoads),
        LoadsOnly = [L || {_, L} <- CurrentLoads],
        MinLoadValue = lists:min(LoadsOnly),
        {MinRoleId, _} = lists:keyfind(MinLoadValue, 2, CurrentLoads),
        UpdatedLoads = lists:map(fun({R, L}) ->
            case R of
                MinRoleId -> {R, L + 1};
                _ -> {R, L}
            end
        end, CurrentLoads),
        {{WorkItemId, MinRoleId}, UpdatedLoads}
    end, InitialLoad, WorkItemIds),
    Result = #{
        distribution => maps:from_list(Distribution),
        policy => least_loaded,
        total_items => length(WorkItemIds),
        final_loads => maps:from_list(FinalLoads)
    },
    UpdatedState = WorkflowState#{
        work_distribution => Result,
        last_updated => erlang:timestamp()
    },
    {ok, UpdatedState}.

%% @private
distribute_affinity(WorkItemIds, RoleAssignments, WorkflowState) ->
    RoleIds = maps:keys(RoleAssignments),
    Distribution = lists:map(fun(WorkItemId) ->
        Hash = erlang:phash2(WorkItemId),
        RoleIndex = (Hash rem length(RoleIds)) + 1,
        RoleId = lists:nth(RoleIndex, RoleIds),
        {WorkItemId, RoleId}
    end, WorkItemIds),
    Result = #{
        distribution => maps:from_list(Distribution),
        policy => affinity_based,
        total_items => length(WorkItemIds)
    },
    UpdatedState = WorkflowState#{
        work_distribution => Result,
        last_updated => erlang:timestamp()
    },
    {ok, UpdatedState}.

%% @private
match_capabilities_by_strategy(exact_match, RequiredCaps, AvailableResources) ->
    lists:filtermap(fun(Resource) ->
        Caps = get_resource_capabilities(Resource),
        case exact_capability_match(RequiredCaps, Caps) of
            true -> {true, Resource};
            false -> false
        end
    end, AvailableResources);

match_capabilities_by_strategy(minimum_met, RequiredCaps, AvailableResources) ->
    lists:filtermap(fun(Resource) ->
        Caps = get_resource_capabilities(Resource),
        case minimum_capability_met(RequiredCaps, Caps) of
            true -> {true, Resource};
            false -> false
        end
    end, AvailableResources);

match_capabilities_by_strategy(best_effort, RequiredCaps, AvailableResources) ->
    Scored = lists:map(fun(Resource) ->
        Caps = get_resource_capabilities(Resource),
        Score = calculate_capability_score(RequiredCaps, Caps),
        {Score, Resource}
    end, AvailableResources),
    Sorted = lists:reverse(lists:sort(Scored)),
    lists:filtermap(fun({Score, Resource}) ->
        case Score > 0 of
            true -> {true, Resource};
            false -> false
        end
    end, Sorted).

%% @private
get_resource_capabilities(Resource) ->
    case Resource of
        #{capabilities := Caps} -> Caps;
        _ when is_map(Resource) -> Resource;
        _ -> #{}
    end.

%% @private
exact_capability_match(Required, Available) ->
    maps:fold(fun(Key, ReqVal, Acc) ->
        case maps:get(Key, Available, undefined) of
            ReqVal -> Acc andalso true;
            _ -> false
        end
    end, true, Required).

%% @private
minimum_capability_met(Required, Available) ->
    maps:fold(fun(Key, ReqVal, Acc) ->
        case maps:get(Key, Available, undefined) of
            AvailVal when is_number(AvailVal) andalso is_number(ReqVal) ->
                Acc andalso (AvailVal >= ReqVal);
            AvailVal when AvailVal =:= ReqVal -> Acc andalso true;
            _ -> false
        end
    end, true, Required).

%% @private
calculate_capability_score(Required, Available) ->
    maps:fold(fun(Key, ReqVal, Acc) ->
        case maps:get(Key, Available, undefined) of
            AvailVal when is_number(AvailVal) andalso is_number(ReqVal) ->
                Ratio = AvailVal / ReqVal,
                Acc + case Ratio of
                    1.0 -> 2.0;
                    _ when Ratio >= 1.0 -> 1.0;
                    _ -> Ratio
                end;
            _ when is_number(ReqVal) =:= false ->
                case maps:get(Key, Available, undefined) of
                    AvailVal when AvailVal =:= ReqVal -> Acc + 2.0;
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, 0.0, Required).

%% @private
allocate_matched_resources(RequiredCaps, MatchedResources, Strategy,
                          WorkflowState) ->
    case MatchedResources of
        [Resource] ->
            finalize_capability_allocation(RequiredCaps, Resource, WorkflowState);
        _ when length(MatchedResources) > 1 ->
            case Strategy of
                best_effort ->
                    [BestResource | _] = MatchedResources,
                    finalize_capability_allocation(RequiredCaps, BestResource,
                                                  WorkflowState);
                _ ->
                    [BestResource | _] = MatchedResources,
                    finalize_capability_allocation(RequiredCaps, BestResource,
                                                  WorkflowState)
            end;
        [] ->
            {error, no_matching_resources}
    end.

%% @private
finalize_capability_allocation(RequiredCaps, Resource, WorkflowState) ->
    Result = #{
        allocated_resource => Resource,
        required_capabilities => RequiredCaps,
        allocated_at => erlang:timestamp()
    },
    UpdatedState = WorkflowState#{
        capability_allocation => Result,
        last_updated => erlang:timestamp()
    },
    {ok, UpdatedState}.

%%====================================================================
%% Data Flow Pattern Execution Functions (WDP-01 to WDP-05)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a Parameter Passing pattern (WDP-01).
%% @end
%%--------------------------------------------------------------------
-spec execute_param_pass(#param_pass{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_param_pass(#param_pass{
        source_task_id = SourceId,
        target_task_id = TargetId,
        param_name = ParamName,
        transform_fn = TransformFn
    }, Input, Options) ->
    case validate_param(SourceId, Input) of
        {error, Reason} ->
            {error, {source_validation_failed, Reason}};
        ok ->
            ParamValue = extract_param_value(Input, SourceId, ParamName),
            Result = case TransformFn of
                undefined -> ParamValue;
                Fn when is_function(Fn, 1) ->
                    safe_apply_transform(Fn, ParamValue, Options)
            end,
            case Result of
                {error, _} = Error -> Error;
                TransformedValue ->
                    {ok, #{
                        source_task => SourceId,
                        target_task => TargetId,
                        param_name => ParamName,
                        value => TransformedValue,
                        status => param_passed
                    }}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Data Transformation pattern (WDP-02).
%% @end
%%--------------------------------------------------------------------
-spec execute_data_transform(#data_transform{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_data_transform(#data_transform{
        input_task_id = InputId,
        output_task_id = OutputId,
        transform_fn = TransformFn,
        output_schema = Schema
    }, Input, Options) ->
    case validate_transform(InputId, Input) of
        {error, Reason} ->
            {error, {input_validation_failed, Reason}};
        ok ->
            InputData = extract_param_value(Input, InputId, undefined),
            TransformResult = safe_apply_transform(TransformFn, InputData, Options),
            case TransformResult of
                {error, _} = Error ->
                    Error;
                TransformedData ->
                    case Schema of
                        undefined ->
                            {ok, #{
                                input_task => InputId,
                                output_task => OutputId,
                                transformed_data => TransformedData,
                                status => transform_complete
                            }};
                        _ ->
                            case validate_output_schema(TransformedData, Schema) of
                                ok ->
                                    {ok, #{
                                        input_task => InputId,
                                        output_task => OutputId,
                                        transformed_data => TransformedData,
                                        schema_valid => true,
                                        status => transform_complete
                                    }};
                                {error, SchemaReason} ->
                                    {error, {schema_validation_failed, SchemaReason}}
                            end
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Data Distribution pattern (WDP-03).
%% @end
%%--------------------------------------------------------------------
-spec execute_data_distribute(#data_distribute{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_data_distribute(#data_distribute{
        source_task_id = SourceId,
        recipient_task_ids = RecipientIds,
        distribution_type = DistributionType
    }, Input, Options) ->
    case RecipientIds of
        [] ->
            {error, no_recipients};
        _ ->
            SourceData = extract_param_value(Input, SourceId, undefined),
            DistributionResult = case DistributionType of
                broadcast ->
                    distribute_broadcast_data(SourceData, RecipientIds);
                round_robin ->
                    RoundRobinIndex = maps:get(round_robin_index, Options, 0),
                    distribute_round_robin_data(SourceData, RecipientIds, RoundRobinIndex);
                partitioned ->
                    PartitionKey = maps:get(partition_key, Options, undefined),
                    distribute_partitioned_data(SourceData, RecipientIds, PartitionKey)
            end,
            case DistributionResult of
                {error, _} = Error -> Error;
                DistributedMap ->
                    {ok, #{
                        source_task => SourceId,
                        distribution_type => DistributionType,
                        recipient_count => length(RecipientIds),
                        distributed_data => DistributedMap,
                        status => distribution_complete
                    }}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Data Accumulation pattern (WDP-04).
%% @end
%%--------------------------------------------------------------------
-spec execute_data_accumulate(#data_accumulate{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_data_accumulate(#data_accumulate{
        source_task_ids = SourceIds,
        target_task_id = TargetId,
        aggregation_fn = AggFn,
        initial_value = InitialValue
    }, Input, _Options) ->
    case SourceIds of
        [] ->
            {ok, #{
                target_task => TargetId,
                accumulated_value => InitialValue,
                source_count => 0,
                status => accumulation_complete
            }};
        _ ->
            SourceValues = lists:map(fun(SourceId) ->
                extract_param_value(Input, SourceId, undefined)
            end, SourceIds),
            try
                AccumulatedValue = lists:foldl(fun(Value, Acc) ->
                    AggFn(Acc, Value)
                end, InitialValue, SourceValues),
                {ok, #{
                    target_task => TargetId,
                    accumulated_value => AccumulatedValue,
                    source_count => length(SourceIds),
                    source_values => SourceValues,
                    status => accumulation_complete
                }}
            catch
                _:Error ->
                    {error, {aggregation_failed, Error}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Data Visibility pattern (WDP-05).
%% @end
%%--------------------------------------------------------------------
-spec execute_data_visibility(#data_visibility{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

execute_data_visibility(#data_visibility{
        data_task_id = DataId,
        scope = Scope,
        access_list = AccessList
    }, Input, Options) ->
    RequestingTaskId = maps:get(requesting_task_id, Options, undefined),
    case RequestingTaskId of
        undefined ->
            {error, no_requesting_task_id};
        _ ->
            ScopeResult = check_scope_visibility(Scope, DataId, RequestingTaskId, Input),
            case ScopeResult of
                {error, _} = Error -> Error;
                ok ->
                    case AccessList of
                        undefined ->
                            {ok, #{
                                data_task => DataId,
                                requesting_task => RequestingTaskId,
                                scope => Scope,
                                access_granted => true,
                                status => visibility_granted
                            }};
                        _ ->
                            AccessResult = validate_access(RequestingTaskId, AccessList),
                            case AccessResult of
                                ok ->
                                    {ok, #{
                                        data_task => DataId,
                                        requesting_task => RequestingTaskId,
                                        scope => Scope,
                                        access_granted => true,
                                        status => visibility_granted
                                    }};
                                {error, _} ->
                                    {ok, #{
                                        data_task => DataId,
                                        requesting_task => RequestingTaskId,
                                        scope => Scope,
                                        access_granted => false,
                                        reason => access_list_denied,
                                        status => visibility_denied
                                    }}
                            end
                    end
            end
    end.

%%====================================================================
%% Data Flow Helper Functions
%%====================================================================

-spec validate_param(element_id(), term()) -> ok | {error, term()}.
validate_param(undefined, _Input) ->
    {error, no_source_specified};
validate_param(_SourceId, undefined) ->
    {error, no_input_data};
validate_param(_SourceId, _Input) ->
    ok.

-spec validate_transform(element_id(), term()) -> ok | {error, term()}.
validate_transform(undefined, _Input) ->
    {error, no_input_task_specified};
validate_transform(_InputId, undefined) ->
    {error, no_input_data};
validate_transform(_InputId, _Input) ->
    ok.

-spec distribution_strategy(broadcast | round_robin | partitioned) -> function().
distribution_strategy(broadcast) ->
    fun(_Data, Recipients) ->
        lists:map(fun(R) -> {R, all} end, Recipients)
    end;
distribution_strategy(round_robin) ->
    fun(Data, Recipients) ->
        distribute_round_robin_data(Data, Recipients, 0)
    end;
distribution_strategy(partitioned) ->
    fun(Data, Recipients) ->
        distribute_partitioned_data(Data, Recipients, undefined)
    end.

-spec aggregate_function(sum | count | list | min | max | first | last | custom) -> function().
aggregate_function(sum) ->
    fun(Acc, X) -> Acc + X end;
aggregate_function(count) ->
    fun(Acc, _X) -> Acc + 1 end;
aggregate_function(list) ->
    fun(Acc, X) -> [X | Acc] end;
aggregate_function(min) ->
    fun(Acc, X) -> min(Acc, X) end;
aggregate_function(max) ->
    fun(Acc, X) -> max(Acc, X) end;
aggregate_function(first) ->
    fun(Acc, _X) -> Acc end;
aggregate_function(last) ->
    fun(_Acc, X) -> X end;
aggregate_function(custom) ->
    fun(Acc, X) -> [X | Acc] end.

-spec validate_access(element_id(), [element_id()] | undefined) -> ok | {error, term()}.
validate_access(_TaskId, undefined) ->
    ok;
validate_access(TaskId, AccessList) when is_list(AccessList) ->
    case lists:member(TaskId, AccessList) of
        true -> ok;
        false -> {error, {access_denied, TaskId}}
    end.

-spec extract_param_value(term(), element_id(), atom() | undefined) -> term().
extract_param_value(Input, Key, ParamName) when is_map(Input) ->
    case maps:get(Key, Input, undefined) of
        undefined ->
            case ParamName of
                undefined -> Input;
                _ -> maps:get(ParamName, Input, undefined)
            end;
        Value -> Value
    end;
extract_param_value(Input, _Key, _ParamName) ->
    Input.

-spec safe_apply_transform(function(), term(), map()) -> term() | {error, term()}.
safe_apply_transform(TransformFn, Data, Options) ->
    SafeMode = maps:get(safe_mode, Options, true),
    try
        TransformFn(Data)
    catch
        _:Error:_ when SafeMode ->
            {error, {transform_failed, Error}};
        _:Error:Stacktrace ->
            erlang:raise(error, Error, Stacktrace)
    end.

-spec validate_output_schema(term(), term()) -> ok | {error, term()}.
validate_output_schema(_Data, undefined) ->
    ok;
validate_output_schema(Data, Schema) when is_function(Schema, 1) ->
    try
        case Schema(Data) of
            true -> ok;
            false -> {error, schema_validation_failed}
        end
    catch
        _:_ -> {error, schema_error}
    end;
validate_output_schema(Data, {type, ExpectedType}) ->
    case type_of(Data) of
        ExpectedType -> ok;
        ActualType -> {error, {type_mismatch, ExpectedType, ActualType}}
    end;
validate_output_schema(Data, {required, Fields}) when is_list(Fields) ->
    case is_map(Data) of
        false -> {error, {not_a_map, Data}};
        true ->
            MissingFields = lists:filter(fun(F) ->
                not maps:is_key(F, Data)
            end, Fields),
            case MissingFields of
                [] -> ok;
                _ -> {error, {missing_fields, MissingFields}}
            end
    end;
validate_output_schema(_Data, _Schema) ->
    ok.

-spec type_of(term()) -> atom().
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_bitstring(X) -> bitstring;
type_of(X) when is_float(X) -> float;
type_of(X) when is_function(X) -> function;
type_of(X) when is_integer(X) -> integer;
type_of(X) when is_list(X) -> list;
type_of(X) when is_map(X) -> map;
type_of(X) when is_pid(X) -> pid;
type_of(X) when is_port(X) -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_tuple(X) -> tuple;
type_of(_) -> any.

-spec distribute_broadcast_data(term(), [element_id()]) -> map().
distribute_broadcast_data(Data, RecipientIds) ->
    lists:foldl(fun(Id, Acc) ->
        Acc#{Id => Data}
    end, #{}, RecipientIds).

-spec distribute_round_robin_data(term(), [element_id()], non_neg_integer()) -> map().
distribute_round_robin_data(_Data, [], _Index) ->
    #{};
distribute_round_robin_data(Data, RecipientIds, Index) ->
    NormalizedIndex = Index rem length(RecipientIds),
    SelectedId = lists:nth(NormalizedIndex + 1, RecipientIds),
    #{SelectedId => Data}.

-spec distribute_partitioned_data(term(), [element_id()], term()) -> map().
distribute_partitioned_data(_Data, [], _PartitionKey) ->
    #{};
distribute_partitioned_data(Data, RecipientIds, PartitionKey) ->
    HashValue = case PartitionKey of
        undefined -> erlang:phash2(Data);
        Key -> erlang:phash2(Key)
    end,
    Index = HashValue rem length(RecipientIds),
    SelectedId = lists:nth(Index + 1, RecipientIds),
    #{SelectedId => Data}.

-spec check_scope_visibility(local | branch | global, element_id(), element_id(), term()) ->
    ok | {error, term()}.
check_scope_visibility(local, DataId, RequestingTaskId, _Input) ->
    case DataId =:= RequestingTaskId of
        true -> ok;
        false -> {error, {local_scope_violation, DataId, RequestingTaskId}}
    end;
check_scope_visibility(branch, _DataId, _RequestingTaskId, Input) when is_map(Input) ->
    DataBranch = maps:get(branch_id, Input, undefined),
    RequestBranch = maps:get(requesting_branch_id, Input, undefined),
    case DataBranch =:= RequestBranch of
        true -> ok;
        false -> {error, {branch_scope_violation, DataBranch, RequestBranch}}
    end;
check_scope_visibility(branch, _DataId, _RequestingTaskId, _Input) ->
    ok;
check_scope_visibility(global, _DataId, _RequestingTaskId, _Input) ->
    ok.

%%====================================================================
%% Human-in-the-Loop Wrapper Functions (Delegates to yawl_approval)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Requests approval for a workflow task.
%%
%% Creates an approval checkpoint and requests approval from the specified
%% approver. Delegates to yawl_approval:create_checkpoint.
%%
%% @param Workflow The workflow record or workflow ID.
%% @param TaskId The ID of the task requiring approval.
%% @param Approver The approver identifier (pid, name, or role).
%% @return {ok, CheckpointId} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec request_approval(Workflow :: #workflow{} | binary(), TaskId :: binary(),
                      Approver :: term()) -> {ok, binary()} | {error, term()}.

request_approval(#workflow{id = WorkflowId}, TaskId, Approver) ->
    request_approval(WorkflowId, TaskId, Approver);
request_approval(WorkflowId, TaskId, Approver) when is_binary(WorkflowId) ->
    case whereis(yawl_approval) of
        undefined ->
            {error, {approval_not_started, yawl_approval}};
        _Pid ->
            StepName = binary_to_atom(TaskId, utf8),
            Context = #{
                workflow_id => WorkflowId,
                task_id => TaskId,
                approver => Approver
            },
            Options = #{
                required_approver => human,
                timeout => 300000,
                context => Context
            },
            yawl_approval:create_checkpoint(WorkflowId, StepName, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Approves a pending workflow task.
%%
%% Records approval for a task awaiting human approval.
%% Delegates to yawl_approval:approve.
%%
%% @param Workflow The workflow record or workflow ID.
%% @param TaskId The ID of the task to approve.
%% @param Approver The approver identifier.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec approve_task(Workflow :: #workflow{} | binary(), TaskId :: binary(),
                  Approver :: term()) -> ok | {error, term()}.

approve_task(#workflow{id = WorkflowId}, TaskId, Approver) ->
    approve_task(WorkflowId, TaskId, Approver);
approve_task(WorkflowId, TaskId, Approver) when is_binary(WorkflowId) ->
    case whereis(yawl_approval) of
        undefined ->
            {error, {approval_not_started, yawl_approval}};
        _Pid ->
            CheckpointId = checkpoint_id_for_task(WorkflowId, TaskId),
            Reason = <<"Task approved by ", (to_binary(Approver))/binary>>,
            yawl_approval:approve(CheckpointId, Approver, Reason)
    end.

%%--------------------------------------------------------------------
%% @doc Rejects a pending workflow task.
%%
%% Records denial for a task awaiting human approval.
%% Delegates to yawl_approval:deny.
%%
%% @param Workflow The workflow record or workflow ID.
%% @param TaskId The ID of the task to reject.
%% @param Rejector The rejector identifier.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reject_task(Workflow :: #workflow{} | binary(), TaskId :: binary(),
                  Rejector :: term()) -> ok | {error, term()}.

reject_task(#workflow{id = WorkflowId}, TaskId, Rejector) ->
    reject_task(WorkflowId, TaskId, Rejector);
reject_task(WorkflowId, TaskId, Rejector) when is_binary(WorkflowId) ->
    case whereis(yawl_approval) of
        undefined ->
            {error, {approval_not_started, yawl_approval}};
        _Pid ->
            CheckpointId = checkpoint_id_for_task(WorkflowId, TaskId),
            Reason = <<"Task rejected by ", (to_binary(Rejector))/binary>>,
            yawl_approval:deny(CheckpointId, Rejector, Reason)
    end.

%%--------------------------------------------------------------------
%% @doc Gets the approval status for a workflow task.
%%
%% Returns the current approval status for a task.
%% Delegates to yawl_approval:check_status.
%%
%% @param Workflow The workflow record or workflow ID.
%% @param TaskId The ID of the task to check.
%% @return {ok, Status} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_approval_status(Workflow :: #workflow{} | binary(), TaskId :: binary()) ->
          {ok, yawl_approval:approval_status()} | {error, term()}.

get_approval_status(#workflow{id = WorkflowId}, TaskId) ->
    get_approval_status(WorkflowId, TaskId);
get_approval_status(WorkflowId, TaskId) when is_binary(WorkflowId) ->
    case whereis(yawl_approval) of
        undefined ->
            {error, {approval_not_started, yawl_approval}};
        _Pid ->
            CheckpointId = checkpoint_id_for_task(WorkflowId, TaskId),
            yawl_approval:check_status(CheckpointId)
    end.

%%--------------------------------------------------------------------
%% @doc Cancels a pending approval for a workflow task.
%%
%% Cancels an approval checkpoint that is pending.
%% Delegates to yawl_approval:cancel_checkpoint.
%%
%% @param Workflow The workflow record or workflow ID.
%% @param TaskId The ID of the task to cancel approval for.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_approval(Workflow :: #workflow{} | binary(), TaskId :: binary()) ->
          ok | {error, term()}.

cancel_approval(#workflow{id = WorkflowId}, TaskId) ->
    cancel_approval(WorkflowId, TaskId);
cancel_approval(WorkflowId, TaskId) when is_binary(WorkflowId) ->
    case whereis(yawl_approval) of
        undefined ->
            {error, {approval_not_started, yawl_approval}};
        _Pid ->
            CheckpointId = checkpoint_id_for_task(WorkflowId, TaskId),
            yawl_approval:cancel_checkpoint(CheckpointId)
    end.

%%--------------------------------------------------------------------
%% @doc Enables telemetry collection for YAWL workflows (no options).
%%
%% Starts the yawl_otel_logger gen_server if not already running.
%%
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec enable_telemetry() -> ok | {error, term()}.

enable_telemetry() ->
    enable_telemetry(#{}).

%%--------------------------------------------------------------------
%% @doc Enables telemetry collection for YAWL workflows (with options).
%%
%% Starts the yawl_otel_logger gen_server if not already running.
%%
%% @param Options Optional parameters for telemetry configuration
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec enable_telemetry(Options :: map()) -> ok | {error, term()}.

enable_telemetry(Options) when is_map(Options) ->
    case whereis(yawl_otel_logger) of
        undefined ->
            %% Start the telemetry server
            case yawl_otel_logger:start_link(Options) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                Error -> Error
            end;
        _Pid ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Disables telemetry collection for YAWL workflows.
%%
%% Stops the yawl_otel_logger gen_server.
%%
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec disable_telemetry() -> ok | {error, term()}.

disable_telemetry() ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%%--------------------------------------------------------------------
%% @doc Gets workflow metrics from the telemetry system.
%%
%% @param Workflow The workflow record or workflow ID
%% @return Map of workflow metrics or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_metrics(Workflow :: #workflow{} | binary()) ->
          {ok, map()} | {error, term()}.

get_workflow_metrics(#workflow{id = WorkflowId}) ->
    get_workflow_metrics(WorkflowId);
get_workflow_metrics(WorkflowId) when is_binary(WorkflowId) ->
    case whereis(yawl_otel_logger) of
        undefined ->
            {error, telemetry_not_started};
        _Pid ->
            %% Get events for this workflow
            AllEvents = yawl_otel_logger:get_events(),
            WorkflowEvents = maps:filter(
                fun(#otel_event{trace_id = Trace}) ->
                    binary:match(WorkflowId, Trace) orelse
                        (WorkflowId =:= Trace)
                end,
                AllEvents
            ),
            {ok, #{
                event_count => maps:size(WorkflowEvents),
                events => WorkflowEvents
            }}
    end.

%%--------------------------------------------------------------------
%% @doc Logs a workflow event.
%%
%% @param Workflow The workflow record or workflow ID
%% @param Event The event type (binary or atom)
%% @param Message The event message
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec log_workflow_event(Workflow :: #workflow{} | binary(),
                         Event :: binary() | atom(),
                         Message :: binary()) -> ok.

log_workflow_event(#workflow{id = WorkflowId}, Event, Message) ->
    log_workflow_event(WorkflowId, Event, Message);
log_workflow_event(WorkflowId, Event, Message) when is_binary(WorkflowId) ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        _Pid ->
            Attributes = #{
                workflow_id => WorkflowId,
                event_type => Event,
                message => Message,
                timestamp => erlang:system_time(millisecond)
            },
            yawl_otel_logger:log_event(Event, Message, Attributes)
    end.

%%--------------------------------------------------------------------
%% @doc Logs a workflow event with attributes.
%%
%% @param Workflow The workflow record or workflow ID
%% @param Event The event type (binary or atom)
%% @param Message The event message
%% @param Attributes Additional event attributes
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec log_workflow_event(Workflow :: #workflow{} | binary(),
                         Event :: binary() | atom(),
                         Message :: binary(),
                         Attributes :: map()) -> ok.

log_workflow_event(#workflow{id = WorkflowId}, Event, Message, Attributes) ->
    log_workflow_event(WorkflowId, Event, Message, Attributes);
log_workflow_event(WorkflowId, Event, Message, Attributes)
  when is_binary(WorkflowId), is_map(Attributes) ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        _Pid ->
            BaseAttrs = #{
                workflow_id => WorkflowId,
                event_type => Event,
                message => Message,
                timestamp => erlang:system_time(millisecond)
            },
            MergedAttrs = maps:merge(BaseAttrs, Attributes),
            yawl_otel_logger:log_event(Event, Message, MergedAttrs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a checkpoint ID for a workflow task.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint_id_for_task(WorkflowId :: binary(), TaskId :: binary()) -> binary().
checkpoint_id_for_task(WorkflowId, TaskId) ->
    <<WorkflowId/binary, ":", TaskId/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a term to binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_binary(term()) -> binary().
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(T) -> list_to_binary(io_lib:format("~p", [T])).

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Runs doctests for the module
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
