# CRE YAWL Workflow Engine - Complete API Reference

This document provides comprehensive documentation for all CRE functions and APIs. CRE implements the YAWL (Yet Another Workflow Language) standard with extensions for human-in-the-loop workflows and observability.

## Table of Contents

1. [Workflow Execution APIs](#workflow-execution-apis)
2. [Task Management APIs](#task-management-apis)
3. [Pattern Convenience APIs](#pattern-convenience-apis)
4. [Human-in-the-Loop APIs](#human-in-the-loop-apis)
5. [Telemetry Wrapper APIs](#telemetry-wrapper-apis)
6. [Configuration APIs](#configuration-apis)
7. [Persistence Wrapper APIs](#persistence-wrapper-apis)
8. [XES Wrapper APIs](#xes-wrapper-apis)
9. [Integration Wrapper APIs](#integration-wrapper-apis)
10. [Error Types](#error-types)

---

## Workflow Execution APIs

### cre_yawl:new_workflow/0

Create a new workflow definition with auto-generated ID.

```erlang
-spec new_workflow() -> #workflow{}.
```

**Returns**:
- `#workflow{}` - New workflow record with generated ID

**Example**:
```erlang
Workflow = cre_yawl:new_workflow(),
```

### cre_yawl:new_workflow/1

Create a new workflow definition with specified ID.

```erlang
-spec new_workflow(Id :: binary()) -> #workflow{}.
```

**Parameters**:
- `Id` - Unique workflow ID as a binary

**Returns**:
- `#workflow{}` - New workflow record

**Example**:
```erlang
Workflow = cre_yawl:new_workflow(<<"order_processing">>),
```

### cre_yawl:validate/1

Validate a workflow structure against YAWL patterns.

```erlang
-spec validate(Workflow :: #workflow{}) -> ok | {error, [binary()]}.
```

**Parameters**:
- `Workflow` - Workflow record to validate

**Returns**:
- `ok` - Successfully validated workflow
- `{error, [binary()]}` - List of validation error messages

**Example**:
```erlang
case cre_yawl:validate(Workflow) of
    ok ->
        % Workflow is valid
        execute_workflow(Workflow);
    {error, Errors} ->
        lists:foreach(fun(E) -> error_logger:error_msg("~s~n", [E]) end, Errors)
end,
```

### cre_yawl:get_errors/1

Get validation errors for a workflow.

```erlang
-spec get_errors(Workflow :: #workflow{}) -> [binary()].
```

**Returns**:
- `[binary()]` - List of error messages (empty if valid)

### cre_yawl:add_task/3

Add a task to a workflow.

```erlang
-spec add_task(Workflow :: #workflow{}, TaskId :: binary(),
               Task :: #task{} | [{atom(), term()}]) -> #workflow{}.
```

**Parameters**:
- `Workflow` - Existing workflow
- `TaskId` - Unique task ID (binary)
- `Task` - Task record or proplist

**Task Options** (proplist):
- `name` - Task display name (binary)
- `type` - Task type: `atomic`, `composite`, `subworkflow`, `multi_instance`
- `split_type` - Gateway split type: `and_split`, `or_split`, `xor_split`
- `join_type` - Gateway join type: `and_join`, `or_join`, `xor_join`
- `metadata` - Additional metadata map

**Returns**:
- `#workflow{}` - Updated workflow

**Example**:
```erlang
Task = [
    {name, <<"Validate Order">>},
    {type, atomic},
    {metadata, #{priority => high}}
],
Workflow1 = cre_yawl:add_task(Workflow, <<"validate_order">>, Task),
```

### cre_yawl:add_condition/3

Add a condition to a workflow.

```erlang
-spec add_condition(Workflow :: #workflow{}, ConditionId :: binary(),
                    Condition :: #yawl_condition{} | term()) -> #workflow{}.
```

**Parameters**:
- `Workflow` - Existing workflow
- `ConditionId` - Unique condition ID
- `Condition` - Condition record or expression

**Returns**:
- `#workflow{}` - Updated workflow

### cre_yawl:connect/3

Connect two tasks in a workflow.

```erlang
-spec connect(Workflow :: #workflow{}, FromId :: binary(),
              ToId :: binary()) -> #workflow{}.
```

**Parameters**:
- `Workflow` - Existing workflow
- `FromId` - Source task ID
- `ToId` - Target task ID

**Returns**:
- `#workflow{}` - Updated workflow

**Example**:
```erlang
Workflow1 = cre_yawl:connect(Workflow, <<"validate_order">>, <<"process_payment">>),
```

### cre_yawl:set_split_type/3

Set the split type for a task gateway.

```erlang
-spec set_split_type(Workflow :: #workflow{}, TaskId :: binary(),
                     SplitType :: and_split | or_split | xor_split) -> #workflow{}.
```

### cre_yawl:set_join_type/3

Set the join type for a task gateway.

```erlang
-spec set_join_type(Workflow :: #workflow{}, TaskId :: binary(),
                    JoinType :: and_join | or_join | xor_join) -> #workflow{}.
```

### cre_yawl:set_workflow_boundaries/3

Set start and end task boundaries for a workflow.

```erlang
-spec set_workflow_boundaries(Workflow :: #workflow{},
                              StartTaskId :: binary(),
                              EndTaskIds :: [binary()]) ->
          #workflow{} | {error, term()}.
```

**Returns**:
- `#workflow{}` - Workflow with boundaries set
- `{error, start_task_not_found}` - Start task doesn't exist
- `{error, {end_task_not_found, TaskId}}` - End task doesn't exist

### cre_yawl:get_workflow_id/1

Get the workflow ID.

```erlang
-spec get_workflow_id(#workflow{}) -> {ok, binary()}.
```

### cre_yawl:get_workflow_name/1

Get the workflow name.

```erlang
-spec get_workflow_name(#workflow{}) -> {ok, binary()}.
```

### cre_yawl:get_tasks/1

Get all tasks from a workflow.

```erlang
-spec get_tasks(#workflow{}) -> {ok, #{binary() => #task{}}}.
```

### cre_yawl:get_connections/1

Get all connections from a workflow.

```erlang
-spec get_connections(#workflow{}) -> {ok, [#connection{}]}.
```

### cre_yawl:get_conditions/1

Get all conditions from a workflow.

```erlang
-spec get_conditions(#workflow{}) -> {ok, #{binary() => #yawl_condition{}}}.
```

---

## Task Management APIs

### cre_yawl:get_task/2

Get task details from a workflow (via tasks map).

```erlang
% Use get_tasks/1 then map lookup
{ok, Tasks} = cre_yawl:get_tasks(Workflow),
Task = maps:get(<<"task_id">>, Tasks),
```

---

## Pattern Convenience APIs

### Basic Control Flow Patterns

### cre_yawl:sequence/0

Create a sequence pattern constructor.

```erlang
-spec sequence() -> #sequence{}.
```

### cre_yawl:parallel_split/0

Create a parallel split pattern constructor.

```erlang
-spec parallel_split() -> #parallel_split{}.
```

### cre_yawl:synchronization/0

Create a synchronization pattern constructor.

```erlang
-spec synchronization() -> #synchronization{}.
```

### cre_yawl:exclusive_choice/0

Create an exclusive choice pattern constructor.

```erlang
-spec exclusive_choice() -> #exclusive_choice{}.
```

### cre_yawl:simple_merge/0

Create a simple merge pattern constructor.

```erlang
-spec simple_merge() -> #simple_merge{}.
```

### cre_yawl:multi_choice/0

Create a multi-choice pattern constructor.

```erlang
-spec multi_choice() -> #multi_choice{}.
```

### cre_yawl:synchronizing_merge/0

Create a synchronizing merge pattern constructor.

```erlang
-spec synchronizing_merge() -> #synchronizing_merge{}.
```

### cre_yawl:multi_merge/0

Create a multi-merge pattern constructor.

```erlang
-spec multi_merge() -> #multi_merge{}.
```

### cre_yawl:discriminator/0

Create a discriminator pattern constructor.

```erlang
-spec discriminator() -> #discriminator{}.
```

### cre_yawl:arbitration/0

Create an arbitration pattern constructor.

```erlang
-spec arbitration() -> #arbitration{}.
```

### Pattern Execution Functions

### cre_yawl:execute_synchronizing_merge/3

Execute a synchronizing merge pattern (WCP-07).

```erlang
-spec execute_synchronizing_merge(#synchronizing_merge{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_multi_merge/3

Execute a multi-merge pattern (WCP-08).

```erlang
-spec execute_multi_merge(#multi_merge{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_discriminator/3

Execute a discriminator pattern (WCP-09).

```erlang
-spec execute_discriminator(#discriminator{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_arbitration/3

Execute an arbitration pattern (WCP-10).

```erlang
-spec execute_arbitration(#arbitration{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### Data Flow Pattern Constructors (WDP-1 to WDP-5)

### cre_yawl:param_pass/2

Create a parameter passing pattern (WDP-1).

```erlang
-spec param_pass(SourceId :: binary(), TargetId :: binary()) -> #param_pass{}.
```

### cre_yawl:param_pass/3

Create a parameter passing pattern with transform.

```erlang
-spec param_pass(SourceId :: binary(), TargetId :: binary(),
                 function() | {atom(), term()}) -> #param_pass{}.
```

### cre_yawl:data_transform/2

Create a data transformation pattern (WDP-2).

```erlang
-spec data_transform(InputId :: binary(), OutputId :: binary()) -> #data_transform{}.
```

### cre_yawl:data_transform/3

Create a data transformation pattern with function.

```erlang
-spec data_transform(InputId :: binary(), OutputId :: binary(),
                     function()) -> #data_transform{}.
```

### cre_yawl:data_distribute/1

Create a data distribution pattern (WDP-3).

```erlang
-spec data_distribute(RecipientIds :: [binary()]) -> #data_distribute{}.
```

### cre_yawl:data_distribute/4

Create a data distribution pattern with options.

```erlang
-spec data_distribute(SourceId :: binary(), RecipientIds :: [binary()],
                      broadcast | round_robin | partitioned, term()) -> #data_distribute{}.
```

### cre_yawl:data_accumulate/1

Create a data accumulation pattern (WDP-4).

```erlang
-spec data_accumulate(SourceIds :: [binary()]) -> #data_accumulate{}.
```

### cre_yawl:data_accumulate/4

Create a data accumulation pattern with function.

```erlang
-spec data_accumulate(SourceIds :: [binary()], TargetId :: binary(),
                      function(), term()) -> #data_accumulate{}.
```

### cre_yawl:data_visibility/2

Create a data visibility pattern (WDP-5).

```erlang
-spec data_visibility(DataId :: binary(), local | branch | global) -> #data_visibility{}.
```

### cre_yawl:data_visibility/3

Create a data visibility pattern with access list.

```erlang
-spec data_visibility(DataId :: binary(), local | branch | global,
                      [binary()]) -> #data_visibility{}.
```

### Data Flow Execution Functions

### cre_yawl:execute_param_pass/3

Execute parameter passing pattern.

```erlang
-spec execute_param_pass(#param_pass{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_data_transform/3

Execute data transformation pattern.

```erlang
-spec execute_data_transform(#data_transform{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_data_distribute/3

Execute data distribution pattern.

```erlang
-spec execute_data_distribute(#data_distribute{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_data_accumulate/3

Execute data accumulation pattern.

```erlang
-spec execute_data_accumulate(#data_accumulate{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_data_visibility/3

Execute data visibility pattern.

```erlang
-spec execute_data_visibility(#data_visibility{}, term(), map()) ->
    {ok, map()} | {error, term()}.
```

### Resource Pattern Constructors (WRP-1 to WRP-5)

### cre_yawl:resource_create/1

Create a resource creation pattern (WRP-1).

```erlang
-spec resource_create(ResourceType :: atom()) -> #resource_create{}.
```

### cre_yawl:role_allocate/2

Create a role allocation pattern (WRP-2).

```erlang
-spec role_allocate(RoleId :: atom(), Capability :: term()) -> #role_allocate{}.
```

### cre_yawl:resource_start/1

Create a resource start pattern (WRP-3).

```erlang
-spec resource_start(ResourceId :: binary()) -> #resource_start{}.
```

### cre_yawl:role_distribute/2

Create a role distribution pattern (WRP-4).

```erlang
-spec role_distribute(WorkItemIds :: [binary()], RoleAssignments :: map()) ->
    #role_distribute{}.
```

### cre_yawl:capability_allocate/2

Create a capability allocation pattern (WRP-5).

```erlang
-spec capability_allocate(Capabilities :: map(), Registry :: [term()]) ->
    #capability_allocate{}.
```

### Resource Pattern Execution Functions

### cre_yawl:execute_resource_create/2

Execute resource creation pattern.

```erlang
-spec execute_resource_create(#resource_create{}, term()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_role_allocate/3

Execute role allocation pattern.

```erlang
-spec execute_role_allocate(#role_allocate{}, map(), term()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_resource_start/2

Execute resource start pattern.

```erlang
-spec execute_resource_start(#resource_start{}, term()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_role_distribute/3

Execute role distribution pattern.

```erlang
-spec execute_role_distribute(#role_distribute{}, map(), term()) ->
    {ok, map()} | {error, term()}.
```

### cre_yawl:execute_capability_allocate/3

Execute capability allocation pattern.

```erlang
-spec execute_capability_allocate(#capability_allocate{}, [term()], term()) ->
    {ok, map()} | {error, term()}.
```

### Additional Patterns (yawl_patterns module)

### yawl_patterns:implicit_termination/1

Create an implicit termination pattern.

```erlang
-spec implicit_termination(TriggerFun :: function()) -> #implicit_termination{}.
```

### yawl_patterns:multiple_instances_no_sync/3

Create multiple instances without synchronization.

```erlang
-spec multiple_instances_no_sync(MapFun :: function(), Count :: pos_integer(),
                                 Input :: list()) -> #multiple_instances_no_sync{}.
```

### yawl_patterns:deferred_choice/3

Create a deferred choice pattern.

```erlang
-spec deferred_choice(OptionAFun :: function(), OptionBFun :: function(),
                      ChoiceFun :: function()) -> #deferred_choice{}.
```

### yawl_patterns:error_handler/2

Create an error handler pattern.

```erlang
-spec error_handler(RiskyFun :: function(), HandlerFun :: function()) ->
    #error_handler{}.
```

### yawl_patterns:retry/2

Create a retry pattern.

```erlang
-spec retry(WorkFun :: function(), Policy :: term()) -> #retry{}.
```

### yawl_patterns:compensate/2

Create a compensation pattern.

```erlang
-spec compensate(ActivityFun :: function(), CompensationFun :: function()) ->
    #compensate{}.
```

### yawl_patterns:triggered_compensation/3

Create a triggered compensation pattern.

```erlang
-spec triggered_compensation(ActivityFun :: function(), CompensationFun :: function(),
                              TriggerFun :: function()) -> #triggered_compensation{}.
```

### yawl_patterns:consecutive_compensate/1

Create consecutive compensation pattern.

```erlang
-spec consecutive_compensate(Activities :: [{function(), function()}]) ->
    #consecutive_compensate{}.
```

---

## Human-in-the-Loop APIs

### yawl_approval:create_checkpoint/3

Create a new approval checkpoint.

```erlang
-spec create_checkpoint(PatternId :: binary(), StepName :: atom(), Options :: map()) ->
    {ok, CheckpointId :: binary()} | {error, term()}.
```

**Options**:
- `required_approver` - `human | simulated | auto` (default: `simulated`)
- `timeout` - milliseconds or `infinity` (default: `30000`)
- `approval_schema` - JSON schema for validation (default: basic schema)
- `context` - Additional context data (default: `#{}`)
- `metadata` - User metadata (default: `#{}`)

**Example**:
```erlang
{ok, CheckpointId} = yawl_approval:create_checkpoint(
    <<"order_workflow">>,
    approve_manager,
    #{
        required_approver => human,
        timeout => 60000,
        context => #{amount => 10000}
    }
),
```

### yawl_approval:request_approval/1

Request approval for a checkpoint.

```erlang
-spec request_approval(CheckpointId :: binary()) ->
    {ok, #approval_decision{}} | {error, term()}.
```

### yawl_approval:approve/3

Approve a checkpoint.

```erlang
-spec approve(CheckpointId :: binary(), Approver :: term(), Reason :: binary()) ->
    ok | {error, term()}.
```

**Example**:
```erlang
ok = yawl_approval:approve(CheckpointId, <<"john.doe">>, <<"Approved within budget">>),
```

### yawl_approval:deny/3

Deny a checkpoint.

```erlang
-spec deny(CheckpointId :: binary(), Approver :: term(), Reason :: binary()) ->
    ok | {error, term()}.
```

**Example**:
```erlang
ok = yawl_approval:deny(CheckpointId, <<"john.doe">>, <<"Exceeds department limit">>),
```

### yawl_approval:check_status/1

Check the status of an approval checkpoint.

```erlang
-spec check_status(CheckpointId :: binary()) ->
    {ok, pending | approved | denied | timeout | cancelled} | {error, term()}.
```

### yawl_approval:wait_for_approval/1

Block until a decision is made or timeout expires.

```erlang
-spec wait_for_approval(CheckpointId :: binary()) ->
    {ok, #approval_decision{}} | {error, timeout | not_found}.
```

### yawl_approval:simulate_approval/2

Simulate approval using Claude Code headless mode.

```erlang
-spec simulate_approval(CheckpointId :: binary(), PromptContext :: map()) ->
    {ok, #approval_decision{}} | {error, term()}.
```

### yawl_approval:list_pending/0

List all pending approval checkpoints.

```erlang
-spec list_pending() -> [CheckpointId :: binary()].
```

### yawl_approval:list_all/0

List all approval checkpoints with statuses.

```erlang
-spec list_all() -> [{CheckpointId :: binary(), Status :: approval_status()}].
```

### yawl_approval:cancel_checkpoint/1

Cancel a pending approval checkpoint.

```erlang
-spec cancel_checkpoint(CheckpointId :: binary()) -> ok | {error, term()}.
```

### yawl_approval:get_checkpoint_context/1

Get checkpoint context as JSON string.

```erlang
-spec get_checkpoint_context(CheckpointId :: binary()) -> binary().
```

---

## Telemetry Wrapper APIs

### yawl_otel_logger:start_link/0

Start the OpenTelemetry logger with default options.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

### yawl_otel_logger:start_link/1

Start the OpenTelemetry logger with options.

```erlang
-spec start_link(Options :: map()) -> {ok, pid()} | {error, term()}.
```

**Options**:
- `max_events` - Maximum events to keep (default: 10000)
- `retention_ms` - Event retention time (default: 24 hours)

### yawl_otel_logger:log_event/3

Log a telemetry event.

```erlang
-spec log_event(EventType :: binary() | atom(), Message :: binary(),
               Attributes :: map()) -> ok.
```

### yawl_otel_logger:log_event/4

Log a telemetry event with level.

```erlang
-spec log_event(EventType :: binary() | atom(), Message :: binary(),
               Attributes :: map(), Level :: debug | info | warning | error) -> ok.
```

**Example**:
```erlang
yawl_otel_logger:log_event(
    <<"task_started">>,
    <<"Processing payment">>,
    #{task_id => <<"payment_123">>, amount => 1000},
    info
),
```

### yawl_otel_logger:log_approval/4

Log an approval decision event.

```erlang
-spec log_approval(CheckpointId :: binary(), Approver :: binary(),
                   Approved :: boolean(), Attributes :: map()) -> ok.
```

### yawl_otel_logger:log_checkpoint/6

Log a checkpoint creation event.

```erlang
-spec log_checkpoint(CheckpointId :: binary(), PatternId :: binary(),
                     StepName :: atom(), RequiredApprover :: binary(),
                     Context :: map(), Attributes :: map()) -> ok.
```

### yawl_otel_logger:log_workflow_start/2

Log workflow start event.

```erlang
-spec log_workflow_start(CaseId :: binary(), PatternId :: binary()) -> ok.
```

### yawl_otel_logger:log_workflow_complete/2

Log workflow completion event.

```erlang
-spec log_workflow_complete(CaseId :: binary(), Status :: binary()) -> ok.
```

### yawl_otel_logger:log_workitem_start/3

Log workitem start event.

```erlang
-spec log_workitem_start(CaseId :: binary(), TaskId :: binary(),
                         TaskName :: binary()) -> ok.
```

### yawl_otel_logger:log_workitem_complete/3

Log workitem completion event.

```erlang
-spec log_workitem_complete(CaseId :: binary(), TaskId :: binary(),
                            Result :: term()) -> ok.
```

### yawl_otel_logger:get_events/0

Get all events.

```erlang
-spec get_events() -> [#otel_event{}].
```

### yawl_otel_logger:get_events/1

Get events by type.

```erlang
-spec get_events(EventType :: binary() | atom()) -> [#otel_event{}].
```

### yawl_otel_logger:get_events_by_trace/1

Get events by trace ID.

```erlang
-spec get_events_by_trace(TraceId :: binary()) -> [#otel_event{}].
```

### yawl_otel_logger:get_recent_events/2

Get recent events.

```erlang
-spec get_recent_events(Limit :: non_neg_integer(),
                        Level :: event_level() | all) -> [#otel_event{}].
```

### yawl_otel_logger:get_traces/0

Get all traces.

```erlang
-spec get_traces() -> [#otel_trace{}].
```

### yawl_otel_logger:clear_events/0

Clear all events.

```erlang
-spec clear_events() -> ok.
```

### yawl_otel_logger:get_stats/0

Get statistics.

```erlang
-spec get_stats() -> map().
```

---

## Configuration APIs

### yawl_resourcing:start_link/0

Start an anonymous resourcing service.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

### yawl_resourcing:start_link/1

Start a named resourcing service.

```erlang
-spec start_link(Name :: atom()) -> {ok, pid()} | {error, term()}.
```

### yawl_resourcing:register_participant/2

Register a new participant.

```erlang
-spec register_participant(Props :: proplists:proplist(), ParticipantId :: binary()) ->
    {ok, binary()} | {error, term()}.
```

**Props**:
- `name` - Participant name
- `roles` - List of roles
- `capabilities` - List of capabilities
- `is_user` - Boolean (default: false)
- `resource_type` - `human | machine | non_human | system`
- `status` - `available | busy | unavailable | offline`

**Example**:
```erlang
{ok, ParticipantId} = yawl_resourcing:register_participant(
    [
        {name, <<"John Doe">>},
        {roles, [<<"manager">>, <<"approver">>]},
        {capabilities, [<<"approve_expense">>]},
        {is_user, true},
        {resource_type, human}
    ],
    <<"user_123">>
),
```

### yawl_resourcing:unregister_participant/1

Unregister a participant.

```erlang
-spec unregister_participant(ParticipantId :: binary()) -> ok | {error, term()}.
```

### yawl_resourcing:allocate_resource/3

Allocate a resource to a task.

```erlang
-spec allocate_resource(TaskId :: binary(), RoleList :: [binary()],
                        Strategy :: eager | lazy) ->
    {ok, AllocationId :: binary()} | {error, term()}.
```

### yawl_resourcing:deallocate_resource/2

Deallocate a resource from a task.

```erlang
-spec deallocate_resource(TaskId :: binary(), AllocationId :: binary()) ->
    ok | {error, term()}.
```

### yawl_resourcing:get_available_resources/1

Get available resources matching criteria.

```erlang
-spec get_available_resources(RoleList :: [binary()]) -> [#participant{}].
```

### yawl_resourcing:get_all_participants/0

Get all registered participants.

```erlang
-spec get_all_participants() -> [#participant{}].
```

### yawl_resourcing:add_resource_to_task/3

Add a resource directly to a task.

```erlang
-spec add_resource_to_task(TaskId :: binary(), ParticipantId :: binary(),
                           Strategy :: eager | lazy) ->
    {ok, binary()} | {error, term()}.
```

### yawl_resourcing:remove_resource_from_task/2

Remove a resource from a task.

```erlang
-spec remove_resource_from_task(TaskId :: binary(), AllocationId :: binary()) -> ok.
```

### yawl_resourcing:check_resource_availability/2

Check if a resource is available.

```erlang
-spec check_resource_availability(ParticipantId :: binary(), RoleList :: [binary()]) ->
    boolean().
```

### yawl_resourcing:get_participant_info/1

Get detailed participant information.

```erlang
-spec get_participant_info(ParticipantId :: binary()) ->
    {ok, #participant{}} | {error, not_found}.
```

### yawl_resourcing:add_role/2

Add a role to a participant.

```erlang
-spec add_role(ParticipantId :: binary(), Role :: binary()) -> ok | {error, term()}.
```

### yawl_resourcing:remove_role/2

Remove a role from a participant.

```erlang
-spec remove_role(ParticipantId :: binary(), Role :: binary()) -> ok | {error, term()}.
```

### yawl_resourcing:add_capability/2

Add a capability to a participant.

```erlang
-spec add_capability(ParticipantId :: binary(), Capability :: binary()) ->
    ok | {error, term()}.
```

### yawl_resourcing:get_resources_by_role/1

Get resources by role.

```erlang
-spec get_resources_by_role(Role :: binary()) -> [#participant{}].
```

### yawl_resourcing:get_resources_by_capability/1

Get resources by capability.

```erlang
-spec get_resources_by_capability(Capability :: binary()) -> [#participant{}].
```

### yawl_resourcing:set_participant_status/2

Set participant status.

```erlang
-spec set_participant_status(ParticipantId :: binary(),
                             Status :: participant_status()) ->
    ok | {error, term()}.
```

### yawl_resourcing:get_participant_status/1

Get participant status.

```erlang
-spec get_participant_status(ParticipantId :: binary()) ->
    {ok, participant_status()} | {error, not_found}.
```

---

## Persistence Wrapper APIs

### yawl_persistence:init_schema/0

Initialize the Mnesia schema with persistent tables.

```erlang
-spec init_schema() -> ok | {error, term()}.
```

**Example**:
```erlang
ok = yawl_persistence:init_schema(),
```

### yawl_persistence:save_case/1

Save or update a workflow case.

```erlang
-spec save_case(Case :: map() | tuple()) ->
    {ok, CaseId :: binary()} | {error, term()}.
```

**Example**:
```erlang
CaseMap = #{
    case_id => <<"case_123">>,
    workflow_id => <<"order_workflow">>,
    spec => #{},
    status => running,
    data => #{amount => 1000}
},
{ok, CaseId} = yawl_persistence:save_case(CaseMap),
```

### yawl_persistence:load_case/1

Load a case by ID.

```erlang
-spec load_case(CaseId :: binary()) -> {ok, map()} | {error, not_found | term()}.
```

### yawl_persistence:delete_case/1

Delete a case and all associated work items.

```erlang
-spec delete_case(CaseId :: binary()) -> ok | {error, term()}.
```

### yawl_persistence:save_workitem/1

Save or update a work item.

```erlang
-spec save_workitem(Workitem :: map() | tuple()) ->
    {ok, WorkitemId :: binary()} | {error, term()}.
```

### yawl_persistence:load_workitems/1

Load all work items for a case.

```erlang
-spec load_workitems(CaseId :: binary()) -> {ok, [map()]} | {error, term()}.
```

### yawl_persistence:list_active_cases/0

List all active cases (running or suspended).

```erlang
-spec list_active_cases() -> {ok, [map()]}.
```

### yawl_persistence:cleanup_expired_cases/0

Delete completed cases older than 24 hours.

```erlang
-spec cleanup_expired_cases() -> {ok, Count :: non_neg_integer()} | {error, term()}.
```

### yawl_persistence:get_case_count/0

Get total count of cases.

```erlang
-spec get_case_count() -> {ok, non_neg_integer()} | {error, term()}.
```

---

## XES Wrapper APIs

### yawl_xes:start_link/0

Start the XES logger with default configuration.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

### yawl_xes:start_link/1

Start the XES logger with a given name.

```erlang
-spec start_link(Name :: atom()) -> {ok, pid()} | {error, term()}.
```

### yawl_xes:stop/0

Stop the XES logger.

```erlang
-spec stop() -> ok.
```

### yawl_xes:new_log/0

Create a new XES log.

```erlang
-spec new_log() -> {ok, LogId :: binary()}.
```

### yawl_xes:new_log/1

Create a new XES log with metadata.

```erlang
-spec new_log(Metadata :: map()) -> {ok, LogId :: binary()}.
```

### yawl_xes:log_event/4

Log a generic event.

```erlang
-spec log_event(LogId :: binary(), ConceptName :: binary(),
                LifecycleTransition :: binary(), Data :: map()) -> ok.
```

### yawl_xes:log_event/5

Log a generic event with case ID.

```erlang
-spec log_event(LogId :: binary(), ConceptName :: binary(),
                LifecycleTransition :: binary(), Data :: map(),
                CaseId :: binary() | undefined) -> ok.
```

### yawl_xes:log_pattern_start/3

Log pattern execution start.

```erlang
-spec log_pattern_start(LogId :: binary(), PatternType :: binary(),
                        PatternId :: binary()) -> ok.
```

### yawl_xes:log_pattern_complete/4

Log pattern execution completion.

```erlang
-spec log_pattern_complete(LogId :: binary(), PatternType :: binary(),
                           PatternId :: binary(), Result :: term()) -> ok.
```

### yawl_xes:log_token_move/4

Log token move in Petri net.

```erlang
-spec log_token_move(LogId :: binary(), Place :: binary(),
                     From :: binary(), To :: binary()) -> ok.
```

### yawl_xes:log_transition_fire/4

Log transition firing.

```erlang
-spec log_transition_fire(LogId :: binary(), Transition :: binary(),
                          Inputs :: list(), Outputs :: list()) -> ok.
```

### yawl_xes:log_case_start/2

Log case start.

```erlang
-spec log_case_start(LogId :: binary(), CaseId :: binary()) -> ok.
```

### yawl_xes:log_case_complete/3

Log case completion.

```erlang
-spec log_case_complete(LogId :: binary(), CaseId :: binary(),
                        Stats :: map()) -> ok.
```

### yawl_xes:log_workitem_start/3

Log workitem start.

```erlang
-spec log_workitem_start(LogId :: binary(), WorkitemId :: binary(),
                         TaskId :: binary()) -> ok.
```

### yawl_xes:log_workitem_complete/4

Log workitem completion.

```erlang
-spec log_workitem_complete(LogId :: binary(), WorkitemId :: binary(),
                            TaskId :: binary(), Result :: term()) -> ok.
```

### yawl_xes:export_xes/1

Export log to XES XML format.

```erlang
-spec export_xes(LogId :: binary()) -> {ok, iodata()}.
```

### yawl_xes:export_xes/2

Export log to XES XML format with output directory.

```erlang
-spec export_xes(LogId :: binary(), OutputDir :: string()) -> {ok, iodata()}.
```

### yawl_xes:get_log/1

Get a log by ID.

```erlang
-spec get_log(LogId :: binary()) -> {ok, #xes_log{}} | {error, not_found}.
```

### yawl_xes:list_logs/0

List all logs.

```erlang
-spec list_logs() -> [{LogId :: binary(), #xes_log{}}].
```

---

## Integration Wrapper APIs

### Workflow Execution Example

```erlang
% Start required services
{ok, _} = yawl_otel_logger:start_link(#{
    max_events => 10000,
    retention_ms => 24 * 60 * 60 * 1000
}),
{ok, _} = yawl_xes:start_link(),
{ok, _} = yawl_approval:start_link(),
{ok, _} = yawl_resourcing:start_link(),

% Initialize persistence
ok = yawl_persistence:init_schema(),

% Create a new workflow
Workflow = cre_yawl:new_workflow(<<"order_processing">>),

% Add tasks
Workflow1 = cre_yawl:add_task(Workflow, <<"validate">>, [
    {name, <<"Validate Order">>},
    {type, atomic}
]),
Workflow2 = cre_yawl:add_task(Workflow1, <<"approve">>, [
    {name, <<"Manager Approval">>},
    {type, atomic}
]),
Workflow3 = cre_yawl:add_task(Workflow2, <<"process">>, [
    {name, <<"Process Payment">>},
    {type, atomic}
]),

% Connect tasks
Workflow4 = cre_yawl:connect(Workflow3, <<"validate">>, <<"approve">>),
Workflow5 = cre_yawl:connect(Workflow4, <<"approve">>, <<"process">>),

% Set boundaries
FinalWorkflow = cre_yawl:set_workflow_boundaries(
    Workflow5,
    <<"validate">>,
    [<<"process">>]
),

% Validate
case cre_yawl:validate(FinalWorkflow) of
    ok ->
        % Create approval checkpoint
        {ok, CheckpointId} = yawl_approval:create_checkpoint(
            <<"order_processing">>,
            approve_manager,
            #{
                required_approver => human,
                timeout => 60000,
                context => #{workflow => <<"order_processing">>}
            }
        ),
        {ok, _} = yawl_approval:request_approval(CheckpointId),
        {ok, Decision} = yawl_approval:wait_for_approval(CheckpointId),
        io:format("Approval: ~p~n", [Decision]);
    {error, Errors} ->
        io:format("Validation errors: ~p~n", [Errors])
end,
```

### Data Flow Example

```erlang
% Create parameter passing pattern
ParamPass = cre_yawl:param_pass(
    <<"task_a">>,
    <<"task_b">>,
    fun(X) -> X * 2 end
),

% Execute it
{ok, Result} = cre_yawl:execute_param_pass(
    ParamPass,
    #{<<"task_a">> => 10},
    #{safe_mode => true}
),

% Create data distribution
Distribute = cre_yawl:data_distribute(
    <<"source">>,
    [<<"dest1">>, <<"dest2">>, <<"dest3">>],
    broadcast,
    #{}
),

% Execute distribution
{ok, DistResult} = cre_yawl:execute_data_distribute(
    Distribute,
    #{<<"source">> => <<"hello world">>},
    #{}
),
```

### Persistence Example

```erlang
% Save a case
Case = #{
    case_id => <<"order_123">>,
    workflow_id => <<"order_processing">>,
    spec => #{},
    status => running,
    data => #{customer => <<"John Doe">>, amount => 1000},
    created_at => erlang:system_time(millisecond)
},
{ok, CaseId} = yawl_persistence:save_case(Case),

% Save work items
Workitem1 = #{
    workitem_id => <<"wi_1">>,
    case_id => CaseId,
    task_id => <<"validate">>,
    status => started,
    data => #{},
    enabled_at => erlang:system_time(millisecond)
},
{ok, _} = yawl_persistence:save_workitem(Workitem1),

% Load case later
{ok, LoadedCase} = yawl_persistence:load_case(CaseId),
io:format("Loaded case: ~p~n", [LoadedCase]),

% Get work items
{ok, Workitems} = yawl_persistence:load_workitems(CaseId),
```

### XES Logging Example

```erlang
% Create new XES log
{ok, LogId} = yawl_xes:new_log(#{process => <<"order_workflow">>}),

% Log case start
ok = yawl_xes:log_case_start(LogId, <<"case_001">>),

% Log pattern execution
ok = yawl_xes:log_pattern_start(LogId, <<"sequence">>, <<"seq_1">>),
ok = yawl_xes:log_pattern_complete(LogId, <<"sequence">>, <<"seq_1">>, ok),

% Log workitem
ok = yawl_xes:log_workitem_start(LogId, <<"wi_1">>, <<"validate">>),
ok = yawl_xes:log_workitem_complete(LogId, <<"wi_1">>, <<"validate">>, ok),

% Export to XES XML
{ok, XESContent} = yawl_xes:export_xes(LogId, "/tmp/xes_logs"),
file:write_file("/tmp/order_workflow.xes", XESContent),
```

---

## Error Types

### Common Error Reasons

| Error Type | Description | Handling |
|------------|-------------|----------|
| `not_found` | Resource doesn't exist | Check IDs and ensure creation |
| `already_decided` | Approval already made | Cannot approve/deny twice |
| `no_resources_available` | No matching resources | Check participant registration |
| `timeout` | Operation timed out | Increase timeout or check conditions |
| `invalid_workflow` | Workflow structure is invalid | Check workflow definition |
| `start_task_not_found` | Start task doesn't exist | Verify task IDs |
| `end_task_not_found` | End task doesn't exist | Verify task IDs |
| `no_recipients` | No recipients for distribution | Add recipient tasks |
| `participant_not_found` | Participant doesn't exist | Register participant first |
| `participant_not_available` | Participant is busy | Check participant status |

### Type Definitions

```erlang
% Workflow records
-record(workflow, {
    id :: binary(),
    name :: binary(),
    tasks :: #{binary() => #task{}},
    conditions :: #{binary() => #yawl_condition{}},
    connections :: [#connection{}],
    start_task_id :: binary() | undefined,
    end_task_ids :: [binary()]
}).

-record(task, {
    id :: binary(),
    name :: binary(),
    type :: atomic | composite | subworkflow | multi_instance,
    split_type :: and_split | or_split | xor_split | undefined,
    join_type :: and_join | or_join | xor_join | undefined,
    metadata :: map()
}).

-record(yawl_condition, {
    id :: binary(),
    expression :: term(),
    description :: binary() | undefined
}).

-record(connection, {
    from_id :: binary(),
    to_id :: binary(),
    condition_id :: binary() | undefined
}).

% Approval records (defined in cre_yawl.hrl)
-record(approval_checkpoint, {
    checkpoint_id :: binary(),
    pattern_id :: binary(),
    step_name :: atom(),
    context :: map(),
    required_approver :: human | simulated | auto,
    timeout :: integer() | infinity,
    approval_schema :: map(),
    created_at :: integer(),
    expires_at :: integer() | undefined,
    metadata :: map()
}).

-record(approval_decision, {
    checkpoint_id :: binary(),
    approved :: boolean(),
    decision_maker :: term(),
    reason :: binary(),
    metadata :: map(),
    decided_at :: integer()
}).

% OTEL records (defined in yawl_otel_logger.hrl)
-record(otel_event, {
    id :: binary(),
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    timestamp :: integer(),
    event_type :: binary() | atom(),
    level :: debug | info | warning | error,
    user_id :: term() | undefined,
    case_id :: binary() | undefined,
    task_id :: binary() | undefined,
    pattern_id :: binary() | undefined,
    message :: binary(),
    attributes :: map()
}).

-record(otel_trace, {
    trace_id :: binary(),
    case_id :: binary(),
    pattern_id :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status :: atom(),
    span_count :: non_neg_integer()
}).

% Persistence records
-record(persistent_case, {
    case_id :: binary(),
    workflow_id :: binary(),
    spec :: term(),
    status :: running | suspended | completed | cancelled | failed,
    data :: map(),
    created_at :: integer(),
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined
}).

-record(persistent_workitem, {
    workitem_id :: binary(),
    case_id :: binary(),
    task_id :: binary(),
    status :: enabled | started | completed | failed | cancelled,
    data :: map(),
    enabled_at :: integer() | undefined,
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined
}).

% Resourcing records
-record(participant, {
    id :: binary(),
    name :: binary(),
    roles :: [binary()],
    capabilities :: [binary()],
    is_user :: boolean(),
    resource_type :: human | machine | non_human | system,
    status :: available | busy | unavailable | offline,
    metadata :: map()
}).

% XES records
-record(xes_log, {
    log_id :: binary(),
    trace_id :: binary(),
    started_at :: integer(),
    events :: list(),
    metadata :: map()
}).

-record(xes_event, {
    event_id :: binary(),
    timestamp :: integer(),
    case_id :: binary() | undefined,
    concept :: map(),
    lifecycle :: map(),
    data :: map()
}).
```

---

This API reference covers all major functions available in CRE. For additional details and examples, refer to:
- [QUICK_START.md](./QUICK_START.md) - Getting started guide
- [YAWL_PATTERNS_REFERENCE.md](./YAWL_PATTERNS_REFERENCE.md) - Pattern reference
- [ARCHITECTURE.md](./ARCHITECTURE.md) - System architecture
- [YAWL_TELEMETRY_GUIDE.md](./YAWL_TELEMETRY_GUIDE.md) - Telemetry details
