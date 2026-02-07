# API Reference

Complete API reference for the CRE YAWL (Yet Another Workflow Language) workflow engine with gen_pnet-based patterns.

## Table of Contents

1. [Workflow Construction API](#workflow-construction-api)
2. [Workflow Validation API](#workflow-validation-api)
3. [Pattern Constructors](#pattern-constructors)
4. [Pattern Execution API](#pattern-execution-api)
5. [Data Flow API](#data-flow-api)
6. [Resource Management API](#resource-management-api)
7. [Human-in-the-Loop API](#human-in-the-loop-api)
8. [Telemetry API](#telemetry-api)
9. [Persistence API](#persistence-api)
10. [XES Logging API](#xes-logging-api)
11. [gen_pnet Callback API](#gen_pnet-callback-api)

---

## Workflow Construction API

### cre_yawl:new_workflow/0

Creates a new workflow with auto-generated ID.

```erlang
-spec new_workflow() -> #workflow{}.
```

**Returns**
- `#workflow{}` - New workflow record with generated ID

**Example**
```erlang
Workflow = cre_yawl:new_workflow().
```

### cre_yawl:new_workflow/1

Creates a new workflow with specified ID.

```erlang
-spec new_workflow(Id :: binary()) -> #workflow{}.
```

**Parameters**
- `Id` - Unique workflow identifier

**Returns**
- `#workflow{}` - New workflow record

**Example**
```erlang
Workflow = cre_yawl:new_workflow(<<"order_processing">>).
```

### cre_yawl:add_task/3

Adds a task to the workflow.

```erlang
-spec add_task(Workflow :: #workflow{}, TaskId :: binary(),
               Task :: #task{} | [{atom(), term()}]) -> #workflow{}.
```

**Parameters**
- `Workflow` - Existing workflow record
- `TaskId` - Unique task identifier
- `Task` - Task record or proplist with properties

**Task Properties**
- `name` - Binary display name
- `type` - `atomic | composite | subworkflow | multi_instance`
- `split_type` - `and_split | or_split | xor_split`
- `join_type` - `and_join | or_join | xor_join`
- `metadata` - Map of additional metadata

**Returns**
- `#workflow{}` - Updated workflow

**Example**
```erlang
W1 = cre_yawl:add_task(Workflow, <<"validate">>, [
    {name, <<"Validate Order">>},
    {type, atomic},
    {metadata, #{priority => high}}
]).
```

### cre_yawl:add_condition/3

Adds a condition to the workflow.

```erlang
-spec add_condition(Workflow :: #workflow{}, ConditionId :: binary(),
                    Condition :: #yawl_condition{} | condition()) -> #workflow{}.
```

**Parameters**
- `Workflow` - Existing workflow record
- `ConditionId` - Unique condition identifier
- `Condition` - Condition record or expression

**Condition Types**
- `binary()` - Boolean variable name
- `{atom(), term()}` - Tuple expression
- `fun(() -> boolean())` - Evaluation function

**Returns**
- `#workflow{}` - Updated workflow

### cre_yawl:connect/3

Connects two tasks with a directed edge.

```erlang
-spec connect(Workflow :: #workflow{}, FromId :: binary(),
              ToId :: binary()) -> #workflow{}.
```

**Parameters**
- `Workflow` - Existing workflow record
- `FromId` - Source task ID
- `ToId` - Target task ID

**Returns**
- `#workflow{}` - Updated workflow

### cre_yawl:set_split_type/3

Sets the gateway split type for a task.

```erlang
-spec set_split_type(Workflow :: #workflow{}, TaskId :: binary(),
                     SplitType :: split_type()) -> #workflow{}.

-type split_type() :: and_split | or_split | xor_split.
```

**Split Types**
- `and_split` - Parallel split (all branches)
- `or_split` - Multi-choice (some branches)
- `xor_split` - Exclusive choice (one branch)

### cre_yawl:set_join_type/3

Sets the gateway join type for a task.

```erlang
-spec set_join_type(Workflow :: #workflow{}, TaskId :: binary(),
                    JoinType :: join_type()) -> #workflow{}.

-type join_type() :: and_join | or_join | xor_join.
```

**Join Types**
- `and_join` - Synchronization (all branches)
- `or_join` - Multi-merge (some branches)
- `xor_join` - Simple merge (one branch)

### cre_yawl:set_workflow_boundaries/3

Sets the start and end task boundaries.

```erlang
-spec set_workflow_boundaries(Workflow :: #workflow{},
                              StartTaskId :: binary(),
                              EndTaskIds :: [binary()]) ->
          #workflow{} | {error, term()}.
```

**Returns**
- `#workflow{}` - Workflow with boundaries set
- `{error, start_task_not_found}` - Start task missing
- `{error, {end_task_not_found, TaskId}}` - End task missing

---

## Workflow Validation API

### cre_yawl:validate/1

Validates workflow structure.

```erlang
-spec validate(Workflow :: #workflow{}) -> ok | {error, [binary()]}.
```

**Validation Checks**
- All referenced tasks exist
- Start and end tasks are valid
- No self-loops
- No duplicate connections
- No cycles
- Split/join consistency

**Returns**
- `ok` - Valid workflow
- `{error, [binary()]}` - List of error messages

### cre_yawl:get_errors/1

Returns validation errors without terminating on first error.

```erlang
-spec get_errors(Workflow :: #workflow{}) -> [binary()].
```

---

## Workflow Accessor API

### cre_yawl:get_workflow_id/1

Gets the workflow ID.

```erlang
-spec get_workflow_id(#workflow{}) -> {ok, binary()}.
```

### cre_yawl:get_workflow_name/1

Gets the workflow name.

```erlang
-spec get_workflow_name(#workflow{}) -> {ok, binary()}.
```

### cre_yawl:get_tasks/1

Gets all tasks from the workflow.

```erlang
-spec get_tasks(#workflow{}) -> {ok, #{binary() => #task{}}}.
```

### cre_yawl:get_connections/1

Gets all connections from the workflow.

```erlang
-spec get_connections(#workflow{}) -> {ok, [#connection{}]}.
```

### cre_yawl:get_conditions/1

Gets all conditions from the workflow.

```erlang
-spec get_conditions(#workflow{}) -> {ok, #{binary() => #yawl_condition{}}}.
```

---

## Pattern Constructors

### Basic Control Flow (WCP-01 to WCP-06)

#### cre_yawl:sequence/0

```erlang
-spec sequence() -> #sequence{}.
```

Creates a sequence pattern for linear task execution.

#### cre_yawl:parallel_split/0

```erlang
-spec parallel_split() -> #parallel_split{}.
```

Creates a parallel split pattern for concurrent execution.

#### cre_yawl:synchronization/0

```erlang
-spec synchronization() -> #synchronization{}.
```

Creates a synchronization pattern for joining parallel branches.

#### cre_yawl:exclusive_choice/0

```erlang
-spec exclusive_choice() -> #exclusive_choice{}.
```

Creates an exclusive choice pattern (XOR split).

#### cre_yawl:simple_merge/0

```erlang
-spec simple_merge() -> #simple_merge{}.
```

Creates a simple merge pattern (XOR join).

#### cre_yawl:multi_choice/0

```erlang
-spec multi_choice() -> #multi_choice{}.
```

Creates a multi-choice pattern (OR split).

### Advanced Synchronization (WCP-07 to WCP-10)

#### cre_yawl:synchronizing_merge/0

```erlang
-spec synchronizing_merge() -> #synchronizing_merge{}.
```

Creates a synchronizing merge pattern.

#### cre_yawl:multi_merge/0

```erlang
-spec multi_merge() -> #multi_merge{}.
```

Creates a multi-merge pattern.

#### cre_yawl:discriminator/0

```erlang
-spec discriminator() -> #discriminator{}.
```

Creates a discriminator pattern (first completion wins).

#### cre_yawl:arbitration/0

```erlang
-spec arbitration() -> #arbitration{}.
```

Creates an N-of-M arbitration pattern.

---

## Pattern Execution API

### cre_yawl:execute_synchronizing_merge/3

Executes a synchronizing merge pattern (WCP-07).

```erlang
-spec execute_synchronizing_merge(#synchronizing_merge{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
```

**Parameters**
- `Pattern` - Synchronizing merge pattern record
- `Input` - Input data map
- `Options` - Execution options

**Options**
- `timeout` - Timeout in milliseconds (default: 5000)

**Returns**
- `{ok, Result}` - Execution result with merge info
- `{error, Reason}` - Execution failure

### cre_yawl:execute_multi_merge/3

Executes a multi-merge pattern (WCP-08).

```erlang
-spec execute_multi_merge(#multi_merge{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
```

### cre_yawl:execute_discriminator/3

Executes a discriminator pattern (WCP-09).

```erlang
-spec execute_discriminator(#discriminator{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
```

### cre_yawl:execute_arbitration/3

Executes an arbitration pattern (WCP-10).

```erlang
-spec execute_arbitration(#arbitration{}, term(), map()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
```

**Parameters**
- `Pattern` - Must have `required_count` field set (N of M)

---

## Data Flow API

### cre_yawl:param_pass/2,3

Creates a parameter passing pattern (WDP-1).

```erlang
-spec param_pass(SourceId :: binary(), TargetId :: binary()) -> #param_pass{}.
-spec param_pass(SourceId :: binary(), TargetId :: binary(),
                 function() | {atom(), term()}) -> #param_pass{}.
```

**Parameters**
- `SourceId` - Source task ID
- `TargetId` - Target task ID
- `TransformFn` - Optional transformation function

### cre_yawl:data_transform/2,3

Creates a data transformation pattern (WDP-2).

```erlang
-spec data_transform(InputId :: binary(), OutputId :: binary()) -> #data_transform{}.
-spec data_transform(InputId :: binary(), OutputId :: binary(), function()) -> #data_transform{}.
```

### cre_yawl:data_distribute/1,4

Creates a data distribution pattern (WDP-3).

```erlang
-spec data_distribute(RecipientIds :: [binary()]) -> #data_distribute{}.
-spec data_distribute(SourceId :: binary(), RecipientIds :: [binary()],
                      broadcast | round_robin | partitioned, term()) -> #data_distribute{}.
```

**Distribution Types**
- `broadcast` - Send to all recipients
- `round_robin` - Distribute in round-robin fashion
- `partitioned` - Partition by key

### cre_yawl:data_accumulate/1,4

Creates a data accumulation pattern (WDP-4).

```erlang
-spec data_accumulate(SourceIds :: [binary()]) -> #data_accumulate{}.
-spec data_accumulate(SourceIds :: [binary()], TargetId :: binary(),
                      function(), term()) -> #data_accumulate{}.
```

**Built-in Aggregators**
- `sum` - Numeric sum
- `count` - Element count
- `list` - Collect as list
- `min` - Minimum value
- `max` - Maximum value
- `first` - First element
- `last` - Last element

### cre_yawl:data_visibility/2,3

Creates a data visibility pattern (WDP-5).

```erlang
-spec data_visibility(DataId :: binary(), local | branch | global) -> #data_visibility{}.
-spec data_visibility(DataId :: binary(), local | branch | global, [binary()]) -> #data_visibility{}.
```

**Visibility Scopes**
- `local` - Only the creating task
- `branch` - Tasks in the same branch
- `global` - All tasks in workflow

---

## Resource Management API

### cre_yawl:resource_create/1

Creates a resource creation pattern (WRP-1).

```erlang
-spec resource_create(ResourceType :: atom()) -> #resource_create{}.
```

**Built-in Resource Types**
- `database_connection`
- `worker_pool`
- `cache`
- `message_queue`
- `file_handle`

### cre_yawl:role_allocate/2

Creates a role allocation pattern (WRP-2).

```erlang
-spec role_allocate(RoleId :: atom(), Capability :: term()) -> #role_allocate{}.
```

**Allocation Strategies**
- `first_fit` - First matching resource
- `best_fit` - Best matching resource
- `random` - Random selection

### cre_yawl:resource_start/1

Creates a resource start pattern (WRP-3).

```erlang
-spec resource_start(ResourceId :: binary()) -> #resource_start{}.
```

### cre_yawl:role_distribute/2

Creates a role distribution pattern (WRP-4).

```erlang
-spec role_distribute(WorkItemIds :: [binary()], RoleAssignments :: map()) -> #role_distribute{}.
```

**Distribution Policies**
- `round_robin` - Cyclic distribution
- `least_loaded` - Balance by workload
- `affinity_based` - Hash-based assignment

### cre_yawl:capability_allocate/2

Creates a capability allocation pattern (WRP-5).

```erlang
-spec capability_allocate(Capabilities :: map(), Registry :: [term()]) -> #capability_allocate{}.
```

**Matching Strategies**
- `exact_match` - Exact capability match required
- `minimum_met` - Minimum requirements met
- `best_effort` - Best available match

---

## Human-in-the-Loop API

### yawl_approval:create_checkpoint/3

Creates an approval checkpoint.

```erlang
-spec create_checkpoint(PatternId :: binary(), StepName :: atom(), Options :: map()) ->
    {ok, CheckpointId :: binary()} | {error, term()}.
```

**Options**
- `required_approver` - `human | simulated | auto`
- `timeout` - Milliseconds or `infinity`
- `approval_schema` - JSON schema for validation
- `context` - Additional context data
- `metadata` - User metadata

### yawl_approval:request_approval/1

Requests approval for a checkpoint.

```erlang
-spec request_approval(CheckpointId :: binary()) ->
    {ok, #approval_decision{}} | {error, term()}.
```

### yawl_approval:approve/3

Approves a checkpoint.

```erlang
-spec approve(CheckpointId :: binary(), Approver :: term(), Reason :: binary()) ->
    ok | {error, term()}.
```

### yawl_approval:deny/3

Denies a checkpoint.

```erlang
-spec deny(CheckpointId :: binary(), Approver :: term(), Reason :: binary()) ->
    ok | {error, term()}.
```

### yawl_approval:check_status/1

Checks approval status.

```erlang
-spec check_status(CheckpointId :: binary()) ->
    {ok, approval_status()} | {error, term()}.

-type approval_status() :: pending | approved | denied | timeout | cancelled.
```

### yawl_approval:wait_for_approval/1

Blocks until decision or timeout.

```erlang
-spec wait_for_approval(CheckpointId :: binary()) ->
    {ok, #approval_decision{}} | {error, timeout | not_found}.
```

### yawl_approval:simulate_approval/2

Simulates approval using Claude Code headless mode.

```erlang
-spec simulate_approval(CheckpointId :: binary(), PromptContext :: map()) ->
    {ok, #approval_decision{}} | {error, term()}.
```

### yawl_approval:list_pending/0

Lists all pending checkpoints.

```erlang
-spec list_pending() -> [CheckpointId :: binary()].
```

### yawl_approval:list_all/0

Lists all checkpoints with statuses.

```erlang
-spec list_all() -> [{CheckpointId :: binary(), Status :: approval_status()}].
```

### yawl_approval:cancel_checkpoint/1

Cancels a pending checkpoint.

```erlang
-spec cancel_checkpoint(CheckpointId :: binary()) -> ok | {error, term()}.
```

### yawl_approval:get_checkpoint_context/1

Gets checkpoint context as JSON string.

```erlang
-spec get_checkpoint_context(CheckpointId :: binary()) -> binary().
```

---

## Telemetry API

### yawl_otel_logger:start_link/0,1

Starts the OpenTelemetry logger.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec start_link(Options :: map()) -> {ok, pid()} | {error, term()}.
```

**Options**
- `max_events` - Maximum events to keep (default: 10000)
- `retention_ms` - Event retention time (default: 86400000)

### yawl_otel_logger:log_event/3,4

Logs a telemetry event.

```erlang
-spec log_event(EventType :: binary() | atom(), Message :: binary(),
               Attributes :: map()) -> ok.
-spec log_event(EventType :: binary() | atom(), Message :: binary(),
               Attributes :: map(), Level :: debug | info | warning | error) -> ok.
```

### yawl_otel_logger:log_approval/4

Logs an approval decision.

```erlang
-spec log_approval(CheckpointId :: binary(), Approver :: binary(),
                  Approved :: boolean(), Attributes :: map()) -> ok.
```

### yawl_otel_logger:log_checkpoint/6

Logs checkpoint creation.

```erlang
-spec log_checkpoint(CheckpointId :: binary(), PatternId :: binary(),
                    StepName :: atom(), RequiredApprover :: binary(),
                    Context :: map(), Attributes :: map()) -> ok.
```

### yawl_otel_logger:log_workflow_start/2

Logs workflow start.

```erlang
-spec log_workflow_start(CaseId :: binary(), PatternId :: binary()) -> ok.
```

### yawl_otel_logger:log_workflow_complete/2

Logs workflow completion.

```erlang
-spec log_workflow_complete(CaseId :: binary(), Status :: binary()) -> ok.
```

### yawl_otel_logger:get_events/0,1

Gets all events or events by type.

```erlang
-spec get_events() -> [#otel_event{}].
-spec get_events(EventType :: binary() | atom()) -> [#otel_event{}].
```

### yawl_otel_logger:get_traces/0

Gets all traces.

```erlang
-spec get_traces() -> [#otel_trace{}].
```

### yawl_otel_logger:clear_events/0

Clears all events.

```erlang
-spec clear_events() -> ok.
```

### yawl_otel_logger:get_stats/0

Gets telemetry statistics.

```erlang
-spec get_stats() -> map().
```

---

## Persistence API

### yawl_persistence:init_schema/0

Initializes Mnesia schema.

```erlang
-spec init_schema() -> ok | {error, term()}.
```

### yawl_persistence:save_case/1

Saves or updates a workflow case.

```erlang
-spec save_case(Case :: map() | tuple()) ->
    {ok, CaseId :: binary()} | {error, term()}.
```

**Case Map Fields**
- `case_id` - Unique case identifier
- `workflow_id` - Workflow specification ID
- `spec` - Workflow specification
- `status` - `running | suspended | completed | cancelled | failed`
- `data` - Case data map
- `created_at` - Creation timestamp

### yawl_persistence:load_case/1

Loads a case by ID.

```erlang
-spec load_case(CaseId :: binary()) -> {ok, map()} | {error, not_found | term()}.
```

### yawl_persistence:delete_case/1

Deletes a case and associated work items.

```erlang
-spec delete_case(CaseId :: binary()) -> ok | {error, term()}.
```

### yawl_persistence:save_workitem/1

Saves or updates a work item.

```erlang
-spec save_workitem(Workitem :: map() | tuple()) ->
    {ok, WorkitemId :: binary()} | {error, term()}.
```

**Workitem Map Fields**
- `workitem_id` - Unique workitem identifier
- `case_id` - Parent case ID
- `task_id` - Task identifier
- `status` - `enabled | started | completed | failed | cancelled`
- `data` - Workitem data map
- `enabled_at` - Enable timestamp
- `started_at` - Start timestamp
- `completed_at` - Completion timestamp

### yawl_persistence:load_workitems/1

Loads all work items for a case.

```erlang
-spec load_workitems(CaseId :: binary()) -> {ok, [map()]} | {error, term()}.
```

### yawl_persistence:list_active_cases/0

Lists all active cases.

```erlang
-spec list_active_cases() -> {ok, [map()]}.
```

### yawl_persistence:cleanup_expired_cases/0

Deletes completed cases older than 24 hours.

```erlang
-spec cleanup_expired_cases() -> {ok, Count :: non_neg_integer()} | {error, term()}.
```

### yawl_persistence:get_case_count/0

Gets total case count.

```erlang
-spec get_case_count() -> {ok, non_neg_integer()} | {error, term()}.
```

---

## XES Logging API

### yawl_xes:start_link/0,1

Starts the XES logger.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec start_link(Name :: atom()) -> {ok, pid()} | {error, term()}.
```

### yawl_xes:stop/0

Stops the XES logger.

```erlang
-spec stop() -> ok.
```

### yawl_xes:new_log/0,1

Creates a new XES log.

```erlang
-spec new_log() -> {ok, LogId :: binary()}.
-spec new_log(Metadata :: map()) -> {ok, LogId :: binary()}.
```

### yawl_xes:log_event/4,5

Logs a generic event.

```erlang
-spec log_event(LogId :: binary(), ConceptName :: binary(),
                LifecycleTransition :: binary(), Data :: map()) -> ok.
-spec log_event(LogId :: binary(), ConceptName :: binary(),
                LifecycleTransition :: binary(), Data :: map(),
                CaseId :: binary() | undefined) -> ok.
```

### yawl_xes:log_pattern_start/3

Logs pattern execution start.

```erlang
-spec log_pattern_start(LogId :: binary(), PatternType :: binary(),
                        PatternId :: binary()) -> ok.
```

### yawl_xes:log_pattern_complete/4

Logs pattern completion.

```erlang
-spec log_pattern_complete(LogId :: binary(), PatternType :: binary(),
                           PatternId :: binary(), Result :: term()) -> ok.
```

### yawl_xes:log_token_move/4

Logs token move in Petri net.

```erlang
-spec log_token_move(LogId :: binary(), Place :: binary(),
                     From :: binary(), To :: binary()) -> ok.
```

### yawl_xes:log_transition_fire/4

Logs transition firing.

```erlang
-spec log_transition_fire(LogId :: binary(), Transition :: binary(),
                          Inputs :: list(), Outputs :: list()) -> ok.
```

### yawl_xes:log_case_start/2

Logs case start.

```erlang
-spec log_case_start(LogId :: binary(), CaseId :: binary()) -> ok.
```

### yawl_xes:log_case_complete/3

Logs case completion.

```erlang
-spec log_case_complete(LogId :: binary(), CaseId :: binary(),
                        Stats :: map()) -> ok.
```

### yawl_xes:log_workitem_start/3

Logs workitem start.

```erlang
-spec log_workitem_start(LogId :: binary(), WorkitemId :: binary(),
                         TaskId :: binary()) -> ok.
```

### yawl_xes:log_workitem_complete/4

Logs workitem completion.

```erlang
-spec log_workitem_complete(LogId :: binary(), WorkitemId :: binary(),
                            TaskId :: binary(), Result :: term()) -> ok.
```

### yawl_xes:export_xes/1,2

Exports log to XES XML format.

```erlang
-spec export_xes(LogId :: binary()) -> {ok, iodata()}.
-spec export_xes(LogId :: binary(), OutputDir :: string()) -> {ok, iodata()}.
```

### yawl_xes:get_log/1

Gets a log by ID.

```erlang
-spec get_log(LogId :: binary()) -> {ok, #xes_log{}} | {error, not_found}.
```

### yawl_xes:list_logs/0

Lists all logs.

```erlang
-spec list_logs() -> [{LogId :: binary(), #xes_log{}}].
```

---

## gen_pnet Callback API

### place_lst/0

Returns list of places in the Petri net.

```erlang
-callback place_lst() -> [atom()].
```

### trsn_lst/0

Returns list of transitions in the Petri net.

```erlang
-callback trsn_lst() -> [atom()].
```

### init_marking/2

Returns initial marking for a place.

```erlang
-callback init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
```

### preset/1

Returns input places for a transition.

```erlang
-callback preset(Trsn :: atom()) -> [atom()].
```

### is_enabled/3

Checks if transition is enabled.

```erlang
-callback is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) -> boolean().
```

### fire/3

Fires a transition, producing new marking.

```erlang
-callback fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    {produce, map()} | {produce, map(), term()} | abort.
```

**Return Values**
- `{produce, NewMode}` - New marking produced
- `{produce, NewMode, NewUsrInfo}` - New marking and user info
- `abort` - Transition aborted

### trigger/3

Callback for token-based processing.

```erlang
-callback trigger(Place :: atom(), Token :: term(), UsrInfo :: term()) ->
    pass | {consume, [term()]}.
```

### init/1

Initializes the gen_pnet.

```erlang
-callback init(UsrInfo :: term()) -> {ok, term()}.
```

### handle_call/3

Handles synchronous calls.

```erlang
-callback handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
    {reply, term(), term()}.
```

### handle_cast/2

Handles asynchronous casts.

```erlang
-callback handle_cast(Request :: term(), NetState :: term()) ->
    {noreply, term()}.
```

### handle_info/2

Handles non-gen_pnet messages.

```erlang
-callback handle_info(Request :: term(), NetState :: term()) ->
    {noreply, term()}.
```

### terminate/2

Cleanup on termination.

```erlang
-callback terminate(Reason :: term(), NetState :: term()) -> ok.
```

### code_change/3

Handles code changes.

```erlang
-callback code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
    {ok, term()}.
```

---

## Type Definitions

### Workflow Types

```erlang
-type workflow() :: #workflow{}.
-type task() :: #task{}.
-type yawl_condition() :: #yawl_condition{}.
-type connection() :: #connection{}.
-type split_type() :: and_split | or_split | xor_split.
-type join_type() :: and_join | or_join | xor_join.
-type condition() :: binary() | {atom(), term()} | fun(() -> boolean()).
-type element_id() :: binary().
-type task_type() :: atomic | composite | subworkflow | multi_instance.
```

### Pattern Types

```erlang
-type pattern() :: #workflow{} | #sequence{} | #parallel_split{} |
                  #synchronization{} | #exclusive_choice{} |
                  #simple_merge{} | #multi_choice{} |
                  #synchronizing_merge{} | #multi_merge{} |
                  #discriminator{} | #arbitration{} |
                  #param_pass{} | #data_transform{} | #data_distribute{} |
                  #data_accumulate{} | #data_visibility{} |
                  #resource_create{} | #role_allocate{} | #resource_start{} |
                  #role_distribute{} | #capability_allocate{}.
```

### Approval Types

```erlang
-type checkpoint_id() :: binary().
-type approver_type() :: human | simulated | auto.
-type approval_status() :: pending | approved | denied | timeout | cancelled.
-type approval_result() :: {ok, #approval_decision{}} | {error, term()}.
```

### Telemetry Types

```erlang
-type event_level() :: debug | info | warning | error.
-type trace_id() :: binary().
-type span_id() :: binary().
```

### XES Types

```erlang
-type log_id() :: binary().
-type trace_id() :: binary().
-type case_id() :: binary().
-type event_id() :: binary().
-type timestamp() :: integer().
```

---

## Records Reference

### workflow

```erlang
-record(workflow, {
    id :: binary(),
    name :: binary(),
    tasks :: #{binary() => #task{}},
    conditions :: #{binary() => #yawl_condition{}},
    connections :: [#connection{}],
    start_task_id :: binary() | undefined,
    end_task_ids :: [binary()]
}).
```

### task

```erlang
-record(task, {
    id :: binary(),
    name :: binary(),
    type :: atomic | composite | subworkflow | multi_instance,
    split_type :: and_split | or_split | xor_split | undefined,
    join_type :: and_join | or_join | xor_join | undefined,
    metadata :: map()
}).
```

### yawl_condition

```erlang
-record(yawl_condition, {
    id :: binary(),
    expression :: condition(),
    description :: binary() | undefined
}).
```

### connection

```erlang
-record(connection, {
    from_id :: binary(),
    to_id :: binary(),
    condition_id :: binary() | undefined
}).
```

### approval_checkpoint

```erlang
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
```

### approval_decision

```erlang
-record(approval_decision, {
    checkpoint_id :: binary(),
    approved :: boolean(),
    decision_maker :: term(),
    reason :: binary(),
    metadata :: map(),
    decided_at :: integer()
}).
```

### otel_event

```erlang
-record(otel_event, {
    id :: binary(),
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    timestamp :: integer(),
    event_type :: binary() | atom(),
    level :: debug | info | warning | error,
    user_id :: term(),
    case_id :: term(),
    task_id :: term(),
    pattern_id :: term(),
    message :: binary(),
    attributes :: map()
}).
```

### otel_trace

```erlang
-record(otel_trace, {
    trace_id :: binary(),
    case_id :: binary(),
    pattern_id :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status :: atom(),
    span_count :: non_neg_integer()
}).
```

### xes_log

```erlang
-record(xes_log, {
    log_id :: binary(),
    trace_id :: binary(),
    started_at :: integer(),
    events :: list(),
    metadata :: map()
}).
```

### xes_event

```erlang
-record(xes_event, {
    event_id :: binary(),
    timestamp :: integer(),
    case_id :: binary() | undefined,
    concept :: map(),
    lifecycle :: map(),
    data :: map()
}).
```

### participant

```erlang
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
```

### persistent_case

```erlang
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
```

### persistent_workitem

```erlang
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
```

---

## Error Reference

### Common Errors

| Error | Description | Resolution |
|-------|-------------|------------|
| `not_found` | Resource/Task not found | Verify IDs exist |
| `invalid_workflow` | Workflow structure invalid | Check validation errors |
| `already_decided` | Approval already made | Cannot approve/deny twice |
| `no_resources_available` | No matching resources | Register participants |
| `timeout` | Operation timed out | Increase timeout |
| `start_task_not_found` | Start task missing | Verify task ID |
| `end_task_not_found` | End task missing | Verify task ID |
| `cycle_detected` | Workflow has cycles | Remove cycles |
| `invalid_split_type` | Split type invalid | Use and_split/or_split/xor_split |
| `invalid_join_type` | Join type invalid | Use and_join/or_join/xor_join |

---

## See Also

- [Pattern Catalog](pattern_catalog.md) - All 43 YAWL patterns
- [Callback Reference](callback_reference.md) - gen_pnet callback details
- [Token Types](token_types.md) - Token type definitions
- [Configuration Options](configuration_options.md) - System configuration
- [Error Codes](error_codes.md) - Complete error reference
- [Module Index](module_index.md) - Module exports
- [Glossary](glossary.md) - Terminology
