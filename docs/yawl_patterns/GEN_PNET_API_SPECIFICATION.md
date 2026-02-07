# YAWL gen_pnet API Specification

**Version**: 1.0.0
**Date**: 2026-02-05
**Status**: API Specification

## Table of Contents

1. [Module API Specifications](#module-api-specifications)
2. [Function Reference](#function-reference)
3. [Type Specifications](#type-specifications)
4. [Error Codes](#error-codes)
5. [Usage Examples](#usage-examples)

---

## Module API Specifications

### Module: `yawl_pnet.erl`

The main gen_pnet behavior wrapper for YAWL workflows.

#### start_workflow/2,3

```erlang
%% @doc Starts a new YAWL workflow as a gen_pnet instance
%%
%% @param PatternModule The module implementing the workflow pattern
%% @param PatternConfig Configuration map for the pattern
%% @param Options Optional gen_pnet startup options
%% @returns {ok, Pid} | {error, Reason}
%%
%% @equiv start_workflow(PatternModule, PatternConfig, [])
-spec start_workflow(PatternModule :: module(),
                     PatternConfig :: map()) ->
    {ok, pid()} | {error, term()}.

-spec start_workflow(PatternModule :: module(),
                     PatternConfig :: map(),
                     Options :: list()) ->
    {ok, pid()} | {error, term()}.
```

**Parameters:**
- `PatternModule`: Module implementing gen_pnet callbacks
- `PatternConfig`: Map containing:
  - `workflow_id`: Binary workflow identifier
  - `initial_data`: Initial data payload
  - `instance_count`: Number of instances (for multi-instance patterns)
  - `branch_count`: Number of parallel branches
  - `condition_fun`: Function for conditional routing
  - `timeout_ms`: Execution timeout
- `Options`: List of gen_pnet options:
  - `{debug, DbgList}`: Debug options
  - `{spawn_opt, SOList}`: Spawn options
  - `{timeout, Timeout}`: Init timeout

**Returns:**
- `{ok, Pid}`: Successfully started workflow
- `{error, {invalid_pattern, Reason}}`: Pattern module invalid
- `{error, {invalid_config, Reason}}`: Configuration invalid

---

### Module: `yawl_pnet_tokens.erl`

Token creation and manipulation utilities.

#### Token Construction Functions

```erlang
%% @doc Creates an initial workflow token
-spec initial_token(Data :: term()) -> colored_token().

%% @doc Creates a task start token
-spec task_token(TaskId :: binary(), InputData :: term()) -> colored_token().

%% @doc Creates a task completion token
-spec completion_token(TaskId :: binary(), OutputData :: term()) -> colored_token().

%% @doc Creates a branch token for parallel execution
-spec branch_token(BranchId :: atom(), Data :: term()) -> colored_token().

%% @doc Creates a data-carrying token
-spec data_token(DataType :: atom(), Data :: term()) -> colored_token().

%% @doc Creates a control token
-spec control_token(ControlType :: atom()) -> colored_token().

%% @doc Creates a cancel token
-spec cancel_token(Reason :: term()) -> colored_token().

%% @doc Creates an error token
-spec error_token(Reason :: term(), Stack :: term()) -> colored_token().

%% @doc Creates a milestone token
-spec milestone_token(MilestoneId :: atom(), Data :: term()) -> colored_token().
```

#### Token Inspection Functions

```erlang
%% @doc Extracts token type
-spec get_token_type(colored_token()) -> token_type().

%% @doc Extracts token payload
-spec get_token_payload(colored_token()) -> term().

%% @doc Extracts token metadata
-spec get_token_metadata(colored_token()) -> map().

%% @doc Checks if token is expired
-spec is_token_expired(colored_token()) -> boolean().

%% @doc Gets token age in milliseconds
-spec token_age(colored_token()) -> non_neg_integer().
```

#### Token Transformation Functions

```erlang
%% @doc Transforms token payload with a function
-spec transform_payload(colored_token(), fun((term()) -> term())) ->
    colored_token().

%% @doc Merges multiple tokens into one
-spec merge_tokens([colored_token()]) -> colored_token().

%% @doc Splits a token into multiple tokens
-spec split_token(colored_token(), SplitFun :: fun((term()) -> [term()])) ->
    [colored_token()].

%% @doc Clones a token with new ID
-spec clone_token(colored_token()) -> colored_token().

%% @doc Updates token metadata
-spec update_metadata(colored_token(), map() | fun((map()) -> map())) ->
    colored_token().
```

---

### Module: `yawl_pattern_registry.erl`

Pattern module registration and discovery.

#### Registration Functions

```erlang
%% @doc Registers a pattern module
-spec register_pattern(PatternId :: binary(), Module :: module()) ->
    ok | {error, term()}.

%% @doc Unregisters a pattern module
-spec unregister_pattern(PatternId :: binary()) -> ok.

%% @doc Gets pattern module by ID
-spec get_pattern(PatternId :: binary()) ->
    {ok, module()} | {error, not_found}.

%% @doc Gets pattern info by ID
-spec get_pattern_info(PatternId :: binary()) ->
    {ok, map()} | {error, not_found}.
```

#### Discovery Functions

```erlang
%% @doc Lists all registered patterns
-spec list_patterns() -> [{binary(), module()}].

%% @doc Lists patterns by category
-spec list_patterns_by_category(Category :: atom()) ->
    [{binary(), module()}].

%% @doc Finds pattern by WCP number
-spec find_pattern_by_wcp(WCPNumber :: {integer(), integer()}) ->
    {ok, module()} | {error, not_found}.

%% @doc Searches patterns by name
-spec search_patterns(SearchTerm :: binary()) ->
    [{binary(), module()}].
```

#### Validation Functions

```erlang
%% @doc Validates pattern implementation
-spec validate_pattern(Module :: module()) ->
    {ok, map()} | {error, term()}.

%% @doc Verifies pattern soundness properties
-spec verify_soundness(Module :: module()) ->
    {ok, map()} | {error, term()}.

%% @doc Runs pattern tests
-spec test_pattern(Module :: module()) ->
    {ok, TestResults :: map()} | {error, term()}.
```

---

### Module: `yawl_pnet_adapter.erl`

Migration adapter between gen_statem and gen_pnet.

#### Conversion Functions

```erlang
%% @doc Converts gen_statem state to gen_pnet marking
-spec statem_to_pnet(State :: term()) ->
    #{atom() => [colored_token()]}.

%% @doc Converts gen_pnet marking to gen_statem state
-spec pnet_to_statem(Marking :: map()) -> map().

%% @doc Converts workitem to token
-spec workitem_to_token(Workitem :: #workitem{}) -> colored_token().

%% @doc Converts token to workitem
-spec token_to_workitem(Token :: colored_token()) -> #workitem{}.
```

#### Migration Functions

```erlang
%% @doc Migrates a running workflow from gen_statem to gen_pnet
-spec migrate_workflow(WorkflowId :: binary()) ->
    {ok, pid()} | {error, term()}.

%% @doc Validates migration readiness
-spec can_migrate(WorkflowId :: binary()) ->
    {ok, boolean()} | {error, term()}.

%% @doc Gets migration status
-spec migration_status(WorkflowId :: binary()) ->
    {ok, map()} | {error, term()}.
```

---

## Function Reference

### State Query Functions

#### get_case_state/1

```erlang
%% @doc Gets current workflow case state
-spec get_case_state(Pid :: pid()) ->
    {ok, CaseState :: map()} | {error, term()}.

%% Returns:
%% #{
%%   case_id := binary(),
%%   workflow_id := binary(),
%%   status := case_status(),
%%   marking := #{atom() => [colored_token()]},
%%   active_transitions := [atom()],
%%   variables := map(),
%%   timestamps := #{
%%     created_at => integer(),
%%     started_at => integer() | undefined,
%%     completed_at => integer() | undefined
%%   }
%% }
```

#### get_marking/1

```erlang
%% @doc Gets current marking of the Petri net
-spec get_marking(Pid :: pid()) ->
    #{atom() => [colored_token()]}.
```

#### get_place_tokens/2

```erlang
%% @doc Gets tokens on a specific place
-spec get_place_tokens(Pid :: pid(), Place :: atom()) ->
    {ok, [colored_token()]} | {error, term()}.
```

### State Manipulation Functions

#### inject_token/3

```erlang
%% @doc Injects a token into a specific place
-spec inject_token(Pid :: pid(), Place :: atom(), Token :: colored_token()) ->
    ok | {error, term()}.
```

#### remove_tokens/2

```erlang
%% @doc Removes all tokens from a place
-spec remove_tokens(Pid :: pid(), Place :: atom()) ->
    ok | {error, term()}.
```

#### set_variable/3

```erlang
%% @doc Sets a workflow variable
-spec set_variable(Pid :: pid(), Key :: term(), Value :: term()) ->
    ok | {error, term()}.
```

#### get_variable/2

```erlang
%% @doc Gets a workflow variable
-spec get_variable(Pid :: pid(), Key :: term()) ->
    {ok, term()} | {error, not_found}.
```

### Control Functions

#### cancel_case/1

```erlang
%% @doc Cancels a running workflow case
-spec cancel_case(Pid :: pid()) -> ok | {error, term()}.
```

#### suspend_case/1

```erlang
%% @doc Suspends a running workflow case
-spec suspend_case(Pid :: pid()) -> ok | {error, term()}.
```

#### resume_case/1

```erlang
%% @doc Resumes a suspended workflow case
-spec resume_case(Pid :: pid()) -> ok | {error, term()}.
```

### Observer Functions

#### subscribe/2

```erlang
%% @doc Subscribes to workflow events
-spec subscribe(Pid :: pid(), Subscriber :: pid()) -> ok.
```

#### unsubscribe/2

```erlang
%% @doc Unsubscribes from workflow events
-spec unsubscribe(Pid :: pid(), Subscriber :: pid()) -> ok.
```

---

## Type Specifications

### Core Types

```erlang
%%--------------------------------------------------------------------
%% Token Types
%%--------------------------------------------------------------------

-type token_type() ::
    initial              |  % Workflow start token
    task_start           |  % Task beginning
    task_complete        |  % Task finished
    branch               |  % Branch routing
    merge                |  % Merge synchronization
    data                 |  % Data carrying
    control              |  % Control flow
    cancel               |  % Cancellation
    error                |  % Error condition
    milestone            |  % Milestone reached
    completion.          % % Workflow completion

-type colored_token() :: #colored_token{
    id          :: binary(),
    type        :: token_type(),
    payload     :: term(),
    metadata    :: map(),
    created_at  :: integer(),
    expires_at  :: integer() | undefined,
    trace_id    :: binary() | undefined
}.

%%--------------------------------------------------------------------
%% Workflow Types
%%--------------------------------------------------------------------

-type case_id() :: binary().
-type workflow_id() :: binary().
-type task_id() :: binary().
-type place_id() :: atom().
-type transition_id() :: atom().

-type case_status() ::
    initialized | running | suspended | completed | cancelled | failed.

-type place_type() ::
    input | output | active | waiting | synchronizing |
    data_buffer | branch_control | milestone | error.

%%--------------------------------------------------------------------
%% Pattern Types
%%--------------------------------------------------------------------

-type pattern_category() ::
    basic_control_flow      |  % WCP-01 to WCP-06
    advanced_branching      |  % WCP-07 to WCP-12
    multiple_instances      |  % WCP-13 to WCP-17
    state_based             |  % WCP-18 to WCP-21
    cancellation            |  % WCP-22 to WCP-25
    data_flow               |  % WDP patterns
    resource_management     |  % WRP patterns
    exception_handling.      % % WHP patterns

-type pattern_info() :: #{
    id := binary(),
    name := binary(),
    category := pattern_category(),
    wcp_number := {integer(), integer()},
    description := binary(),
    module := module(),
    complexity := low | medium | high
}.

%%--------------------------------------------------------------------
%% Marking Types
%%--------------------------------------------------------------------

-type marking() :: #{place_id() => [colored_token()]}.
-type mode() :: #{place_id() => [colored_token()]}.

%%--------------------------------------------------------------------
%% Configuration Types
%%--------------------------------------------------------------------

-type pattern_config() :: #{
    workflow_id := workflow_id(),
    optional(instance_count, non_neg_integer()),
    optional(branch_count, non_neg_integer()),
    optional(condition_fun, function()),
    optional(timeout_ms, non_neg_integer()),
    optional(initial_data, term()),
    optional(variables, map())
}.

-type pnet_options() :: [
    {debug, [atom()]} |
    {spawn_opt, [atom()]} |
    {timeout, non_neg_integer()} |
    {enable_colored, boolean()} |
    {enable_tracing, boolean()} |
    {enable_metrics, boolean()}
].
```

---

## Error Codes

### Error Categories

```erlang
%%--------------------------------------------------------------------
%% Error Type Definitions
%%--------------------------------------------------------------------

-type error_category() ::
    pattern_error |
    config_error |
    state_error |
    token_error |
    transition_error |
    timeout_error |
    cancellation_error.

-type error_reason() ::
    {invalid_pattern, term()} |
    {invalid_config, term()} |
    {invalid_state, term()} |
    {invalid_token, term()} |
    {transition_not_enabled, transition_id()} |
    {transition_failed, transition_id(), term()} |
    {timeout, operation()} |
    {cancelled, reason()} |
    {place_not_found, place_id()} |
    {case_not_found, case_id()}.
```

### Error Handling Pattern

```erlang
%% @doc Handles errors with consistent logging
-spec handle_error(Error :: term(), Context :: map()) ->
    no_return().

handle_error(Error, Context) ->
    Category = classify_error(Error),
    logger:error("YAWL gen_pnet error: category=~p error=~p context=~p",
                 [Category, Error, Context]),
    erlang:raise(error, Error, maps:get(stacktrace, Context, [])).

%% @doc Classifies error into category
-spec classify_error(term()) -> error_category().

classify_error({invalid_pattern, _}) -> pattern_error;
classify_error({invalid_config, _}) -> config_error;
classify_error({invalid_state, _}) -> state_error;
classify_error({invalid_token, _}) -> token_error;
classify_error({transition_not_enabled, _}) -> transition_error;
classify_error({transition_failed, _, _}) -> transition_error;
classify_error({timeout, _}) -> timeout_error;
classify_error({cancelled, _}) -> cancellation_error;
classify_error(_) -> state_error.
```

---

## Usage Examples

### Example 1: Starting a Simple Sequence Workflow

```erlang
%% Start a sequence workflow
{ok, Pid} = yawl_pnet:start_workflow(
    yawl_wcp01_sequence,
    #{
        workflow_id => <<"order_workflow">>,
        initial_data => #{
            customer_id => <<"CUST-001">>,
            items => [<<"ITEM-001">>, <<"ITEM-002">>]
        }
    }
).

%% Query the state
{ok, State} = yawl_pnet:get_case_state(Pid),
#{status := Status} = State.

%% Wait for completion
receive
    {yawl_complete, Pid, Result} ->
        io:format("Workflow complete: ~p~n", [Result])
after 5000 ->
    {error, timeout}
end.
```

### Example 2: Parallel Split with Synchronization

```erlang
%% Start parallel workflow
Config = #{
    workflow_id => <<"parallel_processing">>,
    branch_count => 3,
    branches => [
        {task_a, fun process_branch_a/1},
        {task_b, fun process_branch_b/1},
        {task_c, fun process_branch_c/1}
    ],
    initial_data => input_data
},

{ok, Pid} = yawl_pnet:start_workflow(
    yawl_wcp02_parallel_split,
    Config
).

%% Inject additional tokens if needed
Token = yawl_pnet_tokens:branch_token(task_a, Data),
yawl_pnet:inject_token(Pid, p_branch_1, Token).
```

### Example 3: Multi-Instance Pattern

```erlang
%% Start multi-instance workflow
Config = #{
    workflow_id => <<"batch_processing">>,
    instance_count => 10,
    instance_fun = fun(Item) -> process_item(Item) end,
    input_data => items_to_process
},

{ok, Pid} = yawl_pnet:start_workflow(
    yawl_wcp13_multi_instance_static,
    Config
).

%% Monitor progress
yawl_pnet:subscribe(self(), Pid),
receive
    {yawl_event, Pid, {instance_complete, InstanceId}} ->
        io:format("Instance ~p complete~n", [InstanceId])
after 1000 ->
    ok
end.
```

### Example 4: Cancellation Pattern

```erlang
%% Start cancellable workflow
{ok, Pid} = yawl_pnet:start_workflow(
    yawl_wcp20_cancel_case,
    #{
        workflow_id => <<"long_running">>,
        tasks => long_running_tasks
    }
).

%% Cancel after timeout
erlang:send_after(5000, self(), cancel),

receive
    cancel ->
        ok = yawl_pnet:cancel_case(Pid)
end.

%% Check final state
{ok, #{status := cancelled}} = yawl_pnet:get_case_state(Pid).
```

### Example 5: Pattern Registry

```erlang
%% Register custom pattern
ok = yawl_pattern_registry:register_pattern(
    <<"custom_parallel">>,
    my_custom_pattern_module
).

%% List available patterns
Patterns = yawl_pattern_registry:list_patterns(),
io:format("Available patterns: ~p~n", [Patterns]).

%% Find by category
BasicPatterns = yawl_pattern_registry:list_patterns_by_category(
    basic_control_flow
).

%% Find by WCP number
{ok, Module} = yawl_pattern_registry:find_pattern_by_wcp({2, 1}),
```

---

## Event Notification Protocol

### Event Types

```erlang
%%--------------------------------------------------------------------
%% Event Records
%%--------------------------------------------------------------------

-record(yawl_event, {
    event_type   :: atom(),
    case_id      :: binary(),
    workflow_id  :: binary(),
    timestamp    :: integer(),
    data         :: term(),
    metadata     :: map()
}).

-type yawl_event() :: #yawl_event{}.
```

### Event Flow

```
Workflow Execution                  Subscriber
     │                                   │
     ├───► {yawl_start, Pid, CaseId} ────►
     ├───► {yawl_complete, Pid, Result} ──►
     ├───► {yawl_cancel, Pid, Reason} ────►
     ├───► {yawl_error, Pid, Error} ──────►
     ├───► {yawl_token_produced, Pid, {Place, Token}} ─►
     └───► {yawl_transition_fired, Pid, Transition} ──►
```

### Subscriber Implementation

```erlang
%% Example subscriber module
-module(my_workflow_subscriber).
-behaviour(gen_server).

init([WorkflowPid]) ->
    yawl_pnet:subscribe(WorkflowPid, self()),
    {ok, #{workflow_pid => WorkflowPid}}.

handle_info({yawl_event, WorkflowPid, Event}, State) ->
    handle_yawl_event(Event, State),
    {noreply, State};

handle_info({yawl_complete, WorkflowPid, Result}, State) ->
    logger:info("Workflow ~p completed: ~p", [WorkflowPid, Result]),
    {noreply, State};

handle_info({yawl_error, WorkflowPid, Error}, State) ->
    logger:error("Workflow ~p error: ~p", [WorkflowPid, Error]),
    {noreply, State}.

handle_yawl_event(#yawl_event{event_type = token_produced,
                               case_id = CaseId,
                               data = {Place, Token}}, State) ->
    logger:info("Token produced: case=~p place=~p token=~p",
                [CaseId, Place, Token]).
```

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-05
