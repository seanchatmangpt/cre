# YAWL Integration Guide for CRE Framework

## Overview

This guide provides comprehensive instructions for integrating YAWL workflow patterns into the CRE (Cooperative Runtime Environment) framework. The integration covers pattern implementation, configuration, deployment, and operational aspects of YAWL workflows within the CRE ecosystem.

## Prerequisites

### System Requirements

- **Erlang/OTP**: 25.0 or later
- **CRE Framework**: Latest stable version
- **Memory**: Minimum 1GB RAM (recommended 4GB+ for complex workflows)
- **Storage**: Minimum 500MB disk space for workflow definitions
- **Network**: Standard TCP/IP connectivity for distributed operations

### Dependencies

```erlang
% Required dependencies in rebar.config
{deps, [
    {gen_pnet, ".*", {git, "https://github.com/your-org/gen_pnet.git", "main"}},
    {cre_base, ".*", {git, "https://github.com/your-org/cre-base.git", "main"}},
    {yawl_patterns, ".*", {git, "https://github.com/your-org/yawl-patterns.git", "main"}},
    {telemetry, "~> 1.2"},
    {logger, "~> 1.5"},
    {ssl, "~> 10.7"}
]}.
```

## Installation and Setup

### 1. Clone and Build

```bash
# Clone the repository
git clone https://github.com/your-org/cre-yawl.git
cd cre-yawl

# Install dependencies
rebar3 deps

# Build the application
rebar3 compile
```

### 2. Configuration Setup

Create a configuration file `config/config.yml`:

```yaml
# YAWL Configuration
yawl:
  # Pattern registry settings
  registry:
    enabled: true
    auto_load: true
    cache_size: 1000
    reload_interval: 300000  # 5 minutes

  # Pattern execution settings
  execution:
    max_concurrent_patterns: 50
    timeout_ms: 30000
    retry_attempts: 3
    retry_delay_ms: 1000

  # State management
  state:
    persistence_enabled: true
    storage_backend: ets  # or mnesia
    checkpoint_interval: 60000  # 1 minute
    garbage_collection_interval: 300000  # 5 minutes

  # Resource management
  resources:
    max_resources: 1000
    allocation_strategy: round_robin
    reservation_timeout_ms: 10000

  # Security settings
  security:
    enabled: true
    auth_module: yawl_auth
    encryption_enabled: true
    audit_log_enabled: true

  # Monitoring and logging
  monitoring:
    enabled: true
    metrics_interval: 10000  # 10 seconds
    log_level: info
    log_format: json

  # Performance settings
  performance:
    token_compression: true
    state_caching: true
    event_batching: true
    batch_size: 100
    batch_timeout_ms: 1000
```

### 3. Application Configuration

Update `src/cre.app.src`:

```erlang
{application, cre, [
    {applications, [kernel, stdlib, gen_pnet, telemetry, logger]},
    {mod, {cre, []}},
    {env, [
        {yawl_config, #{
            registry => #{
                enabled => true,
                auto_load => true,
                cache_size => 1000,
                reload_interval => 300000
            },
            execution => #{
                max_concurrent_patterns => 50,
                timeout_ms => 30000,
                retry_attempts => 3,
                retry_delay_ms => 1000
            },
            state => #{
                persistence_enabled => true,
                storage_backend => ets,
                checkpoint_interval => 60000,
                garbage_collection_interval => 300000
            }
        }}
    ]}
]}.
```

## Pattern Integration

### 1. Implementing Custom Patterns

To implement a custom YAWL pattern, extend the base pattern behavior:

```erlang
% src/my_custom_pattern.erl
-module(my_custom_pattern).
-behaviour(gen_pnet).

% gen_pnet callbacks
-export([init_state/1, handle_transition/3, handle_event/3]).

% Custom pattern API
-export([start_workflow/1, stop_workflow/1, get_status/1]).

-record(pattern_state, {
    pattern_type = my_custom_pattern :: atom(),
    workflow_id :: binary(),
    instance_id :: binary(),
    state :: waiting | running | completed | failed,
    data :: map(),
    metadata :: map()
}).

% Initialize pattern state
init_state(Subprocess) ->
    #pattern_state{
        workflow_id = Subprocess,
        instance_id = generate_instance_id(),
        state = waiting,
        data = #{},
        metadata = #{created_at => erlang:system_time(millisecond)}
    }.

% Handle transitions
handle_transition(start, State = #pattern_state{state = waiting}, Data) ->
    TransitionToRunning = State#pattern_state{
        state = running,
        data = maps:merge(State#pattern_state.data, Data),
        metadata = maps:put(started_at, erlang:system_time(millisecond),
                           State#pattern_state.metadata)
    },
    % Emit event
    emit_event(workflow_started, TransitionToRunning),
    {reply, started, TransitionToRunning};

handle_transition(complete, State = #pattern_state{state = running}, _Data) ->
    CompletedState = State#pattern_state{
        state = completed,
        metadata = maps:put(completed_at, erlang:system_time(millisecond),
                           State#pattern_state.metadata)
    },
    emit_event(workflow_completed, CompletedState),
    {reply, completed, CompletedState};

handle_transition(_, State, _) ->
    {noreply, State}.

% Handle events
handle_event(error, State = #pattern_state{state = running}, ErrorData) ->
    ErrorState = State#pattern_state{
        state = failed,
        metadata = maps:put(error_at, erlang:system_time(millisecond),
                           maps:put(error_data, ErrorData, State#pattern_state.metadata))
    },
    emit_event(workflow_failed, ErrorState),
    {reply, error, ErrorState};

handle_event(_, State, _) ->
    {noreply, State}.

% Custom API functions
start_workflow(WorkflowId) ->
    PatternState = init_state(WorkflowId),
    gen_pnet:start(?MODULE, PatternState).

stop_workflow(InstanceId) ->
    gen_pnet:stop(InstanceId).

get_status(InstanceId) ->
    gen_pnet:call(InstanceId, get_status).

% Helper functions
generate_instance_id() ->
    erlang:ref_to_list(erlang:make_ref()).

emit_event(EventType, State) ->
    Event = #{
        type => EventType,
        pattern_type => State#pattern_state.pattern_type,
        workflow_id => State#pattern_state.workflow_id,
        instance_id => State#pattern_state.instance_id,
        timestamp => erlang:system_time(millisecond),
        data => State#pattern_state.data
    },
    % Send to event bus or logging system
    ok.
```

### 2. Registering Patterns

Register patterns with the pattern registry:

```erlang
% src/pattern_registry.erl
-module(pattern_registry).
-export([register_pattern/2, get_pattern/1, list_patterns/0]).

-define(PATTERN_REGISTRY, pattern_registry).

register_pattern(PatternType, PatternModule) when is_atom(PatternType), is_atom(PatternModule) ->
    case code:ensure_loaded(PatternModule) of
        {module, _} ->
            PatternInfo = #pattern_info{
                id = atom_to_binary(PatternType),
                name = atom_to_binary(PatternType),
                module = PatternModule,
                arity = 1,
                description = description_of(PatternModule),
                category = determine_category(PatternModule),
                complexity = determine_complexity(PatternModule)
            },
            ets:insert(?PATTERN_REGISTRY, {PatternType, PatternInfo}),
            {ok, PatternInfo};
        {error, Reason} ->
            {error, Reason}
    end.

get_pattern(PatternType) ->
    case ets:lookup(?PATTERN_REGISTRY, PatternType) of
        [{PatternType, PatternInfo}] ->
            {ok, PatternInfo};
        [] ->
            {error, not_found}
    end.

list_patterns() ->
    ets:tab2list(?PATTERN_REGISTRY).

% Helper functions
description_of(Module) ->
    % Extract description from module attributes or documentation
    case erlang:get_module_attribute(Module, description) of
        undefined -> <<"No description available">>;
        Desc -> Desc
    end.

determine_category(Module) ->
    % Determine pattern category based on naming convention or module attributes
    case atom_to_list(Module) of
        "basic_" ++ _ -> basic_control_flow;
        "multiple_" ++ _ -> multiple_instances;
        "state_" ++ _ -> state_based;
        "extended_" ++ _ -> extended_control_flow;
        "data_" ++ _ -> data_flow;
        "resource_" ++ _ -> resource;
        "exception_" ++ _ -> exception_handling;
        _ -> basic_control_flow  % Default fallback
    end.

determine_complexity(Module) ->
    % Determine complexity based on code analysis
    case erlang:get_module_attribute(Module, complexity) of
        low -> low;
        medium -> medium;
        high -> high;
        undefined -> medium  % Default fallback
    end.
```

### 3. Using Pattern Registry

```erlang
% Initialize pattern registry on startup
init_patterns() ->
    % Register standard patterns
    StandardPatterns = [
        {sequence, yawl_sequence},
        {parallel_split, yawl_parallel_split},
        {synchronization, yawl_synchronization},
        {exclusive_choice, yawl_exclusive_choice},
        {implicit_termination, yawl_implicit_termination},
        {multiple_instances_no_sync, yawl_multiple_instances_no_sync},
        {milestone, yawl_milestone},
        {cancel_activity, yawl_cancel_activity}
    ],

    lists:foreach(fun({Type, Module}) ->
        pattern_registry:register_pattern(Type, Module)
    end, StandardPatterns),

    % Load custom patterns from configuration
    case application:get_env(cre, custom_patterns, []) of
        [] -> ok;
        CustomPatterns ->
            lists:foreach(fun({Type, Module}) ->
                pattern_registry:register_pattern(Type, Module)
            end, CustomPatterns)
    end.
```

## Workflow Integration

### 1. Creating Workflows

```erlang
% src/workflow_manager.erl
-module(workflow_manager).
-export([create_workflow/1, start_workflow/1, stop_workflow/1,
         get_workflow_status/1, add_step/3]).

-record(workflow, {
    id :: binary(),
    name :: binary(),
    version :: binary(),
    steps :: [#step{}],
    current_step :: non_neg_integer(),
    state :: created | running | completed | failed,
    metadata :: map()
}).

-record(step, {
    id :: binary(),
    type :: atom(),
    config :: map(),
    transitions :: map(),
    dependencies :: [binary()]
}).

create_workflow(Id) when is_binary(Id) ->
    Workflow = #workflow{
        id = Id,
        name = <<"Untitled Workflow">>,
        version = <<"1.0.0">>,
        steps = [],
        current_step = 0,
        state = created,
        metadata = #{created_at => erlang:system_time(millisecond)}
    },
    workflow_store:save(Workflow),
    {ok, Workflow}.

add_step(WorkflowId, StepType, Config) ->
    Step = #step{
        id = generate_step_id(),
        type = StepType,
        config = Config,
        transitions = maps:get(transitions, Config, #{}),
        dependencies = maps:get(dependencies, Config, [])
    },
    UpdatedWorkflow = Workflow#workflow{
        steps = [Step | Workflow#workflow.steps]
    },
    workflow_store:save(UpdatedWorkflow),
    {ok, UpdatedWorkflow}.

start_workflow(WorkflowId) ->
    case workflow_store:get(WorkflowId) of
        {ok, Workflow} when Workflow#workflow.state =:= created ->
            case validate_workflow(Workflow) of
                valid ->
                    execute_workflow(Workflow);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Workflow} ->
            {error, workflow_already_started};
        {error, not_found} ->
            {error, workflow_not_found}
    end.

execute_workflow(Workflow) ->
    % Execute each step in sequence
    Steps = Workflow#workflow.steps,
    execute_steps(Steps, 1, Workflow).
```

### 2. Step Execution

```erlang
% src/step_executor.erl
-module(step_executor).
-export([execute_step/3, validate_step/2]).

execute_step(WorkflowId, StepIndex, Step) ->
    case pattern_registry:get_pattern(Step#step.type) of
        {ok, PatternInfo} ->
            PatternModule = PatternInfo#pattern_info.module,
            % Initialize pattern state
            PatternState = PatternModule:init_state(WorkflowId),

            % Execute step with configuration
            StepConfig = Step#step.config,
            Result = PatternModule:handle_transition(start, PatternState, StepConfig),

            % Handle result and update workflow
            handle_step_result(WorkflowId, StepIndex, Result);
        {error, Reason} ->
            {error, Reason}
    end.

validate_step(Step, Workflow) ->
    % Validate step dependencies
    case validate_dependencies(Step, Workflow) of
        valid ->
            % Validate step configuration
            case validate_configuration(Step) of
                valid -> valid;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

### 3. State Management

```erlang
% src/state_manager.erl
-module(state_manager).
-export([save_state/2, load_state/1, get_state/1, delete_state/1]).

% State records
-record(workflow_state, {
    workflow_id :: binary(),
    pattern_states :: #{binary() => #pattern_state{}},
    current_step :: non_neg_integer(),
    state :: atom(),
    timestamp :: non_neg_integer()
}).

save_state(WorkflowId, State) ->
    StateRecord = #workflow_state{
        workflow_id = WorkflowId,
        pattern_states = State#workflow.pattern_states,
        current_step = State#workflow.current_step,
        state = State#workflow.state,
        timestamp = erlang:system_time(millisecond)
    },
    state_store:save(StateRecord).

load_state(WorkflowId) ->
    case state_store:get(WorkflowId) of
        {ok, StateRecord} ->
            #workflow{
                id = StateRecord#workflow_state.workflow_id,
                steps = [], % Reconstruct from pattern states if needed
                current_step = StateRecord#workflow_state.current_step,
                state = StateRecord#workflow_state.state,
                metadata = #{last_updated => StateRecord#workflow_state.timestamp}
            };
        {error, not_found} ->
            {error, not_found}
    end.
```

## Resource Integration

### 1. Resource Management

```erlang
% src/resource_manager.erl
-module(resource_manager).
-export([allocate_resource/2, deallocate_resource/2,
         get_resource_status/1, list_resources/0]).

-record(resource, {
    id :: binary(),
    type :: atom(),
    state :: available | allocated | busy | maintenance,
    allocated_to :: binary() | undefined,
    metadata :: map()
}).

allocate_resource(ResourceId, WorkflowId) ->
    case resource_store:get(ResourceId) of
        {ok, Resource = #resource{state = available}} ->
            UpdatedResource = Resource#resource{
                state = allocated,
                allocated_to = WorkflowId,
                metadata = maps:put(allocated_at, erlang:system_time(millisecond),
                                   Resource#resource.metadata)
            },
            resource_store:save(UpdatedResource),
            {ok, UpdatedResource};
        {ok, Resource} ->
            {error, {resource_busy, Resource#resource.state}};
        {error, not_found} ->
            {error, resource_not_found}
    end.
```

### 2. Database Integration

```erlang
% src/db_integration.erl
-module(db_integration).
-export([save_workflow/1, load_workflow/1, update_workflow/1,
         delete_workflow/1, list_workflows/0]).

-define(DB_TABLE, workflows).

save_workflow(Workflow) ->
    WorkflowRecord = #workflow_record{
        id = Workflow#workflow.id,
        name = Workflow#workflow.name,
        version = Workflow#workflow.version,
        steps = serialize_steps(Workflow#workflow.steps),
        current_step = Workflow#workflow.current_step,
        state = Workflow#workflow.state,
        metadata = Workflow#workflow.metadata
    },
    db:save(?DB_TABLE, WorkflowRecord).

load_workflow(WorkflowId) ->
    case db:read(?DB_TABLE, WorkflowId) of
        {ok, WorkflowRecord} ->
            deserialize_workflow(WorkflowRecord);
        {error, not_found} ->
            {error, not_found}
    end.

serialize_steps(Steps) ->
    lists:map(fun(Step) ->
        #{
            id => Step#step.id,
            type => Step#step.type,
            config => Step#step.config,
            transitions => Step#step.transitions,
            dependencies => Step#step.dependencies
        }
    end, Steps).

deserialize_workflow(Record) ->
    Steps = deserialize_steps(Record#workflow_record.steps),
    #workflow{
        id = Record#workflow_record.id,
        name = Record#workflow_record.name,
        version = Record#workflow_record.version,
        steps = Steps,
        current_step = Record#workflow_record.current_step,
        state = Record#workflow_record.state,
        metadata = Record#workflow_record.metadata
    }.
```

## API Integration

### 1. REST API

```erlang
% src/yawl_api.erl
-module(yawl_api).
-export([start/0, stop/0, handle_request/3]).

-define(PORT, 8080).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/workflows", workflow_handler, []},
            {"/workflows/:id", workflow_handler, []},
            {"/patterns", pattern_handler, []},
            {"/patterns/:type", pattern_handler, []},
            {"/resources", resource_handler, []},
            {"/resources/:id", resource_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(yawl_http, [{port, ?PORT}], #{
        env => #{dispatch => Dispatch},
        idle_timeout => 30000,
        request_timeout => 60000
    }),
    ok.

stop() ->
    cowboy:stop(yawl_http).
```

### 2. WebSocket Integration

```erlang
% src/websocket_handler.erl
-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, handle_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

handle_info({workflow_update, WorkflowId, Status}, State) ->
    Response = #{
        type => workflow_update,
        workflow_id => WorkflowId,
        status => Status,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, {text, jsx:encode(Response)}, State};

handle_info({error, Error}, State) ->
    Response = #{
        type => error,
        error => Error,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, {text, jsx:encode(Response)}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _Req, _State) ->
    ok.
```

## Testing Integration

### 1. Unit Testing

```erlang
% test/my_custom_pattern_test.erl
-module(my_custom_pattern_test).
-include_lib("eunit/include/eunit.hrl").

init_state_test() ->
    State = my_custom_pattern:init_state(<<"test_workflow">>),
    ?assertEqual(<<"test_workflow">>, State#pattern_state.workflow_id),
    ?assertEqual(waiting, State#pattern_state.state),
    ?assertEqual(undefined, State#pattern_state.data).

handle_transition_test() ->
    State = my_custom_pattern:init_state(<<"test_workflow">>),

    % Test start transition
    {reply, started, NewState} = my_custom_pattern:handle_transition(start, State, #{}),
    ?assertEqual(running, NewState#pattern_state.state),

    % Test complete transition
    {reply, completed, CompletedState} = my_custom_pattern:handle_transition(complete, NewState, #{}),
    ?assertEqual(completed, CompletedState#pattern_state.state).
```

### 2. Integration Testing

```erlang
% test/workflow_integration_test.erl
-module(workflow_integration_test).
-include_lib("eunit/include/eunit.hrl").

workflow_lifecycle_test() ->
    % Create workflow
    {ok, Workflow} = workflow_manager:create_workflow(<<"test_workflow">>),

    % Add steps
    {ok, _} = workflow_manager:add_step(<<"test_workflow">>, sequence, #{}),
    {ok, _} = workflow_manager:add_step(<<"test_workflow">>, parallel_split, #{}),

    % Start workflow
    {ok, _} = workflow_manager:start_workflow(<<"test_workflow">>),

    % Check status
    {ok, Status} = workflow_manager:get_workflow_status(<<"test_workflow">>),
    ?assertNotEqual(created, Status#workflow.state).
```

### 3. Performance Testing

```erlang
% test/performance_test.erl
-module(performance_test).
-include_lib("eunit/include/eunit.hrl").

parallel_execution_test() ->
    NumWorkflows = 100,
    StartTime = erlang:monotonic_time(millisecond),

    % Create and start multiple workflows in parallel
    Results = lists:map(fun(I) ->
        WorkflowId = list_to_binary("workflow_" ++ integer_to_list(I)),
        {workflow_manager:create_workflow(WorkflowId),
         workflow_manager:add_step(WorkflowId, sequence, #{}),
         workflow_manager:start_workflow(WorkflowId)}
    end, lists:seq(1, NumWorkflows)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    ?assertEqual(NumWorkflows, length(Results)),
    io:format("Created and started ~p workflows in ~p ms~n",
              [NumWorkflows, Duration]),

    % Verify all workflows started successfully
    lists:foreach(fun({ok, _, {ok, _}}) ->
        ok
    end, Results).
```

## Monitoring and Observability

### 1. Telemetry Integration

```erlang
% src/telemetry_integration.erl
-module(telemetry_integration).
-export([init/0, emit_workflow_event/2]).

init() ->
    % Register telemetry events
    telemetry:events([[
        name => [yawl, workflow, started],
        measurement => integer,
        description => "Workflow started",
        metadata => #{}
    ]]),
    ok.

emit_workflow_event(WorkflowId, EventData) ->
    telemetry:execute([yawl, workflow, started],
                     EventData#{workflow_id => WorkflowId},
                     #{}).
```

### 2. Logging Integration

```erlang
% src/logging_integration.erl
-module(logging_integration).
-export([log_workflow_event/3]).

log_workflow_event(WorkflowId, EventType, Data) ->
    LogEntry = #{
        timestamp => erlang:system_time(millisecond),
        workflow_id => WorkflowId,
        event_type => EventType,
        data => Data
    },
    case application:get_env(cre, log_format, json) of
        json ->
            logger:info("YAWL Event: ~s", [jsx:encode(LogEntry)]);
        text ->
            logger:info("YAWL Event: Workflow=~s, Event=~p, Data=~p",
                       [WorkflowId, EventType, Data])
    end.
```

## Deployment and Operations

### 1. Docker Deployment

```dockerfile
# Dockerfile
FROM erlang:25-alpine

# Install dependencies
RUN apk add --no-cache bash

# Create app directory
WORKDIR /app

# Copy dependencies
COPY rebar.config rebar3.lock ./

# Build dependencies
RUN rebar3 deps

# Copy application code
COPY src ./src
COPY config ./config
COPY test ./test

# Build release
RUN rebar3 release

# Expose port
EXPOSE 8080

# Set entrypoint
ENTRYPOINT ["/app/_build/default/rel/cre/bin/cre"]
CMD ["foreground"]
```

### 2. Kubernetes Deployment

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: yawl-engine
  labels:
    app: yawl-engine
spec:
  replicas: 3
  selector:
    matchLabels:
      app: yawl-engine
  template:
    metadata:
      labels:
        app: yawl-engine
    spec:
      containers:
      - name: yawl-engine
        image: your-registry/yawl-engine:latest
        ports:
        - containerPort: 8080
        env:
        - name: ENVIRONMENT
          value: "production"
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: yawl-secrets
              key: database-url
        resources:
          requests:
            memory: "1Gi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "1000m"
```

### 3. Configuration Management

```bash
# scripts/deploy.sh
#!/bin/bash

# Build Docker image
docker build -t yawl-engine:latest .

# Push to registry
docker push your-registry/yawl-engine:latest

# Update Kubernetes deployment
kubectl set image deployment/yawl-engine yawl-engine=yawl-engine:latest

# Check deployment status
kubectl rollout status deployment/yawl-engine
```

## Troubleshooting

### 1. Common Issues

**Pattern Loading Issues**
```bash
# Check pattern registry
erl -pa ebin -s pattern_registry list_patterns

# Validate pattern modules
erl -pa ebin -s pattern_registry validate_module yawn_sequence
```

**State Management Issues**
```bash
# Check state store
erl -pa ebin -s state_manager list_all_states

# Manually clear corrupted state
erl -pa ebin -s state_manager cleanup_corrupted
```

**Performance Issues**
```bash
# Monitor memory usage
rebar3 shell -e "erlang:memory([total, processes, system])"

# Check execution metrics
rebar3 shell -e "telemetry:list_events()"
```

### 2. Debug Mode

```erlang
% Enable debug logging
logger:set_application_level(yawl, debug).

% Enable pattern tracing
pattern_tracing:enable().

% Enable state tracing
state_tracing:enable().
```

### 3. Health Checks

```erlang
% src/health_check.erl
-module(health_check).
-export([check/0]).

check() ->
    checks = [
        {registry, fun check_registry/0},
        {state_store, fun check_state_store/0},
        {pattern_executors, fun check_pattern_executors/0},
        {database, fun check_database/0}
    ],

    Results = lists:map(fun({Name, CheckFun}) ->
        {Name, CheckFun()}
    end, checks),

    case lists:all(fun({_Name, Status}) -> Status =:= ok end, Results) of
        true -> ok;
        false -> {error, health_check_failed}
    end.

check_registry() ->
    case pattern_registry:list_patterns() of
        [] -> {error, empty_registry};
        _Patterns -> ok
    end.
```

## Conclusion

This integration guide provides a comprehensive foundation for implementing YAWL patterns within the CRE framework. The modular architecture allows for flexible integration while maintaining performance and reliability. The included examples and best practices should help developers successfully deploy and operate YAWL-based workflows in their production environments.

Remember to:
- Test thoroughly in development before production deployment
- Monitor performance and resource usage
- Implement proper error handling and recovery mechanisms
- Keep patterns and dependencies updated
- Follow security best practices for sensitive operations