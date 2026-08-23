# CRE Integration Guide

This guide covers integrating CRE (Common Runtime Environment) with external systems for workflow execution, monitoring, and data exchange.

## Table of Contents

1. [Integration Points](#integration-points)
2. [Starting a Workflow](#starting-a-workflow)
3. [Monitoring Workflow Execution](#monitoring-workflow-execution)
4. [XES Event Logging](#xes-event-logging)
5. [Telemetry Integration](#telemetry-integration)
6. [Persistence and Recovery](#persistence-and-recovery)
7. [Error Handling and Exceptions](#error-handling-and-exceptions)
8. [Example Client Code](#example-client-code)
9. [Configuration Options](#configuration-options)

---

## Integration Points

CRE provides several integration mechanisms for connecting with external systems:

### HTTP/REST API

The HTTP interface provides REST endpoints for workflow management:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/[status.json]` | GET | Engine status and metrics |
| `/history.json` | GET | Workflow execution history |
| `/api/workflow/start` | POST | Start a new workflow |
| `/api/workflow/{case_id}/cancel` | POST | Cancel a running workflow |
| `/api/workflow/{case_id}/suspend` | POST | Suspend a workflow |
| `/api/workflow/{case_id}/resume` | POST | Resume a suspended workflow |

### Native Erlang API

For Erlang/Elixir systems, use the native gen_server API:

```erlang
%% Start the YAWL engine
{ok, EnginePid} = yawl_engine:start_link().

%% Start a workflow
{ok, CaseId} = yawl_engine:start_workflow(EnginePid, Specification).

%% Get workflow state
{ok, State} = yawl_engine:get_case_state(EnginePid).
```

### Message Passing

CRE supports Erlang message passing for event-driven integration:

```erlang
%% Register as an observer
yawl_engine:register_observer(yawl_engine, self()).

%% Receive workflow events
receive
    {yawl_event, {case_started, CaseId}} -> handle_start(CaseId);
    {yawl_event, {case_completed, CaseId}} -> handle_complete(CaseId);
    {yawl_event, {workitem_started, WorkItemId}} -> handle_workitem(WorkItemId)
end.
```

### XML Marshalling

For YAWL specification integration:

```erlang
%% Parse YAWL XML specification
{ok, Spec} = yawl_schema:parse_specification("workflow.yawl").

%% Marshal data to XML
{ok, Xml} = yawl_marshal:marshal_to_xml(Data).

%% Unmarshal XML to Erlang term
{ok, Term} = yawl_marshal:unmarshal_from_xml(Xml, map).
```

---

## Starting a Workflow

Workflows can be started from various specification formats.

### From YAWL XML File

```erlang
%% Parse and start from YAWL XML specification
Spec = "/path/to/workflow.yawl",
{ok, CaseId} = yawl_engine:start_workflow(yawl_engine, Spec).
```

### From Map Specification

```erlang
%% Define workflow as a map
Spec = #{
    id => <<"approval_workflow">>,
    tasks => #{
        <<"submit">> => #{type => atomic},
        <<"review">> => #{type => atomic},
        <<"approve">> => #{type => atomic}
    },
    flows => [
        #{source => <<"submit">>, target => <<"review">>},
        #{source => <<"review">>, target => <<"approve">>}
    ]
},

{ok, CaseId} = yawl_engine:start_workflow(yawl_engine, Spec).
```

### With Options

```erlang
%% Start workflow with options
Options = #{
    cre_master => cre_master,
    observers => [self()],
    case_id => <<"custom_case_id">>
},

{ok, CaseId} = yawl_engine:start_workflow(yawl_engine, Spec, Options).
```

### HTTP Client Example

```bash
# Start workflow via HTTP
curl -X POST http://localhost:4142/api/workflow/start \
  -H "Content-Type: application/json" \
  -d '{
    "id": "approval_workflow",
    "tasks": {
      "submit": {"type": "atomic"}
    }
  }'
```

---

## Monitoring Workflow Execution

### Getting Case State

```erlang
%% Get current workflow state
{ok, State} = yawl_engine:get_case_state(Engine),

%% State contains:
%% - case_id: Workflow case identifier
%% - status: Current status (running, suspended, completed, etc.)
%% - workitems: Map of work item states
%% - data: Case variable data
%% - timestamps: Creation, start, completion times
```

### Listing All Cases

```erlang
%% List all cases with their status
{ok, Cases} = yawl_engine:list_cases(Engine),
%% Returns: [{CaseId, Status}, ...]
```

### Getting Available Work Items

```erlang
%% Get work items ready for execution
{ok, WorkItems} = yawl_engine:get_available_workitems(Engine),

%% Work item record contains:
%% - id: Work item identifier
%% - task_id: Task identifier
%% - status: enabled, started, completed, failed
%% - data: Work item data
%% - timestamps: Enable, start, completion times
```

### Engine Status

```erlang
%% Get overall engine status
{ok, Status} = yawl_engine:get_engine_status(Engine),

%% Status contains:
%% - total_cases: Total number of cases
%% - active_cases: Currently running cases
%% - suspended_cases: Suspended cases
%% - completed_cases: Completed cases
%% - observer_count: Number of registered observers
```

### HTTP Status Endpoint

```bash
# Get engine status via HTTP
curl http://localhost:4142/[status.json]

# Returns JSON:
# {
#   "total_cases": 10,
#   "active_cases": 3,
#   "completed_cases": 7
# }
```

---

## XES Event Logging

CRE supports XES (eXtensible Event Stream) format for process mining integration.

### Recording Events

```erlang
%% XES events are automatically logged for:
%% - Case start/completion
%% - Work item enablement/start/completion
%% - State transitions
%% - Error events
```

### Exporting XES Logs

```erlang
%% Export case events to XES format
{ok, XesXml} = yawl_xes:export_case(CaseId).
```

### XES Event Structure

Events contain standard XES attributes:

- `concept:name` - Event name
- `concept:instance` - Case ID
- `org:resource` - Assigned resource
- `time:timestamp` - Event timestamp
- `lifecycle:transition` - Transition type

---

## Telemetry Integration

CRE uses OpenTelemetry-compatible event tracking through `yawl_monitor`.

### Recording Metrics

```erlang
%% Record a custom metric
yawl_monitor:record_metric(<<"custom.metric">>, Value, Labels),

%% Example:
yawl_monitor:record_metric(<<"task.duration">>, 1500, #{
    task_id => <<"review">>,
    case_id => CaseId
}).
```

### Querying Metrics

```erlang
%% Get metrics for a case
Metrics = yawl_monitor:get_case_metrics(CaseId),

%% Get metrics with a query
Query = #{time_range => {StartTs, EndTs}},
FilteredMetrics = yawl_monitor:get_metrics(Query, Limit),

%% Get engine-level metrics
EngineMetrics = yawl_monitor:get_engine_metrics().
```

### Telemetry Events

Standard telemetry events emitted by CRE:

| Event | Attributes |
|-------|------------|
| `case.started` | `case_id`, `workflow_id`, `timestamp` |
| `case.completed` | `case_id`, `duration_ms`, `timestamp` |
| `case.suspended` | `case_id`, `timestamp` |
| `case.cancelled` | `case_id`, `reason`, `timestamp` |
| `workitem.enabled` | `workitem_id`, `task_id`, `case_id` |
| `workitem.started` | `workitem_id`, `task_id`, `assigned_to` |
| `workitem.completed` | `workitem_id`, `duration_ms` |
| `workitem.failed` | `workitem_id`, `error_reason` |

---

## Persistence and Recovery

CRE provides Mnesia-based persistence for workflow state.

### Enabling Persistence

```erlang
%% Enable persistence (must be called before starting workflows)
ok = yawl_engine:enable_persistence().

%% Or configure via application environment:
%% {yawl, [{persistence_enabled, true}]}
```

### Persistence Operations

```erlang
%% Save case state
ok = yawl_persistence:save_case(CaseRecord),

%% Load case state
{ok, CaseMap} = yawl_persistence:load_case(CaseId),

%% List active (non-completed) cases
{ok, ActiveCases} = yawl_persistence:list_active_cases(),

%% Delete completed case
ok = yawl_engine:delete_completed_case(CaseId).
```

### Recovery on Startup

```erlang
%% Recover active cases after engine restart
{ok, RecoveredCount} = yawl_engine:recover_active_cases().
```

### Persistence Schema

Mnesia tables used for persistence:

| Table | Purpose |
|-------|---------|
| `yawl_cases` | Workflow case records |
| `yawl_workitems` | Work item records |
| `yawl_execution_state` | Pattern execution state |

---

## Error Handling and Exceptions

### Work Item Failure

```erlang
%% Mark a work item as failed
ok = yawl_engine:fail_workitem(
    Engine,
    WorkItemId,
    #{
        reason => <<"Service unavailable">>,
        code => 503,
        timestamp => erlang:system_time(millisecond)
    }
).
```

### Exception Handling Patterns

CRE implements YAWL exception handling patterns:

```erlang
%% Error handler pattern (WHP-01)
ErrorHandler = #pattern_state{
    pattern_type = error_handler,
    subprocess => Activity,
    handler_fun => fun(Error) -> handle_error(Error) end
},

{ok, Result} = yawl_executor:execute_pattern(ErrorHandler, Input).

%% Retry pattern (WHP-02)
RetryPattern = #pattern_state{
    pattern_type = retry,
    choice_data => #{
        max_retries => 3,
        retry_delay_ms => 1000
    }
},

{ok, Result} = yawl_executor:execute_pattern(RetryPattern, Input).
```

### Compensation Patterns

```erlang
%% Compensation pattern (WHP-03)
CompPattern = #pattern_state{
    pattern_type = compensation,
    subprocess => Activity,
    compensation_handler => fun(Compensate) ->
        %% Execute compensation logic
        compensate_activity(Activity)
    end
},
```

### Error Callbacks

```erlang
%% Register observer to receive error events
yawl_engine:register_observer(yawl_engine, self()),

receive
    {yawl_event, {workitem_failed, WorkItemId, ErrorInfo}} ->
        logger:error("Work item failed: ~p ~p", [WorkItemId, ErrorInfo]),
        handle_failure(WorkItemId, ErrorInfo)
end.
```

---

## Example Client Code

### Complete Workflow Lifecycle

```erlang
%% ============================================================
%% Complete workflow integration example
%% ============================================================

-module(workflow_client).
-behaviour(gen_server).

%% API
-export([start_link/0, run_workflow/1, get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Run a workflow with the given specification
run_workflow(Spec) ->
    gen_server:call(?SERVER, {run_workflow, Spec}).

%% Get current workflow status
get_status() ->
    gen_server:call(?SERVER, get_status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start the YAWL engine
    {ok, EnginePid} = yawl_engine:start_link(),

    %% Register as observer for events
    yawl_engine:register_observer(yawl_engine, self()),

    %% Enable persistence
    ok = yawl_engine:enable_persistence(),

    {ok, #{
        engine => EnginePid,
        current_case => undefined,
        status => initialized
    }}.

handle_call({run_workflow, Spec}, _From, State) ->
    Engine = maps:get(engine, State),

    %% Start the workflow
    case yawl_engine:start_workflow(Engine, Spec) of
        {ok, CaseId} ->
            %% Monitor the workflow
            erlang:send_after(1000, self(), check_workflow),
            {reply, {ok, CaseId}, State#{current_case => CaseId, status => running}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_status, _From, State) ->
    CurrentCase = maps:get(current_case, State),
    Status = case CurrentCase of
        undefined -> #{status => no_workflow};
        _ ->
            Engine = maps:get(engine, State),
            {ok, CaseState} = yawl_engine:get_case_state(Engine),
            CaseState
    end,
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(check_workflow, State) ->
    %% Poll workflow status
    Engine = maps:get(engine, State),
    case yawl_engine:get_case_state(Engine) of
        {ok, #{status := completed} = CaseState} ->
            logger:info("Workflow completed: ~p", [CaseState]),
            {noreply, State#{status => completed}};
        {ok, #{status := failed} = CaseState} ->
            logger:error("Workflow failed: ~p", [CaseState]),
            {noreply, State#{status => failed}};
        {ok, #{status := Status}} ->
            %% Continue polling
            erlang:send_after(1000, self(), check_workflow),
            {noreply, State#{status => Status}};
        {error, Reason} ->
            logger:error("Error checking workflow: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({yawl_event, Event}, State) ->
    %% Handle workflow events
    logger:info("Workflow event: ~p", [Event]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
```

### HTTP Client (cURL examples)

```bash
#!/bin/bash
# workflow_client.sh - Bash client for CRE workflow API

CRE_BASE_URL="http://localhost:4142"

# Start a new workflow
start_workflow() {
    local spec_file="$1"
    curl -X POST "$CRE_BASE_URL/api/workflow/start" \
        -H "Content-Type: application/json" \
        -d @"$spec_file"
}

# Get workflow status
get_status() {
    local case_id="$1"
    curl "$CRE_BASE_URL/api/workflow/$case_id/status"
}

# Cancel a workflow
cancel_workflow() {
    local case_id="$1"
    curl -X POST "$CRE_BASE_URL/api/workflow/$case_id/cancel"
}

# Get engine status
get_engine_status() {
    curl "$CRE_BASE_URL/[status.json]"
}

# Main
case "$1" in
    start)
        start_workflow "$2"
        ;;
    status)
        get_status "$2"
        ;;
    cancel)
        cancel_workflow "$2"
        ;;
    engine)
        get_engine_status
        ;;
    *)
        echo "Usage: $0 {start|status|cancel|engine} [args]"
        exit 1
esac
```

### Python Client Example

```python
"""
CRE Python client for workflow integration
"""

import requests
import json
from typing import Dict, Any, Optional

class CREClient:
    """Client for interacting with CRE workflow engine."""

    def __init__(self, base_url: str = "http://localhost:4142"):
        self.base_url = base_url

    def start_workflow(self, spec: Dict[str, Any]) -> str:
        """Start a new workflow from specification."""
        response = requests.post(
            f"{self.base_url}/api/workflow/start",
            json=spec,
            headers={"Content-Type": "application/json"}
        )
        response.raise_for_status()
        result = response.json()
        return result["case_id"]

    def get_case_status(self, case_id: str) -> Dict[str, Any]:
        """Get the status of a workflow case."""
        response = requests.get(
            f"{self.base_url}/api/workflow/{case_id}/status"
        )
        response.raise_for_status()
        return response.json()

    def cancel_case(self, case_id: str) -> None:
        """Cancel a running workflow case."""
        response = requests.post(
            f"{self.base_url}/api/workflow/{case_id}/cancel"
        )
        response.raise_for_status()

    def suspend_case(self, case_id: str) -> None:
        """Suspend a running workflow case."""
        response = requests.post(
            f"{self.base_url}/api/workflow/{case_id}/suspend"
        )
        response.raise_for_status()

    def resume_case(self, case_id: str) -> None:
        """Resume a suspended workflow case."""
        response = requests.post(
            f"{self.base_url}/api/workflow/{case_id}/resume"
        )
        response.raise_for_status()

    def get_engine_status(self) -> Dict[str, Any]:
        """Get overall engine status."""
        response = requests.get(f"{self.base_url}/[status.json]")
        response.raise_for_status()
        return response.json()

    def get_history(self) -> Dict[str, Any]:
        """Get workflow execution history."""
        response = requests.get(f"{self.base_url}/history.json")
        response.raise_for_status()
        return response.json()

# Example usage
if __name__ == "__main__":
    client = CREClient()

    # Define workflow specification
    spec = {
        "id": "approval_workflow",
        "tasks": {
            "submit": {"type": "atomic"},
            "review": {"type": "atomic"},
            "approve": {"type": "atomic"}
        },
        "flows": [
            {"source": "submit", "target": "review"},
            {"source": "review", "target": "approve"}
        ]
    }

    # Start workflow
    case_id = client.start_workflow(spec)
    print(f"Started workflow: {case_id}")

    # Poll for completion
    while True:
        status = client.get_case_status(case_id)
        print(f"Status: {status['status']}")

        if status["status"] in ["completed", "cancelled", "failed"]:
            break

        import time
        time.sleep(1)

    print(f"Workflow final status: {status['status']}")
```

---

## Configuration Options

CRE configuration is managed through the `cre_config` module using `persistent_term` for O(1) access.

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CRE_COOKIE` | Inter-node cookie | (auto-generated) |
| `CRE_DB_PASSWORD` | Database password | (none) |
| `CRE_API_KEY` | API authentication key | (none) |

### Application Configuration

```erlang
%% sys.config or advanced configuration
[
    {cre, [
        {default_port, 4142},
        {status_route, "/[status.json]"},
        {history_route, "/history.json"}
    ]},
    {yawl, [
        {persistence_enabled, true},
        {stateless_checkpoint_dir, "priv/checkpoints"},
        {stateless_max_executions, 1000},
        {stateless_execution_ttl, 3600000}
    ]}
].
```

### Runtime Configuration

```erlang
%% Initialize configuration
ok = cre_config:init().

%% Get configuration values
Port = cre_config:get(cre_default_port),        % 4142
StatusRoute = cre_config:get(cre_status_route), % "/[status.json]"
PollInterval = cre_config:get(cre_client_poll_interval), % 250

%% Set custom configuration
true = cre_config:set(custom_key, custom_value).

%% Get all configuration
AllConfig = cre_config:get_all().
```

### Secrets Management

```erlang
%% Get secret from environment
{ok, Secret} = cre_config:get_secret(api_key),

%% Get secret with default value
{ok, Secret} = cre_config:get_secret(api_key, <<"default_key">>),

%% List available secrets
SecretsList = cre_config:list_secrets(),

%% Validate required secrets
ok = cre_config:validate_secrets().
```

### Web Server Configuration

```erlang
%% Default HTTP port
-define(DEFAULT_PORT, 4142).

%% Status endpoint route
-define(STATUS_ROUTE, "/[status.json]").

%% History endpoint route
-define(HISTORY_ROUTE, "/history.json").
```

### Timeout Configuration

```erlang
%% Default pattern execution timeout
yawl_timeout_default_timeout = 30000. % 30 seconds

%% Deadlock detection interval
yawl_timeout_deadlock_interval = 5000. % 5 seconds

%% Resource leak check interval
yawl_timeout_resource_check_interval = 60000. % 1 minute
```

---

## Additional Resources

- **API Documentation**: See individual module docs (`rebar3 edoc`)
- **YAWL Specification**: [YAWL Homepage](https://www.yawlfoundation.org/)
- **Pattern Reference**: `src/patterns/` for workflow pattern implementations
- **Test Examples**: `test/` for integration test examples
