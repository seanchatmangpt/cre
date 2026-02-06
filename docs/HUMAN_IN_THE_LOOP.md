# Human-in-the-Loop Workflows with LLM Integration

**Objective:** Comprehensive guide to human-in-the-loop (HITL) workflows in CRE v0.2.1, featuring LLM-powered approval decisions, multi-modal approval flows, and seamless integration with the OpenTelemetry observability stack.

## Executive Summary

This system enables YAWL workflows to pause at designated checkpoints and require human approval (simulated or real) before continuing. It integrates Claude Code's headless mode for intelligent approval decision-making.

## Table of Contents

- [What's New in v0.2.1](#whats-new-in-v021)
- [Architecture Overview](#architecture-overview)
- [Key Components](#key-components)
- [Installation](#installation)
- [Real-World Examples](#real-world-examples)
- [Usage Examples](#usage-examples)
- [API Reference](#api-reference)
- [Configuration](#configuration)
- [Integration with OpenTelemetry](#integration-with-opentelemetry)
- [Approval Flow](#approval-flow)
- [Testing](#testing)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)

---

## What's New in v0.2.1

### 🚀 Major Enhancements

- **OpenTelemetry Integration**: All approval events are now logged to `yawl_otel_logger` for comprehensive observability
- **Multi-LLM Support**: Support for OpenAI, Claude, and local LLM models via unified interface
- **Approval Queues**: Enterprise-scale approval workflow with queue management and load balancing
- **Enhanced Middleware**: Advanced middleware patterns with automatic checkpoint creation
- **Improved Error Handling**: Comprehensive error recovery and automatic retry mechanisms
- **Web Dashboard Integration**: Real-time approval status display in the web dashboard
- **Deadline Management**: Automatic escalation and deadline enforcement for approvals

### 🔧 API Improvements

- **`cre_hil` Module**: New unified HITL API replacing legacy modules
- **Batch Approval Requests**: Submit multiple approvals for batch processing
- **Approval Analytics**: Built-in metrics for approval times, conversion rates, and bottlenecks
- **Dynamic Configuration**: Runtime approval configuration updates without restart

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     YAWL Workflow Engine                     │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │  Pattern 1   │───▶│  Approval    │───▶│  Pattern 2   │  │
│  │              │    │  Checkpoint  │    │              │  │
│  └──────────────┘    └──────┬───────┘    └──────────────┘  │
│                              │                              │
└──────────────────────────────┼──────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                    Approval Gateway                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   Pending    │  │  Decision    │  │   Timeout    │      │
│  │   Queue      │  │   Handler    │  │   Handler    │      │
│  └──────────────┘  └──────┬───────┘  └──────────────┘      │
└──────────────────────────────┼──────────────────────────────┘
                               │
                ┌──────────────┴──────────────┐
                │                             │
                ▼                             ▼
┌───────────────────────┐         ┌───────────────────────┐
│   Claude Bridge        │         │   Human Approval      │
│   (simulated mode)     │         │   (manual mode)       │
└───────────────────────┘         └───────────────────────┘
```

---

## Key Components

### 1. `cre_hil` - Human-in-the-Loop Service (v0.2.1+)

Unified HITL service providing comprehensive approval management:

```erlang
%% Start the HITL service
{ok, HilPid} = cre_hil:start_link(#{
    max_queue_size => 1000,
    timeout => 30000,
    llm_config => #{
        provider => openai,
        model => "gpt-4",
        api_key => <<"your-api-key">>
    }
}).

%% Create an approval request
{ok, RequestId} = cre_hil:create_approval_request(#{
    workflow_id => <<"order_processing">>,
    task_name => "fraud_check",
    description => "High-value order requires approval",
    priority => high,
    metadata => #{
        order_id => <<"ORD-12345">>,
        amount => 99999,
        customer_id => <<"CUST-678">>
    }
}).
```

**Features:**
- Multi-modal approval support (human, LLM, automated rules)
- Queue-based load balancing for high-volume scenarios
- Dynamic timeout management based on priority
- Automatic retry with exponential backoff
- Integration with OpenTelemetry logging
- Web dashboard real-time updates

### 2. `cre_llm_integration` - Multi-LLM Integration (v0.2.1+)

Unified LLM integration supporting multiple providers:

```erlang
%% Configure LLM provider
Config = #{
    provider => openai,
    model => "gpt-4",
    api_key => <<"sk-...">>,
    max_tokens => 1000,
    temperature => 0.3,
    timeout => 15000
}.

%% Create approval decision prompt
Context = #{
    workflow_id => <<"fraud_detection">>,
    transaction_data => #{
        amount => 50000,
        location => "unusual_country",
        velocity => "high_frequency"
    },
    customer_history => "high_risk"
},

{ok, Decision} = cre_llm_integration:request_approval(Config, Context, #{
    prompt_template => "fraud_check_prompt",
    required_approvals => 1
}).
```

**Features:**
- Support for OpenAI, Claude, and local models
- Unified API across different providers
- Prompt template management
- Fallback mechanisms for provider failures
- Cost tracking and usage analytics
- Custom model temperature and parameter tuning

### 3. `cre_approval_middleware` - Enhanced Approval Middleware (v0.2.1+)

Advanced middleware with automatic checkpoint detection and routing:

```erlang
%% Configure approval middleware with rules
Config = #{
    auto_detection => true,
    rules => [
        #{
            condition => fun(Context) ->
                maps:get(amount, Context, 0) > 10000
            end,
            approval_type => llm,
            priority => high
        },
        #{
            condition => fun(Context) ->
                maps:get(customer_type, Context) == "enterprise"
            end,
            approval_type => human,
            chain_approvals => 2
        }
    ],
    escalation => #{
        timeout => 30000,
        escalation_level => 2,
        notify => [email, slack]
    }
}.

%% Wrap pattern with enhanced middleware
Wrapped = cre_approval_middleware:with_auto_approval(Pattern, Config).
```

**Features:**
- Automatic approval type detection based on rules
- Multi-level approval chains for enterprise workflows
- Automatic escalation and timeout handling
- Integration with notification systems
- Performance metrics collection
- Real-time dashboard integration

### 4. `cre_approval_worker` - Enterprise Approval Worker (v0.2.1+)

Robust background worker with enterprise features:

```bash
#!/bin/bash
# Start the enhanced approval worker
./scripts/cre_approval_worker.sh \
    --node cre@localhost \
    --queue high_priority \
    --llm-provider openai \
    --max-concurrent 10 \
    --metrics-port 9091

# Or run in distributed mode
./scripts/cre_approval_worker.sh \
    --distributed \
    --worker-group approval_cluster \
    --health-check-interval 30
```

**Features:**
- Distributed worker clusters for high throughput
- Load balancing across multiple workers
- Health monitoring and auto-recovery
- Metrics collection via Prometheus
- Multi-LLM provider support
- Graceful shutdown and checkpoint recovery

---

## Installation

### Prerequisites

1. **CRE YAWL System** - Already installed and running
2. **Claude Code CLI** - Installed and available in PATH
3. **Erlang/OTP** - Version 25 or higher

### Install Claude Code CLI

```bash
# Using npm (recommended)
npm install -g @anthropic-ai/claude-code

# Or using Homebrew (macOS)
brew install claude-code

# Verify installation
claude --version
```

### Configure CRE

Add the approval modules to the CRE supervisor:

```erlang
%% In cre_sup.erl
ApprovalSpec = #{
    id => yawl_approval,
    start => {yawl_approval, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [yawl_approval]
},
```

### Compile Modules

```bash
cd /Users/sac/cre
rebar3 compile
```

---

## Real-World Examples

### Example 1: E-commerce Order Approval Workflow

```erlang
%% Configure order processing workflow with fraud detection
OrderWorkflow = cre_yawl:new_workflow(<<"ecommerce_order">>),

%% Add tasks with different approval requirements
Task1 = cre_yawl:add_task(OrderWorkflow, <<"inventory_check">>,
                        [{type, atomic}, {module, inventory_check}]),
Task2 = cre_yawl:add_task(OrderWorkflow, <<"fraud_detection">>,
                        [{type, atomic}, {module, fraud_detection},
                         {approval_required, true},
                         {approval_config, #{
                             threshold => 0.8,
                             max_amount => 5000,
                             auto_approve => true
                         }}]),
Task3 = cre_yawl:add_task(OrderWorkflow, <<"customer_verification">>,
                        [{type, atomic}, {module, customer_verification}]),
Task4 = cre_yawl:add_task(OrderWorkflow, <<"fraud_review>>,
                        [{type, atomic}, {module, fraud_review},
                         {approval_required, true},
                         {approval_config, #{
                             required_approver => human,
                             escalation_timeout => 300000,
                             notify => [fraud_team, management]
                         }}]),

%% Connect tasks in sequence
OrderWorkflow = cre_yawl:connect(OrderWorkflow, <<"inventory_check">>, <<"fraud_detection">>),
OrderWorkflow = cre_yawl:connect(OrderWorkflow, <<"fraud_detection">>, <<"customer_verification">>),
OrderWorkflow = cre_yawl:connect(OrderWorkflow, <<"customer_verification">>, <<"fraud_review">>),

%% Execute workflow with enhanced approval handling
{ok, Result} = cre_yawl:execute_workflow(OrderWorkflow, #{
    customer_id => <<"CUST-123">>,
    order_total => 12500,
    payment_method => credit_card,
    items => [<<"premium_item">>, <<">>luxury_item">>],
    location => {high_risk_country, "Unusual Location"}
}).
```

### Example 2: Financial Compliance Approval

```erlang
%% Configure financial compliance workflow
ComplianceWorkflow = cre_yawl:new_workflow(<<"financial_compliance">>),

%% Add tasks with escalating approval requirements
Task1 = cre_yawl:add_task(ComplianceWorkflow, <<"risk_assessment">>,
                        [{type, atomic}, {module, risk_assessment},
                         {approval_config, #{
                             type => automated,
                             rules => #{
                                 risk_level => low,
                                 auto_approve => true
                             }
                         }}]),
Task2 = cre_yawl:add_task(ComplianceWorkflow, <<"aml_check">>,
                        [{type, atomic}, {module, aml_check},
                         {approval_config, #{
                             type => llm,
                             provider => openai,
                             prompt_template => "aml_review",
                             model => "gpt-4-turbo"
                         }}]),
Task3 = cre_yawl:add_task(ComplianceWorkflow, <<"manual_review">>,
                        [{type, atomic}, {module, manual_review},
                         {approval_config, #{
                             type => human,
                             required_approvals => 2,
                             chain_approvals => true,
                             timeout => 86400000,  % 24 hours
                             escalation => #{
                                 level_1 => compliance_officer,
                                 level_2 => compliance_director,
                                 level_3 => cfo
                             }
                         }}]),

%% Connect workflow
ComplianceWorkflow = cre_yawl:connect(ComplianceWorkflow, <<"risk_assessment">>, <<"aml_check">>),
ComplianceWorkflow = cre_yawl:connect(ComplianceWorkflow, <<"aml_check">>, <<"manual_review">>),

%% Execute with enhanced monitoring
{ok, Result} = cre_yawl:execute_workflow(ComplianceWorkflow, #{
    transaction_id => <<"TXN-789012">>,
    amount => 1000000,
    customer_id => <<"CUST-HIGH-RISK">>,
    jurisdiction => "offshore",
    transaction_type => wire_transfer
}).
```

### Example 3: IT Change Management Approval

```erlang
%% Configure IT change management workflow
ChangeWorkflow = cre_yawl:new_workflow(<<"it_change_management">>),

%% Add tasks with different approval levels
Task1 = cre_yawl:add_task(ChangeWorkflow, <<"change_assessment">>,
                        [{type, atomic}, {module, change_assessment},
                         {approval_config, #{
                             type => automated,
                             rules => #{
                                 change_impact => low,
                                 service => non_production,
                                 auto_approve => true
                             }
                         }}]),
Task2 = cre_yawl:add_task(ChangeWorkflow, <<"stakeholder_review">>,
                        [{type, atomic}, {module, stakeholder_review},
                         {approval_config, #{
                             type => human,
                             required_approvals => 1,
                             voting_required => true,
                             timeout => 432000000,  % 5 days
                             quorum => 0.6
                         }}]),
Task3 = cre_yawl:add_task(ChangeWorkflow, <<"change_execution">>,
                        [{type, atomic}, {module, change_execution},
                         {approval_config, #{
                             type => human,
                             required_approvals => 2,
                             staggered_approvals => true,
                             emergency_override => true
                         }}]),

%% Connect workflow
ChangeWorkflow = cre_yawl:connect(ChangeWorkflow, <<"change_assessment">>, <<"stakeholder_review">>),
ChangeWorkflow = cre_yawl:connect(ChangeWorkflow, <<"stakeholder_review">>, <<"change_execution">>),

%% execute with change window constraints
{ok, Result} = cre_yawl:execute_workflow(ChangeWorkflow, #{
    change_id => <<"CHANGE-456">>,
    description => "Database schema upgrade",
    impact => "medium",
    services => ["payment_processing", "reporting"],
    scheduled_time => {{2024, 6, 15}, {02, 00, 00}},
    change_window => {180, 300}  % 3-5 hour window
}).
```

### Example 4: Healthcare Patient Treatment Approval

```erlang
%% Configure healthcare treatment workflow
TreatmentWorkflow = cre_yawl:new_workflow(<<"healthcare_treatment">>),

%% Add tasks with regulatory compliance
Task1 = cre_yawl:add_task(TreatmentWorkflow, <<"initial_diagnosis">>,
                        [{type, atomic}, {module, diagnosis}]),
Task2 = cre_yawl:add_task(TreatmentWorkflow, <<"treatment_proposal">>,
                        [{type, atomic}, {module, treatment_planner},
                         {approval_config, #{
                             type => llm,
                             provider => claude,
                             prompt_template => "treatment_review",
                             model => "claude-3-sonnet-20240229",
                             require_consensus => true,
                             timeout => 7200000  % 2 hours
                         }}]),
Task3 = cre_yawl:add_task(TreatmentWorkflow, <<"patient_consent">>,
                        [{type, atomic}, {module, patient_consent},
                         {approval_config, #{
                             type => human,
                             required_approvals => 1,
                             digital_signature => true,
                             audit_trail => true,
                             retention_period => 730  % 2 years
                         }}]),
Task4 = cre_yawl:add_task(TreatmentWorkflow, <<"treatment_execution">>,
                        [{type, atomic}, {module, treatment_executor},
                         {approval_config, #{
                             type => automated,
                             require_secondary_approval => true,
                             emergency_protocol => true
                         }}]),

%% Connect workflow
TreatmentWorkflow = cre_yawl:connect(TreatmentWorkflow, <<"initial_diagnosis">>, <<"treatment_proposal">>),
TreatmentWorkflow = cre_yawl:connect(TreatmentWorkflow, <<"treatment_proposal">>, <<"patient_consent">>),
TreatmentWorkflow = cre_yawl:connect(TreatmentWorkflow, <<"patient_consent">>, <<"treatment_execution">>),

%% Execute with HIPAA compliance
{ok, Result} = cre_yawl:execute_workflow(TreatmentWorkflow, #{
    patient_id => <<"PATIENT-789">>,
    treatment_type => "experimental_therapy",
    risk_level => high,
    alternative_treatments => ["standard_therapy", "clinical_trial"],
    physician_notes => "Patient has no other viable options",
    emergency => false
}).
```

---

## Usage Examples

### Example 1: Simple Approval Checkpoint

```erlang
%% Create workflow with approval
Workflow = yawl_pattern_reference:sequence([<<"step1">>, <<"step2">>, <<"step3">>]),

%% Execute with approval before step2
ApprovalConfig = #{
    checkpoint_before => <<"step2">>,
    required_approver => simulated,
    timeout => 30000
},

%% Wrap with approval
Wrapped = yawl_approval_middleware:with_approval(Workflow, ApprovalConfig),

%% Execute
Result = yawl_executor:execute_pattern(Wrapped, #{}).
```

### Example 2: Multi-Turn Approval

```erlang
%% Start approval session
{ok, SessionId} = yawl_claude_bridge:start_session(#{
    workflow => cancel_case,
    context => #{
        activities => [<<"critical_task">>],
        reason => <<"Production deployment">>
    }
}),

%% First round: request approval
{ok, Response1} = yawl_claude_bridge:continue_session(
    SessionId,
    <<"Please review this cancellation request">>
),

%% Second round: provide more info
case Response1 of
    #{approved := false, needs_info := true} ->
        {ok, Response2} = yawl_claude_bridge:continue_session(
            SessionId,
            <<"Additional context: this is emergency maintenance">>
        )
end.
```

### Example 3: Human Approval via CLI

```bash
# Terminal 1: Start workflow with approval checkpoint
erl -pa _build/default/lib/cre/ebin -eval "
    Pattern = yawl_pattern_reference:cancel_case(
        [fun(_) -> ok end, fun(_) -> ok end],
        fun(_) -> false end
    ),
    {ok, Cpid} = yawl_approval:start_link(),
    {ok, CheckpointId} = yawl_approval:create_checkpoint(
        Pattern, cancel_case, #{required_approver => human}
    ),
    io:format('Checkpoint: ~p~n', [CheckpointId]),
    timer:sleep(infinity).
"

# Terminal 2: Approve the request
erl -pa _build/default/lib/cre/ebin -eval "
    yawl_approval:approve(
        <<"<checkpoint_id>">>,
        human_cli,
        <<"Approved for emergency fix">>
    ),
    init:stop().
"
```

### Example 4: Auto-Approval with Rules

```erlang
%% Auto-approve based on rules
AutoConfig = #{
    required_approver => auto,
    rules => #{
        max_cost => 1000,
        allowed_patterns => [sequence, parallel_split],
        time_window => {0, 18}  % Only 6am-6pm
    }
},

Wrapped = yawl_approval_middleware:with_approval(Workflow, AutoConfig).
```

### Example 5: Middleware Chain

```erlang
%% Chain multiple approval checkpoints
Configs = [
    #{required_approver => auto, step => <<"initialization">>},
    #{required_approver => simulated, step => <<"critical_section">>},
    #{required_approver => human, step => <<"finalization">>}
],

Wrapped = yawl_approval_middleware:chain_approval(Workflow, Configs).
```

---

## API Reference

### `yawl_approval` Module

#### `create_checkpoint/3`

```erlang
-spec create_checkpoint(binary(), atom(), map()) ->
    {ok, checkpoint_id()} | {error, term()}.
```

Creates a new approval checkpoint.

**Parameters:**
- `PatternId` - The ID of the pattern requiring approval
- `StepName` - The name of the step requiring approval
- `Options` - Map of approval options

**Options:**
- `required_approver` - `human | simulated | auto` (default: `simulated`)
- `timeout` - milliseconds or `infinity` (default: `30000`)
- `approval_schema` - JSON schema for validation
- `context` - Additional context data
- `metadata` - User metadata

#### `request_approval/1`

```erlang
-spec request_approval(checkpoint_id()) ->
    {ok, #approval_decision{}} | {error, term()}.
```

Requests approval for a checkpoint.

#### `approve/3`

```erlang
-spec approve(checkpoint_id(), term(), binary()) ->
    ok | {error, term()}.
```

Approves a checkpoint.

#### `deny/3`

```erlang
-spec deny(checkpoint_id(), term(), binary()) ->
    ok | {error, term()}.
```

Denies a checkpoint.

#### `check_status/1`

```erlang
-spec check_status(checkpoint_id()) ->
    {ok, approval_status()} | {error, term()}.
```

Checks the status of an approval checkpoint.

#### `simulate_approval/2`

```erlang
-spec simulate_approval(checkpoint_id(), map()) ->
    {ok, #approval_decision{}} | {error, term()}.
```

Simulates approval using Claude Code headless mode.

### `yawl_claude_bridge` Module

#### `prompt_claude/2,3`

```erlang
-spec prompt_claude(binary(), json_schema()) -> bridge_result().
-spec prompt_claude(binary(), json_schema(), map()) -> bridge_result().
```

Executes a Claude Code prompt and gets JSON response.

**Options:**
- `timeout` - milliseconds (default: `30000`)
- `allowed_tools` - list of allowed tool patterns
- `max_tokens` - maximum response tokens (default: `4096`)
- `model` - claude model to use (default: `auto`)

#### `start_session/1`

```erlang
-spec start_session(map()) -> session_result().
```

Starts a new Claude Code approval session.

#### `continue_session/2`

```erlang
-spec continue_session(session_id(), binary()) -> bridge_result().
```

Continues an existing session with a new prompt.

#### `end_session/1`

```erlang
-spec end_session(session_id()) -> ok | {error, term()}.
```

Ends a Claude Code session.

### `yawl_approval_middleware` Module

#### `with_approval/2,3`

```erlang
-spec with_approval(term(), approval_config()) -> #approval_wrapped{}.
-spec with_approval(term(), approval_config(), atom()) -> #approval_wrapped{}.
```

Wraps a pattern with approval middleware.

#### `approve_before/2`

```erlang
-spec approve_before(#approval_wrapped{}, approval_config()) ->
    #approval_wrapped{}.
```

Creates middleware that approves before execution.

#### `approve_after/2`

```erlang
-spec approve_after(#approval_wrapped{}, approval_config()) ->
    #approval_wrapped{}.
```

Creates middleware that approves after execution.

#### `approve_around/3`

```erlang
-spec approve_around(#approval_wrapped{}, approval_config(), middleware_fun()) ->
    #approval_wrapped{}.
```

Creates middleware that approves around execution.

---

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CRE_NODE` | Erlang node name | `cre@localhost` |
| `CRE_COOKIE` | Erlang cookie | - |
| `CLAUDE_CMD` | Path to Claude CLI | `claude` |
| `CRE_APPROVAL_POLL_INTERVAL` | Poll interval in seconds | `5` |
| `CRE_APPROVAL_LOG_FILE` | Worker log file | `/tmp/cre_approval_worker.log` |

### Erlang Configuration

```erlang
%% In sys.config or app.config
{yawl_approval, [
    {default_approver, simulated},
    {default_timeout, 30000},
    {max_pending_approvals, 100},
    {enable_xes_logging, true},
    {claude_command, "/usr/local/bin/claude"}
]}.
```

---

## Integration with OpenTelemetry

### Overview

All approval workflows in CRE v0.2.1 integrate seamlessly with the OpenTelemetry observability stack via the `yawl_otel_logger` module. This provides comprehensive monitoring, tracing, and analytics for human-in-the-loop processes.

### Automatic Logging

All approval events are automatically logged with structured metadata:

```erlang
%% Approval request creation is automatically logged
yawl_otel_logger:log_checkpoint(
    CheckpointId,
    PatternId,
    TaskName,
    Approver,
    Context,
    #{priority => high}
).

%% Approval decisions are logged with timing
yawl_otel_logger:log_approval(
    CheckpointId,
    Approver,
    Decision,
    #{response_time => 1500, comments => "Manual review required"}
).
```

### Metrics Collection

The system automatically collects key performance metrics:

```erlang
%% Get approval analytics
Metrics = yawl_otel_logger:get_stats(),
%% #{approval_count => 125,
%%   avg_response_time => 4500,
%%   approval_rate => 0.85,
%%   pending_approvals => 15}

%% Get workflow-specific metrics
WorkflowMetrics = yawl_otel_logger:get_events_by_trace(TraceId),
```

### Distributed Tracing

Approval workflows are traced across the entire system:

```erlang
%% Start workflow with trace context
{ok, TraceContext} = yawl_otel_logger:start_workflow_trace(<<"order_processing">>),

%% All approval activities inherit the trace
ApprovalTrace = yawl_otel_logger:checkpoint_trace(CheckpointId, TraceContext),

%% Export traces for analysis
{ok, TraceData} = yawl_otel_logger:export_traces("approval_traces.json").
```

### Dashboard Integration

Approval metrics are visualized in the web dashboard:

```erlang
%% Configure dashboard to show approval analytics
DashboardConfig = #{
    show_approval_queue => true,
    show_response_time_chart => true,
    show_approval_heatmap => true,
    real_time_updates => true
}.
```

### Performance Monitoring

Monitor approval performance with real-time alerts:

```erlang
%% Configure performance thresholds
PerformanceConfig = #{
    max_response_time => 30000,  % 30 seconds
    min_approval_rate => 0.80,   % 80% approval rate
    max_queue_size => 50
}.

%% Set up automatic alerts
AlertFun = fun(Metrics) ->
    case Metrics of
        #{avg_response_time := RT} when RT > 30000 ->
            logger:warning("Slow approval response detected: ~p ms", [RT]);
        #{approval_rate := Rate} when Rate < 0.80 ->
            logger:warning("Low approval rate detected: ~p", [Rate])
    end
end.
```

---

## Approval Flow Diagram

```
┌─────────────────┐
│  YAWL Pattern   │
│   Execution     │
└────────┬────────┘
         │
         ▼
┌─────────────────────────┐
│  Approval Checkpoint    │
│  - Pause Execution      │
│  - Save State           │
│  - Generate Prompt      │
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│  Claude Bridge          │
│  - Invoke headless      │
│  - Parse JSON response  │
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│  Decision Handler       │
│  - Approved? Continue   │
│  - Denied? Rollback     │
│  - Timeout? Cancel      │
└────────┬────────────────┘
         │
         ▼
┌─────────────────┐
│  Resume / Stop  │
└─────────────────┘
```

---

## JSON Schema Examples

### Basic Approval Schema

```json
{
  "type": "object",
  "properties": {
    "approved": {"type": "boolean"},
    "reason": {"type": "string"}
  },
  "required": ["approved"]
}
```

### Complex Approval with Modifications

```json
{
  "type": "object",
  "properties": {
    "approved": {"type": "boolean"},
    "modifications": {
      "type": "object",
      "properties": {
        "timeout": {"type": "integer"},
        "parameters": {"type": "object"}
      }
    },
    "comment": {"type": "string"}
  }
}
```

---

## Testing

### Run Unit Tests

```bash
cd /Users/sac/cre
rebar3 eunit --module=yawl_approval_test
```

### Run Integration Tests

```bash
rebar3 ct --suite=yawl_approval_SUITE
```

### Manual Testing

```erlang
%% Start Erlang node
erl -pa _build/default/lib/cre/ebin -setcookie cre -name test@localhost

%% Connect to CRE node
erlang:set_cookie(cre),
net_kernel:connect_node('cre@localhost'),

%% Test approval creation
{ok, Pid} = yawl_approval:start_link(),
{ok, Cid} = yawl_approval:create_checkpoint(
    <<"test">>, test_step,
    #{required_approver => auto}
),

%% Test approval request
{ok, Decision} = yawl_approval:request_approval(Cid),
io:format("Decision: ~p~n", [Decision]).
```

---

## Troubleshooting

### Issue: Claude CLI Not Found

**Error:** `"Claude CLI not found at: claude"`

**Solution:**
1. Install Claude Code CLI
2. Set the `CLAUDE_CMD` environment variable
3. Or configure in Erlang:

```erlang
yawl_claude_bridge:set_claude_command("/path/to/claude").
```

### Issue: Approval Timeout

**Error:** `{error, approval_timeout}`

**Solutions:**
1. Increase the timeout value:
```erlang
Options = #{timeout => 60000},  % 60 seconds
```
2. Check network connectivity to Claude API
3. Verify Claude CLI authentication

### Issue: JSON Validation Failed

**Error:** `{error, {validation_failed, Reason}}`

**Solutions:**
1. Verify the JSON schema format
2. Check Claude response structure
3. Use a simpler schema for testing

### Issue: CRE Node Not Available

**Error:** `"CRE node not available"`

**Solutions:**
1. Ensure CRE is running: `cre status`
2. Check node name matches: `erl -name cre@localhost`
3. Verify cookie: `erlang:set_cookie(cre)`

---

## Success Criteria

- [x] Approval checkpoint module compiles
- [x] Claude Code headless bridge works
- [x] Workflow pauses and resumes correctly
- [x] JSON schema validation working
- [x] Session continuation for multi-turn approvals
- [x] Both simulated and real approval modes work
- [x] Test suite passes
- [x] Documentation complete

---

## Best Practices

### Design Patterns

#### 1. Tiered Approval Architecture

Implement approval hierarchies based on risk and authority:

```erlang
%% Define approval tiers based on transaction characteristics
ApprovalTiers = #{
    low => #{
        type => automated,
        rules => #{amount =< 1000, customer_type => standard},
        auto_approve => true
    },
    medium => #{
        type => llm,
        rules => #{amount =< 10000, risk_score =< 0.7},
        required_approvals => 1,
        timeout => 300000
    },
    high => #{
        type => human,
        rules => #{amount > 10000, risk_score > 0.7},
        required_approvals => 2,
        escalation_timeout => 86400000,
        audit_trail => true
    }
}.
```

#### 2. Circuit Breaker Pattern

Prevent cascading failures in approval systems:

```erlang
%% Implement circuit breaker for external approval services
CircuitBreaker = #{
    failure_threshold => 5,
    recovery_timeout => 60000,
    max_concurrent_requests => 10,
    fallback => fun() -> {error, service_unavailable} end
}.
```

#### 3. Idempotency Keys

Ensure safe retry mechanisms for approval requests:

```erlang
%% Generate idempotency key for approval requests
IdempotencyKey = crypto:strong_rand_bytes(16),
Timestamp = erlang:system_time(millisecond),

%% Include in approval request
ApprovalRequest = #{
    idempotency_key => IdempotencyKey,
    timestamp => Timestamp,
    workflow_id => <<"order_processing">>,
    request_data => OrderData
}.
```

### Performance Optimization

#### 1. Batch Processing

Process multiple approvals in batches for efficiency:

```erlang
%% Submit batch approval requests
BatchRequests = [
    #{workflow_id => <<"ord_123">>, amount => 1500, priority => medium},
    #{workflow_id => <<"ord_124">>, amount => 2500, priority => high},
    #{workflow_id => <<"ord_125">>, amount => 800, priority => low}
],

{ok, BatchResults} = cre_hil:process_batch_requests(BatchRequests).
```

#### 2. Caching

Cache frequently accessed approval templates:

```erlang
%% Cache approval templates for performance
ets:insert(approval_templates, {
    "fraud_check_template",
    #{
        provider => openai,
        model => "gpt-4-turbo",
        prompt_template => fraud_check,
        timeout => 30000,
        retry_policy => exponential
    }
}).
```

#### 3. Asynchronous Processing

Use asynchronous processing for long-running approvals:

```erlang
%% Submit approval request and get callback
{ok, RequestId} = cre_hil:create_async_approval(Request, fun(Result) ->
    handle_approval_result(RequestId, Result)
end).

%% Handle result asynchronously
handle_approval_result(RequestId, Result) ->
    case Result of
        {approved, Context} ->
            continue_workflow(RequestId, Context);
        {denied, Reason} ->
            notify_workflow_failure(RequestId, Reason)
    end.
```

### Security Considerations

#### 1. Authorization and Validation

Implement strict authorization for approval actions:

```erlang
%% Validate approval permissions
is_approver_authorized(Approver, RequestType, Context) ->
    case check_user_permissions(Approver, RequestType) of
        true ->
            validate_approval_context(Context);
        false ->
            {error, unauthorized}
    end.
```

#### 2. Audit Trail

Maintain comprehensive audit trails for compliance:

```erlang
%% Log all approval activities with audit trail
log_approval_activity(ActivityType, RequestId, Approver, Decision, Metadata) ->
    AuditEntry = #{
        timestamp => erlang:system_time(millisecond),
        activity_type => ActivityType,
        request_id => RequestId,
        approver => Approver,
        decision => Decision,
        metadata => Metadata,
        ip_address => get_client_ip(),
        user_agent => get_user_agent()
    },
    yawl_otel_logger:log_audit(AuditEntry).
```

#### 3. Data Encryption

Encrypt sensitive approval data:

```erlang
%% Encrypt sensitive approval data
encrypt_approval_data(PlainData, Key) ->
    crypto:block_encrypt(aes_cbc_256, Key, PlainData).

%% Decrypt for processing
decrypt_approval_data(EncryptedData, Key) ->
    crypto:block_decrypt(aes_cbc_256, Key, EncryptedData).
```

### Monitoring and Alerting

#### 1. Key Metrics Monitoring

Monitor critical approval metrics:

```erlang
%% Define key metrics to monitor
CriticalMetrics = [
    {approval_response_time, {max, 30000}},
    {approval_success_rate, {min, 0.90}},
    {pending_approvals, {max, 100}},
    {error_rate, {max, 0.05}}
].

%% Set up alerts
MetricsFun = fun(Metrics) ->
    lists:foreach(fun({Metric, {Threshold, Type}}) ->
        Value = maps:get(Metric, Metrics, 0),
        case check_threshold(Value, Threshold, Type) of
            true ->
                trigger_alert(Metric, Value, Threshold);
            false ->
                ok
        end
    end, CriticalMetrics)
end.
```

#### 2. SLA Monitoring

Implement Service Level Agreements:

```erlang
%% Define SLA levels for different approval types
SLAPolicies = #{
    emergency => #{
        max_response_time => 300000,  % 5 minutes
        max_approvals => 3,
        notification_level => critical
    },
    high_priority => #{
        max_response_time => 1800000,  % 30 minutes
        max_approvals => 2,
        notification_level => high
    },
    standard => #{
        max_response_time => 86400000,  % 24 hours
        max_approvals => 1,
        notification_level => normal
    }
}.
```

### Error Handling and Recovery

#### 1. Retry Policies

Implement intelligent retry mechanisms:

```erlang
%% Configure retry policies
RetryPolicy = #{
    max_retries => 3,
    base_delay => 1000,
    max_delay => 30000,
    backoff_factor => 2,
    retryable_errors => [timeout, network_error, rate_limited]
}.

%% Execute with retry
execute_with_retry(Fun, RetryPolicy) ->
    execute_with_retry(Fun, RetryPolicy, 0).
```

#### 2. Circuit Breaker State Management

Monitor and manage circuit breaker states:

```erlang
%% Track circuit breaker state
CircuitState = monitor_circuit_breaker(#{
    failure_count => 0,
    state => closed,
    last_failure_time => undefined,
    recovery_attempts => 0
}).

%% Update state on failures
update_circuit_state(CircuitState, failure) ->
    case CircuitState of
        #{state := closed, failure_count := Count} when Count >= 5 ->
            CircuitState#{state => open, failure_count => Count + 1};
        #{state := half_open} ->
            CircuitState#{state => open, recovery_attempts => 0};
        _ ->
            CircuitState#{failure_count := maps:get(failure_count, CircuitState, 0) + 1}
    end.
```

### Testing Strategies

#### 1. Unit Testing

Test individual approval components:

```erlang
%% Test approval decision logic
approval_decision_test_() ->
    [
        {"Automated approval for low-risk", ?_assertEqual(
            {approved, auto},
            make_approval_decision(low_risk_context, auto)
        )},
        {"Manual approval required for high-risk", ?_assertEqual(
            {pending, human},
            make_approval_decision(high_risk_context, auto)
        )}
    ].
```

#### 2. Integration Testing

Test end-to-end approval workflows:

```erlang
%% Test complete approval workflow
integration_test_() ->
    [
        {"End-to-end approval process",
            fun() ->
                Workflow = create_test_workflow(),
                {ok, Result} = cre_yawl:execute_workflow(Workflow, test_context()),
                ?assertMatch({ok, _}, Result)
            end}
    ].
```

#### 3. Performance Testing

Test approval system performance:

```erlang
%% Test approval throughput
performance_test_() ->
    [
        {"High volume approval processing",
            fun() ->
                Requests = generate_test_requests(1000),
                {Time, Results} = timer:tc(fun() ->
                    cre_hil:process_batch_requests(Requests)
                end),
                ?assert(length(Results) =:= 1000),
                ?assert(Time < 10000)  % Should complete in <10s
            end}
    ].
```

---

## Notes

- **LLM-simulated approval** uses Claude Code to make decisions based on context
- **Human approval mode** waits for external approval via API or CLI
- **Session continuation** supports iterative approval conversations
- **All approvals** are logged to XES for audit trail
- **Timeout handling** ensures workflows don't hang indefinitely

---

## References

- [Claude Code Documentation](https://docs.anthropic.com/claude-code)
- [YAWL Workflow Patterns](https://www.yawlfoundation.org/)
- [CRE YAWL Implementation](./YAWL_PATTERNS_REFERENCE.md)
