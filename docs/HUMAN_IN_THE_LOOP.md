# Human-in-the-Loop Workflow with Claude Code Headless Mode

**Objective:** Integrate Claude Code headless mode for LLM-powered human-in-the-loop workflow simulation within CRE YAWL patterns.

## Executive Summary

This system enables YAWL workflows to pause at designated checkpoints and require human approval (simulated or real) before continuing. It integrates Claude Code's headless mode for intelligent approval decision-making.

## Table of Contents

- [Architecture Overview](#architecture-overview)
- [Key Components](#key-components)
- [Installation](#installation)
- [Usage Examples](#usage-examples)
- [API Reference](#api-reference)
- [Configuration](#configuration)
- [Approval Flow](#approval-flow)
- [Testing](#testing)
- [Troubleshooting](#troubleshooting)

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

### 1. `yawl_approval` - Approval Checkpoint Manager

Core module for managing approval checkpoints:

```erlang
%% Create a checkpoint
{ok, CheckpointId} = yawl_approval:create_checkpoint(
    <<"my_pattern">>,
    critical_step,
    #{
        required_approver => simulated,
        timeout => 30000,
        context => #{data => <<"important">>}
    }
).

%% Request approval
{ok, Decision} = yawl_approval:request_approval(CheckpointId).
```

**Features:**
- Create approval checkpoints with configurable timeouts
- Support for human, simulated, and auto approval modes
- Persistent decision storage
- XES logging for audit trails

### 2. `yawl_claude_bridge` - Claude Code Headless Bridge

Bridge to Claude Code's CLI headless mode:

```erlang
%% Prompt Claude with JSON schema validation
Prompt = <<"Should this workflow step proceed?">>,
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"approved">> => #{<<"type">> => <<"boolean">>},
        <<"reason">> => #{<<"type">> => <<"string">>}
    }
},

{ok, Response} = yawl_claude_bridge:prompt_claude(Prompt, Schema).
```

**Features:**
- JSON schema validation for responses
- Session continuation for multi-turn conversations
- Configurable tool permissions
- Custom model selection

### 3. `yawl_approval_middleware` - Approval Middleware

Middleware pattern for wrapping workflow execution:

```erlang
%% Wrap a pattern with approval middleware
Config = #{
    required_approver => simulated,
    on_denied => rollback
},

Wrapped = yawl_approval_middleware:with_approval(Pattern, Config).
```

**Middleware Types:**
- `approve_before/2` - Approve before execution
- `approve_after/2` - Approve after execution
- `approve_around/3` - Approve before and after
- `conditional_approval/3` - Conditional approval

### 4. `approval_worker.sh` - CLI Polling Script

Background worker that polls for pending approvals:

```bash
#!/bin/bash
# Start the approval worker
./scripts/approval_worker.sh --poll-interval 5 --node cre@localhost
```

**Features:**
- Polls for pending approval requests
- Invokes Claude Code headless mode
- Submits decisions back to CRE
- Configurable polling interval

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
