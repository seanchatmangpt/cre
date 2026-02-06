# CRE Human-in-the-Loop Workflow Demo

## Y Combinator Final Review - Quick Start

### What This Demonstrates

CRE (Common Runtime Environment) now features **human-in-the-loop workflow approval** using:

1. **YAWL Patterns** - Workflow orchestration with approval checkpoints
2. **Headless Claude Integration** - LLM-powered approval decisions
3. **Monte Carlo Simulation** - Predict approval delays and bottlenecks
4. **XES Logging** - Full audit trail of all approvals

### Quick Demo (1 minute)

```bash
# Fast demo mode - auto-approves checkpoints
./scripts/demo_workflow.sh --fast
```

### Full Demo (5 minutes)

```bash
# Interactive demo - requires human approval at checkpoints
./scripts/demo_workflow.sh --full
```

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     CRE Workflow Engine                      │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐   │
│  │   Compile    │───▶│  Approval    │───▶│   Run Tests  │   │
│  │   Step       │    │  Checkpoint  │    │   Step       │   │
│  └──────────────┘    └──────────────┘    └──────────────┘   │
│                            │                                 │
│                            ▼                                 │
│                   ┌──────────────┐                          │
│                   │  Human/LLM   │                          │
│                   │   Approver   │                          │
│                   └──────────────┘                          │
│                                                               │
│  • Monte Carlo Simulation predicts delays                    │
│  • XES logs every decision                                   │
│  • Approval worker polls for pending requests                │
└─────────────────────────────────────────────────────────────┘
```

### Key Features

| Feature | Description |
|---------|-------------|
| **Approval Checkpoints** | Pause workflow at any point for approval |
| **Auto/Human/Simulated** | Three approval modes for different scenarios |
| **Claude Integration** | Headless Claude Code provides LLM approval decisions |
| **Simulation** | Monte Carlo analysis predicts approval delays |
| **Audit Trail** | XES logging captures all decisions |

### API Examples

```erlang
%% Create an approval checkpoint
{ok, Cid} = yawl_approval:create_checkpoint(
    <<"test_workflow">>,
    critical_step,
    #{required_approver => simulated, timeout => 30000}
).

%% Request approval (pauses workflow)
{ok, Decision} = yawl_approval:request_approval(Cid).

%% Or wait for decision
{ok, Decision} = yawl_approval:wait_for_approval(Cid).

%% Check status
{ok, Status} = yawl_approval:check_status(Cid).
```

### Files

| File | Purpose |
|------|---------|
| `src/yawl_approval.erl` | Approval checkpoint gen_server |
| `src/yawl_claude_bridge.erl` | Claude Code headless integration |
| `src/yawl_simulation.erl` | Monte Carlo workflow simulation |
| `src/test_approval_workflow.erl` | Demo workflow definition |
| `scripts/demo_workflow.sh` | Main demo script |
| `scripts/approval_worker.sh` | Polling approval worker |
| `scripts/approval_bot.erl` | Automated approval bot |
| `docs/workflow_dashboard.html` | Visual dashboard |

### Dashboard

Open `docs/workflow_dashboard.html` in a browser to see:
- Pending approvals
- Workflow status
- Approval history
- Real-time updates

### For Y Combinator Reviewers

**The Innovation:** CRE brings **human oversight** to automated workflows without slowing down execution. Approval checkpoints can be:
- **Auto-approved** for safe, known-good operations
- **Simulated** via Claude Code for intelligent default decisions
- **Human-reviewed** for critical operations

**Use Cases:**
- Production deployments with approval gates
- Financial transaction workflows
- Medical/healthcare decision systems
- Any workflow needing human oversight + automation

---

**Built with Erlang/OTP • YAWL Patterns • Claude Code Integration**
