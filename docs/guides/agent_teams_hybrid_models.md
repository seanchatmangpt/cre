# Agent Teams: Hybrid Model Strategy

**Implementation with Opus, Communication with Haiku**

This guide documents a cost-optimized pattern for Claude Code Agent Teams that uses different models for different purposes.

## Overview

| Task Type | Model | Cost Ratio | Why |
|-----------|-------|------------|-----|
| **Implementation** | Opus | 10x | Complex reasoning, code generation |
| **Communication** | Haiku | 1x | Simple messaging, status updates |
| **Coordination** | Haiku | 1x | Task assignment, synthesis |

**Result: ~60-70% token cost reduction** compared to all-Opus teams.

---

## Model Pricing Reference

| Model | Input ($/MTok) | Output ($/MTok) | Best For |
|-------|---------------|-----------------|----------|
| Haiku | $0.50 | $2.50 | Fast tasks, messaging, summaries |
| Sonnet | $3.00 | $15.00 | Balanced workloads |
| Opus | $5.00 | $25.00 | Complex reasoning, architecture |

Source: [Claude API Pricing Guide 2026](https://aifreeapi.com/en/posts/claude-api-pricing-per-million-tokens)

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        TEAM LEAD (Haiku)                        │
│                  Coordination, messaging, synthesis              │
│                         ~$0.50/MTok input                       │
└───────────────────────────────┬─────────────────────────────────┘
                                │
            ┌───────────────────┼───────────────────┐
            │                   │                   │
            ▼                   ▼                   ▼
┌───────────────────┐ ┌───────────────────┐ ┌───────────────────┐
│  IMPLEMENTER #1   │ │  IMPLEMENTER #2   │ │   RESEARCHER      │
│     (Opus)        │ │     (Opus)        │ │     (Haiku)       │
│ Code, tests, bugs │ │ Code, tests, bugs │ │  Docs, analysis   │
│  ~$5.00/MTok      │ │  ~$5.00/MTok      │ │  ~$0.50/MTok      │
└───────────────────┘ └───────────────────┘ └───────────────────┘
```

---

## Implementation Pattern

### 1. Spawn Implementation Teammates with Opus

```javascript
// Implementation work - use Opus for complex reasoning
Task({
  name: "implementer-backend",
  model: "opus",  // Complex code generation
  prompt: "Implement the REST API endpoints...",
  subagent_type: "coder",
  team_name: "my-project"
})

Task({
  name: "implementer-frontend",
  model: "opus",  // Complex code generation
  prompt: "Build the React components...",
  subagent_type: "coder",
  team_name: "my-project"
})
```

### 2. Spawn Research/Communication Teammates with Haiku

```javascript
// Research and documentation - use Haiku
Task({
  name: "researcher",
  model: "haiku",  // Fast, cheap for research
  prompt: "Research best practices for...",
  subagent_type: "researcher",
  team_name: "my-project"
})

Task({
  name: "documenter",
  model: "haiku",  // Simple summarization
  prompt: "Document the API endpoints...",
  subagent_type: "docs-writer",
  team_name: "my-project"
})
```

### 3. Team Lead Uses Haiku for Coordination

```javascript
// Team lead should run on Haiku for cost efficiency
TeamCreate({
  team_name: "my-project",
  agent_type: "researcher",  // Will inherit lead's model
  description: "Project with hybrid model strategy"
})

// All coordination messages are cheap
SendMessage({
  type: "message",
  recipient: "implementer-backend",
  content: "Status update needed",
  summary: "Brief check-in"
})
```

---

## Cost Calculation Example

**Scenario: 3-hour feature implementation**

| Component | Model | Est. Tokens | Cost |
|-----------|-------|-------------|------|
| Lead coordination | Haiku | 50K input | $0.03 |
| Implementer #1 | Opus | 500K input | $2.50 |
| Implementer #2 | Opus | 500K input | $2.50 |
| Researcher | Haiku | 100K input | $0.05 |
| Messaging overhead | Haiku | 50K input | $0.03 |
| **Total** | | | **$5.11** |

**Comparison:**
- All-Opus team: ~$15.00
- Hybrid strategy: ~$5.11
- **Savings: 66%**

---

## Role-Based Model Selection

### Always Use Opus

| Role | Reason |
|------|--------|
| `coder` | Complex code generation requires reasoning |
| `architect` | System design decisions are critical |
| `debugger` | Bug investigation needs deep analysis |
| `security-reviewer` | Security analysis can't be compromised |

### Always Use Haiku

| Role | Reason |
|------|--------|
| `researcher` | Information retrieval is straightforward |
| `docs-writer` | Summarization is simple |
| `coordinator` | Task assignment doesn't need reasoning |
| `notifier` | Status updates are trivial |

### Context-Dependent

| Role | Opus When | Haiku When |
|------|-----------|------------|
| `tester` | Writing complex test logic | Running and summarizing tests |
| `reviewer` | Deep code review | Formatting feedback |

---

## Prompt Templates

### Opus Implementation Prompt

```markdown
You are an implementer on the {team_name} team.

**Your Model**: Opus (you have full reasoning capability)

**Your Task**: {specific_implementation_task}

**Communication Protocol**:
- Use SendMessage with type "message" for all communication
- Keep messages brief - the lead uses Haiku (cheaper model)
- Report completion with summary, not full details

**Constraints**:
- Focus on implementation, not documentation
- Signal blockers early
- Coordinate with other implementers via messaging
```

### Haiku Research Prompt

```markdown
You are a researcher on the {team_name} team.

**Your Model**: Haiku (fast, cost-effective)

**Your Task**: {research_task}

**Communication Protocol**:
- Use SendMessage for findings
- Summarize in bullet points
- Include sources as markdown links

**Constraints**:
- Quick research cycles
- Surface-level analysis is acceptable
- Escalate complex questions to Opus implementers
```

---

## Anti-Patterns

### ❌ Don't: All Opus Team

```javascript
// Wasteful - $15 for simple coordination
Task({ name: "coordinator", model: "opus", ... })
Task({ name: "notifier", model: "opus", ... })
Task({ name: "documenter", model: "opus", ... })
```

### ❌ Don't: All Haiku Team

```javascript
// Risky - complex code will be low quality
Task({ name: "architect", model: "haiku", ... })
Task({ name: "security-reviewer", model: "haiku", ... })
```

### ✅ Do: Match Model to Task Complexity

```javascript
// Optimal - right model for each role
Task({ name: "implementer", model: "opus", ... })    // Complex
Task({ name: "researcher", model: "haiku", ... })    // Simple
Task({ name: "reviewer", model: "sonnet", ... })     // Moderate
```

---

## Metrics & Monitoring

Track model usage per team:

```bash
# Check team config for model assignments
cat ~/.claude/teams/{team-name}/config.json | jq '.members[].model'

# Estimate costs
# Opus: input_tokens * $5/1M + output_tokens * $25/1M
# Haiku: input_tokens * $0.50/1M + output_tokens * $2.50/1M
```

---

## Sources

- [Claude Code Agent Teams Docs](https://code.claude.com/docs/en/agent-teams)
- [Anthropic C Compiler Project](https://www.anthropic.com/engineering/building-c-compiler)
- [Claude API Pricing Guide 2026](https://aifreeapi.com/en/posts/claude-api-pricing-per-million-tokens)
- [Choosing the Right Model - Claude API](https://platform.claude.com/docs/en/about-claude/models/choosing-a-model)

---

## Summary

| Strategy | Cost | Quality | Best For |
|----------|------|---------|----------|
| All Opus | High | Highest | Critical systems |
| All Haiku | Low | Lower | Simple tasks |
| **Hybrid** | **Medium** | **High** | **Most projects** |

**Key Insight**: Communication tokens vastly outnumber implementation tokens. Using Haiku for messaging while keeping Opus for coding gives you 90% of the quality at 50% of the cost.
