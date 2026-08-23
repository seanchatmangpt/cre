# CRE Quick Reference Guide

**Version:** 0.3.0 | **OTP Support:** 25.0-28.x | **Last Updated:** 2026-02-09

---

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [YAWL Patterns Quick Reference](#yawl-patterns-quick-reference)
3. [Petri Net API Reference](#petri-net-api-reference)
4. [Mining API Reference](#mining-api-reference)
5. [Pattern API Reference](#pattern-api-reference)
6. [Workflow Modules](#workflow-modules)
7. [Common Commands](#common-commands)
8. [Error Codes](#error-codes)

---

## Core Concepts

### Architecture Philosophy

```
One OTP runner (gen_pnet), everything else pure helpers
```

**Key Components:**
- `gen_pnet` - Petri net behavior (state machine)
- `gen_yawl` - YAWL workflow wrapper
- Pure functional modules - Stateless utilities

### File Organization

```
src/
├── core/          # gen_pnet runtime (DO NOT modify)
├── pnet/          # Petri Net algebra
├── wf/            # Workflow utilities
├── yawl/          # YAWL compilation, validation
└── patterns/      # 43 workflow control-flow patterns
```

---

## YAWL Patterns Quick Reference

### Basic Control Flow (WCP 01-06)

| Pattern | Code | Description | Module |
|---------|------|-------------|--------|
| WCP-01 | Sequence | Linear task execution | `pattern_sequence` |
| WCP-02 | Parallel Split | Concurrent execution | `pattern_parallel` |
| WCP-03 | Synchronization | Join parallel paths | `pattern_sync` |
| WCP-04 | Exclusive Choice | One of many branches | `pattern_choice` |
| WCP-05 | Simple Merge | converge exclusive | `pattern_merge` |
| WCP-06 | Multi Choice | Zero or more branches | `pattern_multi_choice` |

### Advanced Synchronization (WCP 07-10)

| Pattern | Code | Description | Module |
|---------|------|-------------|--------|
| WCP-07 | Synchronizing Merge | Converge with sync | `pattern_sync_merge` |
| WCP-08 | Multi Merge | Multiple merge points | `pattern_multi_merge` |
| WCP-09 | Discriminator | First-of-many sync | `pattern_discriminator` |
| WCP-10 | Arbitrary Cycles | Loops in workflow | `pattern_cycle` |

### Multiple Instances (WCP 11-17)

| Pattern | Code | Description | Module |
|---------|------|-------------|--------|
| WCP-11 | MI w/o Synchronization | Parallel instances | `pattern_mi_no_sync` |
| WCP-12 | MI w/ a Priori Design Time | Fixed count | `pattern_mi_design_time` |
| WCP-13 | MI w/ a Priori Runtime | Dynamic count | `pattern_mi_runtime` |
| WCP-14 | MI w/o a Priori | Data-driven count | `pattern_mi_no_priori` |
| WCP-15 | MI w/ MI Synchronization | Complex sync | `pattern_mi_sync` |

### State-Based Patterns (WCP 18-20)

| Pattern | Code | Description | Module |
|---------|------|-------------|--------|
| WCP-18 | Deferred Choice | Event-based selection | `pattern_deferred_choice` |
| WCP-19 | Interleaved Parallel | Alternating execution | `pattern_interleaved` |
| WCP-20 | Milestone | State-based enabling | `pattern_milestone` |

### Data Flow Patterns (WDP 01-05)

| Pattern | Code | Description | Module |
|---------|------|-------------|--------|
| WDP-01 | Tuple Binding | Variable binding | `pattern_tuple_binding` |
| WDP-02 | Tuple Visibility | Data scope | `pattern_tuple_visibility` |
| WDP-03 | Tuple Transitivity | Data passing | `pattern_tuple_transitivity` |

### Resource Patterns (WRP 01-05)

| Pattern | Code | Description | Module |
|---------|------|-------------|--------|
| WRP-01 | Direct Distribution | Assignment | `pattern_direct_distribution` |
| WRP-02 | Role-Based Distribution | Role assignment | `pattern_role_based` |
| WRP-03 | Deferred Distribution | Late binding | `pattern_deferred_distribution` |
| WRP-04 | Auto Start | Automatic initiation | `pattern_auto_start` |
| WRP-05 | Visible Allocation | Transparency | `pattern_visible_allocation` |

---

## Petri Net API Reference

### pnet_types Module

```erlang
% Create a type
-type pnet_type() :: atom() | {atom(), Args::list()}.

% Type operations
pnet_types:combine(Type1, Type2) -> CombinedType.
pnet_types:compatible(Type1, Type2) -> boolean().
pnet_types:validate(Type, Value) -> ok | {error, Reason}.
```

### pnet_mode Module

```erlang
% Execution modes
-type pnet_mode() :: auto | manual | step | replay.

% Mode operations
pnet_mode:set_mode(Pid, Mode) -> ok.
pnet_mode:get_mode(Pid) -> Mode.
pnet_mode:execute(Pid, Mode) -> Result.
```

### pnet_marking Module

```erlang
% Marking operations
pnet_marking:tokens(Place, Marking) -> [Token].
pnet_marking:add_token(Place, Token, Marking) -> NewMarking.
pnet_marking:remove_token(Place, Token, Marking) -> NewMarking.
pnet_marking:has_token(Place, Token, Marking) -> boolean().
```

---

## Mining API Reference

### Core Mining Functions

```erlang
% Alpha algorithm
mining_alpha:discover_log(Log) -> {ok, PNet} | {error, Reason}.

% Alpha++ algorithm
mining_alpha_plus:discover_log(Log) -> {ok, PNet} | {error, Reason}.

% Inductive miner
mining_inductive:discover_log(Log) -> {ok, PNet} | {error, Reason}.

% Heuristic miner
mining_heuristic:discover_log(Log) -> {ok, PNet} | {error, Reason}.
```

### Predictive Mining

```erlang
% RNN-based prediction
pred_rnn:train(Log, Config) -> {ok, Model} | {error, Reason}.
pred_rnn:predict(Model, Trace) -> Prediction.

% Transformer prediction
pred_transformer:train(Log, Config) -> {ok, Model} | {error, Reason}.
pred_transformer:predict(Model, Trace) -> Prediction.
```

### Reinforcement Learning Mining

```erlang
% DQN-based mining
mining_dqn:train(Log, Config) -> {ok, Model} | {error, Reason}.
mining_dqn:discover(Model, Log) -> {ok, PNet} | {error, Reason}.
```

---

## Pattern API Reference

### Pattern Creation

```erlang
% Create a pattern
pattern:new(Name, Type, Opts) -> {ok, Pattern} | {error, Reason}.

% Validate pattern
pattern:validate(Pattern) -> ok | {error, Reason}.

% Execute pattern
pattern:execute(Pattern, Input) -> {ok, Output} | {error, Reason}.
```

### Pattern Composition

```erlang
% Sequential composition
pattern:seq(Pattern1, Pattern2) -> CombinedPattern.

% Parallel composition
pattern:par(Patterns) -> CombinedPattern.

# Choice composition
pattern:choice(Patterns) -> CombinedPattern.
```

---

## Workflow Modules

### wf_modules

```erlang
% Timer management
wf_timer:start(Name, Duration, Callback) -> {ok, Pid}.
wf_timer:cancel(Pid) -> ok.
wf_timer:remaining(Pid) -> milliseconds().

% Task execution
wf_task:execute(Task, Input) -> {ok, Output} | {error, Reason}.
wf_task:execute_async(Task, Input) -> {ok, Pid}.

% Scope management
wf_scope:create(Scope, Parent) -> {ok, ScopePid}.
wf_scope:enter(ScopePid) -> ok.
wf_scope:exit(ScopePid) -> ok.
```

---

## Common Commands

### Build & Test

```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit        # Unit tests
rebar3 ct           # Common Test (integration)

# Analysis
rebar3 dialyzer     # Type analysis
rebar3 xref         # Cross-reference checks
rebar3 efmt -c      # Check formatting
```

### Development

```bash
# Start shell
rebar3 shell

# Run specific test
rebar3 eunit --module=pattern_sequence

# Generate documentation
rebar3 edoc
```

---

## Error Codes

### Common Errors

| Code | Description | Resolution |
|------|-------------|------------|
| `einval` | Invalid argument | Check input types |
| `enoent` | Pattern not found | Verify pattern name |
| `ealready` | Already exists | Check for duplicates |
| `eexists` | Dependency exists | Resolve dependencies |
| `timeout` | Operation timeout | Increase timeout |
| `enomem` | Out of memory | Reduce load |

### Pattern-Specific Errors

| Code | Description |
|------|-------------|
| `invalid_structure` | Pattern structure invalid |
| `missing_token` | Required token not present |
| `sync_failed` | Synchronization failed |
| `cycle_detected` | Circular dependency |

---

## See Also

- [ARCHITECTURE.md](../ARCHITECTURE.md) - Full architecture documentation
- [MINING_API_REFERENCE.md](../MINING_API_REFERENCE.md) - Mining API details
- [PATTERNS_API_REFERENCE.md](../PATTERNS_API_REFERENCE.md) - Pattern API details
- [GEN_PNET_USER_GUIDE.md](../GEN_PNET_USER_GUIDE.md) - gen_pnet usage guide
