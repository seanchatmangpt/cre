# YAWL Model Checker - Bounded Model Checking Validation Backend

## Overview

This module provides formal verification of YAWL workflows through bounded model checking. It compiles workflows to Petri nets and performs state space exploration to detect deadlocks, unreachable transitions, and completion problems.

## Features

- **Deadlock Detection**: Identifies states with no enabled transitions that are not final states
- **Dead Transition Detection**: Finds transitions (flows) that never fired during exploration
- **Completion Checking**: Verifies that at least one execution path reaches a final state
- **Bounded Exploration**: Configurable depth and token bounds for performance
- **Integration**: Seamlessly integrates with existing `yawl_validate` infrastructure

## Usage

### Basic Validation

```erlang
%% Validate with default bounds (depth=15, token_bound=10)
{ok, Warnings} = yawl_model_checker:validate(Spec).

%% Validate with custom bounds
{ok, Warnings} = yawl_model_checker:validate(Spec, #{depth => 20, token_bound => 15}).
```

### Enable Model Checking in Validation Pipeline

```erlang
%% In sys.config or application environment
{cre, [
    {enable_model_checking, true}
]}.

%% Now model checking runs as part of yawl_validate:validate/1
{ok, Warnings} = yawl_validate:validate(Spec).
```

## Compilation Strategy

1. **Tasks and Conditions** → Places in Petri net
2. **Flows** → Transitions (consume from source, produce to target)
3. **Initial Marking** → Tokens at input conditions

## Property Checking

### Deadlock Detection
A deadlock is a state where:
- No transitions are enabled
- The state is not a final marking (tokens not at output conditions)

### Dead Transition Detection
A dead transition:
- Exists in the workflow (has a flow definition)
- Never fires during bounded exploration
- Indicates an unreachable task or condition

### Completion Checking
A workflow can complete if:
- At least one trace reaches a final marking
- Final marking = all tokens at output conditions

## Performance

- Simple workflows (2-3 tasks): 9-13ms
- Medium workflows (10-20 tasks): ~50-100ms (estimated)
- Large workflows (50+ tasks): Configurable via depth bounds

## Architecture

```
yawl_model_checker (Main API)
    ├── yawl_pnet_compiler (Workflow → Petri Net)
    ├── yawl_explorer (Bounded State Space Exploration)
    └── Property Checking (Deadlock, Dead Transitions, Completion)
```

## Dependencies

- `pnet_types`: Petri net type definitions
- `pnet_marking`: Marking algebra (add, take, apply operations)
- `yawl_validate`: Validation error types and integration

## Error Codes

- `deadlock_detected`: Deadlock state found
- `dead_transition`: Unreachable transition found
- `no_completion_path`: Workflow cannot reach completion

## Testing

Run the test suite:

```bash
rebar3 eunit -m yawl_model_checker_tests
```

## Future Enhancements

- Support for all 43 YAWL patterns (currently supports basic control flow)
- Colored Petri net validation
- Counterexample generation
- Visualization of state spaces
- Integration with external model checkers (SPIN, NuSMV)

## References

- Research: `.wreckit/items/018-validation-backend-with-bounded-model-checking/research.md`
- Implementation Plan: `.wreckit/items/018-validation-backend-with-bounded-model-checking/plan.md`
- Progress Log: `.wreckit/items/018-validation-backend-with-bounded-model-checking/progress.log`
