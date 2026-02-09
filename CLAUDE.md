# CRE - Common Runtime Environment

Erlang/OTP YAWL workflow engine with Petri Net patterns. OTP 25+ required.

## Build Commands

```bash
rebar3 compile       # MUST run after ANY .erl or .hrl change
rebar3 eunit         # Unit tests
rebar3 ct            # Integration tests
rebar3 dialyzer      # Type checking
rebar3 xref          # Cross-reference analysis
```

## Architecture

```
src/
├── core/      - gen_pnet runtime (DO NOT modify without understanding callbacks)
├── pnet/      - Petri Net algebra (types, markings, modes)
├── wf/        - Workflow utilities
├── yawl/      - YAWL compilation and validation
└── patterns/  - 43 YAWL control-flow patterns
```

## Critical Principles

- **Always compile first**: Run `rebar3 compile` before tests or analysis
- **OTP behaviors**: Use gen_server, gen_statem, supervisor patterns correctly
- **Type safety**: All exported functions MUST have `-spec` annotations
- **Testing**: Tests go in `test/`, never in `src/` or project root
- **State handling**: gen_server clauses MUST bind `= State` and return full State

## Rules

See `.claude/rules/` for detailed conventions.
