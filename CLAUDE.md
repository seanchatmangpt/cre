# CRE - Common Runtime Environment

Erlang/OTP YAWL workflow engine with Petri Net patterns. OTP 25+ (tested through 28).

## Commands

```bash
rebar3 compile          # Build (MUST run after any .erl/.hrl change)
rebar3 eunit            # Unit tests
rebar3 ct               # Common Test (integration)
rebar3 dialyzer         # Type analysis
rebar3 xref             # Cross-reference checks
rebar3 efmt -c          # Check formatting
```

## Architecture

Single OTP runner (`gen_yawl`) wrapping `gen_pnet`. Everything else is pure stateless helpers.

- `src/core/` - gen_pnet runtime (DO NOT modify without understanding full callback chain)
- `src/pnet/` - Petri Net algebra (types, markings, modes, choice)
- `src/wf/` - Workflow utilities (timers, tasks, scopes)
- `src/yawl/` - YAWL compilation, validation, execution
- `src/patterns/` - 43 YAWL workflow control-flow patterns
- `test/` - EUnit and Common Test suites

## Critical Rules

- Source in `src/`, tests in `test/`, docs in `docs/`. NEVER put files in project root.
- MUST run `rebar3 compile` after modifying `.erl` or `.hrl` files to verify correctness.
- See @.claude/rules/erlang.md for Erlang code conventions.
- See @.claude/rules/testing.md for test conventions.
