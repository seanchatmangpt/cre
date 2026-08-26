# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

# CRE - Common Runtime Environment

Erlang/OTP YAWL workflow engine with Petri Net patterns. **OTP 28 REQUIRED**.

## Build & Test Commands

```bash
rebar3 compile              # Compile after .erl/.hrl changes (REQUIRED before tests)
rebar3 eunit                # Run all unit tests
rebar3 eunit --module=foo   # Run specific module tests
rebar3 ct                   # Run Common Test integration suites
rebar3 dialyzer             # Static type analysis
rebar3 cover                # Generate coverage report
```

## Docker Workflow

When Docker is available, use containers for build/test:
```bash
docker buildx bake --load                           # Build multi-arch image
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit
```

gVisor sandbox environments (Claude Code Web) meet containerization requirements. SessionStart.sh bootstraps OTP 28 with pre-built static binaries.

## Architecture

**Core Runtime:** Single OTP runner (`gen_yawl`) wrapping `gen_pnet` (Petri Net behavior). Everything else is pure stateless helpers.

| Directory | Purpose |
|-----------|---------|
| `src/core/` | gen_pnet runtime (DO NOT modify without understanding callback chain) |
| `src/pnet/` | Petri Net algebra (types, markings, modes, choice) |
| `src/wf/` | Workflow utilities (timers, tasks, scopes, tokens) |
| `src/yawl/` | YAWL compilation, validation, execution |
| `src/patterns/` | 43+ YAWL workflow control-flow patterns |
| `src/ln_ctrl/` | Lightning control plane (cancel, budget, scheduling) |
| `src/rust_nifs/` | Rust NIF implementations for performance-critical paths |
| `test/` | EUnit and Common Test suites |

**Key Modules:**
- `cre_yawl.erl` - Main YAWL API (98KB, central orchestrator)
- `cre_yawl_patterns.erl` - Pattern definitions (155KB)
- `cre_master.erl` - CRE master process (gen_pnet)
- `cre_worker.erl` - Worker behavior implementation

## YAWL Patterns (36+ Implemented)

Control-flow patterns in `src/patterns/`:
- Sequence, Parallel Split, Synchronization, Exclusive Choice
- Multi-Choice, Discriminator, N-out-of-M, Deferred Choice
- Interleaved Routing, Milestone, Critical Section, Structured Loop
- Cancellation patterns (cancel_activity, cancel_case, cancel_region)

Data patterns in `src/wf/`: Parameter passing, transform, distribute, accumulate

## Critical Rules

- **Source in `src/`, tests in `test/`, docs in `docs/`** - NEVER put files in project root
- **MUST run `rebar3 compile`** after any `.erl` or `.hrl` modification
- **Use `logger` module** - NEVER use `io:format` or `error_logger` in production code
- **gen_pnet behavior** - This project uses Petri Net behavior; understand callback chain before modifying `src/core/`

## Erlang Conventions

- All `handle_call`/`handle_cast` clauses MUST bind `= State` and return full `State`
- Use `-spec` type annotations on all exported functions
- Use `maps` over `proplists` for key-value data
- All `case` expressions on format atoms MUST include catch-all `_ ->` clause
- ETS tables: use `named_table` and `ets:whereis/1` for existence checks

## Test Conventions

- Unit tests: EUnit with `-include_lib("eunit/include/eunit.hrl").`
- Integration tests: Common Test (files ending in `_SUITE.erl`)
- Test functions: `_test` suffix or `_test_() -> [...]` generators
- Assertions: `?assertEqual`, `?assertMatch`, `?assertException`

## Available Agents

- `erlang-reviewer` - Code review for OTP compliance
- `test-runner` - Execute and analyze tests
- `dialyzer-check` - Type safety verification
- `docker-workflow` - Docker-first build/test
