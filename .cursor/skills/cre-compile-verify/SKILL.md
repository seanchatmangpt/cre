---
name: cre-compile-verify
description: Post-edit verification for Erlang. MUST run rebar3 compile after any .erl or .hrl modification. Use when editing Erlang source or header files.
---

# CRE Compile Verify

After modifying any `.erl` or `.hrl` file, verify compilation succeeds before considering the change complete.

## Rule

MUST run `rebar3 compile` after any Erlang source or header edit.

## Execution

Prefer Docker when available:

```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile
```

If in a gVisor or cloud environment where Docker is unavailable, use whatever build path SessionStart.sh or the project has bootstrapped (e.g., native rebar3).

## When to use

- Immediately after saving changes to `src/**/*.erl` or `src/**/*.hrl` or `include/**/*.hrl`
- Before running tests or suggesting the task is done
