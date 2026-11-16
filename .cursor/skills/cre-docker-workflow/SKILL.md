---
name: cre-docker-workflow
description: Runs compile, verify, and tests for CRE inside Docker. Use when modifying .erl or .hrl files, running tests, or validating the build. All work must be done in containers.
---

# CRE Docker Workflow

All build, compile, and test operations for CRE must run inside Docker.

## Commands

**Compile:**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 compile
```

**Unit tests (EUnit):**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit
```

**Full validation (compile + Common Test):**
```bash
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 sh -c "rebar3 compile && rebar3 ct"
```

## When to use

- After editing any `.erl` or `.hrl` file
- Before committing changes
- When validating deployment readiness

Never run `rebar3` directly on the host. When Docker is unavailable (e.g., gVisor cloud IDE), SessionStart.sh may fall back to native tooling; otherwise use the container.
