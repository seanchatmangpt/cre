# CRE - Common Runtime Environment

Erlang/OTP project implementing a YAWL (Yet Another Workflow Language) engine with Petri Net-based workflow patterns.

## Build

```bash
rebar3 compile
rebar3 eunit
```

## Project Structure

- `src/` - Erlang source files
- `test/` - EUnit tests
- `docs/` - Documentation
- `thesis/` - Academic thesis materials

## Key Modules

- `yawl_engine` - Core workflow execution engine (gen_server)
- `yawl_executor` - Unified pattern executor for all 43 YAWL workflow control patterns
- `cre_yawl_patterns` - Petri Net pattern definitions (gen_pnet)
- `cre_yawl_exception` - Exception handling and compensation (gen_pnet)
- `cre_yawl_worker` - Task worker (cre_worker behavior)
- `cre_yawl_client` - Client API (gen_server)

## Important Instructions

- Do what has been asked; nothing more, nothing less.
- NEVER create files unless they're absolutely necessary for achieving your goal.
- ALWAYS prefer editing an existing file to creating a new one.
- NEVER proactively create documentation files (*.md) or README files.
- Never save working files, text/mds and tests to the root folder.
