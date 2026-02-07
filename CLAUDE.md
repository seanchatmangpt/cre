# CRE - Common Runtime Environment

Erlang/OTP project implementing a YAWL (Yet Another Workflow Language) workflow engine with Petri Net-based patterns.

## Stack

- Language: Erlang/OTP 25+ (tested through OTP 28)
- Build: rebar3
- Dependencies: gen_pnet, cowboy 2.12, jsx, jsone, cowlib, ranch, poolboy, doctest, lib_combin
- Behaviors: gen_server, gen_pnet, cre_worker, pnet_net

## Architecture (Joe Armstrong Design)

One real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts.

**Single Runtime Component:**
- `gen_pnet` - The only OTP process maintaining state

**Pure Helper Modules (stateless):**
- `pnet_types` - Type definitions and validation (complete)
- `pnet_marking` - Multiset marking algebra (complete)
- `pnet_mode` - Mode enumeration utilities (complete)
- `pnet_choice` - Deterministic nondeterminism (WIP)
- `pnet_receipt` - Receipt tracking and effects (WIP)
- `wf_timerq` - Deadline queue for token injections (WIP)
- `wf_task` - External task token constructors (WIP)
- `wf_scope` - Boundary mapping helpers (WIP)
- `yawl_validate` - YAWL specification validation (WIP)
- `yawl_compile` - YAWL compilation to net modules (WIP)

## Key Commands

```bash
rebar3 compile          # Compile
rebar3 eunit            # Unit tests
rebar3 ct               # Common Test integration tests
rebar3 dialyzer         # Static type analysis
rebar3 xref             # Cross-reference checks
rebar3 cover            # Test coverage
rebar3 efmt -c          # Format code with EFmt
rebar3 edoc             # Generate API docs
rebar3 escriptize       # Build standalone executable
rebar3 shell            # Interactive Erlang shell
```

## Project Structure

- `src/` - Erlang source files and headers
- `src/core/` - Core OTP components (gen_pnet)
- `src/pnet/` - Petri Net pure utilities
- `src/wf/` - Workflow utilities
- `src/yawl/` - YAWL-specific modules
- `src/patterns/` - Workflow pattern implementations
- `src/api/` - API modules
- `src/integration/` - Integration modules
- `src/http/` - HTTP/web modules
- `src/app/` - Application modules
- `test/` - EUnit and Common Test suites
- `docs/` - Documentation
- `thesis/` - Academic thesis materials

## Key Modules

- `yawl_engine` - Core workflow execution engine (gen_server)
- `yawl_executor` - Unified executor for all 43 YAWL workflow control patterns
- `cre_yawl_patterns` - Petri Net workflow pattern definitions (gen_pnet)
- `cre_yawl_exception` - Exception handling and compensation (gen_pnet)
- `cre_yawl_worker` - Task worker implementation (cre_worker behavior)
- `cre_yawl_client` - Client API (gen_server)
- `yawl_persistence` - Workflow state persistence
- `yawl_reporter` - Report generation (JSON, XML, CSV, HTML)
- `yawl_marshal` - XML marshalling/unmarshalling
