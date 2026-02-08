# CRE - Common Runtime Environment

Erlang/OTP project implementing a YAWL (Yet Another Workflow Language) workflow engine with Petri Net-based patterns.

## Stack

- Language: Erlang/OTP 25+ (tested through OTP 28)
- Build: rebar3
- Dependencies: gen_pnet, cowboy 2.12, jsx, jsone, cowlib, ranch, poolboy, doctest, lib_combin
- Behaviors: gen_server, gen_pnet, cre_worker, pnet_net

## Architecture (Joe Armstrong Design)

One real OTP runner (`gen_yawl`), everything else pure helpers/utilities + message contracts.

**Single Runtime Component:**
- `gen_yawl` - Workflow execution (wraps gen_pnet, supports 3-tuple fire/3)
- `gen_pnet` - CRE's extended version (inject/step/drain) when tests/patterns run

**Pure Helper Modules (stateless):**
- `pnet_types` - Type definitions and validation (complete)
- `pnet_marking` - Multiset marking algebra (complete)
- `pnet_mode` - Mode enumeration utilities (complete)
- `pnet_choice` - Deterministic nondeterminism (complete)
- `pnet_receipt` - Receipt tracking and effects (complete)
- `wf_timerq` - Deadline queue for token injections (complete)
- `wf_task` - External task token constructors (complete)
- `wf_scope` - Boundary mapping helpers (complete)
- `yawl_validate` - YAWL specification validation (complete)
- `yawl_compile` - YAWL compilation to net modules (complete)

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

- `gen_yawl` - Runtime for workflow execution (gen_server)
- `yawl_engine` - Legacy workflow execution engine (gen_server)
- `yawl_executor` - Unified executor for all 43 YAWL workflow control patterns
- `cre_yawl_patterns` - Petri Net workflow pattern definitions (gen_pnet)
- `cre_yawl_exception` - Exception handling and compensation (gen_pnet)
- `cre_yawl_worker` - Task worker implementation (cre_worker behavior)
- `cre_yawl_client` - Client API (gen_server)
- `wf_yaml_spec` - YAML 0.2 parser and validation
- `wf_yawl_executor` - Load/compile/run YAML workflows via gen_yawl
- `yawl_persistence` - Workflow state persistence
- `yawl_reporter` - Report generation (JSON, XML, CSV, HTML)
- `yawl_marshal` - XML marshalling/unmarshalling
