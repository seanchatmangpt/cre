# CRE - Common Runtime Environment

Erlang/OTP project implementing a YAWL (Yet Another Workflow Language) workflow engine with Petri Net-based patterns.

## Stack

- Language: Erlang/OTP 25+ (tested through OTP 28)
- Build: rebar3
- Dependencies: gen_pnet, cowboy 2.12, jsx, jsone, cowlib, ranch
- Behaviors: gen_server, gen_pnet, cre_worker

## Key Commands

- `rebar3 compile` - compile the project
- `rebar3 eunit` - run EUnit tests
- `rebar3 ct` - run Common Test suites
- `rebar3 dialyzer` - static type analysis
- `rebar3 xref` - cross-reference checks
- `rebar3 cover` - test coverage report

## Project Structure

- `src/` - Erlang source files (69 modules) and header files (.hrl)
- `test/` - EUnit and Common Test suites (39 test modules)
- `docs/` - Documentation and reference materials
- `thesis/` - Academic thesis materials

## Key Modules

- `yawl_engine` - Core workflow execution engine (gen_server)
- `yawl_executor` - Unified executor for all 43 YAWL workflow control patterns (WCP-01 through WCP-43)
- `cre_yawl_patterns` - Petri Net workflow pattern definitions (gen_pnet)
- `cre_yawl_exception` - Exception handling and compensation (gen_pnet)
- `cre_yawl_worker` - Task worker implementation (cre_worker behavior)
- `cre_yawl_client` - Client API (gen_server)
- `yawl_persistence` - Workflow state persistence
- `yawl_reporter` - Report generation (JSON, XML, CSV, HTML)
- `yawl_marshal` - XML marshalling/unmarshalling
