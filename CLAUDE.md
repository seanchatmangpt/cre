# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CRE is a **YAWL (Yet Another Workflow Language)** workflow engine implemented in Erlang/OTP. Starting from its origins as a Cuneiform runtime environment, CRE has evolved into a comprehensive workflow management system with 43 workflow patterns, human-in-the-loop approval flows, OpenTelemetry observability, and a web-based dashboard.

### New Architecture (Joe Armstrong Design)
The project has been refactored to follow Joe Armstrong's principle: **one real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts**. See the [diataxis architecture documentation](docs/DIATAXIS_ARCHITECTURE.md) and [complete API reference](docs/COMPLETE_API_REFERENCE.md) for details on the new architecture.

#### Implementation Status (WIP Files)
The implementation is actively in progress with several core utility modules completed:
- ✅ `src/pnet_types.erl` - Complete type system with total validation functions
- ✅ `src/pnet_marking.erl` - Complete multiset marking algebra implementation
- ✅ `src/pnet_mode.erl` - Complete mode enumeration utilities including colored net support

## Development Commands

### Build & Dependency Management (using rebar3)
```bash
# Install dependencies
rebar3 deps

# Compile the project
rebar3 compile

# Run tests
rebar3 eunit          # Unit tests
rebar3 ct            # Common Test integration tests
rebar3 cover         # Coverage analysis

# Quality assurance
rebar3 dialyzer      # Static type analysis (configured for unmatched_returns, error_handling, underspecs)
rebar3 xref          # Cross-reference dependency analysis
rebar3 efmt -c       # Format code with EFmt

# Documentation
rebar3 edoc          # Generate API documentation

# Create standalone executable
rebar3 escriptize    # Creates 'cre' escript in _build/default/bin/

# Interactive development
rebar3 shell         # Start interactive Erlang shell with project loaded
```

### Specialized Scripts
```bash
# Human-in-the-loop approval worker
./scripts/approval_worker.sh
  # Environment variables:
  # CRE_APPROVAL_POLL_INTERVAL=5
  # CRE_NODE=cre@localhost
  # CRE_COOKIE=secret
  # CLAUDE_CMD=claude
  # CRE_APPROVAL_LOG_FILE=/tmp/cre_approval_worker.log

# Y Combinator demo workflow
./scripts/demo_workflow.sh
```

### Development Workflow
```bash
# 1. Setup development environment
rebar3 deps
rebar3 compile

# 2. Run comprehensive tests
rebar3 ct -v          # Verbose Common Test
rebar3 cover --export # Export coverage reports

# 3. Check code quality
rebar3 dialyzer      # Must pass with no warnings
rebar3 xref          # Check dependency issues

# 4. Format code
rebar3 efmt -c

# 5. Generate documentation
rebar3 edoc
```

## Architecture Overview

### New Architecture Components

**Single Runtime Component:**
- `gen_pnet` - The only OTP process maintaining state (Joe Armstrong design)

**Pure Helper Modules (stateless utilities):**
- ✅ `pnet_types` - Type definitions and validation (WIP: Complete)
- ✅ `pnet_marking` - Multiset marking algebra (WIP: Complete)
- ✅ `pnet_mode` - Mode enumeration utilities (WIP: Complete)
- ⚠️ `pnet_choice` - Deterministic nondeterminism (WIP: Needs implementation)
- ⚠️ `pnet_receipt` - Receipt tracking and effects (WIP: Needs implementation)
- ⚠️ `wf_timerq` - Deadline queue for token injections (WIP: Needs implementation)
- ⚠️ `wf_task` - External task token constructors (WIP: Needs implementation)
- ⚠️ `wf_scope` - Boundary mapping helpers (WIP: Needs implementation)
- ⚠️ `yawl_validate` - YAWL specification validation (WIP: Needs implementation)
- ⚠️ `yawl_compile` - YAWL compilation to net modules (WIP: Needs implementation)

**Behavior Contracts:**
- `pnet_net` - Net semantics behavior for workflow definitions

### Key Design Principles
- **One real OTP runner**: Only `gen_pnet` maintains state
- **Pure utilities**: All helper modules are stateless
- **Message contracts**: Clean inter-process communication
- **Progress loop**: Automatic token processing in `gen_pnet`

### Key Data Structures

```erlang
% Client state
-record(client_state, {
    cre_name,           % Target CRE instance
    client_mod,         % Client callback module
    usr_info,           % User-specific info
    request_map = #{},  % Pending requests
    reply_map = #{},    % Received replies
    state_map = #{}     % Client state
}).

% Workflow case state
-record(case_state, {
    case_id,           % Unique workflow instance
    status,            % Case lifecycle state
    workitems = #{},   % Active work items
    data = #{},        % Workflow variables
    timestamps = #{}   % Creation, start, completion
}).
```

### Communication Patterns

- **Request-Response**: cre_client → cre_master → cre_worker → results
- **Event-Driven**: yawl_ipc broadcasts workflow events
- **Pub-Sub**: Observer registration for workflow state changes
- **Worker Pool**: cre_master manages idle/busy worker states

## Configuration Management

### Main Configuration (`src/cre_config.erl`)
The system uses persistent_term storage for O(1) configuration access:

**Key Configuration Categories:**
- Authentication: PBKDF2 iterations (100,000), session timeout (3600s), password requirements
- YAWL Stateless: Max concurrent executions (1000), TTL (3,600,000ms)
- Web Service: Port 4142, routes /[status.json], /history.json
- YAWL Timeout: Deadlock detection, resource check intervals

### Environment Variables
```bash
# For approval worker
CRE_APPROVAL_POLL_INTERVAL=5
CRE_NODE=cre@localhost
CRE_COOKIE=secret
CLAUDE_CMD=claude
CRE_APPROVAL_LOG_FILE=/tmp/cre_approval_worker.log

# For Docker
ERL_AFLAGS="-name cre@localhost -setcookie secret"
REBAR_COLOR=1
```

## Testing Infrastructure

### Test Organization
- **74 test modules** in `/test/` directory
- **EUnit** for unit tests (files ending with `_test.erl`)
- **Common Test** for integration tests (files ending with `_SUITE.erl`)
- **Test Coverage**: Generated in `_build/test/cover/`

### Key Test Areas
- YAWL workflow patterns execution
- Stateful and stateless workflows
- Resource allocation and scheduling
- Authentication and authorization
- HTTP/JSON interfaces
- Performance and integration tests
- Human-in-the-loop approval workflows

### Running Tests
```bash
# Run all tests
rebar3 ct

# Run specific test suite
rebar3 ct -v -c test/yawl_engine_SUITE

# Run with coverage
rebar3 cover --export

# Generate coverage report
rebar3 cover --verbose
```

## Development Best Practices

### Code Style
- Standard Erlang formatting (4-space indentation)
- Module names use camelCase
- Function names use snake_case
- Comprehensive docstrings for all public APIs

### Quality Gates
- **Dialyzer**: Must pass with no warnings (unmatched_returns, error_handling, underspecs)
- **Test Coverage**: Comprehensive coverage required for all modules
- **EFmt**: Code must be formatted
- **XRef**: No unresolved dependencies

### Development Workflow
1. Make changes
2. Run `rebar3 efmt -c` to format code
3. Run `rebar3 dialyzer` to check types
4. Run `rebar3 ct` to run tests
5. Run `rebar3 cover --export` to check coverage

## YAWL Workflow Patterns

### New Pattern Implementation
The new architecture implements YAWL patterns through the `pnet_net` behavior. See [diataxis architecture documentation](docs/DIATAXIS_ARCHITECTURE.md#tutorial-getting-started) for pattern examples.

### Basic Pattern Example (New)
```erlang
% Define a YAWL pattern as a pnet_net behavior module
-module(sequence_pattern).
-behaviour(pnet_net).

places() -> [start, step1, step2, end].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [step1].

init(_NetArg) -> [].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, #{start := [init]}, _UsrInfo) -> [#{start => []}].
modes(t2, #{step1 := [done]}, _UsrInfo) -> [#{step1 => []}].

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{step1 => [done]}}.

fire(t2, #{step1 => []}, _UsrInfo) ->
    {produce, #{end => [complete]}}.
```

### Human-in-the-Loop Workflows
- Use `wf_task` constructors for external task tokens
- Poll task states using `gen_pnet:ls/2`
- Implement approval workers that inject task completion tokens
- Receipts provide audit trails for all transitions

### YAWL Compilation
- Use `yawl_compile:compile/2` to convert YAWL XML to net modules
- `yawl_validate:validate/1` ensures specification compliance
- Generated modules implement `pnet_net` behavior

## Production Deployment

### Deployment Methods
1. **Hex Package**: `rebar3 hex publish`
2. **GitHub Release**: Tag and release workflow
3. **Docker Container**: Use provided Dockerfile
4. **EScript**: `_build/default/bin/cre` standalone executable

### Production Considerations
- **Monitoring**: OpenTelemetry integration for metrics and tracing
- **Security**: PBKDF2 password hashing, node cookies, TLS configuration
- **Performance**: Persistent terms for config, connection pooling, load balancing
- **Persistence**: Checkpoint/recovery for long-running workflows
- **Scaling**: Horizontal scaling across Erlang cluster nodes

### Environment Setup
```bash
# Development
git clone <repository>
rebar3 deps
rebar3 compile
rebar3 shell

# Production (Hex)
mix hex.publish
# OR rebar3 hex.publish

# Docker
docker build -t cre .
docker run -p 4142:4142 cre
```

## Key Dependencies

### New Architecture Dependencies
The new architecture has minimal dependencies, following Joe Armstrong's principle of simplicity:

### Core Dependencies
- `rebar3 3.24.0+` - Build tool
- OTP 25+ (tested on 25, 26, 27, 28)
- `crypto` - Built-in for hashing and security
- `random` - Built-in for deterministic choice

### Optional Dependencies
- `jsx 3.1.0` - JSON processing (for YAWL/XML parsing)
- `xmerl` - Built-in XML parsing (for YAWL validation)

### New Architecture Features
- **Pure Erlang**: All utilities are pure Erlang modules
- **Stateless**: No persistent state outside `gen_pnet`
- **Message Contracts**: Clean inter-process communication
- **Self-contained**: Minimal external dependencies

## Important Notes

### New Architecture Highlights
- **Joe Armstrong Design**: Only `gen_pnet` is a real OTP process, everything else is pure utilities
- **Single Runtime Component**: State maintained only in `gen_pnet`
- **Pure Utilities**: All helper modules (`pnet_types`, `pnet_marking`, etc.) are stateless
- **Message Contracts**: Clean interfaces between components
- **Progress Loop**: Automatic token processing in `gen_pnet`

### Migration Status
- The project has been refactored to the new architecture
- See [migration guide](docs/MIGRATION_GUIDE.md) for transition details
- [Diataxis architecture documentation](docs/DIATAXIS_ARCHITECTURE.md) provides comprehensive overview
- [Complete API reference](docs/COMPLETE_API_REFERENCE.md) for all modules

### OTP Version
- **OTP 25+ support** with specific patches for OTP 28 compatibility
- **Security**: Uses `crypto:pbkdf2_*` instead of bcrypt for OTP 28
- **Performance**: Leverages persistent_term for O(1) configuration access
- **Observability**: Receipt-based audit trails with subscription support
- **Standards**: YAWL pattern compliance with 43 workflow patterns