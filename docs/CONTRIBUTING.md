# Contributing to CRE

Thank you for your interest in contributing to CRE (Common Runtime Environment)! This document provides guidelines for contributing code, reporting issues, and participating in the development community.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Environment Setup](#development-environment-setup)
- [Code Style Guidelines](#code-style-guidelines)
- [Testing](#testing)
- [Adding New YAWL Patterns](#adding-new-yawl-patterns)
- [Implementing gen_pnet Behaviors](#implementing-gen_pnet-behaviors)
- [Commit Message Conventions](#commit-message-conventions)
- [Pull Request Process](#pull-request-process)
- [Running Checks Before Contributing](#running-checks-before-contributing)
- [Getting Help](#getting-help)

---

## Getting Started

CRE is a YAWL (Yet Another Workflow Language) workflow engine implemented in Erlang/OTP. It follows Joe Armstrong's design philosophy: **one real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts.**

### Project Architecture

```
src/
|-- core/        # gen_pnet - The only OTP process maintaining state
|-- pnet/        # Pure Petri net utilities (stateless)
|-- wf/          # Workflow pure helpers (stateless)
|-- yawl/        # YAWL-specific modules
|-- patterns/    # YAWL workflow patterns (gen_pnet implementations)
|-- api/         # Client/worker API modules
|-- integration/ # External service integration
|-- http/        # HTTP/web modules
|-- app/         # Application modules
```

**Key Principle**: Only `gen_pnet` and modules implementing the `gen_pnet` behavior should maintain state. All other modules are pure functions.

---

## Development Environment Setup

### Prerequisites

- **Erlang/OTP**: Version 25.0 or higher (tested through OTP 28)
- **rebar3**: Version 3.0.0 or higher
- **Git**: For version control

### Installation

```bash
# Clone the repository
git clone https://github.com/seanchatmangpt/cre.git
cd cre

# Install Erlang/OTP (choose one method)

# Option 1: Using asdf (recommended)
asdf install erlang 27.2
asdf local erlang 27.2

# Option 2: Using kerl
kerl build 27.2 27.2
kerl install 27.2 ~/kerl/27.2
. ~/kerl/27.2/activate

# Install rebar3
# Download to project root for consistency
curl -L -o rebar3 https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
./rebar3 version

# Install dependencies
./rebar3 deps

# Compile the project
./rebar3 compile
```

### Verification

```bash
# Verify compilation
./rebar3 compile

# Run basic tests
./rebar3 eunit

# Start interactive shell
./rebar3 shell
```

---

## Code Style Guidelines

### Erlang/OTP Conventions

#### File Organization

- **Module naming**: Use lowercase with underscores for multi-word names
  - Good: `yawl_engine`, `pnet_marking`, `wf_timerq`
  - Avoid: `yawlEngine`, `Yawl_Engine`

- **File structure**:
```erlang
%% -*- erlang -*-
%%--------------------------------------------------------------------
%% @doc Module description
%% @end
%%--------------------------------------------------------------------

-module(module_name).
-author("author@example.com").

%% API exports
-export([function1/0, function2/1]).

%% gen_pnet/gen_server callbacks (if applicable)
-export([init/1, handle_call/3, ...]).

%%====================================================================
%% Includes
%%====================================================================

-include("header.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type my_type() :: atom() | binary().
-record(my_record, {field1, field2}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Function description.
-spec function1() -> return_type().
function1() ->
    ok.

%%====================================================================
%% Callback Functions
%%====================================================================

%% @doc gen_pnet callback description.
init(UsrInfo) ->
    {ok, UsrInfo}.
```

#### Naming Conventions

| Category | Convention | Example |
|----------|------------|---------|
| Modules | `lowercase_with_underscores` | `yawl_engine`, `pnet_marking` |
| Functions | `lowercase_with_underscores` | `create_workflow`, `get_state` |
| Variables | `CamelCase` or `snake_case` | `WorkflowId`, `workflow_id` |
| Records | `#record_name{}` | `#workflow{id = ...}` |
| Macros | `ALL_CAPS` | `?MAX_TIMEOUT` |
| Places (Petri nets) | `'p_place_name'` (quoted atoms) | `'p_start'`, `'p_choice'` |
| Transitions (Petri nets) | `'t_transition_name'` (quoted atoms) | `'t_select_a'` |

#### Indentation and Formatting

- Use **4 spaces** for indentation (no tabs)
- Maximum line length: **100 characters** (soft limit), **120 characters** (hard limit)
- Use `rebar3 efmt -c` to format code

```bash
# Format all source files
rebar3 efmt -c

# Check formatting without making changes
rebar3 efmt -c --check
```

#### Documentation

Every public function must have documentation:

```erlang
%% @doc
%% Creates a new workflow with the given identifier.
%%
%% Parameters:
%%   - WorkflowId: Unique binary identifier for the workflow
%%   - Options: Proplist with optional configuration
%%
%% Returns:
%%   - {ok, WorkflowPid}: Workflow started successfully
%%   - {error, Reason}: Workflow failed to start
%%
%% Example:
%%   ```erlang
%%   > {ok, Pid} = my_module:start_workflow(<<"my_wf">>, []).
%%   {ok, <0.123.0>}
%%   ```
%% @end
-spec start_workflow(WorkflowId :: binary(), Options :: proplists:proplist()) ->
    {ok, pid()} | {error, term()}.
start_workflow(WorkflowId, Options) ->
    %% implementation
    ok.
```

#### Type Specifications

Always include `-spec` for exported functions:

```erlang
%% Type definitions
-type workflow_id() :: binary().
-type workflow_state() :: running | completed | failed | cancelled.
-type workflow_result() :: {ok, term()} | {error, term()}.

%% Spec with type references
-spec execute_workflow(WorkflowId :: workflow_id()) ->
    workflow_result().
execute_workflow(WorkflowId) ->
    %% implementation
    {ok, result}.
```

---

## Testing

CRE uses multiple testing frameworks:

- **EUnit** - Unit testing
- **Common Test** - Integration testing
- **Doctest** - Documentation-based testing
- **Concuerror** - Concurrency testing (optional)

### Running Tests

```bash
# Run all EUnit tests
rebar3 eunit

# Run all Common Test suites
rebar3 ct

# Run both EUnit and Common Test
rebar3 ct && rebar3 eunit

# Run specific test module
rebar3 eunit --module=pnet_marking_test

# Run specific Common Test suite
rebar3 ct --suite=yawl_integration_SUITE

# Run specific test case
rebar3 ct --suite=yawl_integration_SUITE --case=basic_execution_test

# Generate coverage report
rebar3 cover

# Open coverage in browser (macOS)
open _build/test/cover/index.html
```

### Writing Unit Tests (EUnit)

```erlang
%% -*- erlang -*-
%% @doc Tests for my_module
-module(my_module_test).
-include_lib("eunit/include/eunit.hrl").

%% Simple test
basic_test() ->
    ?assertEqual(expected, my_module:function(input)).

%% Test with setup/teardown
complex_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     %% Test code here
                     ?assert(true)
                 end)
         ]
     end}.

setup() ->
    %% Initialize test state
    ok.

cleanup(_State) ->
    %% Clean up
    ok.
```

### Writing Integration Tests (Common Test)

```erlang
%% -*- erlang -*-
%% @doc Integration tests for YAWL workflows
-module(my_workflow_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Export required callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Export test cases
-export([workflow_execution_test/1]).

all() ->
    [workflow_execution_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cre),
    Config.

end_per_suite(_Config) ->
    application:stop(cre),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

workflow_execution_test(_Config) ->
    %% Test workflow execution
    {ok, Executor} = wf_yawl_executor:load_workflow("test/fixtures/simple.yawl"),
    {ok, Pid, _CaseId} = wf_yawl_executor:start_workflow(Executor, #{}),
    {ok, _Receipts} = wf_yawl_executor:execute_step(Pid, 100),
    ?assert(true).
```

### Test Coverage Targets

- **Core modules** (`gen_pnet`, `pnet_*`): 100%
- **Workflow modules** (`wf_*`, `yawl_*`): 90%+
- **Utility modules**: 85%+
- **Integration modules**: 80%+

---

## Adding New YAWL Patterns

CRE implements YAWL workflow patterns as `gen_pnet` behaviors. Each pattern is a Petri net with defined places and transitions.

### Pattern Structure

```erlang
%% -*- erlang %%
-module(my_pattern).
-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2,
         preset/1, is_enabled/3, fire/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, trigger/3]).

%% API exports
-export([new/1, start/1, execute/2]).

%%====================================================================
%% Records
%%====================================================================

-record(my_pattern_state, {
    config :: map(),
    start_time :: integer()
}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type my_pattern_state() :: #my_pattern_state{}.
-export_type([my_pattern_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Creates a new pattern state.
-spec new(Config :: map()) -> my_pattern_state().
new(Config) ->
    #my_pattern_state{
        config = Config,
        start_time = erlang:system_time(millisecond)
    }.

%% @doc Starts the pattern as a gen_pnet process.
-spec start(Config :: map()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    State = new(Config),
    gen_pnet:start_link(?MODULE, State, []).

%%====================================================================
%% gen_pnet Callbacks - Net Structure
%%====================================================================

%% @doc Returns the list of places in the Petri net.
-spec place_lst() -> [atom()].
place_lst() ->
    [
        'p_start',      %% Entry place
        'p_processing', %% Processing place
        'p_complete'    %% Exit place
    ].

%% @doc Returns the list of transitions in the Petri net.
-spec trsn_lst() -> [atom()].
trsn_lst() ->
    [
        't_begin',     %% Start transition
        't_finish'     %% Complete transition
    ].

%% @doc Returns the initial marking for a place.
-spec init_marking(Place :: atom(), UsrInfo :: my_pattern_state()) ->
    [term()].
init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_Place, _UsrInfo) ->
    [].

%% @doc Returns the preset (input places) for a transition.
-spec preset(Trsn :: atom()) -> [atom()].
preset('t_begin') -> ['p_start'];
preset('t_finish') -> ['p_processing'];
preset(_) -> [].

%% @doc Checks if a transition is enabled.
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: my_pattern_state()) ->
    boolean().
is_enabled('t_begin', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_processing' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%% @doc Fires a transition, consuming and producing tokens.
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: my_pattern_state()) ->
    {produce, map()} | abort.
fire('t_begin', #{'p_start' := [start]}, UsrInfo) ->
    %% Consume from p_start, produce to p_processing
    {produce, #{
        'p_start' => [],
        'p_processing' => [processing]
    }, UsrInfo};
fire('t_finish', #{'p_processing' := [processing]}, UsrInfo) ->
    %% Consume from p_processing, produce to p_complete
    {produce, #{
        'p_processing' => [],
        'p_complete' => [complete]
    }, UsrInfo};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%====================================================================
%% gen_pnet Callbacks - Process Interface
%%====================================================================

init(State) ->
    {ok, State}.

handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

place_lst_test() ->
    Expected = ['p_start', 'p_processing', 'p_complete'],
    ?assertEqual(Expected, place_lst()).

trsn_lst_test() ->
    Expected = ['t_begin', 't_finish'],
    ?assertEqual(Expected, trsn_lst()).

preset_test() ->
    ?assertEqual(['p_start'], preset(t_begin)),
    ?assertEqual(['p_processing'], preset(t_finish)).

-endif.
```

### Pattern Implementation Checklist

1. **Define Petri Net Structure**
   - [ ] Places (where tokens reside)
   - [ ] Transitions (what can fire)
   - [ ] Presets (input places for each transition)

2. **Implement gen_pnet Callbacks**
   - [ ] `place_lst/0` - List all places
   - [ ] `trsn_lst/0` - List all transitions
   - [ ] `init_marking/2` - Initial tokens per place
   - [ ] `preset/1` - Input places for transitions
   - [ ] `is_enabled/3` - When transitions can fire
   - [ ] `fire/3` - Token consumption/production

3. **Add Tests**
   - [ ] Unit tests for callbacks
   - [ ] Integration test for full execution
   - [ ] Edge cases (empty input, multiple tokens)

4. **Add Documentation**
   - [ ] Module docstring with pattern description
   - [ ] API documentation
   - [ ] Examples in docs/YAWL_PATTERNS_REFERENCE.md

---

## Implementing gen_pnet Behaviors

The `gen_pnet` behavior is the core of CRE. It implements Petri net semantics as an OTP process.

### Core Callbacks

```erlang
%% Place and Transition Definition
-callback place_lst() -> [atom()].
-callback trsn_lst() -> [atom()].
-callback init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
-callback preset(Trsn :: atom()) -> [atom()].

%% Firing Semantics
-callback is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    boolean().
-callback fire(Trsn :: atom(), Mode :: map(), UsrInfo :: term()) ->
    {produce, ProduceMap :: map()} | abort.
```

### Optional Callbacks

```erlang
%% Process Interface (standard gen_server)
-callback init(UsrInfo :: term()) -> {ok, UsrInfo :: term()}.
-callback handle_call(Request :: term(), From :: {pid(), term()},
                      NetState :: term()) ->
    {reply, Reply :: term(), NetState :: term()}.
-callback handle_cast(Request :: term(), NetState :: term()) ->
    {noreply, NetState :: term()}.
-callback handle_info(Info :: term(), NetState :: term()) ->
    {noreply, NetState :: term()}.
-callback code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
    {ok, NetState :: term()}.
-callback terminate(Reason :: term(), NetState :: term()) -> ok.

%% Token-based side effects
-callback trigger(Place :: atom(), Token :: term(), UsrInfo :: term()) ->
    pass | {consume, [term()]}.
```

### Key Concepts

#### Places and Transitions

Places hold tokens (as lists). Transitions consume tokens from input places and produce tokens to output places.

```erlang
%% Places store tokens as lists
Mode = #{
    'p_input' => [token1, token2],  %% Two tokens
    'p_output' => []                 %% Empty
}.

%% Transitions consume from preset, produce to postset
%% When 't_process' fires:
%%   - Consumes from 'p_input'
%%   - Produces to 'p_output'
```

#### Mode Structure

The mode is a map of places to token lists:

```erlang
Mode = #{
    'p1' => [a, b, c],  %% Place p1 has 3 tokens
    'p2' => [d],         %% Place p2 has 1 token
    'p3' => []           %% Place p3 is empty
}.
```

#### Firing Semantics

A transition fires when:
1. It is enabled (`is_enabled/3` returns true)
2. The mode contains required tokens

The `fire/3` callback returns:
```erlang
{produce, #{
    'p_output' => [new_token],  %% Tokens to add
    'p_input' => []              %% Tokens to remove (empty = consume all)
}, NewUsrInfo}.
```

---

## Commit Message Conventions

CRE uses a structured commit message format:

### Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types

| Type | Description |
|------|-------------|
| `feat` | New feature |
| `fix` | Bug fix |
| `docs` | Documentation only changes |
| `style` | Code style changes (formatting, whitespace) |
| `refactor` | Code refactoring |
| `perf` | Performance improvement |
| `test` | Adding or updating tests |
| `chore` | Build process or auxiliary tool changes |
| `pattern` | Adding new YAWL pattern |

### Scopes

| Scope | Description |
|-------|-------------|
| `yawl` | YAWL engine and patterns |
| `pnet` | Petri net core (`gen_pnet`, `pnet_*`) |
| `wf` | Workflow utilities |
| `api` | Client/worker API |
| `http` | HTTP endpoints |
| `telemetry` | OpenTelemetry integration |
| `docs` | Documentation |

### Examples

```
feat(yawl): add arbitrary cycle pattern (WCP-08)

Implements the Arbitrary Cycle pattern allowing workflows
to loop back to any previous condition. Uses pnet_marking
for token tracking and integrates with wf_timerq for
timeout handling.

Closes #123
```

```
fix(pnet): correct marking algebra for token consumption

The take/2 function was not properly handling multisets
when consuming tokens. Fixed by using proper multiplicity
tracking in pnet_marking.

Fixes #456
```

```
pattern(wcp): add structured loop pattern implementation

Implements WCP-23 Structured Loop pattern with:
- Pre-test and post-test variants
- Loop counter tracking
- Break condition support

Tests: 12 new tests in structured_loop_test.erl
Docs: Updated YAWL_PATTERNS_REFERENCE.md
```

---

## Pull Request Process

### Branch Strategy

```bash
# Create a feature branch
git checkout -b feat/my-new-feature

# Create a bugfix branch
git checkout -b fix/issue-description

# Create a pattern branch
git checkout -b pattern/wcp-08-arbitrary-cycle
```

### Before Submitting

1. **Run all checks**:
```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit
rebar3 ct

# Format code
rebar3 efmt -c

# Type analysis
rebar3 dialyzer

# XREF checks
rebar3 xref
```

2. **Update documentation** if needed
3. **Add tests** for new functionality
4. **Update CHANGELOG** (if user-visible)

### Pull Request Template

```markdown
## Description
Brief description of changes.

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## YAWL Pattern
- [ ] Implements new pattern (specify which)
- [ ] Updates existing pattern

## Testing
- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] All tests pass (`rebar3 eunit && rebar3 ct`)
- [ ] Coverage meets requirements

## Checklist
- [ ] Code formatted with `rebar3 efmt -c`
- [ ] Documentation updated
- [ ] Commit messages follow conventions
- [ ] No merge conflicts with master

## Related Issues
Closes #(issue number)
```

---

## Running Checks Before Contributing

### Full Check Command

```bash
# Run all quality checks
rebar3 compile && \
rebar3 eunit && \
rebar3 ct && \
rebar3 dialyzer && \
rebar3 xref && \
rebar3 efmt -c --check
```

### Individual Checks

```bash
# Compilation check
rebar3 compile

# Unit tests
rebar3 eunit

# Integration tests
rebar3 ct

# Type checking
rebar3 dialyzer

# Cross-reference checks
rebar3 xref

# Code formatting check
rebar3 efmt -c --check

# Apply formatting
rebar3 efmt -c
```

### Pre-commit Hook (Optional)

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
set -e

echo "Running pre-commit checks..."

# Format check
echo "Checking code format..."
rebar3 efmt -c --check

# Compile
echo "Compiling..."
rebar3 compile

# Quick tests
echo "Running quick tests..."
rebar3 eunit

echo "Pre-commit checks passed!"
```

Make it executable:
```bash
chmod +x .git/hooks/pre-commit
```

---

## Getting Help

### Documentation

- **Architecture**: See `docs/ARCHITECTURE.md`
- **Testing Guide**: See `docs/TESTING.md`
- **YAWL Patterns**: See `docs/YAWL_PATTERNS_REFERENCE.md`
- **API Reference**: See `docs/API_REFERENCE.md`
- **Quick Start**: See `docs/QUICK_START.md`

### Interactive Development

```bash
# Start shell with code loaded
rebar3 shell

# Load a module for testing
1> l(my_module).

# Test a function
2> my_module:test_function().

# Start observer for process monitoring
3> observer:start().

# Check application status
4> application:which_applications().
```

### Debugging Tips

```bash
# Enable debug logging
1> logger:set_application_level(cre, debug).

# Trace function calls
2> dbg:tracer().
3> dbg:p(all, c).
4> dbg:tp(my_module, my_function, x).

# Check process info
5> erlang:process_info(pid(), memory).
6> erlang:process_info(pid(), dictionary).
```

### Community Resources

- **GitHub Issues**: Report bugs and request features
- **Existing Tests**: Look at `test/` for examples
- **Source Code**: Reference existing patterns in `src/patterns/`

### Key Modules to Understand

1. **`gen_pnet`**: Core Petri net behavior
2. **`pnet_marking`**: Token algebra
3. **`pnet_types`**: Type validators
4. **`wf_yawl_executor`**: Workflow execution
5. **`yawl_engine`**: YAWL engine core

---

## Project-Specific Guidelines

### Pure Functions vs State

**Keep modules stateless** unless implementing `gen_pnet`:

```erlang
%% GOOD: Pure function in helper module
-module(wf_helper).
-export([calculate_result/1]).

calculate_result(Input) ->
    %% Pure computation, no state
    Input * 2.

%% GOOD: Stateful gen_pnet implementation
-module(my_pattern).
-behaviour(gen_pnet).
%% ... callbacks that manage state ...
```

### Error Handling

Use explicit error returns:

```erlang
%% GOOD: Explicit error returns
-spec do_work(Input :: term()) -> {ok, term()} | {error, term()}.
do_work(Input) ->
    case validate(Input) of
        ok -> {ok, process(Input)};
        {error, Reason} -> {error, Reason}
    end.

%% AVOID: Throwing for expected errors
%% Only throw for truly exceptional conditions
```

### Logging

Use the standard `logger` module:

```erlang
%% Log levels
logger:debug("Debug info: ~p", [Data]),
logger:info("Info message"),
logger:warning("Warning: ~p", [Reason]),
logger:error("Error occurred: ~p", [Error]).
```

---

Thank you for contributing to CRE! Your contributions help make workflow management better for everyone.
