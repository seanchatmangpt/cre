# Research: Example workflows and demonstrations

**Date**: 2025-01-18
**Item**: 028-example-workflows-and-demonstrations

## Research Question
Complex API and semantics need concrete examples to demonstrate usage patterns and validate implementation correctness.

**Motivation:** Provides usage reference, validates substrate works correctly, demonstrates key patterns, supports documentation with working code.

**Success criteria:**
- All 4 examples implemented
- Each has runnable demo
- Expected trace documented
- Tests verify trace structure

**Signals:** priority: medium, urgency: Required for usability and validation

## Summary

CRE (Common Runtime Environment) has **extensive workflow infrastructure and patterns** but **lacks comprehensive, runnable example demonstrations** that serve multiple purposes: usage reference, implementation validation, pattern demonstration, and documentation support. The research reveals that CRE has:

1. **Rich pattern library**: All 43 YAWL patterns implemented as `gen_pnet` modules in `src/patterns/`
2. **Comprehensive docs**: Architecture, API, pattern references, and tutorials exist
3. **Test coverage**: 689 of 760 tests passing (96% pass rate)
4. **Demo infrastructure**: `test/yawl_of_demo.erl` for Order Fulfillment, `demo_runner.erl` for AGI Symposium
5. **Tracing system**: `ln_trace` module with structured event logging and export capabilities

**However**, the examples are **fragmented and not user-friendly**:
- Documentation references non-existent `examples/` directory (`docs/EXAMPLES.md:11-13` claims examples exist at `examples/workflows/` but the directory is empty)
- No centralized "4 core examples" demonstrating key workflow scenarios
- Missing expected trace documentation for each example
- No test verification that traces match expected structure
- Tutorials exist but lack accompanying runnable demos

**Recommendation**: Create **4 comprehensive, runnable examples** that demonstrate:
1. **Basic sequential workflow** - Simple 3-step process (WCP-01 Sequence)
2. **Parallel workflow with synchronization** - Multi-branch approval (WCP-02/03)
3. **Human-in-the-loop approval** - Decision checkpoint with Claude integration
4. **Complex multi-pattern workflow** - Order fulfillment demonstrating 10+ patterns

Each example should include:
- Complete, runnable Erlang module with `-moduledoc`
- Demo script (`examples/example_name_demo.sh`)
- Expected trace output (`examples/example_name_trace.md`)
- Test verifying trace structure (`test/example_name_trace_test.erl`)
- Documentation linking from tutorials

## Current State Analysis

### Existing Implementation

#### Pattern Infrastructure (Complete)
**All 43 YAWL patterns are implemented** as `gen_pnet` behavior modules:
- `src/patterns/sequence.erl` (WCP-01)
- `src/patterns/parallel_split.erl` (WCP-02)
- `src/patterns/synchronization.erl` (WCP-03)
- `src/patterns/exclusive_choice.erl` (WCP-04)
- And 39 more patterns across WCP, WDP, WRP, WHP categories

**Pattern registry**: `src/core/yawl_pattern_registry.erl` maps pattern IDs to modules

#### Documentation (Extensive but Fragmented)
- **`docs/EXAMPLES.md:1-659`** - Claims examples exist at `examples/workflows/` but directory is empty
- **`docs/tutorials/getting_started.md:1-100+`** - Tutorial references examples that don't exist
- **`docs/tutorials/basic_patterns_tutorial.md:1-100+`** - Pattern tutorial with code examples
- **`docs/tutorials/PATTERN_EXAMPLES.md:1-590`** - Test net pattern examples (not user-facing)
- **`docs/guides/order_fulfillment_example.md:1-100+`** - Order Fulfillment guide
- **`docs/ARCHITECTURE.md:1-300+`** - System architecture documentation
- **`docs/START_HERE.md:1-100+`** - Navigation hub pointing to examples

#### Test Infrastructure (Robust)
- **`test/yawl_of_demo.erl:1-100+`** - Order Fulfillment demo (functional)
- **`test/yawl_of_helpers.erl`** - Test helpers for YAWL workflows
- **`test/demo_runner.erl:1-87`** - AGI Symposium demo runner
- **96% test pass rate** (689 of 760 tests passing)

#### Tracing System (Complete)
- **`src/ln_trace.erl:1-138`** - Structured event tracing with:
  - Event types: `case_started`, `step_started`, `step_completed`, `branch_chosen`, `join_waiting`, `effect_requested`, `effect_completed`, `scope_cancelled`, `case_completed`, `case_failed`, `case_cancelled`
  - Configurable levels: `none`, `min`, `full`
  - Sequential event numbering
  - Export formats: map, list, json
  - Persistence: `save/2`, `load/1`

#### Existing Demo Patterns
1. **Order Fulfillment** (`test/yawl_of_demo.erl`)
   - Complex 5-subprocess workflow from CAISE 2013 paper
   - Demonstrates: sequential execution, payment branching, cancellation, timeouts
   - Status: ✅ Implemented and tested
   - Gap: Not exposed as user-facing example

2. **AGI Symposium** (`test/agi_symposium_otel.erl`, `test/demo_runner.erl`)
   - Participant management workflow with dashboard
   - Demonstrates: parallel execution, telemetry, web dashboard
   - Status: ✅ Implemented
   - Gap: Complex, not suitable as introductory example

### Key Files

#### Core Infrastructure
- **`src/core/gen_pnet.erl`** - Core Petri net runtime (step/1, marking/1, trigger/3)
- **`src/core/gen_yawl.erl:24-194`** - YAWL behavior wrapper around gen_pnet
- **`src/ln_trace.erl:1-138`** - Structured tracing with export/persistence
- **`src/core/yawl_pattern_registry.erl`** - Pattern ID → module mapping

#### Pattern Modules (Examples to Model After)
- **`src/patterns/sequence.erl:1-67`** - Minimal gen_yawl implementation
- **`src/patterns/parallel_split.erl:1-787`** - Comprehensive with XES logging
- **`src/patterns/exclusive_choice.erl:1-596`** - Full-featured with lib_combin integration

#### Test/Demo Modules (Existing Examples)
- **`test/yawl_of_demo.erl:1-100+`** - Order Fulfillment demo (good reference)
- **`test/demo_runner.erl:1-87`** - Demo runner pattern with OTEL integration
- **`test/yawl_of_helpers.erl`** - Test helper functions

#### Documentation (Claims Examples Exist)
- **`docs/EXAMPLES.md:11-13`** - References non-existent `examples/workflows/`
- **`docs/tutorials/getting_started.md:39`** - References `examples/example_basic_workflow.erl`
- **`docs/tutorials/PATTERN_EXAMPLES.md:1-590`** - Test net examples (internal use)

#### Tracing and Verification
- **`src/ln_trace.erl:42-100`** - Event types, emit/2, get_all/1, export/2
- **`src/ln_trace_replay.erl`** - Replay functionality (exists but not explored)
- **`src/wf/wf_audit_log.erl:1-493`** - Persistent audit trail with receipts

## Technical Considerations

### Dependencies

**Internal modules to integrate with:**
- **`gen_pnet`** - Core Petri net execution engine
- **`gen_yawl`** - YAWL behavior wrapper (or use gen_pnet directly for simplicity)
- **`ln_trace`** - Structured tracing for expected output
- **`yawl_pattern_registry`** - Pattern lookups (if using pattern modules)
- **`lib_combin`** - Nondeterministic choice (for branching examples)

**External dependencies:**
- **OTP logger** - Standard logging (already integrated via `include_lib("kernel/include/logger.hrl")`)
- **rebar3** - Build system

### Patterns to Follow

#### 1. Module Structure (from `src/patterns/sequence.erl:1-67`)
```erlang
-module(example_workflow).
-behaviour(gen_pnet).

%% Required callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([new/1, start/1, run/1, get_trace/1]).
```

#### 2. Demo Entry Point (from `test/yawl_of_demo.erl:82-100`)
```erlang
%% @doc Runs the demo with default settings
-spec run() -> execution_result().
run() ->
    run(#{}).

%% @doc Runs the demo with custom options
-spec run(map()) -> execution_result().
run(Options) when is_map(Options) ->
    % ... implementation
```

#### 3. Tracing Integration (from `src/ln_trace.erl:80-95`)
```erlang
%% Emit event with automatic timestamp and seq number
emit(#{timestamp := _} = Event, #trace_state{events = Events, seq = Seq} = State) ->
    NewEvents = [Event#{seq => Seq} | Events],
    State#trace_state{events = NewEvents, seq = Seq + 1};

%% Emit event type shorthand
emit(EventType, State) ->
    emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => EventType,
        data => #{}
    }, State).
```

#### 4. Test Verification Pattern (from `test/yawl_of_demo.erl`)
- Create demo module with `run/0`, `run/1` entry points
- Return execution result with status, marking, stats
- Include `print_results/1` for human-readable output

#### 5. Documentation Pattern
- **Moduledoc** at top of module with description and usage
- **Function docs** with `-doc()` or `-moduledoc` attributes
- **Type specs** for all exported functions
- **Example code** in doc comments

#### 6. Demo Script Pattern (from `test/demo_runner.erl:6-87`)
- Shell script in `examples/` directory
- Compile and run demo with appropriate options
- Handle errors gracefully
- Print expected output format

### Recommended Example Structure

Each example should follow this structure:

```
examples/
├── example_1_basic_sequence.erl       # Module implementing workflow
├── example_1_basic_sequence_demo.sh   # Shell script to run demo
├── example_1_basic_sequence_trace.md  # Expected trace output
├── example_2_parallel_sync.erl
├── example_2_parallel_sync_demo.sh
├── example_2_parallel_sync_trace.md
├── example_3_human_approval.erl
├── example_3_human_approval_demo.sh
├── example_3_human_approval_trace.md
├── example_4_order_fulfillment.erl
├── example_4_order_fulfillment_demo.sh
└── example_4_order_fulfillment_trace.md

test/
├── example_1_trace_test.erl           # Verify trace structure
├── example_2_trace_test.erl
├── example_3_trace_test.erl
└── example_4_trace_test.erl
```

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Examples don't demonstrate actual use cases** | High | Base examples on real scenarios: order processing, approval workflows, parallel data processing, human-in-the-loop decisions |
| **Trace output varies due to non-determinism** | Medium | Use fixed random seeds, deterministic scheduling, mock external dependencies for consistent traces |
| **Examples become outdated as API evolves** | Medium | Keep examples in sync with API changes, add CI tests that run examples to verify they work |
| **Too much complexity in examples** | Medium | Start simple (Example 1: 3 steps), gradually add complexity, document each pattern used |
| **Missing documentation for examples** | High | Require moduledoc, function docs, inline comments, and separate trace documentation |
| **Tests too brittle (exact trace matching)** | Medium | Test structure and key events, not exact timestamps or sequence numbers; use pattern matching on trace events |
| **Examples not integrated with tutorials** | Low | Add cross-references from tutorials to examples, link from examples to relevant tutorial sections |
| **Performance issues in demo execution** | Low | Set reasonable timeouts, use efficient algorithms, document performance characteristics |

## Recommended Approach

### High-Level Strategy

1. **Create 4 Progressive Examples**
   - **Example 1**: Basic Sequence (WCP-01) - "Hello World" of workflows
   - **Example 2**: Parallel + Sync (WCP-02/03) - Multi-branch coordination
   - **Example 3**: Human Approval - Claude integration with checkpoints
   - **Example 4**: Order Fulfillment - Complex multi-pattern workflow

2. **Each Example Includes**
   - **Runnable module** with comprehensive documentation
   - **Demo script** for easy execution
   - **Expected trace** documenting execution flow
   - **Trace verification test** ensuring correctness

3. **Integration Points**
   - Link from `docs/EXAMPLES.md` to examples
   - Reference examples from tutorials
   - Add example runner to `test/` directory
   - Include examples in CI/CD pipeline

### Implementation Plan

**Phase 1: Basic Sequence (Example 1)**
1. Create `examples/example_1_basic_sequence.erl`
   - Implement 3-step workflow: Initialize → Process → Finalize
   - Use gen_pnet behavior (not gen_yawl for simplicity)
   - Integrate ln_trace for execution logging
   - Add comprehensive moduledoc

2. Create `examples/example_1_basic_sequence_demo.sh`
   - Compile module
   - Run with rebar3 shell
   - Execute demo: `example_1_basic_sequence:run()`
   - Print trace output

3. Create `examples/example_1_basic_sequence_trace.md`
   - Document expected trace events
   - Show sequence of transitions
   - Include sample output

4. Create `test/example_1_trace_test.erl`
   - Verify trace structure
   - Check for required events
   - Validate final marking

**Phase 2: Parallel + Synchronization (Example 2)**
1. Create `examples/example_2_parallel_sync.erl`
   - Implement parallel split (WCP-02) and synchronization (WCP-03)
   - Use case: Multi-step verification (credit check + background check)
   - Demonstrate branch coordination

2. Create demo script, trace docs, and test (similar to Example 1)

**Phase 3: Human Approval (Example 3)**
1. Create `examples/example_3_human_approval.erl`
   - Integrate `yawl_approval` checkpoint system
   - Use `yawl_claude_bridge` for LLM decisions
   - Demonstrate auto/human/simulated approval modes

2. Create demo script showing different approval modes
3. Document trace with approval decision points
4. Test approval checkpoint creation and decision flow

**Phase 4: Order Fulfillment (Example 4)**
1. Extract from existing `test/yawl_of_demo.erl`
2. Create user-facing module in `examples/`
3. Simplify for demonstration purposes
4. Document complex trace with multiple subprocesses

**Phase 5: Documentation and Integration**
1. Update `docs/EXAMPLES.md` with links to new examples
2. Add cross-references from tutorials
3. Create `examples/README.md` with quick start guide
4. Add example execution to CI/CD pipeline

### Example Content Specifications

#### Example 1: Basic Sequence
**Patterns**: WCP-01 (Sequence)
**Use Case**: Document processing workflow
**Steps**:
1. Receive document
2. Validate document
3. Store document
**Expected Trace**:
- `case_started`
- `step_started` (receive)
- `step_completed` (receive)
- `step_started` (validate)
- `step_completed` (validate)
- `step_started` (store)
- `step_completed` (store)
- `case_completed`

#### Example 2: Parallel + Synchronization
**Patterns**: WCP-02 (Parallel Split), WCP-03 (Synchronization)
**Use Case**: Loan application verification
**Steps**:
1. Receive application
2. Parallel: Credit check + Background check
3. Synchronize: Wait for both
4. Make decision
**Expected Trace**:
- `case_started`
- `step_started` (receive)
- `step_completed` (receive)
- `branch_chosen` (parallel split)
- `step_started` (credit_check)
- `step_started` (background_check)
- `step_completed` (credit_check)
- `join_waiting` (waiting for background_check)
- `step_completed` (background_check)
- `step_started` (decision)
- `step_completed` (decision)
- `case_completed`

#### Example 3: Human Approval
**Patterns**: Custom approval checkpoint
**Use Case**: Code deployment approval
**Steps**:
1. Compile code
2. Create approval checkpoint
3. Wait for approval (auto/human/simulated)
4. Deploy if approved
**Expected Trace**:
- `case_started`
- `step_started` (compile)
- `step_completed` (compile)
- `effect_requested` (approval_checkpoint)
- `effect_completed` (approval_decision)
- `step_started` (deploy)
- `step_completed` (deploy)
- `case_completed`

#### Example 4: Order Fulfillment
**Patterns**: WCP-01, WCP-04, WCP-06, WCP-23, WHP-01
**Use Case**: E-commerce order processing
**Subprocesses**:
1. Ordering (WCP-01, WCP-26)
2. Carrier Appointment (WCP-06, WCP-08)
3. Payment (WCP-04, WHP-01, WCP-23)
4. Freight In Transit (WCP-25, WCP-16)
5. Freight Delivered (WCP-18)
**Expected Trace**: Complex multi-page trace with ~50 events

## Open Questions

1. **Should examples use gen_pnet or gen_yawl behavior?**
   - gen_yawl provides XES logging and enhanced features
   - gen_pnet is simpler and more direct
   - **Recommendation**: Use gen_pnet for Examples 1-2 (simplicity), gen_yawl for Examples 3-4 (realism)

2. **Should examples be in examples/ or test/ directory?**
   - examples/ is standard location but currently empty
   - test/ has existing demos (yawl_of_demo.erl)
   - **Recommendation**: Create new examples/ directory, keep test/ for unit tests

3. **How to handle external dependencies in examples?**
   - Example 3 (Human Approval) requires Claude integration
   - Example 4 (Order Fulfillment) has multiple subprocesses
   - **Recommendation**: Use mocks/stubs for external deps, document clearly

4. **Should trace verification be exact or structural?**
   - Exact matching is brittle (timestamps, sequence numbers)
   - Structural matching is more flexible
   - **Recommendation**: Test structure and key events, ignore exact timestamps

5. **How to demonstrate trace export and replay?**
   - ln_trace supports export to JSON/map
   - ln_trace_replay supports replay (not explored in research)
   - **Recommendation**: Add trace export to demo script, show replay in Example 4

6. **Should examples include error scenarios?**
   - All examples currently show happy path
   - Error handling is important for real workflows
   - **Recommendation**: Add error variant to Example 1 (validation failure)

7. **How to integrate with existing tutorials?**
   - Tutorials reference examples that don't exist
   - Need to add cross-references
   - **Recommendation**: Update tutorials to point to new examples, add "See also" links

8. **Should examples be part of CI/CD pipeline?**
   - Ensures examples stay working
   - Catches API breakage early
   - **Recommendation**: Add example execution to CI, fail if trace structure doesn't match

9. **What level of documentation is required?**
   - Moduledoc, function docs, inline comments?
   - Separate tutorial for each example?
   - **Recommendation**: Comprehensive moduledoc, inline comments, separate trace docs, link from tutorials

10. **How to handle non-determinism in trace output?**
    - Random choices, parallel execution order
    - Makes exact trace matching impossible
    - **Recommendation**: Use fixed seeds, document allowed variations, test structure not exact sequence
