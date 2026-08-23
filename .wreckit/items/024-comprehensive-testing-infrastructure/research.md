# Research: Comprehensive testing infrastructure

**Date**: 2026-02-11
**Item**: 024-comprehensive-testing-infrastructure

## Research Question
Complex concurrent workflow system needs comprehensive validation beyond basic unit tests to ensure correctness, determinism, and performance.

**Motivation:** Provides confidence in correctness, catches edge cases, validates determinism guarantees, ensures performance targets are met, supports regression testing.

**Success criteria:**
- Unit tests for all primitives
- Property tests validate key invariants
- Bounded model checks detect deadlocks
- Determinism/replay tests pass
- Performance benchmarks run with reported metrics

**Technical constraints:**
- Implement minimal generator framework for property tests (no PropEr in stdlib)
- Bounded exploration up to depth D and token bound K
- Microbench: 10k task steps, 100 branch par join, repeated discriminator
- Targets: bounded overhead per step, efficient cancellation

**Signals:** priority: critical, urgency: Mandatory validation per acceptance criteria

## Summary

CRE is a complex concurrent workflow system built on Erlang/OTP using Petri nets as its formal foundation. The current testing infrastructure consists primarily of EUnit-based unit tests (`test/*.erl`) with 40+ test files covering basic execution, patterns, persistence, and integration scenarios. However, the system lacks **comprehensive testing infrastructure** for:

1. **Property-based testing** - No systematic validation of key invariants across all inputs
2. **Bounded model checking** - No automated deadlock/state space exploration
3. **Determinism validation** - Partial implementation exists (`ln_sched`, `wf_deterministic_replay_test`) but not comprehensive
4. **Performance benchmarking** - No systematic performance measurement infrastructure

The research reveals that CRE has **some foundational components** already in place:
- A deterministic scheduler (`ln_sched.erl`) with 3 modes (deterministic, nondeterministic, replay)
- A tracing system (`ln_trace.erl`) for structured event logging
- Partial determinism tests (`wf_deterministic_replay_test.erl`)
- Comprehensive Petri net type system (`pnet_types.erl`, `pnet_marking.erl`, `pnet_mode.erl`)
- 36+ implemented workflow patterns in `src/patterns/*.erl`

However, these components are **not integrated** into a comprehensive testing framework. The system needs:
- A minimal generator framework (PropEr is not in stdlib)
- Property test suites for all primitives
- Bounded model checking for deadlock detection
- Performance benchmark infrastructure
- Systematic determinism/replay validation

The implementation must follow the **Joe Armstrong design philosophy**: pure helper modules for testing, minimal state, clear separation of concerns.

## Current State Analysis

### Existing Implementation

#### 1. Test Infrastructure

**EUnit-based test suite** (`/Users/sac/cre/test/*.erl`):
- 40+ test files covering various aspects
- Basic pattern tests: `yawl_patterns_test.erl` (lines 1-948)
- Integration tests: `yawl_integration_performance_test.erl` (lines 1-2037)
- Determinism tests: `wf_deterministic_replay_test.erl` (lines 1-240)
- Persistence, recovery, execution, control, data, resource tests

**Test configuration** (`rebar.config:75-76`):
```erlang
{eunit_tests, [{application, cre}]}.
{eunit_opts, [no_tty]}.
```

**Test profile** (`rebar.config:65-67`):
```erlang
{test, [{cover_enabled, false},
        {erl_opts, [debug_info, {doc, "excerpt"}, {d, 'TEST'}, {i, "src/wf"}, {i, "include"}]},
        {deps, [{meck, "0.9.2"}]}]}.
```

#### 2. Deterministic Scheduling (Partial Implementation)

**Scheduler module** (`/Users/sac/cre/src/ln_sched.erl:1-163`):
- Already implements 3 modes: `deterministic`, `nondeterministic`, `replay`
- `init/1`, `init/2`: Initialize scheduler with mode and optional seed
- `choose/2`: Select from candidates based on mode
- `record_choice/3`: Record nondeterministic choices
- `get_log/1`, `get_mode/1`: Query scheduler state
- `verify_choice/3`: Validate choice matches expected type

**Choice types supported** (lines 27-30):
- `xor_selection` - Exclusive choice branches
- `defer_race` - Deferred choice races
- `task_selection` - Task scheduling
- `join_order` - Parallel join ordering

**Tracing module** (`/Users/sac/cre/src/ln_trace.erl:1-138`):
- Structured event tracing with event types: `case_started`, `step_started`, `step_completed`, `branch_chosen`, `join_waiting`, `effect_requested`, `effect_completed`, `scope_cancelled`, `case_completed`, `case_failed`, `case_cancelled`
- Event buffering with sequence numbers
- Configurable trace levels: `none | min | full`
- Event range queries and export in multiple formats: `map | list | json`

**Control behavior** (`/Users/sac/cre/src/ln_ctrl.erl:1-329`):
- Integrates scheduler, tracing, effect handling, cancellation
- Options: `{scheduler, Mode}`, `{step_quanta, Quanta}`, `{trace, Level}`, `{budget, Budget}`, `{effect_handler, Module}`
- **Key limitation**: This is an experimental system not integrated with production `gen_yawl` workflows

#### 3. Determinism Tests (Existing but Limited)

**Deterministic replay test** (`/Users/sac/cre/test/wf_deterministic_replay_test.erl`):
- Tests that same seed produces same hash chain (line 46-61)
- Tests that same seed produces same first move (line 70-77)
- Tests receipts in firing order (line 105-124)
- Tests final marking hash determinism (line 133-148)
- Tests hash chain consistency (line 157-179)

**Pattern**: Uses `gen_pnet:start_link` with `#{seed => Seed}` option, executes workflow, collects receipts, hashes them, and compares.

**Limitations**:
- Only tests simple choice networks
- Not comprehensive across all patterns
- No systematic property-based testing
- No integration with `ln_sched` module

#### 4. Petri Net Foundation (Strong Foundation for Testing)

**Type system** (`/Users/sac/cre/src/pnet/pnet_types.erl:1-558`):
- Comprehensive type validators for `place()`, `trsn()`, `token()`, `marking()`, `mode()`, `cmode()`, `move()`, `receipt()`
- All validation functions are total (never crash) - safe for use in guards
- Provides `is_marking/1`, `is_mode/1`, `is_cmode/1`, `is_move/1` validators

**Marking algebra** (`/Users/sac/cre/src/pnet/pnet_marking.erl:1-488`):
- `new/1`: Creates empty marking with given places
- `get/2`, `set/3`: Basic marking operations
- `add/2`, `take/2`: Multiset union/subtraction with multiplicity
- `apply/2`, `apply/3`: Atomic consume+produce operations
- `hash/1`: Stable hash independent of insertion order (SHA-256)
- **Marking representation**: `#{place() => [token()]}` - places map to token lists

**Mode enumeration** (`/Users/sac/cre/src/pnet/pnet_mode.erl:1-353`):
- `preset_counts/1`: Counts multiplicity of places in preset list
- `enum_modes/2`: Enumerates deterministic modes for transition firing
- `enum_cmodes/4`: Enumerates colored modes with variable bindings
- Uses `combinations/2` to generate all valid token selections from places

#### 5. Workflow Patterns (36+ Patterns to Test)

**Pattern implementations** (`/Users/sac/cre/src/patterns/*.erl`):
- Basic patterns: `sequence.erl`, `parallel_split.erl`, `synchronization.erl`, `exclusive_choice.erl`, `simple_merge.erl`
- Advanced synchronization: `discriminator.erl`, `n_out_of_m.erl`, `arbitration.erl`, `multi_merge.erl`
- Multiple instances: `multiple_instances_static.erl`, `multiple_instances_runtime.erl`, `multiple_instances_dynamic.erl`
- Cancellation: `cancel_activity.erl`, `cancel_case.erl`, `cancel_region.erl`
- Loops: `structured_loop.erl`, `implicit_termination.erl`
- Advanced: `critical_section.erl`, `protocol_pattern.erl`, `try_catch.erl`, `interleaved_routing.erl`

**Pattern registry** (`/Users/sac/cre/src/core/yawl_pattern_registry.erl:1-199`):
- Maps 43 pattern IDs to module names
- `pattern_module/1`: Looks up module for pattern ID
- `all_patterns/0`: Returns list of all registered patterns

**Test coverage** (`/Users/sac/cre/test/yawl_patterns_test.erl`):
- Tests all 43 YAWL patterns (WCP-1 through WCP-43)
- Verifies pattern record creation
- Tests workflow validation
- Tests integration scenarios
- **Limitation**: Mostly structural tests, not property-based

### Key Files

**Core scheduling and tracing (already implemented):**
- `/Users/sac/cre/src/ln_sched.erl:1-163` - Complete scheduler with 3 modes
- `/Users/sac/cre/src/ln_trace.erl:1-138` - Structured event tracing
- `/Users/sac/cre/src/ln_ctrl.erl:1-329` - Control behavior using scheduler

**Petri net infrastructure (strong foundation):**
- `/Users/sac/cre/src/pnet/pnet_types.erl:1-558` - Type definitions
- `/Users/sac/cre/src/pnet/pnet_marking.erl:1-488` - Marking algebra
- `/Users/sac/cre/src/pnet/pnet_mode.erl:1-353` - Mode enumeration

**Core execution engines:**
- `/Users/sac/cre/src/core/gen_yawl.erl:1-1556` - Production YAWL wrapper, line 928-1048 continue loop
- `/Users/sac/cre/src/core/gen_pnet.erl:1-1556` - Base Petri net behavior, line 706-721 continue loop

**Pattern implementations (need comprehensive testing):**
- `/Users/sac/cre/src/patterns/sequence.erl` - WCP-01: Sequence pattern
- `/Users/sac/cre/src/patterns/parallel_split.erl` - WCP-02: Parallel split
- `/Users/sac/cre/src/patterns/exclusive_choice.erl` - WCP-04: Exclusive choice
- `/Users/sac/cre/src/patterns/critical_section.erl` - WCP-39: Critical section (potential deadlock case)
- All 36+ patterns in `/Users/sac/cre/src/patterns/` directory

**Existing tests (patterns to follow):**
- `/Users/sac/cre/test/wf_deterministic_replay_test.erl:1-240` - Determinism test patterns
- `/Users/sac/cre/test/yawl_patterns_test.erl:1-948` - Pattern test structure
- `/Users/sac/cre/test/yawl_integration_performance_test.erl:1-2037` - Integration and performance test patterns

**Validation infrastructure:**
- `/Users/sac/cre/src/core/yawl_validate.erl:1-1197` - YAWL validation patterns
- `/Users/sac/cre/src/yawl/yawl_schema.erl:1-1104` - YAWL XML parsing and validation

## Technical Considerations

### Dependencies

**External dependencies needed:**
- **PropEr** (optional) - Property-based testing library for Erlang
  - Not currently in `rebar.config`
  - Can be added as test dependency
  - Alternative: implement minimal generator framework (as specified in constraints)

**Internal modules to integrate with:**
- `ln_sched` - Scheduler (already exists, needs integration with tests)
- `ln_trace` - Tracing (already exists, needs integration with tests)
- `gen_yawl` - Primary execution engine
- `gen_pnet` - Base Petri net semantics
- `pnet_marking` - Marking algebra for state hashing
- `pnet_mode` - Mode enumeration for state space exploration
- `yawl_validate` - Integration with existing validation

**Existing test utilities:**
- `meck` (0.9.2) - Already in test profile for mocking
- EUnit - Standard Erlang testing framework (already used)

### Patterns to Follow

#### 1. Deterministic Testing Pattern

From `/Users/sac/cre/test/wf_deterministic_replay_test.erl:46-61`:
```erlang
deterministic_replay_test() ->
    Run = fun(Seed) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
        {ok, _} = gen_pnet:inject(P, #{in => [go]}),
        {ok, Rs} = gen_pnet:drain(P, 10),
        Hs = [receipt_to_hash(R) || R <- Rs],
        ok = gen_pnet:stop(P),
        Hs
    end,

    H1 = Run(9),
    H2 = Run(9),

    ?assertEqual(H1, H2),
    ?assert(lists:all(fun is_integer/1, H1)),
    ok.
```

**Pattern**: Create test runner function, execute twice with same seed, compare results.

#### 2. Property-Based Testing Pattern (PropEr)

Standard PropEr pattern (to implement):
```erlang
-module(primitives_prop).
-include_lib("proper/include/proper.hrl").

prop_marking_add_is_commutative() ->
    ?FORALL({M1, M2},
            {marking(), marking()},
            equals(pnet_marking:add(M1, M2),
                   pnet_marking:add(M2, M1))).

prop_marking_hash_is_stable() ->
    ?FORALL(M, marking(),
            equals(pnet_marking:hash(M),
                   pnet_marking:hash(M))).
```

#### 3. Bounded Model Checking Pattern

From Item 018 research (`yawl_model_checker` design):
```erlang
%% Bounded exploration
-spec explore(State :: pnet_types:marking(), Bounds :: bounds()) ->
    {ok, [execution_trace()]} | {error, Reason}.

explore(InitialState, Bounds) ->
    DepthLimit = maps:get(depth, Bounds, 20),
    TokenLimit = maps:get(tokens, Bounds, 10),
    explore_loop([InitialState], #{}, [], 0, DepthLimit, TokenLimit).

explore_loop([], _Visited, Traces, _Depth, _MaxDepth, _TokenLimit) ->
    {ok, lists:reverse(Traces)};
explore_loop([State | Rest], Visited, Traces, Depth, MaxDepth, TokenLimit) ->
    case maps:get(State, Visited, false) of
        true -> explore_loop(Rest, Visited, Traces, Depth, MaxDepth, TokenLimit);
        false ->
            NewVisited = Visited#{State => true},
            case check_deadlock(State) of
                true -> {error, {deadlock, State, Traces}};
                false ->
                    Successors = get_successors(State, TokenLimit),
                    explore_loop(Successors ++ Rest, NewVisited, [State | Traces], Depth + 1, MaxDepth, TokenLimit)
            end
    end.
```

#### 4. Performance Benchmarking Pattern

From `/Users/sac/cre/test/yawl_integration_performance_test.erl:1080-1134`:
```erlang
pattern_execution_benchmark_test() ->
    Patterns = [
        {sequence, fun() -> cre_yawl:sequence() end},
        {parallel_split, fun() -> cre_yawl:parallel_split() end}
    ],

    Metrics = lists:map(fun({PatternName, PatternFun}) ->
        %% Warm up
        lists:foreach(fun(_) -> PatternFun() end, lists:seq(1, 10)),

        %% Measure
        Iterations = 1000,
        StartTime = erlang:monotonic_time(microsecond),

        lists:foreach(fun(_) -> PatternFun() end, lists:seq(1, Iterations)),

        EndTime = erlang:monotonic_time(microsecond),
        DurationUs = EndTime - StartTime,
        DurationMs = DurationUs div 1000,
        Throughput = (Iterations * 1000000) div DurationUs,

        #perf_metric{
            test_name = atom_to_binary(PatternName),
            start_time = StartTime,
            end_time = EndTime,
            duration_ms = DurationMs,
            operations = Iterations,
            throughput = Throughput
        }
    end, Patterns).
```

#### 5. Test Suite Organization Pattern

From `/Users/sac/cre/test/yawl_patterns_test.erl:138-155`:
```erlang
sequence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_CrePid) ->
         [
          ?_test(begin
                     Workflow = cre_yawl:new_workflow(),
                     ?assert(is_record(Workflow, workflow))
                 end),
          ?_test(begin
                     Workflow1 = cre_yawl:new_workflow(),
                     Workflow2 = cre_yawl:add_task(Workflow1, <<"t1">>, [{type, atomic}]),
                     ?assertMatch(#workflow{}, Workflow2)
                 end)
         ]
     end}.
```

**Pattern**: Use EUnit's `setup` pattern with setup/cleanup functions.

### Sources of Nondeterminism in Current System

**From Item 013 research:**

1. **Enabled transition selection**:
   - Multiple transitions may be enabled simultaneously
   - Current implementation: undefined order (depends on list iteration)
   - Needs: Stable ordering in deterministic mode

2. **Parallel branch execution order**:
   - `parallel_split` enables multiple output places
   - Join ordering is nondeterministic
   - Needs: Record join order for replay

3. **Token selection from places**:
   - When multiple tokens in a place, which one is consumed?
   - Current: undefined (list head)
   - Needs: Stable selection order

4. **Race condition handling**:
   - `deferred_choice` waits for first of multiple messages
   - Current: depends on message arrival order
   - Needs: Record winner for replay

5. **External effect ordering**:
   - Multiple effects may complete in different orders
   - Current: depends on external services
   - Needs: Record completion order

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **PropEr not in stdlib** | High - Need to implement minimal generator framework | Implement simple generator module following PropEr patterns; use existing `rand` module for random generation |
| **State space explosion in model checking** | High - Complex workflows may have too many states | Implement bounded exploration with configurable depth D and token bound K; use pruning strategies; prioritize error detection over completeness |
| **Performance test flakiness** | Medium - Benchmarks may vary between runs | Use statistical methods (median, percentiles); run multiple iterations; use controlled environment; report variability |
| **Integration complexity with existing engines** | High - gen_yawl doesn't use ln_sched | Phase integration: start with new patterns, add scheduler mode to gen_yawl; provide opt-in flag for deterministic mode |
| **Hidden nondeterminism sources** | Critical - Unknown sources may cause replay divergence | Add tracing for all system interactions; fuzz testing; verify replay produces same trace |
| **Testing infrastructure becomes maintenance burden** | Medium - Complex tests may break frequently | Keep tests simple and focused; use pure helper modules; document test patterns; provide test utilities |
| **Property test generation slow** | Medium - Complex generators may be slow | Use smart constructors with bias; limit generator size; use shrinking strategies; cache generated values |
| **Model checking incomplete** | High - May miss edge cases | Document coverage limitations; use multiple bounds (D, K); combine with property-based testing; provide known deadlock test cases |

## Recommended Approach

Based on research findings, here's the recommended implementation strategy:

### Phase 1: Minimal Generator Framework (1-2 weeks)

**Objective**: Implement lightweight property testing without external PropEr dependency.

**1. Create `test/framework/gen_simple.erl`**:
```erlang
-module(gen_simple).
-export([int/0, int/1, non_neg_int/0, atom/0, binary/0, list/1]).
-export([frequency/1, oneof/1, suchthat/2]).
-export([run/3]).

%% Generate random integer
int() -> int(?DEFAULT_RANGE).
int(Range) -> rand:uniform(Range * 2) - Range.

%% Generate list using generator
list(ItemGen) ->
    Size = non_neg_int() rem ?MAX_LIST_SIZE,
    [ItemGen() || _ <- lists:seq(1, Size)].

%% Run property test
run(PropFun, NumTests, Seed) ->
    rand:seed_s(exrop, Seed),
    run_tests(PropFun, NumTests, []).

run_tests(_PropFun, 0, Errors) ->
    case Errors of
        [] -> ok;
        _ -> {error, lists:reverse(Errors)}
    end;
run_tests(PropFun, NumTests, Errors) ->
    case catch PropFun() of
        ok -> run_tests(PropFun, NumTests - 1, Errors);
        Error -> run_tests(PropFun, NumTests - 1, [Error | Errors])
    end.
```

**2. Create generator modules for CRE types**:
- `test/framework/gen_marking.erl` - Generate random markings
- `test/framework/gen_pattern.erl` - Generate random workflow patterns
- `test/framework/gen_workflow.erl` - Generate random workflows

**3. Add to rebar.config**:
```erlang
{test, [
    {erl_opts, [debug_info, {doc, "excerpt"}, {d, 'TEST'}, {i, "test/framework"}]},
    {deps, [{meck, "0.9.2"}]}
]}.
```

### Phase 2: Unit Tests for All Primitives (2-3 weeks)

**Objective**: Ensure complete unit test coverage for all primitive operations.

**1. Test marking algebra** (`test/pnet_marking_test.erl`):
```erlang
%% Test basic operations
add_commutative_test() ->
    ?FORALL({M1, M2}, {marking(), marking()},
        equals(pnet_marking:add(M1, M2), pnet_marking:add(M2, M1))).

add_associative_test() ->
    ?FORALL({M1, M2, M3}, {marking(), marking(), marking()},
        equals(pnet_marking:add(pnet_marking:add(M1, M2), M3),
               pnet_marking:add(M1, pnet_marking:add(M2, M3)))).

take_inverse_of_add_test() ->
    ?FORALL({M, Tokens}, {marking(), tokens()},
        begin
            M1 = pnet_marking:add(M, Tokens),
            {ok, M2} = pnet_marking:take(M1, Tokens),
            equals(M, M2)
        end).

hash_stable_test() ->
    ?FORALL(M, marking(),
        equals(pnet_marking:hash(M), pnet_marking:hash(M))).

hash_collision_test() ->
    ?FORALL({M1, M2}, {marking(), marking()},
        begin
            H1 = pnet_marking:hash(M1),
            H2 = pnet_marking:hash(M2),
            M1 =:= M2 orelse H1 =/= H2
        end).
```

**2. Test mode enumeration** (`test/pnet_mode_test.erl`):
```erlang
enum_modes_exhaustive_test() ->
    ?FORALL({Marking, Transition}, {marking(), transition_def()},
        begin
            Modes = pnet_mode:enum_modes(Transition, Marking),
            lists:all(fun(Mode) -> pnet_types:is_mode(Mode) end, Modes)
        end).

enum_modes_valid_test() ->
    ?FORALL({Marking, Transition}, {marking(), transition_def()},
        begin
            Modes = pnet_mode:enum_modes(Transition, Marking),
            lists:all(fun(Mode) ->
                %% Verify mode has correct tokens from preset
                validate_mode_tokens(Mode, Transition, Marking)
            end, Modes)
        end).
```

**3. Test scheduler modes** (`test/ln_sched_test.erl`):
```erlang
deterministic_always_first_test() ->
    ?FORALL(Candidates, non_empty_list(atom()),
        begin
            Sched = ln_sched:init(deterministic),
            {Choice, _} = ln_sched:choose(label_candidates(Candidates), Sched),
            equals(Choice, hd(lists:sort(Candidates)))
        end).

nondeterministic_records_choice_test() ->
    ?FORALL({Candidates, Seed}, {non_empty_list(atom()), int()},
        begin
            Sched0 = ln_sched:init(nondeterministic, Seed),
            {Choice, Sched1} = ln_sched:choose(label_candidates(Candidates), Sched0),
            Log = ln_sched:get_log(Sched1),
            equals(length(Log), 1)
        end).

replay_follows_log_test() ->
    ?FORALL({Candidates, Seed}, {non_empty_list(atom()), int()},
        begin
            Sched0 = ln_sched:init(nondeterministic, Seed),
            {Choice, Sched1} = ln_sched:choose(label_candidates(Candidates), Sched0),
            Log = ln_sched:get_log(Sched1),

            Sched2 = ln_sched:init(replay, Log),
            {Choice2, _} = ln_sched:choose(label_candidates(Candidates), Sched2),
            equals(Choice, Choice2)
        end).
```

**4. Test tracing** (`test/ln_trace_test.erl`):
```erlang
emit_increments_sequence_test() ->
    ?FORALL(Events, list(event_type()),
        begin
            Trace0 = ln_trace:new(full),
            lists:foldl(fun(Event, Trace) ->
                Seq1 = ln_trace:next_seq(Trace),
                Trace1 = ln_trace:emit({Event, #{}}, Trace),
                Seq2 = ln_trace:next_seq(Trace1),
                equals(Seq2, Seq1 + 1)
            end, Trace0, Events)
        end).

get_range_returns_subset_test() ->
    ?FORALL({Events, Start, End}, {list(event_type()), non_neg_int(), non_neg_int()},
        begin
            Trace0 = lists:foldl(fun(E, T) -> ln_trace:emit({E, #{}}, T) end,
                                 ln_trace:new(full), Events),
            AllEvents = ln_trace:get_all(Trace0),
            RangeEvents = ln_trace:get_range(Trace0, Start, End),
            length(RangeEvents) =< length(AllEvents)
        end).
```

### Phase 3: Property Tests for Key Invariants (3-4 weeks)

**Objective**: Validate critical invariants hold across all inputs.

**1. Create `test/properties/` directory structure**:
```
test/properties/
  ├── marking_properties.erl
  ├── pattern_properties.erl
  ├── workflow_properties.erl
  └── scheduler_properties.erl
```

**2. Marking properties** (`test/properties/marking_properties.erl`):
```erlang
%% Marking algebra properties
prop_add_identity() ->
    ?FORALL(M, marking_gen(),
        equals(pnet_marking:add(M, #{}), M)).

prop_add_commutative() ->
    ?FORALL({M1, M2}, {marking_gen(), marking_gen()},
        equals(pnet_marking:add(M1, M2),
               pnet_marking:add(M2, M1))).

prop_add_associative() ->
    ?FORALL({M1, M2, M3}, {marking_gen(), marking_gen(), marking_gen()},
        equals(pnet_marking:add(pnet_marking:add(M1, M2), M3),
               pnet_marking:add(M1, pnet_marking:add(M2, M3)))).

prop_take_cancels_add() ->
    ?FORALL({M, Tokens}, {marking_gen(), tokens_gen()},
        begin
            M1 = pnet_marking:add(M, Tokens),
            case pnet_marking:take(M1, Tokens) of
                {ok, M2} -> equals(M, M2);
                {error, _} -> true
            end
        end).

prop_hash_deterministic() ->
    ?FORALL(M, marking_gen(),
        equals(pnet_marking:hash(M), pnet_marking:hash(M))).

prop_hash_order_independent() ->
    ?FORALL({M1, M2}, {marking_gen(), marking_gen()},
        begin
            M1 = M2,
            equals(pnet_marking:hash(M1), pnet_marking:hash(M2))
        end).
```

**3. Scheduler properties** (`test/properties/scheduler_properties.erl`):
```erlang
prop_deterministic_same_choice() ->
    ?FORALL({Candidates, Seed}, {candidates_gen(), int_gen()},
        begin
            Sched1 = ln_sched:init(deterministic),
            {Choice1, _} = ln_sched:choose(Candidates, Sched1),

            Sched2 = ln_sched:init(deterministic),
            {Choice2, _} = ln_sched:choose(Candidates, Sched2),

            equals(Choice1, Choice2)
        end).

prop_replay_reproduces_nondeterministic() ->
    ?FORALL({Candidates, Seed}, {candidates_gen(), int_gen()},
        begin
            %% Run in nondeterministic mode
            Sched0 = ln_sched:init(nondeterministic, Seed),
            {Choice, Sched1} = ln_sched:choose(Candidates, Sched0),
            Log = ln_sched:get_log(Sched1),

            %% Replay
            Sched2 = ln_sched:init(replay, Log),
            {ChoiceReplay, _} = ln_sched:choose(Candidates, Sched2),

            equals(Choice, ChoiceReplay)
        end).

prop_choice_log_is_complete() ->
    ?FORALL({Candidates, Seed, Iterations},
            {candidates_gen(), int_gen(), range(1, 100)},
        begin
            Sched0 = ln_sched:init(nondeterministic, Seed),
            {Choices, FinalSched} = lists:foldl(
                fun(_, {Acc, S}) ->
                    {Choice, S1} = ln_sched:choose(Candidates, S),
                    {[Choice | Acc], S1}
                end, {[], Sched0}, lists:seq(1, Iterations)),

            Log = ln_sched:get_log(FinalSched),
            equals(length(Log), Iterations)
        end).
```

**4. Pattern properties** (`test/properties/pattern_properties.erl`):
```erlang
%% Sequence pattern properties
prop_sequence_preserves_marking() ->
    ?FORALL({InputMarking, Tasks}, {marking_gen(), list(task_gen())},
        begin
            %% Sequence should process tasks one at a time
            %% Final marking should have all tokens at end place
            Net = compile_sequence_pattern(Tasks),
            FinalMarking = execute_pattern(Net, InputMarking),
            has_all_tokens_at_end(FinalMarking, Net)
        end).

prop_parallel_split_distributes_tokens() ->
    ?FORALL({InputMarking, Branches}, {marking_gen(), range(2, 10)},
        begin
            %% Parallel split should create tokens in all branches
            Net = compile_parallel_split(Branches),
            FinalMarking = execute_pattern(Net, InputMarking),
            tokens_in_all_branches(FinalMarking, Branches)
        end).

prop_exclusive_choice_one_branch() ->
    ?FORALL({InputMarking, Branches}, {marking_gen(), range(2, 5)},
        begin
            %% Exclusive choice should execute exactly one branch
            Net = compile_exclusive_choice(Branches),
            FinalMarking = execute_pattern(Net, InputMarking),
            exactly_one_branch_executed(FinalMarking, Branches)
        end).
```

### Phase 4: Bounded Model Checking for Deadlocks (3-4 weeks)

**Objective**: Detect deadlocks, livelocks, and unreachable states.

**1. Create `test/validate/` directory**:
```
test/validate/
  ├── workflow_explorer.erl
  ├── deadlock_detector.erl
  ├── state_space_analyzer.erl
  └── model_checker_test.erl
```

**2. Workflow explorer** (`test/validate/workflow_explorer.erl`):
```erlang
-module(workflow_explorer).

-export([explore/2, explore/3]).
-export([get_successors/2]).

-record(explorer_state, {
    visited :: #{pnet_marking:marking() => true},
    traces :: [execution_trace()],
    depth :: non_neg_integer(),
    max_depth :: pos_integer(),
    token_bound :: non_neg_integer()
}).

-spec explore(Workflow :: term(), Bounds :: map()) -> {ok, [trace()]} | {error, term()}.
explore(Workflow, Bounds) ->
    explore(Workflow, Bounds, #{}).

explore(Workflow, Bounds, Options) ->
    MaxDepth = maps:get(depth, Bounds, 20),
    TokenBound = maps:get(tokens, Bounds, 10),

    InitialState = get_initial_marking(Workflow),
    Explorer = #explorer_state{
        visited = #{marking_hash(InitialState) => true},
        traces = [],
        depth = 0,
        max_depth = MaxDepth,
        token_bound = TokenBound
    },

    explore_loop([InitialState], Explorer).

explore_loop([], #explorer_state{traces = Traces}) ->
    {ok, lists:reverse(Traces)};
explore_loop([State | Rest], #explorer_state{depth = Depth, max_depth = MaxDepth} = Explorer) ->
    case Depth >= MaxDepth of
        true -> explore_loop(Rest, Explorer);
        false ->
            case check_deadlock(State) of
                {error, Deadlock} ->
                    {error, {deadlock, State, Deadlock}};
                ok ->
                    Successors = get_successors(State, Explorer),
                    NewVisited = lists:foldl(
                        fun(S, Acc) -> Acc#{marking_hash(S) => true}
                        end, Explorer#explorer_state.visited, Successors),

                    NewExplorer = Explorer#explorer_state{
                        visited = NewVisited,
                        depth = Depth + 1
                    },
                    explore_loop(Successors ++ Rest, NewExplorer)
            end
    end.

get_successors(State, #explorer_state{token_bound = TokenBound}) ->
    %% Use pnet_mode to enumerate all enabled transitions
    case token_count(State) > TokenBound of
        true -> [];
        false ->
            Enabled = get_enabled_transitions(State),
            lists:flatmap(fun(Transition) ->
                Modes = pnet_mode:enum_modes(Transition, State),
                [fire_transition(Transition, Mode, State) || Mode <- Modes]
            end, Enabled)
    end.

check_deadlock(Marking) ->
    case get_enabled_transitions(Marking) of
        [] ->
            case is_final_marking(Marking) of
                true -> ok;
                false -> {error, {deadlock, no_enabled_transitions}}
            end;
        _ -> ok
    end.
```

**3. Deadlock detector** (`test/validate/deadlock_detector.erl`):
```erlang
-module(deadlock_detector).

-export([detect/2, detect/3]).
-export([report_deadlock/1]).

%% Detect deadlocks in workflow
detect(Workflow, Bounds) ->
    detect(Workflow, Bounds, []).

detect(Workflow, Bounds, Options) ->
    case workflow_explorer:explore(Workflow, Bounds) of
        {ok, _Traces} -> {ok, no_deadlock};
        {error, {deadlock, State, Reason}} ->
            {error, {deadlock_found, State, Reason}}
    end.

%% Report deadlock in readable format
report_deadlock({deadlock, State, Reason}) ->
    #{
        type => deadlock,
        reason => Reason,
        marking => pnet_marking:to_map(State),
        enabled_transitions => get_enabled_transitions(State),
        token_count => token_count(State)
    }.
```

**4. Test cases** (`test/validate/model_checker_test.erl`):
```erlang
%% Test known deadlock case: incorrect critical section
critical_section_deadlock_test() ->
    %% Create workflow with resource deadlock
    Workflow = create_deadlocking_critical_section(),

    Bounds = #{depth => 10, tokens => 5},
    Result = deadlock_detector:detect(Workflow, Bounds),

    ?assertMatch({error, {deadlock_found, _, _}}, Result).

%% Test known safe case: correct critical section
critical_section_safe_test() ->
    %% Create workflow with proper resource ordering
    Workflow = create_safe_critical_section(),

    Bounds = #{depth => 10, tokens => 5},
    Result = deadlock_detector:detect(Workflow, Bounds),

    ?assertMatch({ok, no_deadlock}, Result).

%% Test bounded exploration terminates
bounded_exploration_terminates_test() ->
    %% Create complex workflow
    Workflow = create_complex_workflow(50, 10),

    Bounds = #{depth => 10, tokens => 5},
    Result = workflow_explorer:explore(Workflow, Bounds),

    ?assertMatch({ok, _}, Result).
```

### Phase 5: Determinism and Replay Tests (2-3 weeks)

**Objective**: Ensure deterministic execution and exact replay.

**1. Extend existing determinism tests** (`test/wf_determinism_test.erl`):
```erlang
%% Comprehensive determinism test suite
-module(wf_determinism_test).

%% Test all patterns with determinism
all_patterns_deterministic_test_() ->
    Patterns = yawl_pattern_registry:all_patterns(),
    lists:map(fun(PatternId) ->
        {atom_to_list(PatternId) ++ "_deterministic", fun() ->
            Workflow = create_pattern_workflow(PatternId),
            test_deterministic_execution(Workflow)
        end}
    end, Patterns).

test_deterministic_execution(Workflow) ->
    Seed = 42,

    Run = fun(S) ->
        {ok, P} = gen_yawl:start_link(Workflow, #{scheduler => deterministic, seed => S}),
        {ok, Trace1} = execute_to_completion(P),
        Hash = trace_hash(Trace1),
        ok = gen_yawl:stop(P),
        Hash
    end,

    Hash1 = Run(Seed),
    Hash2 = Run(Seed),

    ?assertEqual(Hash1, Hash2).

%% Test replay matches original
replay_matches_original_test_() ->
    Patterns = [sequence, parallel_split, exclusive_choice],
    lists:map(fun(Pattern) ->
        {atom_to_list(Pattern) ++ "_replay", fun() ->
            Workflow = create_pattern_workflow(Pattern),

            %% Run with nondeterministic scheduler
            {ok, P1} = gen_yawl:start_link(Workflow, #{scheduler => nondeterministic, seed => 42}),
            {ok, Trace1} = execute_to_completion(P1),
            Choices = get_choice_log(P1),
            ok = gen_yawl:stop(P1),

            %% Replay
            {ok, P2} = gen_yawl:start_link(Workflow, #{scheduler => replay, choices => Choices}),
            {ok, Trace2} = execute_to_completion(P2),
            ok = gen_yawl:stop(P2),

            ?assertEqual(Trace1, Trace2)
        end}
    end, Patterns).
```

**2. Integrate ln_sched with gen_yawl**:

Modify `/Users/sac/cre/src/core/gen_yawl.erl`:
```erlang
%% In init/1, add scheduler initialization
init(NetArg) ->
    SchedulerMode = proplists:get_value(scheduler_mode, NetArg, nondeterministic),
    Sched = case SchedulerMode of
        replay ->
            Choices = proplists:get_value(choices, NetArg, []),
            ln_sched:init(replay, Choices);
        _ ->
            Seed = proplists:get_value(seed, NetArg),
            case Seed of
                undefined -> ln_sched:init(SchedulerMode);
                _ -> ln_sched:init(SchedulerMode, Seed)
            end
    end,

    %% ... existing init code ...

    {ok, #wrapper_state{
        net_state = NetState,
        scheduler = Sched,  %% Add scheduler to state
        ...
    }}.

%% In progress loop, use scheduler for transition selection
handle_cast(continue, #wrapper_state{net_state = NetState, scheduler = Sched0} = WrapperState) ->
    Enabled = get_enabled_transitions(NetState),
    Candidates = label_candidates(Enabled),

    {Selected, Sched1} = ln_sched:choose(Candidates, Sched0),
    {Transition, _Label} = Selected,

    %% Fire transition
    case fire_transition(Transition, NetState) of
        {ok, NewNetState} ->
            NewWrapperState = WrapperState#wrapper_state{
                net_state = NewNetState,
                scheduler = Sched1
            },
            continue(self()),
            {noreply, NewWrapperState};
        {error, Reason} ->
            {stop, {transition_failed, Reason}, WrapperState}
    end.
```

### Phase 6: Performance Benchmarks (2-3 weeks)

**Objective**: Systematic performance measurement with reported metrics.

**1. Create `test/bench/` directory**:
```
test/bench/
  ├── microbench.erl
  ├── pattern_bench.erl
  ├── scaling_bench.erl
  └── benchmark_runner.erl
```

**2. Microbenchmark framework** (`test/bench/microbench.erl`):
```erlang
-module(microbench).

-export([measure/2, measure/3]).
-export([run_suite/1, report/1]).

-record(bench_result, {
    name :: binary(),
    iterations :: pos_integer(),
    total_us :: non_neg_integer(),
    min_us :: non_neg_integer(),
    max_us :: non_neg_integer(),
    avg_us :: float(),
    median_us :: float(),
    p95_us :: float(),
    p99_us :: float()
}).

%% Measure function execution time
measure(Fun, Iterations) when Iterations > 0 ->
    measure(Fun, Iterations, #{}).

measure(Fun, Iterations, Options) ->
    %% Warm up
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, min(Iterations div 10, 100))),

    %% Measure
    Times = [timer:tc(Fun) || _ <- lists:seq(1, Iterations)],
    TimesUs = [Us || {Us, _} <- Times],

    TotalUs = lists:sum(TimesUs),
    MinUs = lists:min(TimesUs),
    MaxUs = lists:max(TimesUs),
    AvgUs = TotalUs / Iterations,
    SortedUs = lists:sort(TimesUs),
    MedianUs = median(SortedUs),
    P95Us = percentile(SortedUs, 95),
    P99Us = percentile(SortedUs, 99),

    #bench_result{
        name = maps:get(name, Options, <<"unnamed">>),
        iterations = Iterations,
        total_us = TotalUs,
        min_us = MinUs,
        max_us = MaxUs,
        avg_us = AvgUs,
        median_us = MedianUs,
        p95_us = P95Us,
        p99_us = P99Us
    }.

%% Run benchmark suite
run_suite(Benchmarks) when is_list(Benchmarks) ->
    lists:map(fun({Name, Fun, Iterations}) ->
        Result = measure(Fun, Iterations, #{name => Name}),
        Result
    end, Benchmarks).

%% Generate report
report(Results) ->
    lists:foreach(fun(#bench_result{name = Name, avg_us = Avg, p95_us = P95}) ->
        io:format("~s: avg=~p us, p95=~p us~n", [Name, Avg, P95])
    end, Results).

median(List) ->
    Len = length(List),
    Mid = Len div 2,
    lists:nth(Mid + 1, List).

percentile(List, P) ->
    Len = length(List),
    Index = max(1, min(Len, (P * Len) div 100)),
    lists:nth(Index, List).
```

**3. Pattern benchmarks** (`test/bench/pattern_bench.erl`):
```erlang
-module(pattern_bench).

-export([run_microbenchmarks/0, run_scaling_benchmarks/0]).

%% Microbench: 10k task steps, 100 branch parallel join
run_microbenchmarks() ->
    Benchmarks = [
        {<<"sequence_10k_steps">>, fun() -> bench_sequence(10000) end, 100},
        {<<"parallel_100_branches">>, fun() -> bench_parallel(100) end, 50},
        {<<"discriminator_repeated">>, fun() -> bench_discriminator(1000) end, 50}
    ],

    Results = microbench:run_suite(Benchmarks),
    microbench:report(Results).

bench_sequence(Steps) ->
    Workflow = create_long_sequence(Steps),
    {ok, P} = gen_yawl:start_link(Workflow, []),
    {ok, _Receipts} = gen_yawl:drain(P, Steps * 2),
    ok = gen_yawl:stop(P).

bench_parallel(Branches) ->
    Workflow = create_parallel_split(Branches),
    {ok, P} = gen_yawl:start_link(Workflow, []),
    {ok, _Receipts} = gen_yawl:drain(P, Branches * 2),
    ok = gen_yawl:stop(P).

bench_discriminator(Repeats) ->
    Workflow = create_repeated_discriminator(Repeats),
    {ok, P} = gen_yawl:start_link(Workflow, []),
    {ok, _Receipts} = gen_yawl:drain(P, Repeats * 3),
    ok = gen_yawl:stop(P).

%% Scaling benchmarks
run_scaling_benchmarks() ->
    Sizes = [10, 50, 100, 500, 1000],

    lists:foreach(fun(Size) ->
        io:format("Testing with ~p tasks...~n", [Size]),

        SeqResult = microbench:measure(
            fun() -> bench_sequence(Size) end,
            max(10, 1000 div Size),
            #{name => <<"sequence_", (integer_to_binary(Size))/binary>>}
        ),
        print_result(SeqResult)
    end, Sizes).

print_result(#bench_result{name = Name, avg_us = Avg, p95_us = P95, p99_us = P99}) ->
    io:format("~s: avg=~.2f us, p95=~.2f us, p99=~.2f us~n",
              [Name, Avg, P95, P99]).
```

**4. Benchmark targets validation**:
```erlang
%% Verify bounded overhead per step
prop_bounded_overhead_test() ->
    ?FORALL(Size, range(10, 1000),
        begin
            Result = microbench:measure(
                fun() -> bench_sequence(Size) end,
                max(10, 1000 div Size),
                #{name => <<"overhead_test">>}
            ),

            AvgPerStep = Result#bench_result.avg_us / Size,
            %% Target: < 100 us per step
            AvgPerStep < 100
        end).

%% Verify efficient cancellation
prop_cancellation_efficient_test() ->
    ?FORALL({Size, CancelAt}, {range(100, 1000), range(10, 90)},
        begin
            Workflow = create_long_sequence(Size),
            {ok, P} = gen_yawl:start_link(Workflow, []),

            Start = erlang:monotonic_time(microsecond),
            gen_yawl:cancel(P),
            wait_for_termination(P),
            CancelTime = erlang:monotonic_time(microsecond) - Start,

            ok = gen_yawl:stop(P),

            %% Cancellation should be fast (< 10ms)
            CancelTime < 10000
        end).
```

### Phase 7: Integration and Documentation (1-2 weeks)

**Objective**: Integrate all testing components and document usage.

**1. Create test suite Makefile**:
```makefile
# Makefile for running comprehensive tests

.PHONY: test unit properties model-check determinism bench all

test: all

unit:
	rebar3 eunit --verbose

properties:
	rebar3 eunit --verbose --application=cre test/properties/*

model-check:
	rebar3 eunit --verbose --application=cre test/validate/*

determinism:
	rebar3 eunit --verbose --application=cre test/wf_determinism_test.erl

bench:
	rebar3 escript test/bench/benchmark_runner.escript

all: unit properties model-check determinism
	@echo "All tests passed!"
```

**2. Create testing guide** (`docs/TESTING.md`):
```markdown
# CRE Testing Guide

## Running Tests

### Unit Tests
```bash
make unit
# or
rebar3 eunit
```

### Property-Based Tests
```bash
make properties
# or
rebar3 eunit --verbose test/properties/*
```

### Model Checking
```bash
make model-check
# or
rebar3 eunit test/validate/model_checker_test.erl
```

### Determinism Tests
```bash
make determinism
# or
rebar3 eunit test/wf_determinism_test.erl
```

### Benchmarks
```bash
make bench
# or
rebar3 escript test/bench/benchmark_runner.escript
```

## Writing Property Tests

Use the `gen_simple` framework for generating test data:

```erlang
prop_marking_add_commutative() ->
    ?FORALL({M1, M2}, {marking_gen(), marking_gen()},
        equals(pnet_marking:add(M1, M2),
               pnet_marking:add(M2, M1))).
```

## Writing Model Checks

Use the `workflow_explorer` for bounded state space exploration:

```erlang
explore_workflow_test() ->
    Workflow = create_workflow(),
    Bounds = #{depth => 10, tokens => 5},
    Result = workflow_explorer:explore(Workflow, Bounds).
```

## Performance Targets

- Overhead per step: < 100 us
- Cancellation latency: < 10 ms
- Parallel join (100 branches): < 50 ms
- Sequence (10k steps): < 1 s
```

**3. Update CI/CD** (`.github/workflows/test.yml`):
```yaml
name: Comprehensive Tests

on: [push, pull_request]

jobs:
  unit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - run: make unit

  properties:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - run: make properties

  model-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - run: make model-check

  determinism:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - run: make determinism

  bench:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - run: make bench
```

## Open Questions

1. **PropEr vs minimal framework**: Should we add PropEr as a test dependency or implement minimal generator framework as specified in constraints?
   - **Recommendation**: Implement minimal framework for Phase 1 to meet constraints. Evaluate PropEr for future phases.

2. **Integration scope for scheduler**: Should ln_sched be integrated into gen_yawl directly or through a separate layer?
   - **Recommendation**: Add scheduler state to gen_yawl's wrapper_state record, modify continue loop to use scheduler for transition selection.

3. **Depth and token bounds for model checking**: What are reasonable defaults for depth D and token bound K?
   - **Recommendation**: Start with D=20, K=10. Make configurable per workflow type. Add to research: "Use different bounds for different pattern complexities."

4. **Performance targets**: What are the specific performance goals mentioned in constraints?
   - **Recommendation**: Clarify "bounded overhead per step" - suggest < 100 us/step based on typical Erlang VM performance.

5. **Test execution time**: How long can the full test suite take?
   - **Recommendation**: Target < 5 minutes for full test suite. Use parallel test execution where possible.

6. **Coverage requirements**: Should we measure code coverage?
   - **Recommendation**: Use cover tool to ensure > 80% coverage for critical modules (pnet_*, ln_sched, gen_yawl).

7. **Regression testing strategy**: How to detect regressions?
   - **Recommendation**: Store benchmark results in version control, fail if performance degrades > 20% from baseline.

8. **Property test shrinking**: Should minimal framework implement shrinking?
   - **Recommendation**: Yes, implement basic shrinking for integers and lists to make failures easier to diagnose.

9. **Determinism testing scope**: Should all patterns be tested for determinism or just a subset?
   - **Recommendation**: Test all 43 YAWL patterns for determinism. Use pattern registry to automatically generate tests.

10. **Model checking completeness**: How do we know model checking found all bugs?
    - **Recommendation**: Document that model checking is bounded (D, K). Use multiple increasing bounds to build confidence. Combine with property-based testing.
