# Test Gaps Analysis

**Generated**: 2026-02-08
**Project**: CRE (Common Runtime Environment)
**Total Source Modules**: 247
**Total Test Files**: 104 (90 EUnit + 14 Common Test)
**Estimated Test Coverage**: 54% (need 76% more modules tested)

---

## Executive Summary

The CRE codebase has significant test coverage gaps across multiple domains. While core YAWL patterns and some mining modules have good test coverage, many auxiliary modules, infrastructure components, and recent features lack comprehensive tests.

**Key Statistics**:
- **Modules Without Tests**: 134 of 247 (54%)
- **Test Files**: 104 total
  - EUnit tests: 90
  - Common Test suites: 14
- **Test Coverage**: Estimated 54% of codebase

---

## Test Coverage by Directory

### Well-Tested Directories (> 70% coverage)

| Directory | Modules | Tests | Coverage | Notes |
|-----------|---------|-------|----------|-------|
| `test/` (root) | 90 | 90 | 100% | Direct test files |
| `test/mining/` | 11 | 11 | 100% | Mining modules well tested |
| `test/patterns/` | 9 | 9 | 100% | Pattern strategies tested |

### Partially-Tested Directories (30-70% coverage)

| Directory | Modules | Tests | Coverage | Gaps |
|-----------|---------|-------|----------|------|
| `src/core/` | 13 | 5 | 38% | gen_yawl, gen_pnet need more tests |
| `src/wf/` | 76 | 14 | 18% | Most workflow utilities untested |
| `src/yawl/` | 51 | 22 | 43% | Engine, executor need coverage |
| `src/patterns/` | 59 | 12 | 20% | Basic patterns tested, advanced not |
| `src/mining/` | 14 | 11 | 79% | Good coverage, few gaps |
| `src/active/` | 7 | 0 | 0% | Active token behavior untested |

### Untested Directories (< 30% coverage)

| Directory | Modules | Tests | Coverage | Priority |
|-----------|---------|-------|----------|----------|
| `src/ga/` | 6 | 2 | 33% | Medium |
| `src/ml/` | 4 | 0 | 0% | High |
| `src/api/` | 13 | 0 | 0% | High |
| `src/http/` | 10 | 0 | 0% | Medium |
| `src/integration/` | 4 | 0 | 0% | Medium |
| `src/diagnostics/` | 3 | 0 | 0% | Low |
| `src/rust_implementations/` | 16 | 5 | 31% | Low (Rust has own tests) |
| `src/verification/` | 4 | 0 | 0% | High |
| `src/nato/` | 5 | 0 | 0% | Low |
| `src/security/` | 2 | 0 | 0% | High |

---

## Critical Test Gaps

### 1. Active Token Behavior (CRITICAL)

**Modules**: `src/active/gen_active_token.erl`, `src/core/place_coordinator.erl`

**Current Tests**: None

**Required Test Coverage**:
```erlang
%% test/active/active_token_SUITE.erl
-module(active_token_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Token Lifecycle
t_token_birth(Config) -> ...
t_token_initialization(Config) -> ...
t_token_idle_state(Config) -> ...
t_token_place_registration(Config) -> ...
t_token_participation(Config) -> ...
t_token_migration(Config) -> ...
t_token_communication(Config) -> ...
t_token_expiration(Config) -> ...
t_token_termination(Config) -> ...

%% Error Cases
t_token_creation_failure(Config) -> ...
t_migration_failure(Config) -> ...
t_communication_timeout(Config) -> ...
t_recovery_from_failed(Config) -> ...

%% Integration
t_place_coordinator_tracking(Config) -> ...
t_multiple_tokens_same_place(Config) -> ...
t_token_concurrent_operations(Config) -> ...
```

**Estimated Effort**: 12 hours

---

### 2. Verification Module (HIGH)

**Modules**: `src/verification/soundness.erl`, `src/diagnostics/woflan.erl`

**Current Tests**: None

**Required Test Coverage**:
```erlang
%% test/verification/soundness_SUITE.erl
-module(soundness_SUITE).

%% Soundness Checking
t_sound_workflow(Config) -> ...
t_unsound_workflow_deadlock(Config) -> ...
t_unsound_workflow_livelock(Config) -> ...
t_unsound_workflow_unbounded(Config) -> ...

%% Woflan Diagnostics
t_woflan_sound_workflow(Config) -> ...
t_woflan_unsound_workflow(Config) -> ...
t_woflan_siphon_analysis(Config) -> ...
t_woflan_trap_analysis(Config) -> ...
```

**Estimated Effort**: 8 hours

---

### 3. ML/Predictive Mining (HIGH)

**Modules**: `src/mining/pred_rnn.erl`, `src/mining/pred_training.erl`, `src/ml/*.erl`

**Current Tests**: Partial (predictive_mining_tests.erl exists)

**Required Test Coverage**:
```erlang
%% test/mining/pred_training_SUITE.erl
-module(pred_training_SUITE).

t_sequence_extraction(Config) -> ...
t_feature_extraction(Config) -> ...
t_training_set_build(Config) -> ...
t_train_test_split(Config) -> ...
t_sequence_padding(Config) -> ...
t_batch_normalization(Config) -> ...
t_cross_validation(Config) -> ...
t_model_checkpoint(Config) -> ...

%% test/mining/pred_rnn_SUITE.erl
-module(pred_rnn_SUITE).

t_rnn_forward_pass(Config) -> ...
t_rnn_backward_pass(Config) -> ...
t_lstm_cell(Config) -> ...
t_gru_cell(Config) -> ...
t_sequence_prediction(Config) -> ...
```

**Estimated Effort**: 10 hours

---

### 4. API Modules (HIGH)

**Modules**: `src/api/*.erl`

**Current Tests**: None

**Required Test Coverage**:
```erlang
%% test/api/cre_api_SUITE.erl
-module(cre_api_SUITE).

t_client_start_stop(Config) -> ...
t_workflow_submission(Config) -> ...
t_workflow_query(Config) -> ...
t_workflow_cancellation(Config) -> ...
t_batch_operations(Config) -> ...
t_api_error_handling(Config) -> ...
t_api_authentication(Config) -> ...
```

**Estimated Effort**: 8 hours

---

### 5. YAWL Engine (MEDIUM)

**Modules**: `src/yawl/yawl_engine.erl`, `src/yawl/yawl_executor.erl`

**Current Tests**: Partial (cre_yawl_SUITE.erl exists)

**Required Test Coverage**:
```erlang
%% test/yawl/yawl_engine_advanced_SUITE.erl
-module(yawl_engine_advanced_SUITE).

t_workflow_lifecycle(Config) -> ...
t_workflow_persistence(Config) -> ...
t_workflow_recovery(Config) -> ...
t_workflow_monitoring(Config) -> ...
t_workflow_telemetry(Config) -> ...
t_concurrent_workflows(Config) -> ...
```

**Estimated Effort**: 6 hours

---

## Test Infrastructure Gaps

### Missing Test Utilities

1. **Event Log Generators**
   - Need utilities to generate realistic XES/OCEL logs
   - Various patterns (sequential, parallel, loops)
   - Anomaly injection for testing

2. **Workflow Fixtures**
   - Complex workflow definitions
   - Edge case workflows
   - Performance test workflows

3. **Mock Generators**
   - Mnesia mock for testing
   - Process registry mock
   - External service mocks

4. **Test Assertions**
   - Custom assertions for Petri nets
   - Workflow state assertions
   - Pattern-specific matchers

---

## Test Priority Matrix

| Priority | Module | Category | Effort | Impact |
|----------|--------|----------|--------|--------|
| CRITICAL | gen_active_token | Active | 12h | Enables advanced patterns |
| CRITICAL | place_coordinator | Active | 8h | Token coordination |
| HIGH | pred_training | ML | 6h | Predictive mining |
| HIGH | pred_rnn | ML | 8h | RNN models |
| HIGH | soundness | Verification | 8h | Correctness guarantees |
| HIGH | cre_api | API | 8h | Public interface |
| HIGH | security | Security | 4h | Safety |
| MEDIUM | yawl_engine | Core | 6h | Workflow execution |
| MEDIUM | wf_persistent_timer | WF | 4h | Timer reliability |
| MEDIUM | ga_constitution | GA | 4h | Configuration |
| LOW | diagnostics | Support | 2h | Debugging |
| LOW | telemetry | Support | 2h | Observability |

**Total Estimated Effort**: 72 hours

---

## Recommended Testing Strategy

### Phase 1: Critical Functionality (Week 1-2)
1. Active token behavior tests (20h)
2. Verification/soundness tests (8h)
3. Critical security tests (4h)

### Phase 2: Core Features (Week 3-4)
1. ML/Predictive mining tests (14h)
2. API tests (8h)
3. YAWL engine advanced tests (6h)

### Phase 3: Infrastructure (Week 5)
1. Workflow utilities tests (8h)
2. GA constitution tests (4h)
3. Telemetry/diagnostics tests (4h)

### Phase 4: Coverage Expansion (Week 6+)
1. Remaining modules
2. Edge case coverage
3. Property-based testing

---

## Property-Based Testing Opportunities

The following modules would benefit from PropEr property-based tests:

1. **Pattern Modules**
   - Commutativity of parallel execution
   - Idempotence of cancellation
   - Associativity of choice operations

2. **Mining Modules**
   - Alpha algorithm produces sound nets
   - Conformance checking metrics bounded
   - Process discovery invariants

3. **Core Modules**
   - gen_pnet state transition invariants
   - Token conservation
   - Marking preservation

---

## Test Coverage Goals

| Category | Current | Target | Gap |
|----------|---------|--------|-----|
| Active Token | 0% | 90% | +90% |
| Mining | 79% | 90% | +11% |
| Patterns | 20% | 80% | +60% |
| Verification | 0% | 85% | +85% |
| API | 0% | 85% | +85% |
| Core WF | 43% | 80% | +37% |
| ML | 0% | 85% | +85% |
| **Overall** | **54%** | **80%** | **+26%** |

---

## Testing Best Practices for CRE

### 1. Test Organization
- EUnit for unit tests (fast, isolated)
- Common Test for integration (slow, stateful)
- Separate test data in `test/fixtures/`

### 2. Test Naming
- `t_<function>_success(Config)` - happy path
- `t_<function>_error_<condition>(Config)` - error cases
- `t_<function>_edge_<condition>(Config)` - edge cases

### 3. Test Structure
```erlang
t_example_test(Config) ->
    %% Arrange
    Input = setup_input(),
    Expected = expected_output(),

    %% Act
    Actual = module_under_test:function(Input),

    %% Assert
    ?assertEqual(Expected, Actual).
```

### 4. Async Testing
```erlang
t_async_test(Config) ->
    {ok, Pid} = module:start_link(),
    Ref = monitor(process, Pid),

    Pid ! {async_request, self()},

    receive
        {response, Result} ->
            ?assertMatch(expected, Result);
        {'DOWN', Ref, process, Pid, Reason} ->
            ?fail({unexpected_exit, Reason})
    after 5000 ->
        ?assertMatch({error, timeout}, module:get_status())
    end.
```

---

## Test Metrics Dashboard

Proposed metrics to track:

1. **Coverage**
   - Line coverage per module
   - Branch coverage per module
   - Function coverage per module

2. **Test Health**
   - Pass rate per suite
   - Flaky test detection
   - Test execution time trends

3. **Test Debt**
   - Modules without tests
   - Uncovered critical paths
   - Deprecated tests

---

## Continuous Integration

Recommended CI test pipeline:

```yaml
test_pipeline:
  stage: test
  script:
    - rebar3 compile
    - rebar3 eunit -v
    - rebar3 ct -v
    - rebar3 dialyzer
    - rebar3 cover
  coverage: '/(\d+\.\d+)%/'
  artifacts:
    paths:
      - _build/test/cover/
```

---

## Next Steps

1. **Immediate**: Create test for active token behavior (enables advanced patterns)
2. **Short-term**: Add verification tests (correctness guarantees)
3. **Medium-term**: Expand coverage to 80% overall

---

**Last Updated**: 2026-02-08
**Test Owner**: Development Team
**Next Review**: Bi-weekly during test expansion
