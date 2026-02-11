# Implementation Plan Summary

## Structured Tracing and Replay Logging

### Overview
Extend the existing `ln_trace` module (138 lines) with persistence, replay verification, and OTP logger integration. Instrument `gen_pnet:step/1` and `wf_engine` lifecycle for comprehensive workflow tracing.

### Key Decisions

#### Architecture
- **Extend `ln_trace`** rather than replace it - it's already lightweight and well-designed
- **Functional API** (state in/out) not gen_server - keeps it simple and testable
- **OTP logger integration** for centralized logging - already used in 50+ modules
- **Adapters** for existing telemetry systems - avoid breaking `yawl_telemetry` and `yawl_logging`

#### Scope Decisions
✅ **IN SCOPE:**
- Trace persistence (save/load to JSON file)
- OTP logger integration
- Trace verification and diff
- Adapters for yawl_telemetry and yawl_logging
- Reduction step instrumentation
- Workflow lifecycle instrumentation
- Application configuration

❌ **OUT OF SCOPE:**
- Replacing existing telemetry systems
- Full OpenTelemetry distributed tracing (already exists)
- Real-time trace streaming
- Trace visualization UI
- Trace compression
- Distributed replay
- Changing event formats
- Modifying receipt format

### Implementation Phases

1. **Phase 1**: Extend `ln_trace` with persistence and OTP logger (Priority 1)
2. **Phase 2**: Create `ln_trace_replay` for verification (Priority 2)
3. **Phase 3**: Create `ln_trace_adapter` for telemetry integration (Priority 2)
4. **Phase 4**: Instrument `gen_pnet:step/1` (Priority 3)
5. **Phase 5**: Instrument `wf_engine` lifecycle (Priority 3)
6. **Phase 6**: Add application configuration (Priority 3)
7. **Phase 7**: Add comprehensive tests (Priority 4)
8. **Phase 8**: Add documentation (Priority 4)

### Success Criteria

#### Functional
- [ ] Trace can be saved to file and loaded back
- [ ] Trace events appear in OTP logger
- [ ] Two traces can be compared for exact match
- [ ] Traces can be exported to yawl_telemetry and yawl_logging
- [ ] Reduction steps are logged
- [ ] Workflow lifecycle events are logged
- [ ] Trace level is configurable

#### Performance
- [ ] Zero overhead at 'none' level
- [ ] <1% overhead at 'min' level
- [ ] Documented overhead at 'full' level

#### Quality
- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] Type checking passes (dialyzer)
- [ ] No regressions in existing functionality

### User Stories (9 total)

**Priority 1 (Core functionality):**
- US-001: Add trace persistence to ln_trace
- US-002: Integrate ln_trace with OTP logger

**Priority 2 (Verification and integration):**
- US-003: Create ln_trace_replay module
- US-004: Create ln_trace_adapter module

**Priority 3 (Instrumentation):**
- US-005: Instrument gen_pnet:step/1
- US-006: Instrument wf_engine lifecycle
- US-007: Add application configuration

**Priority 4 (Quality and documentation):**
- US-008: Add comprehensive tests
- US-009: Add documentation and examples

### Key Files

#### Existing
- `src/ln_trace.erl` - Core tracing (extend)
- `src/core/gen_pnet.erl` - Petri net engine (instrument)
- `src/wf/wf_engine.erl` - Workflow engine (instrument)
- `src/pnet/pnet_receipt.erl` - Receipt format (use for verification)
- `src/yawl_telemetry.erl` - Existing telemetry (create adapter)
- `src/yawl_logging.erl` - YAWL logging (create adapter)

#### New
- `src/ln_trace_replay.erl` - Replay and verification
- `src/ln_trace_adapter.erl` - Telemetry adapters

### Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Performance overhead at 'min' level | High | Zero-overhead at 'none', minimal at 'min' |
| Non-deterministic replay | High | Use hash-based receipts from pnet_receipt |
| Integration complexity | Medium | Adapters instead of replacement |
| Memory growth | Medium | Configurable max_events with trimming |

### Next Steps

1. Review and approve implementation plan
2. Create feature branch: `wreckit/017-structured-tracing-and-replay-logging`
3. Start with Phase 1 (US-001, US-002)
4. Complete each phase with automated and manual verification
5. Update PRD as stories are completed
