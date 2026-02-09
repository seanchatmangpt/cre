# NATO Concuerror Deterrence Tests

Protocol and concurrency stress tests for governance coordination, modelled as a NATO-style emergency session "STRAT-STABILITY 2030". Tests verify critical invariants under adversarial interleavings.

## Invariants

1. **No publish without consensus**: A communique cannot be published until all delegations approve (or stand-aside if policy allows).

2. **Two-channel confirmation for posture change**: An "alert posture change" requires independent confirmation from both Intel and Military Advisory channels.

3. **No split-brain draft**: At most one "current" draft at any instant (single-writer model).

4. **No classified leak**: Press Office cannot receive `{classified, _}` events when `press_receives => declassified_only`.

5. **Termination**: Session must end in one of `published`, `chair_summary`, or `adjourned`.

6. **Audit trail consistency**: The official record is an ordered stream; every publish references a valid draft hash.

## Modules

| Module | Purpose |
|--------|---------|
| `nato_conf` | gen_server holding conference state (drafts, approvals, posture, press, audit) |
| `nato_assert` | Invariant checks over nato_conf state |
| `nato_ids` | Draft hashing for audit chain integrity |

## Running Tests

### EUnit (no Concuerror)

All 6 scenarios run as EUnit tests:

```bash
rebar3 eunit --module=nato_deterrence_concuerror_tests
```

### Concuerror (optional)

Concuerror explores many interleavings to find race conditions. It requires the `concuerror` profile:

```bash
rebar3 as concuerror eunit --module=nato_deterrence_concuerror_tests
```

To run Concuerror's model checker on one scenario:

```bash
rebar3 as concuerror eunit --module=nato_deterrence_concuerror_tests
escript scripts/run_concuerror.escript
```

**Note**: Concuerror 0.21.0 does not support `erlang:monitor/3`, which is used internally by `gen_server`. If you see "Concuerror does not support calls to built-in erlang:monitor/3", the nato_conf gen_server hits this limitation. The EUnit tests validate logic without full interleaving exploration.

### CLI (Concuerror)

If Concuerror is installed separately:

```bash
concuerror -m nato_deterrence_concuerror_tests -t scenario_consensus_publish
```

### wf_conc Spec

Generate a Concuerror spec via `wf_conc`:

```erlang
Spec = wf_conc:spec(#{
    module => nato_deterrence_concuerror_tests,
    entry => scenario_consensus_publish,
    timeout_ms => 5000
}).
% Returns #{tool => concuerror, args => ["--module", "nato_deterrence_concuerror_tests", ...]}
```

## Scenarios

| Scenario | Invariants tested |
|----------|-------------------|
| `scenario_consensus_publish` | Consensus, audit chain |
| `scenario_two_channel_posture_gate` | Two-channel gate, audit |
| `scenario_draft_race_single_writer` | Single current draft |
| `scenario_no_classified_leak_to_press` | No classified in press |
| `scenario_deadlock_fallback_chair_summary` | Termination |
| `scenario_audit_chain_integrity` | Audit chain, hash reference |

## Extension Points

- **Partition simulation**: Inject message drops or delayed delivery; prove invariants still hold.
- **Appeal chain**: Chair ruling, appeal, vote interleaved with drafting.
- **Multi-room breakout**: Sub-conferences feeding a single final communique.
- **Two-stage publish**: "Pre-brief" then "public release" with strict gating.

## Policy Options

- `publish_requires => unanimous` – All delegations must approve.
- `allow_stand_aside => true | false` – Whether stand-aside counts toward consensus.
- `posture_requires_two_channels => true` – Both intel and mil must confirm.
- `press_receives => declassified_only` – Filter classified from press outbox.
- `fallback => chair_summary_after_rounds` – When consensus fails, end with chair_summary.
- `max_rounds => N` – Rounds before fallback triggers.
