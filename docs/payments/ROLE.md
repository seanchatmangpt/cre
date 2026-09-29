# CRE payments role

`src/core/cre_effect_claim.erl` is a pure, in-memory effect-claim state machine (a map-based
store; no persistence, no concurrency control, no rail adapter, no external actions of any
kind). It is a model of the claim/observation rules, exercised by
`test/effect_claim/cre_effect_claim_tests.erl` against the module itself. It is not a durable or
distributed claim store.

| Falsifier | What is exercised |
|---|---|
| F2 changed beneficiary/amount reuses authorization | `effect_id/1` changes when any of the 12 identity fields changes (each tested); `authorize_check/3` returns `REFUSED_EFFECT_IDENTITY_MISMATCH` for a different effect. It compares ids only; it does not verify who issued the authorization. |
| F3 timeout permits blind retry | `submit/2` refuses on `unknown`; `prepare/2` refuses a second live effect for the same obligation in prepared, submitted, unknown, accepted and settled; only `rejected`/`returned`/`reversed` release it. |
| F4 ids joinable | Only `effect_id/1` is covered: deterministic sha256 over the identity fields (pinned vector). Joinability across obligation/idempotency key/rail correlation/ledger/receipt is not modelled. |
| F5 SETTLED without finality | `observe/3` refuses `settled` with `settlement_without_acceptance` from prepared, submitted, unknown, rejected, returned and reversed (state and settlement count unchanged, also under `replay/2`); only `accepted` may settle, a second settle is `illegal_transition`, and `accepted` cannot regress via `timeout`/`accepted`/`rejected` observations. The module has no notion of rail finality beyond this state; acceptance is an observation, not proof of finality. |
| F6 replay triggers external DO | `replay/2` folds observations into state. The module has no external-action surface, so the empty action list is structural, not evidence about a real rail or hook system. |

Re-preparing the identical effect is tested as a no-op (state, claim record and obligation index unchanged) in all eight states.

Invariants checked by a seeded random walk over one obligation (200 runs x 40 steps, several
effects per obligation): settled effects <= 1 and live effects <= 1.

Gate: `scripts/effect_claim_gate.sh` builds a minimal rebar3 project from only this module and
`test/effect_claim/` and runs `rebar3 eunit` in it. The repo-wide `rebar3 eunit` does not
compile at the base commit (syntax errors in `src/yawl/yawl_xes.erl` and other modules,
unrelated to this change), so it is not a usable gate for this module.

Not covered: F1, F7-F10 (other repos). Fields are a non-normative CASTLE operational
extension; no ISO 20022 or FIBO conformance is claimed.
