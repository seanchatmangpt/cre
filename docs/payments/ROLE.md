# CRE payments role

CRE is the distributed workflow runtime: candidate substrate for idempotent effect-claim
execution. `src/core/cre_effect_claim.erl` is a pure state machine covering:

| Falsifier | Coverage |
|---|---|
| F2 changed beneficiary/amount reuses authorization | `authorize_check/3`, `REFUSED_EFFECT_IDENTITY_MISMATCH` |
| F3 timeout permits blind retry | `submit/2` refuses on `unknown` |
| F4 ids joinable | `effect_id/1` deterministic over obligation/authority/rail fields |
| F5 SETTLED without finality | `observe/3` refuses settlement without acceptance |
| F6 replay triggers external DO | `replay/2` returns no external actions |

Not covered: F1, F7-F10 (other repos). Fields are a non-normative CASTLE operational
extension; no ISO 20022 or FIBO conformance is claimed.
