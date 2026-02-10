Alright. This is a **good inflection point** in the codebase.
What you’ve pasted is not “just another module” — **`gen_yawl` is now the semantic center of gravity** for CRE/YAWL execution. If you don’t aggressively refactor around it, you’ll end up with a split-brain engine.

Below is a **concrete, ordered refactor plan** to make **`gen_yawl` the single authoritative execution substrate** and to eliminate accidental bypasses, shadow logic, and semantic drift.

I’ll keep this crisp and surgical.

---

# Goal

> **All workflow execution, state mutation, transition firing, and recovery MUST flow through `gen_yawl`.**
> Anything else is legacy compatibility or a thin adapter.

No hidden `gen_pnet` calls.
No alternate fire loops.
No duplicated marking logic.

---

## Phase 0 — Define the Rule (Non-Negotiable)

Before touching code, lock in the invariant:

> **Invariant:**
> *If a Petri net transition fires, user state (`usr_info`) MAY change, and that change MUST be observable, durable, and replayable.*

This is exactly what `gen_yawl:fire/3 → {produce, Map, NewUsrInfo}` enforces.

Everything else must bow to this.

---

## Phase 1 — Inventory & Containment

### 1.1 Find All gen_pnet Entry Points

Search for:

* `gen_pnet:start_link`
* `gen_pnet:step`
* `gen_pnet:drain`
* direct `#net_state{}` mutation
* any custom fire loops

**Action:**

* Mark every usage as one of:

  * ✅ Adapter (acceptable)
  * ⚠️ Duplicate execution logic (must be removed)
  * ❌ Illegal bypass (must be deleted)

If code can mutate markings or consume tokens **without** going through `gen_yawl:progress/2`, it’s illegal.

---

### 1.2 Freeze gen_pnet at the Boundary

You already did the right thing:
`gen_yawl` **wraps** `gen_pnet`, not extends it semantically.

Now enforce:

* No other module is allowed to call `gen_pnet` directly
* `gen_pnet` becomes an internal dependency, not a public API

This is a *conceptual firewall*.

---

## Phase 2 — Mandatory Refactors (Hard Requirements)

### 2.1 All fire/3 Implementations Must Support 3-Tuple

Audit every YAWL pattern module:

```erlang
fire(Trsn, Mode, UsrInfo) -> ...
```

**Required refactor:**

* Even if they don’t update user info today, they must **return `{produce, Map, UsrInfo}`** explicitly or consciously return 2-tuple.
* No pattern module should assume state is immutable.

This prevents future “why didn’t state update?” bugs.

---

### 2.2 Eliminate Any External usr_info Mutation

Search for:

* `NetState#net_state{usr_info = ...}` outside `gen_yawl`

**Action:**

* Move logic into:

  * `fire/3`
  * or `trigger/3` (token-scoped side effects only)

**Rule:**
`usr_info` is *transition-scoped*, not *process-scoped*.

---

### 2.3 Single Progress Loop Authority

You already centralized execution in:

```erlang
progress/2
attempt_progress/4
handle_cast(continue, ...)
```

Now enforce:

* No module may:

  * spin its own continue loop
  * call `fire/3` directly
  * simulate marking evolution

All execution must pass through:

```erlang
gen_yawl:step/1
gen_yawl:drain/2
continue(self())
```

---

## Phase 3 — API Surface Cleanup

### 3.1 Make gen_yawl the Public Engine

Public docs, examples, and APIs must say:

❌ “gen_pnet-based execution”
✅ **“gen_yawl execution engine”**

Concrete actions:

* Update docs
* Update examples
* Update typespecs to reference `gen_yawl:fire_result()`

---

### 3.2 Deprecate gen_pnet Symbols (Soft)

Do **not** remove gen_pnet.
Do mark it as **internal**:

* Move docs to `@private`
* Stop exporting helper functions externally
* Add comments: “Do not call directly; use gen_yawl”

This keeps backward compatibility without encouraging misuse.

---

## Phase 4 — Recovery, Telemetry, and Checkpoint Coherence

You already did something important here:

* `yawl_recovery:maybe_checkpoint/…`
* `yawl_checkpoint:checkpoint_save/2`
* `yawl_telemetry:emit/2`

Now tighten the rule:

> **Every state mutation must be representable as:**
> `(Marking_before, Fire, Marking_after, UsrInfo_after)`

### Actions:

* Ensure checkpoints capture:

  * marking
  * usr_info
* Ensure replay re-enters via `gen_yawl:init/1 + marking injection`
* No “partial restore” paths

This is what makes CRE auditable, replayable, and enterprise-safe.

---

## Phase 5 — Mechanical Enforcement (Optional but Strongly Recommended)

### 5.1 Add Guard Rails

Add compile-time or runtime assertions:

* Crash if:

  * a callback returns `{produce, Map}` but mutates state elsewhere
  * a transition fires without going through `progress/2`
* Log warnings when `fire/3` returns 2-tuple repeatedly (future debt signal)

---

### 5.2 Narrow the Escape Hatches

Limit these to **explicit legacy adapters only**:

* `inject/2`
* `withdraw/2`
* `cancel_region/2`

And document them as **operational overrides**, not workflow logic.

---

## Phase 6 — Mental Model (This Matters)

Think of `gen_yawl` as:

> **A deterministic state transition machine with side-effect scheduling**, not “a Petri net runner”.

* Petri nets = topology
* `fire/3` = state transition function
* `usr_info` = workflow ontology
* tokens = causality, not state

Once everyone on the team internalizes that, the architecture stabilizes.

---

## Final Verdict

You already did the **hard part correctly**:

* Wrapped `gen_pnet`
* Centralized progress
* Made state mutation explicit
* Made recovery first-class

Now the task is **discipline and deletion**, not invention.

Refactor until **no meaningful workflow logic can execute without passing through `gen_yawl`**.

At that point:

* YAWL semantics are enforceable
* Human-in-the-loop state is correct
* GCP / enterprise readiness becomes straightforward
* Future extensions stop being dangerous

This is the right spine. Everything else must align to it.



Below is a **Product Requirements Document (PRD)** for making **CRE – YAWL Workflow Engine** ready for **Google Cloud Marketplace** distribution.
This is written as an execution-grade PRD: clear scope, concrete deliverables, and acceptance criteria, not marketing fluff.

---

# PRD: CRE – Google Cloud Marketplace Readiness

## 1. Purpose & Goals

### Objective

Prepare **CRE (YAWL Workflow Engine)** for **production-grade distribution, deployment, billing, and support** via **Google Cloud Marketplace**.

### Success Criteria

* CRE is installable from Marketplace with **one-click deployment**
* Supports **billing integration**, **metering**, and **licensing**
* Meets **GCP security, compliance, and operational standards**
* Clear **support, upgrade, and lifecycle story**
* Approved by Google Marketplace review

---

## 2. Target Users

### Primary Personas

* **Platform Engineers** deploying workflow engines on GKE or Compute Engine
* **Data / ML Infrastructure Teams** orchestrating distributed workflows
* **Research & HPC teams** migrating from on-prem Erlang clusters
* **Enterprises** requiring auditability, XES logs, and human-in-the-loop controls

### Deployment Context

* GKE (primary)
* Compute Engine VM (secondary)
* Private VPCs
* Enterprise IAM + audit constraints

---

## 3. Marketplace Distribution Model

### Supported Marketplace Models

**Required (v1):**

* Container-based solution via **GKE Application**
* Paid or Free (decision required)

**Optional (v2):**

* VM-based deployment (single-node CRE master + workers)

### Billing Strategy (Decision Required)

Choose **one** initially:

* BYOL (Bring Your Own License)
* Usage-based (recommended long-term)
* Free OSS listing with paid support SKU

---

## 4. Architecture Requirements (GCP)

### Reference Architecture

* GKE Cluster

  * CRE Master (StatefulSet)
  * CRE Workers (Deployment, autoscaled)
  * Optional Web Dashboard
* Cloud Load Balancer / Ingress
* Cloud Monitoring + Logging
* Persistent storage (GCS / Persistent Disk)
* Optional Cloud SQL / AlloyDB (future)

### Mandatory GCP Integrations

* **GKE**
* **Cloud Logging**
* **Cloud Monitoring**
* **IAM**
* **VPC-native networking**

---

## 5. Deployment Requirements

### Helm Chart (Required)

* Parameterized Helm chart for CRE
* Values:

  * Node counts
  * Worker autoscaling
  * Resource requests/limits
  * Persistence
  * TLS
  * Auth mode
  * Telemetry toggles

### Marketplace Deployment Spec

* `application.yaml` (Marketplace schema)
* Schema-based UI inputs
* Validation rules
* Defaults for minimal viable deployment

### One-Click Install

* Deployable from Marketplace UI
* No manual kubectl steps required

---

## 6. Container & Image Requirements

### Container Images

* Multi-arch (amd64 mandatory)
* Hosted in **Artifact Registry**
* Immutable version tags
* SBOM generation

### Image Security

* Distroless or minimal base image
* No root containers
* Vulnerability scanning (Trivy / GCP scanning)

---

## 7. Configuration & Secrets

### Secrets Management

* Kubernetes Secrets (minimum)
* Optional Secret Manager integration
* No secrets in Helm values.yaml

### Config

* ConfigMaps for runtime settings
* Hot-reload where possible
* Environment-variable override support

---

## 8. Observability & Operations

### Logging

* Structured logs to Cloud Logging
* Correlation IDs
* Workflow / case identifiers

### Metrics

* Cloud Monitoring export
* Workflow throughput
* Task latency
* Worker utilization
* Failure counts

### Tracing

* OpenTelemetry → Cloud Trace
* Toggleable (cost-aware)

### Health Checks

* Liveness & readiness probes
* Startup probes for Erlang nodes

---

## 9. Security & Compliance

### Mandatory Controls

* RBAC for Kubernetes
* Principle of least privilege IAM
* NetworkPolicies
* Pod Security Standards (baseline+)

### Encryption

* TLS in transit
* Encrypted disks at rest
* Optional customer-managed encryption keys (CMEK)

### Compliance Readiness

* Audit logging
* XES log retention
* Deterministic workflow replay support

---

## 10. Authentication & Authorization

### Required

* GCP IAM integration (service accounts)
* Role-based access in dashboard

### Optional (v2)

* OAuth / OIDC
* Identity-Aware Proxy (IAP)

---

## 11. Marketplace Licensing & Metering

### Metering Integration

* Usage metric definition:

  * Active workflows
  * Worker-hours
  * Task executions
* Reporting via Marketplace Metering API

### License Enforcement

* Startup validation
* Grace periods
* Clear failure modes

---

## 12. Documentation Requirements

### Marketplace Listing Docs

* Product overview
* Architecture diagram
* Deployment guide
* Upgrade guide
* Security model
* Cost model

### Operational Docs

* Scaling
* Backup & restore
* Disaster recovery
* Debugging playbook

### Legal

* License (Apache 2.0 confirmed)
* Third-party notices
* Privacy policy
* Support terms

---

## 13. Upgrade & Lifecycle

### Versioning

* Semantic versioning
* Immutable releases
* Backward-compatible configs

### Upgrades

* Rolling upgrades supported
* Data migration strategy
* Downgrade policy defined

### Decommissioning

* Clean uninstall
* Data retention options

---

## 14. Support & SRE Readiness

### Support Model

* Community vs paid tiers
* SLA definitions
* Escalation paths

### Monitoring Alerts

* Preconfigured alert policies
* SLO definitions
* Error budget tracking

---

## 15. Testing & Validation

### Required Tests

* Helm install/uninstall
* GKE conformance
* Load testing
* Failure injection
* Network partition tests

### Marketplace Validation

* Automated deployment validation
* Security scan pass
* Billing verification
* UI schema validation

---

## 16. Acceptance Criteria (Go / No-Go)

Marketplace readiness is achieved when:

* ✅ One-click deploy works in a fresh GCP project
* ✅ Passes Marketplace technical & security review
* ✅ Metering reports usage correctly
* ✅ Logs and metrics visible in GCP console
* ✅ Upgrade path documented and tested
* ✅ Support and licensing terms approved

---

## 17. Out of Scope (v1)

* Multi-cloud marketplace support
* Advanced IAM federation
* SLA-backed HA across regions
* Managed SaaS offering

---

## 18. Open Decisions (Must Resolve)

1. **Free vs Paid Marketplace listing**
2. **Metering unit definition**
3. **GKE-only vs GKE + VM**
4. **Dashboard exposure by default**
5. **Support tier commitment**

---

This PRD defines *everything required* for Marketplace approval and enterprise-grade deployment.
Execution now becomes a sequencing problem: **Helm → Observability → Security → Metering → Docs → Review**.
