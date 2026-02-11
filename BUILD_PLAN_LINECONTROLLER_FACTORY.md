# LineController Factory - Build Plan & Swarm Coordination

**Project**: Manufacturing Integrations from Industry Ontology + Deterministic OTP Execution
**MVP Lane**: Security Ops (SOC Automation)
**Build Model**: 20-Agent Swarm with Parallel Tracks
**Timeline**: Aggressive phase delivery

---

## EXECUTIVE SUMMARY

We are building a **replacement manufacturing system** that will eliminate job categories:
- SOC analysts doing routine incident triage (replaced by manufactured line)
- Integration engineers maintaining custom API glue (replaced by ontology → manufacture)
- Ops engineers manually managing incident workflows (replaced by deterministic lines)

The system manufactures integrations and applications deterministically from industry ontology (Σ), runs them as supervised OTP processes with stop-the-line capability, and emits receipts for every build and execution.

**Key Claims (to be proven with metrics):**
- "Manufacturing speed: spec change → new artifacts in <30 seconds"
- "Determinism: identical inputs → identical trace ordering"
- "Cancellation: post-cancel effects = zero"
- "Replacement ROI: ontology work costs 20% of hand-coding equivalent"

---

## 1. MVP LANE CHOICE: SECURITY OPS (INCIDENT TRIAGE)

### Why Security Ops?
- **Board visibility**: SOC breaches/incidents are board-level risk; automation is board-level topic
- **Clear replacement**: triage/escalation analysts are routine-work category (replaceable)
- **Deterministic**: incident classification rules are deterministic (high precision)
- **Easy to manufacture**: 4 standard connectors cover 80% of use case
- **Evidence requirement**: incident handling demands audit trails (native to manufacturing model)

### Lane Definition: Incident Triage to Escalation

```
[Incident Event]
  → Classification (SIEM enrichment)
  → Severity Gate (rule-based escalation)
  → Containment (EDR/SOAR commands)
  → Evidence Pack (create audit ticket)
  → Escalation OR Auto-Resolve
```

**Replacement Target**: reduce SOC analyst time on triage from 40% → 10% (4-6 FTE per 100-person org).

---

## 2. INDUSTRY ONTOLOGY (Σ) - SECURITY OPS LANE

### Σ Core Entities
```
Incident
  ├─ classification: [Malware, Phishing, Recon, Unauthorized Access, ...]
  ├─ severity: [Critical, High, Medium, Low]
  ├─ affected_systems: [System] (CMDB references)
  ├─ event_stream: [Event]  (from SIEM, EDR, network)
  └─ receipt_log: [Receipt]

Gate (decision point)
  ├─ condition: SPARQL ASK query
  ├─ true_path: Line
  ├─ false_path: Line
  └─ evidence: [fact, decision_data]

Stop (hard halt)
  ├─ scope: [activity, region, case]
  ├─ reason: string
  └─ restart_condition: optional

Station (connector/effect)
  ├─ protocol: [HTTP, GRPC, webhook, event_bus]
  ├─ idempotency: Receipt hash
  └─ rollback: optional cancellation handler

Budget
  ├─ max_effects: N
  ├─ max_latency_ms: N
  ├─ max_cost_$ : N
  └─ consequence: {stop | degrade | alert}

Receipt
  ├─ id: UUID
  ├─ timestamp: ISO8601
  ├─ hash: SHA256(inputs)
  ├─ event_type: {build, effect, gate, stop}
  └─ artifacts: [output_refs]
```

### Σ Connectors (Inventory)
```
SIEMConnector (Splunk/Elastic)
  - ingest(AlertEvent) → Incident (classification enrichment)
  - query(SPARQL) → [Incident]
  - receipt: hash(alert_id, enrichment_rules, timestamp)

EDRConnector (CrowdStrike/Sentinel)
  - quarantine(System) → {ok | error}
  - get_evidence(IncidentID) → {file_hashes, process_tree, network}
  - receipt: hash(device_id, action, execution_id)

TicketConnector (Jira/ServiceNow)
  - create(Incident) → TicketID
  - update_status(TicketID, Status) → {ok | error}
  - receipt: hash(ticket_id, state_transition, timestamp)

NotificationConnector (Slack/PagerDuty/Email)
  - notify(recipient, message, urgency) → {ok | error}
  - receipt: hash(recipient_id, message_hash, send_timestamp)
```

---

## 3. CORE COMPONENTS (BUILD SEQUENCE)

### Phase 1: Foundation (ln_ctrl minimal + factory skeleton)

**Deliverable**: `ln_ctrl` runs sequences + parallel splits + synchronization + scope cancellation + receipts.

**Modules**:
- `ln_ctrl_core.erl` - line execution engine (extend WF Substrate)
- `ln_ctrl_budget.erl` - budget tracking + enforcement
- `ln_ctrl_receipt.erl` - receipt generation + append-only log
- `ln_ctrl_andon.erl` - green/yellow/red status signaling

**Tests**:
- Line execution happy path
- cancel-scope halts effects
- receipts are deterministic
- budget constraints enforced

### Phase 2: Factory Pipeline (ggen)

**Deliverable**: RDF ontology → SPARQL extraction → Tera templates → generated Erlang modules + Terraform.

**Pipeline**:
```
input/security-ops.rdf
  ↓ [SPARQL queries]
  ├─ incidents.rdf
  ├─ connectors.rdf
  ├─ gates.rdf
  └─ lines.rdf
  ↓ [Tera templates]
  ├─ generated/soc_triage_line.erl
  ├─ generated/soc_triage_app.erl
  ├─ generated/incident_connector_siem.erl
  ├─ generated/incident_connector_edr.erl
  ├─ generated/incident_connector_ticket.erl
  ├─ generated/incident_connector_notify.erl
  ├─ generated/Makefile
  └─ generated/terraform/main.tf
  ↓ [Quality Gates]
  ├─ rdf:validate
  ├─ sparql:closure_check
  ├─ erlang:compile
  └─ receipt:issue
```

**Tools**:
- `ggen_cli` - entry points: `ggen validate`, `ggen sync`, `ggen show_andon`
- `ggen_rdf` - ontology loader + validator
- `ggen_sparql` - query executor
- `ggen_template` - Tera renderer + context builder
- `ggen_codegen` - Erlang AST generation

### Phase 3: Connectors (4 implementations)

**Deliverable**: Each connector compiles, has smoke tests, emits receipts.

- `incident_connector_siem.erl` (Splunk mock for MVP)
- `incident_connector_edr.erl` (Sentinel mock)
- `incident_connector_ticket.erl` (ServiceNow mock)
- `incident_connector_notify.erl` (Slack mock)

**Each connector**:
- Generated from ontology + capability spec
- Implements `ln_connector` behavior
- Has receipt emission for every effect
- Has mock/stub mode for testing

### Phase 4: Manufactured Apps (3 lines)

**Deliverable**: Generated OTP apps that run under supervision.

- `soc_triage_line.erl` - happy path: incident → classify → gate → escalate → receipt
- `soc_rework_line.erl` - exception path: rejected gate → escalation team → rework gate → re-enter
- `soc_evidence_line.erl` - evidence pack: extract SIEM/EDR/ticket artifacts → audit snapshot → receipt

### Phase 5: Proof Demos

**Deliverable**: 3 runnable demo scripts + proof metrics.

- `demo_manufacture.sh` - edit ontology → regenerate artifacts → show receipt hash change
- `demo_run_line.sh` - run soc_triage_line with real incident data → show receipts
- `demo_stop_the_line.sh` - trigger incident → let it run → cancel scope → show no post-cancel effects → restart → replay

### Phase 6: GCP Deployment + Board Assets

**Deliverable**: Terraform module + Docker images + board memo + FAQ.

- Terraform: GKE deployment of ln_ctrl + connector inventory
- Docker: multi-stage build, production-ready
- Board memo: replacement framing + transition economics
- One-slide summary: triage analyst replacement cost/benefit
- FAQ: 10 Q&A, no jargon

---

## 4. SWARM ROLES & ASSIGNMENTS

| Role | Responsibility | Input | Output | Agent |
|------|------------------|-------|--------|-------|
| 1 | Product thesis + board memo (replacement framing, not softened) | Business case | BOARD_MEMO.md, ONE_SLIDE.md | TBD |
| 2 | Choose lane + Σ skeleton ontology | Market analysis | rdf/security-ops-skeleton.rdf | TBD |
| 3 | Lane-specific ontology (entities, events, gates, stops) | Lane requirements | rdf/security-ops-full.rdf | TBD |
| 4 | Connector capability ontology (what each SaaS does) | Connector specs | rdf/connectors.rdf | TBD |
| 5 | Integration mapping ontology (source → target transforms) | Data mapping rules | rdf/mappings.rdf | TBD |
| 6 | SPARQL query author (extractors for generation) | Ontology | sparql/incidents.rq, gates.rq, connectors.rq, lines.rq | TBD |
| 7 | Erlang OTP module templates (line plans, connectors) | Ontology + structure | templates/line.tera, connector.tera, app.tera | TBD |
| 8 | Infrastructure templates (Docker, K8s, Terraform) | Deployment model | templates/Dockerfile.tera, k8s.tera, terraform.tera | TBD |
| 9 | Generation rules + CLI runner + andon gates | Pipeline | ggen_cli.erl, ggen_codegen.erl, rules.yaml | TBD |
| 10 | Receipt system (build receipts + run receipts) | Receipt spec | ln_ctrl_receipt.erl, receipt_log.erl | TBD |
| 11 | ln_ctrl core API/behavior (seq, par, join, xor) | WF Substrate foundation | ln_ctrl_core.erl | TBD |
| 12 | ln_ctrl cancellation + budgets | Cancel semantics + budget spec | ln_ctrl_cancel.erl, ln_ctrl_budget.erl | TBD |
| 13 | ln_ctrl determinism + replay | Replay semantics | ln_ctrl_sched.erl, ln_ctrl_replay.erl | TBD |
| 14 | Connector runtime harness (effect boundary) | Effect spec | ln_connector.erl, ln_effect.erl | TBD |
| 15 | Connector #1: SIEM (Splunk mock) | SIEM interface spec | incident_connector_siem.erl + tests | TBD |
| 16 | Connector #2: EDR (Sentinel mock) | EDR interface spec | incident_connector_edr.erl + tests | TBD |
| 17 | Connector #3: Ticket (ServiceNow mock) | Ticket interface spec | incident_connector_ticket.erl + tests | TBD |
| 18 | Connector #4: Notification (Slack mock) | Notification spec | incident_connector_notify.erl + tests | TBD |
| 19 | Demo harness + failure injection tests | Line definitions + failure scenarios | demo_manufacture.sh, demo_run_line.sh, demo_stop_the_line.sh, test_*_failures.erl | TBD |
| 20 | GCP deployment + marketplace packaging | Deployment model | terraform/gke/main.tf, Dockerfile, MARKETPLACE.md, RUNBOOK.md | TBD |

---

## 5. BUILD SEQUENCE (STRICT ORDER)

### Sprint 1: Foundation (Days 1-2)
- [ ] Role 1: Board memo + one-slide summary draft (lanes choice locked)
- [ ] Role 2: Σ skeleton ontology (RDF structure)
- [ ] Role 3: Lane ontology v0.1 (entities, events, gates)
- [ ] Role 11-13: ln_ctrl core implementation (seq, par, cancel, receipts, determinism)

### Sprint 2: Manufacturing Spec (Days 3-4)
- [ ] Role 4-5: Connector + mapping ontology
- [ ] Role 6: SPARQL extractors
- [ ] Role 7-8: Tera templates
- [ ] Role 9: ggen CLI skeleton

### Sprint 3: Factory & Connectors (Days 5-7)
- [ ] Role 9: Generate connectors v1 from ontology
- [ ] Role 10: Receipt system implementation
- [ ] Role 14: Connector harness
- [ ] Role 15-18: Implement 4 connectors + smoke tests
- [ ] Role 9: Quality gates (compile, validate receipts)

### Sprint 4: Manufactured Apps (Days 8-9)
- [ ] Role 3 + 6 + 9: Generate 3 line plans from ontology
- [ ] Role 11-14: Line apps compile and run under supervision

### Sprint 5: Proof & Demo (Days 10-11)
- [ ] Role 19: End-to-end demo scripts + failure injection
- [ ] Role 11-14: Prove determinism + cancellation + replay
- [ ] Role 10: Prove receipts are deterministic

### Sprint 6: GCP & Board (Days 12)
- [ ] Role 20: Terraform module + Docker images + runbook
- [ ] Role 1: Final board memo with real metrics from demos
- [ ] Role 20: Marketplace listing draft

---

## 6. ACCEPTANCE CRITERIA (HARD GATES)

**Manufacturing:**
- [ ] RDF change → artifacts regenerated with new receipt hash within 30s
- [ ] Generated code compiles without manual edits
- [ ] SPARQL queries close (no dangling URIs)
- [ ] Quality gates block invalid generation

**Execution:**
- [ ] ln_ctrl runs all 3 demo lines
- [ ] cancel-scope halts; andon shows red; no post-cancel effects
- [ ] Deterministic mode: identical inputs → identical trace order (3 runs)
- [ ] Replay mode: recorded choice log reproduced exactly
- [ ] Receipts are stored + accessible via API

**Evidence:**
- [ ] Build receipts: hash(ontology+templates+params) → hash(artifacts)
- [ ] Run receipts: every effect emits receipt with hash(inputs) + result
- [ ] Andon signals: green (gates passed) / yellow (warnings) / red (stop)
- [ ] Receipt log is append-only + tamper-evident (hash chain)

**Packaging:**
- [ ] Terraform: `terraform init → terraform apply` deploys on GCP
- [ ] Docker: image builds, runs ln_ctrl + connector inventory
- [ ] Board memo: states replacement thesis + transition plan + FTE savings estimate
- [ ] Marketplace: candidate packaging ready for GCP review

---

## 7. GIT STRUCTURE

```
/home/user/cre/
├── src/wf/                   # WF Substrate foundation (already exists)
│   ├── wf_term.erl
│   ├── wf_vm.erl
│   ├── wf_compile.erl
│   ├── wf_exec.erl
│   └── ...
│
├── src/ln_ctrl/              # Line Control runtime (new)
│   ├── ln_ctrl_core.erl
│   ├── ln_ctrl_budget.erl
│   ├── ln_ctrl_receipt.erl
│   ├── ln_ctrl_andon.erl
│   ├── ln_connector.erl
│   └── ...
│
├── src/ggen/                 # Manufacturing pipeline (new)
│   ├── ggen_cli.erl
│   ├── ggen_rdf.erl
│   ├── ggen_sparql.erl
│   ├── ggen_template.erl
│   ├── ggen_codegen.erl
│   └── ...
│
├── ontology/                 # Industry ontology packs (new)
│   ├── security-ops/
│   │   ├── skeleton.rdf
│   │   ├── incidents.rdf
│   │   ├── connectors.rdf
│   │   ├── gates.rdf
│   │   ├── mappings.rdf
│   │   └── lines.rdf
│   └── ...
│
├── sparql/                   # SPARQL extraction queries (new)
│   ├── incidents.rq
│   ├── gates.rq
│   ├── connectors.rq
│   └── lines.rq
│
├── templates/                # Code generation templates (new)
│   ├── line.tera
│   ├── connector.tera
│   ├── app.tera
│   ├── Dockerfile.tera
│   └── terraform.tera
│
├── connectors/               # Generated connector inventory (new)
│   ├── incident_connector_siem.erl
│   ├── incident_connector_edr.erl
│   ├── incident_connector_ticket.erl
│   ├── incident_connector_notify.erl
│   └── ...
│
├── apps/                     # Manufactured applications (new)
│   ├── soc_triage_line.erl
│   ├── soc_rework_line.erl
│   ├── soc_evidence_line.erl
│   └── ...
│
├── demo/                     # Proof demos (new)
│   ├── demo_manufacture.sh
│   ├── demo_run_line.sh
│   ├── demo_stop_the_line.sh
│   ├── incident_samples.json
│   └── ...
│
├── terraform/                # GCP deployment (new)
│   ├── gke/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   └── outputs.tf
│   └── ...
│
├── docs/                     # Documentation (new)
│   ├── BOARD_MEMO_REPLACEMENT.md
│   ├── ONE_SLIDE_SUMMARY.md
│   ├── FAQ_EXECUTIVE.md
│   ├── ARCHITECTURE.md
│   ├── ONTOLOGY_STANDARD.md
│   ├── FACTORY_PIPELINE.md
│   ├── OPERATIONS.md
│   ├── CONNECTOR_SPEC.md
│   └── ...
│
└── test/                     # All tests (new)
    ├── ln_ctrl_test*.erl
    ├── ggen_test*.erl
    ├── connector_test*.erl
    └── integration_test*.erl
```

---

## 8. NEXT IMMEDIATE ACTIONS

**Day 1 (NOW):**

1. ✅ Confirm lane choice: **Security Ops** (incident triage) - LOCKED
2. ⬜ Role 1: Create board memo skeleton
3. ⬜ Role 2: Start RDF skeleton (Σ entities)
4. ⬜ Role 11: Extend WF Substrate → ln_ctrl_core
5. ⬜ Create initial test skeletons for acceptance criteria

**This Document**: Frozen as master plan. All updates via commit messages, no editing.

---

**Status**: BUILD STARTING
**Lane**: Security Ops (SOC Triage Automation)
**MVP Scope**: Locked (4 connectors, 3 lines, stop-the-line, determinism proof)
**Replacement Target**: 40% → 10% SOC analyst time on triage (4-6 FTE/100-person org)

**GO.**
