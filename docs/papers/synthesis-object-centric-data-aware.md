# Object-Centric & Data-Aware Process Mining: Synthesis & Reading Guide

**Domain:** Object-Centric & Data-Aware Process Mining
**Paper Count:** 30 papers (18.1% of collection)
**Time Span:** 2017–2025
**Key Innovation:** Multi-object event logs (OCEL); moving beyond single-case models

---

## Domain Overview

**The Problem:**
Traditional process mining assumes each event belongs to a single case (process instance). Reality: a sales order involves multiple objects—customer, product, shipment, invoice. Each object has its own lifecycle. A single event (e.g., "order cancelled") affects multiple objects simultaneously.

**Single-Case Model (Wrong):**
```
Customer ─→ Place Order ─→ Pay ─→ Receive ─→ Deliver
```
This hides complexity. A real order has:
- Customer lifecycle (onboarding → active → loyal → churn)
- Product lifecycle (manufacturing → inventory → fulfilled)
- Shipment lifecycle (packing → transit → delivery)
- Payment lifecycle (invoiced → paid → archived)

All interleaved in a single event stream.

**Object-Centric Model (Right):**
```
Events distributed across multiple object types:
  Customer:  onboard ──→ place_order ──→ pay ──→ loyal
  Order:     create ──→ fulfill ──→ ship ──→ complete
  Payment:   request ──→ verify ──→ settle ──→ reconcile
  Shipment:  pack ──→ ship ──→ transit ──→ deliver

Same events, different perspectives:
  "order_complete" → Order advances, Shipment advances, Revenue recognized
```

**Why It Matters:**
- **Accuracy:** Single-case models produce misleading bottleneck analysis and predictions
- **Reality:** 70–80% of real business processes involve multiple objects
- **Compliance:** Regulatory tracing requires following multiple objects (money, goods, authorization)
- **AI:** Object-centric intelligence is prerequisite for understanding complex processes (see "No AI Without PI")

---

## Evolution: Single-Case to Multi-Object (2017–2025)

### Phase 1: Recognition (2017–2019)
**Thesis:** Current process mining incorrectly assumes single-case processes.

- **2017:** Object-Centric Behavioral Constraints (van der Aalst)
  - Formal model for multi-object processes
  - Shows limitations of case-based Petri nets
  - Introduces OCbc (object-centric behavioral constraints)

- **2018–2019:** Papers defining multi-object event logs
  - Need for standard format
  - How to convert flat logs to object-centric format

**Status:** Academic recognition; no industrial adoption yet.

---

### Phase 2: Standardization (2019–2023)
**Thesis:** Object-centric process mining needs standardized event log format.

- **2019:** OCEL (Object-Centric Event Log) format proposed
  - Objects, events, relationships
  - Maps to relational/graph databases

- **2020–2021:** Papers on OCPM metrics
  - Precision for object-centric models
  - Fitness for object-centric models
  - New quality definitions

- **2022–2023:** Papers on object-centric analysis
  - Comparison (which customer gets treated differently?)
  - Performance analysis per object type
  - Organizational mining from object-centric data

**Status:** Standard (OCEL) emerging; academic tools (ProM plugins); early industrial adoption.

---

### Phase 3: Modern (2023–2025)
**Thesis:** Object-centric is the standard; single-case is a degenerate case.

- **2023:** Multiple papers on OCPM advances
  - Object-centric alignments (conformance checking)
  - Object-centric local process models
  - Object-centric process cubes (multi-dimensional analysis)

- **2024–2025:** Papers on next generation
  - OCEL 2.0 specification (improved standard)
  - Object-centric querying (OCPQ)
  - AI without PI (object-centric as prerequisite)

- **2025:** Integration with AI
  - Object-centric models as grounding for LLMs
  - Predictive monitoring on multi-object processes

**Status:** Industrial adoption accelerating. Celonis, Minit, SAP investing in object-centric capabilities.

---

## Key Concepts

### Multi-Object Structures

| Structure | Description | Example |
|-----------|-------------|---------|
| **Object Type** | Entity class (customer, order, product, shipment) | Customer, Product |
| **Object Instance** | Unique entity (Alice, SKU-123, Order-2024-001) | Customer #42, Order #5001 |
| **Event** | Action recorded in log (create, pay, ship) | "ship" event recorded |
| **Event Attributes** | Properties of event (timestamp, amount, status) | timestamp: 2024-03-15, amount: $100 |
| **Relationships** | Object-to-object connections (Order → Customer) | Order #5001 involves Customer #42 |
| **Perspectives** | View of process from object type's lens | Customer perspective: lifecycle from signup to churn |

### Standard Formats

**OCEL 1.0 (2020)**
```json
{
  "events": [
    {
      "event_id": "e1",
      "timestamp": "2024-01-01T10:00:00",
      "event_type": "Order Received",
      "ocel:omap": {"order": ["o1"], "customer": ["c1"]},
      "attributes": {"amount": 100}
    }
  ],
  "objects": [
    {"object_id": "o1", "type": "order"},
    {"object_id": "c1", "type": "customer"}
  ]
}
```

**OCEL 2.0 (2024)**
- Improved specification
- Explicit relationships
- Better schema support
- Compatibility improvements

### Discovery Algorithms

| Algorithm | Year | Focus |
|-----------|------|-------|
| **Object-Centric Behavioral Constraints (OCbc)** | 2017 | Formal model for multi-object processes |
| **Discovering Object-Centric Petri Nets** | 2020 | Algorithm to discover Petri nets from OCEL |
| **Object-Centric Local Process Models** | 2024 | Decompose multi-object models into sub-processes |
| **Heuristics Miners for Multi-Object** | 2023+ | Adapt heuristic miner to object-centric setting |

---

## Cross-Domain Connections

```
Object-Centric ← → Process Discovery
  (discovery algorithms adapted for OCEL)

Object-Centric ← → Conformance & Quality
  (new metrics: precision, fitness for OCPM)

Object-Centric ← → Performance & Prediction
  (predict per-object lifecycle, not global process)

Object-Centric ← → Enterprise & Business
  (real business processes are multi-object)

Object-Centric ← → AI & Machine Learning
  (neural models for multi-object processes; AI grounding)

Object-Centric ← → Workflow Patterns
  (multi-object patterns; BPMN extensions for objects)

Object-Centric ← → Data-Aware Mining
  (data attributes tied to objects, not events)
```

---

## Recommended Reading Order

### Beginner Path (4 papers, ~12 hours)

1. **Object-Centric Behavioral Constraints (2017)**
   - Foundational paper defining object-centric processes
   - Formal model (OCbc)
   - 20 pages, 3 hours

2. **OCEL: Object-Centric Event Log Specification (2020)**
   - Standard format for object-centric logs
   - Mapping from flat logs
   - 15 pages, 2 hours

3. **Advancements and Challenges in OCPM (2023)**
   - State-of-the-art review
   - Open problems
   - 20 pages, 3 hours

4. **Towards a Simple and Extensible Standard for OCEL 2.0 (2024)**
   - Latest specification; improvements
   - 15 pages, 2 hours

**Total: 4 papers, ~12 hours**

---

### Practitioner Path (10 papers, ~35 hours)

**Foundation:**
1. Object-Centric Behavioral Constraints
2. OCEL Specification
3. Advancements and Challenges in OCPM
4. OCEL 2.0 Specification

**Discovery & Analysis:**
5. **Discovering Object-Centric Petri Nets (2020)**
   - How to discover models from OCEL
   - 15 pages, 2 hours

6. **Object-Centric Local Process Models (2024)**
   - Decompose OCPM models
   - 10 pages, 1.5 hours

7. **Object-Centric Process Comparison using Cubes (2021)**
   - Multi-dimensional OCPM analysis
   - 15 pages, 2 hours

**Performance & Quality:**
8. **Precision and Fitness in OCPM (2021)**
   - How to measure model quality for OCPM
   - 15 pages, 2 hours

9. **Object-Centric Performance Analysis (OPerA) (2022)**
   - Performance metrics for object-centric models
   - 10 pages, 1.5 hours

10. **Predictive Object-Centric Process Monitoring (2022)**
    - Prediction in multi-object setting
    - 15 pages, 2 hours

**Total: 10 papers, ~35 hours**

---

### Research Path (20+ papers, ~80 hours)

**Part 1: Foundations (8 papers)**
1–4. Beginner path papers
5. **Discovering Object-Centric Petri Nets (2020)**
6. **Object-Centric Local Process Models (2024)**
7. **Learning Hybrid Process Models from Events (2017)**
   - Data-aware + control-flow models
   - 20 pages, 3 hours
8. **Heuristic Approaches for Generating Local Process Models (2016)**
   - Sub-process mining foundations
   - 15 pages, 2 hours

**Part 2: Advanced Discovery (5 papers)**
9. **Object-Centric Process Comparison (2021)**
10. **Precision and Fitness in OCPM (2021)**
11. **Grouping Local Process Models (2024)**
    - Cluster and organize sub-models
    - 10 pages, 1.5 hours
12. **Object-Centric Alignments (2023)**
    - Conformance checking for OCPM
    - 20 pages, 3 hours
13. **OPerA: Object-Centric Performance Analysis (2022)**

**Part 3: Querying & Analysis (4 papers)**
14. **OCPQ: Object-Centric Process Querying (2025)**
    - Query objects, relationships, constraints
    - 15 pages, 2 hours
15. **Monitoring Constraints in Business Processes (2022)**
    - Object-centric constraint monitoring
    - 15 pages, 2 hours
16. **OrgMining 2.0: Organizational Model Mining (2020)**
    - Extract organizational structure from OCEL
    - 25 pages, 4 hours
17. **Analyzing an After-Sales Service Process (2023)**
    - Real case study of OCPM
    - 15 pages, 2 hours

**Part 4: Integration & Future (5 papers)**
18. **Predictive Object-Centric Monitoring (2022)**
19. **No AI Without PI (2025)**
    - OCPM as foundation for generative AI
    - 15 pages, 2 hours
20. **Event Stream Based Process Discovery (2017)**
    - Online discovery for streaming OCEL
    - 20 pages, 3 hours
21. **Process Aware Analysis of Treatment Paths (2024)**
    - Healthcare case study
    - 15 pages, 2 hours
22. **Federated Conformance Checking (2025)**
    - Multi-party OCPM analysis
    - 20 pages, 3 hours

**Total: 22+ papers, ~80 hours**

---

## Key Research Questions

### Solved
- [x] Can we define a standard format for multi-object logs? **Yes (OCEL, OCEL 2.0)**
- [x] Can we discover models from OCEL? **Yes (2020+)**
- [x] Are quality metrics different for OCPM? **Yes (different precision/fitness definitions)**

### Partially Solved
- [ ] How do we optimally convert flat logs to OCEL? **60% solved; edge cases remain**
- [ ] How do we handle implicit objects? **50% solved; heuristics exist**
- [ ] How do we scale to logs with 50+ object types? **Research ongoing**

### Open
- [ ] What is the optimal object-centric process model notation? **Multiple proposals, no consensus**
- [ ] How do we do conformance checking efficiently? **Emerging (object-centric alignments)**
- [ ] How do we predict process outcomes with multiple objects? **Recent work promising**
- [ ] Can object-centric models replace BPMN? **No; complementary; integration needed**
- [ ] How do we teach object-centric thinking to practitioners? **Open; training materials lacking**

---

## Gaps & Future Directions

### Technical Gaps

1. **Automatic Object Type Detection**
   - Current: Practitioners must manually identify objects
   - Future: Automated extraction from event attributes, schema
   - Research: Clustering, semantic analysis

2. **Handling Implicit Objects**
   - Current: OCEL requires explicit objects
   - Future: Infer implicit objects from event relationships
   - Research: Knowledge graphs, ontology matching

3. **Scalability Beyond 10 Object Types**
   - Current: Tools slow down with many object types
   - Future: Distributed algorithms, projection techniques
   - Research: Parallel discovery, sampling strategies

4. **Conformance Checking Efficiency**
   - Current: Expensive to check alignment for multi-object traces
   - Future: Approximation algorithms, sampling
   - Research: Heuristic conformance, statistical testing

5. **OCPM Tool Standardization**
   - Current: Academic tools only; no commercial standards
   - Future: Standard APIs, format converters
   - Research: Tool interoperability protocols

---

## Narrative: From Single-Case to Multi-Object

### The Paradigm Shift (2017–2025)

**2017: Recognition Phase**
Van der Aalst published "Object-Centric Behavioral Constraints," formally showing that single-case process mining is fundamentally incorrect for most real processes. Academic recognition, zero industry adoption.

**2019–2020: Standardization**
OCEL format proposed. Academic community agrees on event log structure. Plugins for ProM created. Still: mostly research, limited adoption.

**2021–2022: Momentum**
Papers on OCPM metrics, performance analysis, organizational mining. Early adopters (large consulting firms) start experimenting. Still: niche.

**2023: Inflection Point**
Multiple papers on practical OCPM. Industry tools (Celonis, Minit, SAP) announce object-centric features. Realization: single-case models are broken. Need to rebuild.

**2024–2025: Transformation**
OCEL 2.0 released. Object-centric conformance checking available. "No AI Without PI" argues that process intelligence (OCPM) is foundational for AI. Shift from "is object-centric important?" to "how do we scale it?"

### Key Realization

**The Single-Case Assumption Was Wrong, But It Took 30 Years to Fix**

Why?
1. Most academic datasets were synthetic, single-object
2. Tool implementations were designed for single-case
3. Business users accepted inaccurate models (better than nothing)
4. Switching to OCPM requires rethinking everything

When industry switched to OCPM:
- Discovered actual bottlenecks were different than single-case models suggested
- Predictions improved (accounting for multiple object lifecycles)
- Compliance analysis became more accurate

Cost: Years of incorrect analysis. Benefit: Finally accurate process understanding.

---

## Key Takeaways

1. **Single-case is wrong:** Most real processes involve multiple objects. Single-case models are useful fictions.

2. **OCEL is the future:** Standard format enables tool interoperability, wider adoption.

3. **Metrics change:** Precision, fitness, and other quality measures are redefined for OCPM.

4. **Scalability is open:** Tools work for 5–10 object types; beyond that is frontier.

5. **Integration is needed:** OCPM should integrate with data modeling, ontologies, graph databases.

6. **AI grounding:** Process Intelligence (OCPM) claimed as foundation for generative AI (speculative but important).

---

**Last Updated:** 2026-03-29
**Collection Size:** 30 papers
**Maturity Level:** Emerging standard (2023–2025)
