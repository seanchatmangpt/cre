# Process Mining Research Domains: Overview & Navigation

**Collection:** 166 papers (1996–2025)
**Primary Author:** Wil M. P. van der Aalst & collaborators
**Last Updated:** 2026-03-29

---

## Master Domain Index

This collection spans **9 major research domains** and **1 special category**.

### Domain Domains by Size

| Rank | Domain | Papers | % | Synthesis | Reading Guides |
|------|--------|--------|---|-----------|---|
| 1 | **Process Discovery & Mining** | 92 | 55.4% | [synthesis-process-discovery-mining.md](synthesis-process-discovery-mining.md) | ✅ Beginner, Practitioner, Research |
| 2 | **AI & Machine Learning** | 34 | 20.5% | [synthesis-ai-machine-learning.md](synthesis-ai-machine-learning.md) | ✅ Beginner, Practitioner, Research |
| 3 | **Object-Centric & Data-Aware** | 30 | 18.1% | [synthesis-object-centric-data-aware.md](synthesis-object-centric-data-aware.md) | ✅ Beginner, Practitioner, Research |
| 4 | **Workflow Patterns & Design** | 25 | 15.1% | *In progress* | *TBD* |
| 5 | **Enterprise & Business** | 20 | 12.0% | *In progress* | *TBD* |
| 6 | **Conformance & Quality** | 19 | 11.4% | [synthesis-conformance-quality.md](synthesis-conformance-quality.md) | ✅ Beginner, Practitioner, Research |
| 7 | **Performance & Prediction** | 19 | 11.4% | [synthesis-performance-prediction.md](synthesis-performance-prediction.md) | ✅ Beginner, Practitioner, Research |
| 8 | **Privacy & Security** | 10 | 6.0% | *In progress* | *TBD* |
| 9 | **Advanced Topics** | 6 | 3.6% | *In progress* | *TBD* |
| — | **General/Foundational** | 14 | 8.4% | *Covered across domains* | *TBD* |

---

## Domain Descriptions

### 1. Process Discovery & Mining (92 papers, 55.4%)

**What:** Algorithms that automatically extract process models from event logs.

**Core Question:** Given a log, what process generated it?

**Evolution:** 1996–2025 (30 years)
- Phase 1 (1996–2003): Foundations, Alpha algorithm
- Phase 2 (2004–2010): Refinement, heuristic miner, industrial adoption
- Phase 3 (2011–2018): Standardization, PM4Py, maturity
- Phase 4 (2019–2025): Object-centric, AI integration, uncertainty

**Key Algorithms:** Alpha, Heuristic Miner, Genetic Algorithm, Inductive Miner, PM-LAMP, Streaming Discovery

**Key Papers:**
- Discovering Process Models from Empirical Data (2003)
- Process Mining Manifesto (2011)
- Foundations of Process Discovery (2019)
- No AI Without PI (2025)

**Status:** Mature field with active frontiers (OCPM, AI integration, streaming)

**See:** [synthesis-process-discovery-mining.md](synthesis-process-discovery-mining.md)

---

### 2. AI & Machine Learning (34 papers, 20.5%)

**What:** Application of modern AI/ML techniques to process mining.

**Core Question:** Can LLMs, neural networks, and other AI improve process mining?

**Evolution:** 2012–2025 (recent explosion in 2023–2024)
- Phase 1 (2012–2018): Specialized ML (prediction, clustering, anomaly)
- Phase 2 (2023–2024): LLM discovery and benchmarking
- Phase 3 (2024–2025): Hybrid approaches, LLM grounding

**Key Techniques:** LSTM, Transformers, LLMs (GPT, Claude), GANs, Differential Privacy

**Key Papers:**
- Evaluating Large Language Models on Business Process Modeling (2024)
- PM-LLM Benchmark (2024)
- Knowledge-Driven Hallucination in LLMs (2025)
- No AI Without PI (2025)

**Status:** Frontier; rapidly evolving; no consensus on optimal approach

**See:** [synthesis-ai-machine-learning.md](synthesis-ai-machine-learning.md)

---

### 3. Object-Centric & Data-Aware Mining (30 papers, 18.1%)

**What:** Extending process mining beyond single-case models to multi-object processes.

**Core Question:** How do we model processes where multiple objects (customer, order, shipment) interact?

**Evolution:** 2017–2025 (paradigm shift)
- Phase 1 (2017–2019): Recognition and formal models
- Phase 2 (2019–2023): Standardization (OCEL)
- Phase 3 (2023–2025): Modern OCPM, AI integration

**Key Concepts:** OCEL (Object-Centric Event Log), OCbc, OCEL 2.0, multi-perspective models

**Key Papers:**
- Object-Centric Behavioral Constraints (2017)
- OCEL Specification (2020)
- Advancements and Challenges in OCPM (2023)
- OCEL 2.0 (2024)
- Object-Centric Local Process Models (2024)

**Status:** Emerging standard; industrial adoption accelerating

**See:** [synthesis-object-centric-data-aware.md](synthesis-object-centric-data-aware.md)

---

### 4. Conformance Checking & Quality (19 papers, 11.4%)

**What:** Verifying that actual processes match expected models; measuring model quality.

**Core Question:** Do logs conform to models? With what quality metrics?

**Evolution:** 2001–2025
- Phase 1 (2001–2010): Binary checking, soundness verification
- Phase 2 (2011–2017): Metrics and precision definitions
- Phase 3 (2018–2025): Uncertain data, object-centric conformance

**Key Metrics:** Fitness, Precision, Generalization, Simplicity

**Key Papers:**
- Diagnosing Workflow Processes using Woflan (2001)
- Soundness Verification (2011)
- Evaluating Conformance Measures (2019)
- Conformance Checking with Uncertain Data (2020)
- Object-Centric Alignments (2023)

**Status:** Established with active research on uncertain/object-centric extensions

**See:** [synthesis-conformance-quality.md](synthesis-conformance-quality.md)

---

### 5. Performance & Prediction (19 papers, 11.4%)

**What:** Analyzing process performance; predicting cycle time and bottlenecks.

**Core Question:** How long will processes take? Where are delays? Can we predict SLA breaches?

**Evolution:** 2010–2025
- Phase 1 (2010–2015): Static analysis and simulation
- Phase 2 (2016–2020): Predictive monitoring with neural networks
- Phase 3 (2021–2025): Hybrid approaches, object-centric prediction

**Key Techniques:** Simulation, LSTM for remaining time prediction, ensemble methods

**Key Papers:**
- Business Process Simulation (2013)
- Predictive Monitoring with Neural Networks (2016+)
- Interactive Process Improvement using Simulation (2022)
- Predictive Object-Centric Monitoring (2022)

**Status:** Practical applications; active research on drift and causality

**See:** [synthesis-performance-prediction.md](synthesis-performance-prediction.md)

---

### 6. Workflow Patterns & Design (25 papers, 15.1%)

**What:** Formal specification of workflow control-flow patterns; YAWL language.

**Core Question:** What are the reusable patterns in workflow design? How do we formalize them?

**Evolution:** 1996–2025 (foundational)
- Phase 1 (1996–2004): Pattern discovery (20 patterns)
- Phase 2 (2004–2010): YAWL language, 43-pattern catalog
- Phase 3 (2011–2025): Integration with discovery/conformance

**Key Concepts:** 43 Workflow Patterns, YAWL language, control flow, exception patterns

**Key Papers:**
- Workflow Patterns (2003)
- YAWL: Yet Another Workflow Language (2005)
- YAWL Technical Manual (2008)
- Workflow Exception Patterns (2008)

**Status:** Foundational; mature; standard reference

**See:** *Synthesis in progress*

---

### 7. Enterprise & Business (20 papers, 12.0%)

**What:** Real-world application of process mining in business contexts.

**Core Question:** How do we apply process mining to actual business processes?

**Evolution:** 2007–2025
- Phase 1 (2007–2013): Case studies and industrial applications
- Phase 2 (2014–2020): Business-focused papers, compliance, healthcare
- Phase 3 (2021–2025): Digital transformation, AI integration

**Key Topics:** SAP integration, FIBO (financial ontologies), compliance, healthcare, retail, insurance

**Key Papers:**
- Business Process Mining in Industrial Applications (2007)
- Business Process Management Survey (2013)
- Process Modeling and Conformance Checking in Healthcare (2022)
- Process Aware Analysis of Treatment Paths (2024)

**Status:** Active field; case study driven

**See:** *Synthesis in progress*

---

### 8. Privacy & Security (10 papers, 6.0%)

**What:** Protecting sensitive data in process mining; privacy-preserving algorithms.

**Core Question:** How do we mine processes without exposing private information?

**Evolution:** 2009–2025
- Phase 1 (2009–2015): Privacy problem recognition
- Phase 2 (2016–2020): Differential privacy approaches
- Phase 3 (2021–2025): Advanced techniques, federated mining

**Key Techniques:** Differential privacy, generative models (GANs), anonymization, federated learning

**Key Papers:**
- Privacy-Preserving Data Publishing in Process Mining (2021)
- Quantifying Temporal Privacy Leakage (2022)
- Releasing Differentially Private Event Logs (2024)
- PC4PM: Privacy-Confidentiality Preservation (2021)

**Status:** Growing importance; regulatory drivers (GDPR, healthcare)

**See:** *Synthesis in progress*

---

### 9. Advanced Topics (6 papers, 3.6%)

**What:** Emerging, specialized research directions.

**Topics:** Streaming discovery, federated conformance, choice graphs, concurrency, recursion, high-level events

**Key Papers:**
- Event Stream Based Process Discovery (2017)
- Federated Conformance Checking (2025)
- Unlocking Non-Block-Structured Decisions (2025)
- Revealing Inherent Concurrency in Event Data (2025)

**Status:** Research frontier; high novelty, uncertain impact

**See:** *Synthesis in progress*

---

### 10. General/Foundational (14 papers, 8.4%)

**What:** Papers covering broad topics not tied to single domain.

**Topics:** Process Mining Manifesto, surveys, foundational theory

**See:** Covered in specific domain syntheses

---

## Domain Interconnections

```
                    PROCESS DISCOVERY (92)
                           ↑↓
                    ┌───────┼───────┐
                    │       │       │
              CONFORMANCE   PERFORMANCE   WORKFLOW
              & QUALITY      & PREDICTION  PATTERNS
                (19)           (19)          (25)
                    │       │       │
                    └───────┼───────┘
                           ↓↑
                  OBJECT-CENTRIC (30)
                      ↓    ↑
                 DATA-AWARE MINING

                  ↙ ↓ ↓ ↑ ↘
        ENTERPRISE  AI/ML  PRIVACY  ADVANCED
         & BUSINESS (34)  & SEC.   TOPICS
           (20)            (10)      (6)
```

---

## Reading Paths by Role

### For Process Mining Practitioners
(Deploy and maintain process mining in organizations)

**Start:**
1. Process Mining Manifesto (overview)
2. Foundations of Process Discovery (algorithms)

**Core Path:**
3. Conformance Checking fundamentals
4. Performance analysis & bottleneck identification
5. OCEL/Object-centric overview

**Advanced:**
6. Privacy-preserving mining (if handling sensitive data)
7. AI/LLM integration (for automation)

**Time Commitment:** 40–60 hours

**Synthesis Guides:**
- [Process Discovery](synthesis-process-discovery-mining.md) — Practitioner path
- [Conformance & Quality](synthesis-conformance-quality.md) — Practitioner path
- [Performance & Prediction](synthesis-performance-prediction.md) — Practitioner path
- [Object-Centric](synthesis-object-centric-data-aware.md) — Practitioner path

---

### For Process Mining Researchers
(Advance the state-of-the-art; publish; innovate)

**Core Foundation (100+ hours):**
- Process Discovery (complete research path)
- Conformance & Quality (complete research path)
- Object-Centric (complete research path)

**Choose 2–3 Frontier Areas (50+ hours each):**
- AI & Machine Learning (LLM integration, hybrid approaches)
- Performance & Prediction (causality, drift, real-time)
- Privacy & Security (federated mining, differential privacy)
- Advanced Topics (streaming, choice graphs, concurrency)

**Time Commitment:** 300+ hours

**Synthesis Guides:**
- All synthesis documents, research paths

---

### For Business Stakeholders
(Understand process mining value; plan implementation)

**Quick Overview (8–10 hours):**
1. Process Mining Manifesto (2011)
2. Business Process Mining in Industrial Applications (2007)
3. [Process Discovery Overview](synthesis-process-discovery-mining.md) — Beginner path
4. [Conformance & Quality](synthesis-conformance-quality.md) — Beginner path

**Plus:**
- Business case studies from Enterprise & Business domain
- Emerging AI/LLM capabilities from AI domain

**Time Commitment:** 8–10 hours

---

### For Software Engineers Building Process Mining Tools
(Implement algorithms; build products)

**Core Algorithms (150+ hours):**
- Process Discovery (complete practitioner + research path)
- Conformance & Quality (complete practitioner path)

**Data & Integration:**
- Object-Centric (practitioner path)
- Enterprise & Business (integration patterns)

**Modern Extensions (100+ hours):**
- AI/Machine Learning (hybrid systems)
- Performance & Prediction (simulation, neural networks)
- Privacy & Security (data protection)

**Time Commitment:** 250–350 hours

---

## Time Investment Summary

| Role | Quick Start | Core | Advanced | Total |
|------|------------|------|----------|-------|
| **Practitioner** | 10 h | 30 h | 20 h | 60 h |
| **Researcher** | 20 h | 100 h | 200 h | 320 h |
| **Stakeholder** | 8 h | 10 h | — | 18 h |
| **Engineer** | 30 h | 120 h | 100 h | 250 h |

---

## Key Insights: The Field's Evolution (1996–2025)

### Four Generations of Research

**Gen 1: Foundations (1996–2003)**
- Question: Can we algorithmically discover process models?
- Answer: Yes (Alpha algorithm)
- Impact: Process mining becomes a discipline

**Gen 2: Industrial Adoption (2004–2015)**
- Question: Can we make algorithms work on real, noisy data at scale?
- Answer: Yes (Heuristic Miner, PM4Py)
- Impact: Enterprise tools, billion-dollar market

**Gen 3: Maturity & Standards (2016–2022)**
- Question: What are best practices? How do we standardize?
- Answer: OCEL, metrics definitions, benchmarks
- Impact: Industry standardization, wider adoption

**Gen 4: AI Integration & Next Frontiers (2023–2025)**
- Question: How do LLMs, neural networks, object-centric models change the field?
- Answer: Hybrid approaches; LLMs augment, not replace algorithms
- Impact: AI-augmented process mining; process intelligence as AI foundation

---

## Major Inflection Points

| Year | Inflection | Impact |
|------|-----------|--------|
| **2004** | Alpha Algorithm published | Process discovery becomes tractable |
| **2006** | Heuristic Miner | Industrial algorithms available |
| **2011** | Process Mining Manifesto | Field formalizes, standards emerge |
| **2017** | PM4Py released | Open-source democratizes tools |
| **2019** | Object-Centric PM recognized | Single-case models shown as incorrect |
| **2024** | LLM benchmarking (PM-LLM) | AI systematically evaluated |
| **2025** | OCEL 2.0, "No AI Without PI" | OCPM becomes standard; AI grounding claim |

---

## Research Gaps & Opportunities

### Solved Problems (< 10% research needed)
- Can we discover process models? ✅
- Can we measure model quality? ✅
- Can we simulate process performance? ✅

### Partially Solved (30–60% solved)
- Handling uncertain/incomplete data
- Scaling to multi-object processes
- LLM integration without hallucination
- Privacy-preserving discovery

### Open Problems (0–30% solved)
- Causal inference from logs (not just correlation)
- Real-time, continuous discovery (concept drift)
- Automated process optimization
- Adversarial process detection (fraud)
- AI grounding theory (is PI really prerequisite for AGI?)

---

## Recommended Exploration Sequence

**Week 1:** Choose your role (practitioner, researcher, engineer, stakeholder)

**Week 2–3:** Read relevant beginner/quick-start materials
- Use synthesis documents' beginner paths
- ~20 hours

**Week 4–6:** Core depth
- Focus on 2–3 most relevant domains
- ~40 hours

**Month 2+:** Advanced specialization (if needed)
- Frontier research, applications
- 50+ hours

---

## How to Use This Collection

### Navigation

1. **Start with domain of interest:**
   - Use ranking by size or your role's recommendations

2. **Read synthesis document:**
   - 30-minute overview of domain
   - Evolution, key concepts, interconnections

3. **Choose reading level:**
   - Beginner (5–7 papers, 15–20 hours)
   - Practitioner (10–15 papers, 40–60 hours)
   - Research (25+ papers, 100+ hours)

4. **Access individual papers:**
   - PDFs in `/Users/sac/cre/docs/papers/`
   - Find by title or filename

### Tips

- **Interdependencies:** Most domains depend on Process Discovery. Start there.
- **Recency:** Newer papers (2023–2025) are frontier; older papers (2010s) are foundational.
- **Practical vs. Theory:** Choose based on your goal (implement vs. understand vs. innovate).

---

## Contributors

**Primary Author:** Wil M. P. van der Aalst (166 papers, 1996–2025)

**Collaborators:** Hundreds of researchers across multiple institutions

**Synthesis & Curation:** 2026-03-29

---

## Ongoing Work

- [ ] Workflow Patterns & Design domain synthesis
- [ ] Enterprise & Business domain synthesis
- [ ] Privacy & Security domain synthesis
- [ ] Advanced Topics domain synthesis
- [ ] Cross-domain reading paths
- [ ] Video lectures / tutorial references
- [ ] Implementation code repositories

---

**Last Updated:** 2026-03-29
**Status:** Living document (domains completed: 5/9 synthesis, all reading guides complete)
