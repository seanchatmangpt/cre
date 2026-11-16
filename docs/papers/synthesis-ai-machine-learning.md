# AI & Machine Learning in Process Mining: Synthesis & Reading Guide

**Domain:** AI & Machine Learning in Process Mining
**Paper Count:** 34 papers (20.5% of collection)
**Time Span:** 2012–2025
**Primary Focus:** LLM integration, neural models, AI-assisted automation

---

## Domain Overview

**What It Is:**
This domain represents the intersection of modern AI/ML techniques with process mining. It explores how machine learning (neural networks, deep learning, LLMs) can enhance, augment, or replace traditional algorithmic process mining. The shift from pure algorithms to hybrid human-AI-algorithm systems.

**Why It Matters:**
- **Speed:** LLMs generate reasonable process models in seconds vs. hours for algorithms
- **Grounding:** Algorithms provide formal guarantees; LLMs provide flexibility and speed
- **Integration:** Modern enterprise systems use both (e.g., Celonis integrating copilot features)
- **Benchmark:** How good are LLMs at process mining vs. algorithms? Nobody knows yet
- **Paradigm Shift:** Process mining moving from purely algorithmic to AI-augmented

**Core Research Questions:**
1. Can LLMs generate accurate process models from event logs or natural language?
2. How do we prevent LLM hallucinations in process modeling contexts?
3. How do neural networks compare to algorithmic discovery on quality metrics?
4. What is the optimal hybrid approach (algorithms + LLMs + humans)?
5. Can process intelligence improve generative AI quality?

---

## Evolution: From Algorithms to AI (2012–2025)

### Phase 1: Specialized ML for Mining (2012–2018)
**Thesis:** Machine learning can improve specific aspects of process mining (prediction, clustering, classification).

**Focus Areas:**
- Predictive process monitoring (neural networks for remaining time prediction)
- Activity clustering (group similar activities using unsupervised learning)
- Anomaly detection (identify deviant behavior)
- Classification (predict process outcome)

**Key Papers:**
- Heuristic Miners (2012) – streaming with ML
- Text-aware predictive monitoring (2021)
- Event log sampling for prediction (2022)

**Status:** Mature, in production. Standard feature in process mining tools.

---

### Phase 2: LLM Discovery (2023–2024)
**Thesis:** Large language models can assist or replace algorithmic discovery.

**Breakthrough Moment:** 2023–2024 saw explosion of papers asking "Can LLMs do process mining?"

**Key Papers:**
- "Evaluating LLMs on Business Process Modeling" (2024)
- "Process Modeling with Large Language Models" (2024)
- "ProMoAI: Process Modeling with Generative AI" (2024)
- "Leveraging LLMs for Enhanced Process Model Comprehension" (2024)
- "PM-LLM Benchmark: Evaluating LLMs on Process Mining Tasks" (2024)

**Findings:**
- ✅ LLMs are surprisingly good at generating BPMN from descriptions
- ✅ LLMs excel at model refinement, explaining models to non-technical users
- ⚠️ LLMs hallucinate; accuracy on logs alone is lower than algorithms
- ⚠️ LLMs struggle with complex, non-free-choice constructs
- ❌ LLMs can't verify soundness; algorithms can

**Conclusion:** LLMs are useful for *some* tasks (explanation, requirements gathering) but not replacements for algorithmic discovery on logs.

---

### Phase 3: Hybrid & Foundational (2024–2025)
**Thesis:** The future is hybrid: algorithms + LLMs + domain knowledge.

**Recent Papers:**
- "Knowledge-Driven Hallucination in LLMs on Process Modeling" (2025)
  - When and why LLMs fail; how to mitigate
  - Use domain knowledge (ontologies, constraints) to ground LLMs

- "No AI Without PI: Object-Centric Process Mining" (2025)
  - Process Intelligence as prerequisite for generative AI
  - Algorithm-discovered models → LLM grounding → better outputs

- "Releasing Differentially Private Event Logs Using Generative Models" (2024)
  - GANs to synthesize synthetic logs while preserving privacy
  - Enables data sharing without exposing sensitive information

**New Paradigm:**
```
Algorithmic Discovery (precise, formal)
         ↓
LLM Enhancement (explain, refine, extend)
         ↓
Domain Grounding (constraints, ontologies)
         ↓
Human Review & Validation
         ↓
Deployment-Ready Process Model
```

---

## Key Concepts Within Domain

### ML Techniques Applied to Process Mining

| Technique | Use Case | Status | Notes |
|-----------|----------|--------|-------|
| **Supervised Learning** | Remaining time prediction, outcome prediction | ✅ Mature | Regression/classification on sequence features |
| **Neural Networks (LSTM)** | Next activity prediction, anomaly detection | ✅ Mature | RNNs for sequential patterns |
| **Transformers** | Process model generation, sequence-to-sequence | 🔄 Emerging | Attention mechanisms; sequence alignment |
| **Generative Models (GANs)** | Synthetic log generation, privacy preservation | 🔄 Emerging | Creates realistic event sequences |
| **Clustering** | Activity grouping, case classification | ✅ Mature | K-means, hierarchical clustering |
| **Anomaly Detection** | Outlier traces, deviant behavior | ✅ Mature | Isolation forests, autoencoders |
| **Large Language Models** | Model generation, explanation, grounding | 🆕 Frontier | ChatGPT, Claude, specialized PM models |
| **Knowledge Graphs** | Ontology integration, semantic grounding | 🔄 Emerging | Link process models to domain knowledge |

### LLM-Specific Challenges in Process Mining

| Challenge | Description | Status | Solution |
|-----------|-------------|--------|----------|
| **Hallucination** | LLM generates plausible but false model details | Open | Use algorithms to ground LLM; domain constraints |
| **Complex Structures** | LLMs struggle with non-free-choice, loops, implicit decisions | Partial | Fine-tune on structured data; symbolic+neural hybrid |
| **Verification** | LLMs can't prove soundness, formal properties | Open | Symbolic verification post-LLM; algorithm as validator |
| **Data Sensitivity** | Training on process logs may leak sensitive information | Active | Differential privacy in training |
| **Prompt Engineering** | Quality varies wildly by prompt; no standard format | Partial | Few-shot learning, chain-of-thought prompts |
| **Alignment** | LLM outputs misaligned with formal process model semantics | Open | Domain-specific training, reinforcement learning |

---

## Cross-Domain Connections

```
AI & ML ← → Process Discovery
  (LLM-assisted discovery; algorithms + neural networks)

AI & ML ← → Conformance & Quality
  (ML for prediction; symbolic verification for correctness)

AI & ML ← → Enterprise & Business
  (industry adoption of copilots; SAP, Celonis integrations)

AI & ML ← → Object-Centric Mining
  (neural models for complex multi-object processes)

AI & ML ← → Privacy & Security
  (differential privacy; generative models for data protection)

AI & ML ← → Performance & Prediction
  (neural networks for next-activity, remaining-time prediction)

AI & ML ← → Workflow Patterns
  (LLM understanding of pattern language; constraint grounding)
```

---

## Recommended Reading Order

### Beginner Path (5 papers, ~15 hours)

1. **Process Modeling with Large Language Models (2024)**
   - Overview of LLM applications to process mining
   - What works, what doesn't
   - 15 pages, 2 hours

2. **Evaluating Large Language Models on Business Process Modeling (2024)**
   - Benchmark framework for LLM evaluation
   - Multiple datasets, multiple LLMs
   - 20 pages, 3 hours

3. **PM-LLM Benchmark (2024)**
   - Comprehensive evaluation of LLMs on process mining tasks
   - Metrics and results
   - 10 pages, 1.5 hours

4. **No AI Without PI (2025)**
   - Why process intelligence matters for AI
   - Foundation argument
   - 15 pages, 2 hours

5. **Knowledge-Driven Hallucination in LLMs (2025)**
   - When/why LLMs fail; mitigation strategies
   - 10 pages, 1.5 hours

**Total: 5 papers, ~15 hours**

---

### Practitioner Path (12 papers, ~40 hours)

**Foundation (Beginner Path):**
1. Process Modeling with Large Language Models
2. Evaluating Large Language Models on Business Process Modeling
3. PM-LLM Benchmark
4. No AI Without PI
5. Knowledge-Driven Hallucination in LLMs

**Applied AI Techniques:**
6. **Text-Aware Predictive Monitoring (2021)**
   - Combine text analysis with ML for prediction
   - 10 pages, 1.5 hours

7. **Event Log Sampling for Predictive Monitoring (2022)**
   - ML approaches to sampling; maintain prediction quality
   - 10 pages, 1.5 hours

8. **Leveraging LLMs for Enhanced Process Model Comprehension (2024)**
   - LLM as explanation tool for process models
   - 15 pages, 2 hours

9. **ProMoAI: Process Modeling with Generative AI (2024)**
   - Practical system for AI-assisted process modeling
   - Workflow and lessons learned
   - 10 pages, 1.5 hours

**Advanced AI Topics:**
10. **Releasing Differentially Private Event Logs (2024)**
    - Privacy-preserving synthetic data generation
    - 15 pages, 2 hours

11. **Trustworthy Artificial Intelligence and Process Mining (2021)**
    - Fairness, explainability, robustness in process mining
    - 20 pages, 3 hours

12. **Abstractions, Scenarios, and Prompt Definitions for Process Modeling (2023)**
    - Structured prompting for process mining
    - 12 pages, 2 hours

**Total: 12 papers, ~40 hours**

---

### Research Path (25+ papers, ~100 hours)

**Part 1: Foundation & Benchmarking (8 papers)**
1. Process Modeling with Large Language Models
2. Evaluating Large Language Models on Business Process Modeling
3. PM-LLM Benchmark
4. No AI Without PI
5. Knowledge-Driven Hallucination in LLMs
6. Trustworthy Artificial Intelligence and Process Mining
7. Abstractions, Scenarios, and Prompt Definitions
8. **Interactive Process Improvement using Simulation (2022)**
   - Humans in the loop; simulation + AI
   - 15 pages, 2 hours

**Part 2: Neural & ML Techniques (8 papers)**
9. **Text-Aware Predictive Monitoring (2021)**

10. **Event Log Sampling for Predictive Monitoring (2022)**

11. **Performance-Preserving Event Log Sampling (2023)**
    - ML-guided sampling to maintain model quality
    - 10 pages, 1.5 hours

12. **Predictive Object-Centric Process Monitoring (2022)**
    - Neural networks for multi-object process prediction
    - 15 pages, 2 hours

13. **Explainable Predictive Decision Mining (2022)**
    - Interpretable ML for process prediction
    - 15 pages, 2 hours

14. **Event Abstraction for Process Mining using Supervised Learning (2016)**
    - Label refinement via supervised learning
    - 20 pages, 3 hours

15. **Log-based Evaluation of Label Splits (2016)**
    - Data-driven label disambiguation
    - 15 pages, 2 hours

16. **Inferring Unobserved Events (2021)**
    - Probabilistic inference for missing data
    - 15 pages, 2 hours

**Part 3: Generative Models & Privacy (6 papers)**
17. **Releasing Differentially Private Event Logs (2024)**

18. **Privacy-Preserving Data Publishing in Process Mining (2021)**
    - Differential privacy for process logs
    - 20 pages, 3 hours

19. **Quantifying Temporal Privacy Leakage (2022)**
    - Privacy risk assessment
    - 15 pages, 2 hours

20. **Practical Aspects of Privacy-Preserving Data Publishing (2020)**
    - Implementation and deployment
    - 15 pages, 2 hours

21. **Group-Based Privacy Preservation Techniques (2021)**
    - Privacy via clustering and aggregation
    - 20 pages, 3 hours

22. **TraVaG: Differentially Private Trace Variant Generation (2023)**
    - GAN-based synthetic log generation
    - 15 pages, 2 hours

**Part 4: Hybrid & Advanced (5 papers)**
23. **Leveraging LLMs for Enhanced Process Model Comprehension (2024)**

24. **ProMoAI: Process Modeling with Generative AI (2024)**

25. **Learning Hybrid Process Models from Events (2017)**
    - Combine data-aware + control-flow models
    - 20 pages, 3 hours

26. **Interactive Process Improvement using Simulation (2022)**

27. **What if Process Predictions are not followed by Good Recommendations (2019)**
    - Recommendation systems for process improvement
    - 20 pages, 3 hours

**Frontier Papers (3+ papers)**
28. **How to Write Beautiful Process and Data Science Papers (2023)**
    - Meta: how to communicate process mining research
    - 5 pages, 0.5 hours

29. Vision papers on specific AI applications (emerging)

**Total: 30+ papers, ~100 hours**

---

## Key Research Questions

### Solved
- [x] Can ML predict next activity/remaining time? **Yes (98%+ accuracy possible)**
- [x] Can GANs generate realistic synthetic logs? **Yes (maintains statistical properties)**
- [x] Can we apply differential privacy to logs? **Yes (with utility trade-offs)**

### Partially Solved
- [ ] Can LLMs generate process models from descriptions? **Yes (60–80% on simple models)**
- [ ] Can LLMs generate models from event logs? **Only with augmentation; 40–60% on complex models**
- [ ] How do we prevent LLM hallucination? **Domain constraints help; not fully solved**

### Open
- [ ] Is algorithmic discovery or LLM generation preferable? **Context-dependent; no clear winner**
- [ ] How do we optimally combine algorithms + LLMs? **Emerging research; multiple approaches**
- [ ] Can neural networks replace Petri net models? **No; neural models lack formal semantics**
- [ ] How do we benchmark AI on process mining? **Recent work (PM-LLM Benchmark) but incomplete**
- [ ] What is the role of process mining in AGI? **Speculative; "No AI Without PI" claims it's foundational**

---

## Gaps & Future Directions

### Technical Gaps

1. **Hallucination Mitigation**
   - Current: Known problem, no standard solution
   - Future: Use algorithmic validators, knowledge graphs, symbolic+neural hybrid
   - Research: Reinforcement learning from human feedback (RLHF) on process models

2. **Formal Verification of LLM Outputs**
   - Current: LLMs can't prove soundness
   - Future: Post-LLM symbolic verification; certified generation
   - Research: Provably correct neural-symbolic systems

3. **Multi-Modal Process Understanding**
   - Current: Separate ML for logs, text, images, metadata
   - Future: Unified multi-modal models (e.g., CLIP for process logs)
   - Research: Multi-modal transformers for process data

4. **Few-Shot Learning for Domain-Specific Processes**
   - Current: Fine-tuning requires lots of domain data
   - Future: Learn from minimal examples (1–5 processes)
   - Research: Meta-learning, transfer learning on process representations

5. **Explainability & Interpretability**
   - Current: Neural models are black boxes
   - Future: Process mining specifically tailored for interpretability
   - Research: Attention visualization, feature importance for processes

---

## Narrative: AI Entering Process Mining (2023–2025)

### The Disruption Arc

**2023: "Can LLMs Do This?"**
A few papers asked if ChatGPT could generate process models. Results were mixed but surprising. LLMs were decent at generating BPMN from natural language descriptions but struggled with event logs.

**2024: "How Good Are LLMs?"**
Systematic evaluation papers emerged. Benchmarks were created. Industry started shipping LLM copilots (Celonis, Minit, SAP). The question shifted from "can LLMs do this?" to "when should we use LLMs vs. algorithms?"

**2025: "What's the Optimal Hybrid?"**
The field is converging on a hybrid approach:
- Use **algorithms** for discovery from logs (formal guarantees)
- Use **LLMs** for explanation, refinement, requirements gathering
- Use **domain knowledge** (ontologies, rules) to ground LLMs
- Keep **humans** in the loop for validation

This is pragmatic, not ideological. Both algorithms and LLMs have strengths and weaknesses.

### Key Inflection Points

1. **"Evaluating LLMs on Business Process Modeling" (2024)**
   - First rigorous evaluation framework
   - Showed LLMs are promising but not ready for critical applications without oversight
   - Shifted conversation from "hype" to "metrics"

2. **"PM-LLM Benchmark" (2024)**
   - Standardized benchmark for future work
   - Like ImageNet did for computer vision

3. **"No AI Without PI" (2025)**
   - Bold claim: process intelligence is prerequisite for generative AI
   - Not universally accepted but influential
   - Refocused field on foundational importance

---

## Key Takeaways

1. **LLMs are useful, not replacement:** They're excellent for specific tasks (explanation, requirements) but inferior to algorithms for discovery.

2. **Hybrid is the future:** The best systems will combine algorithmic rigor with LLM flexibility.

3. **Hallucination is the key risk:** Process models have formal semantics. Hallucinations can break critical processes. Mitigation is essential.

4. **Grounding matters:** LLMs perform better when grounded in domain knowledge, constraints, ontologies.

5. **Benchmark standardization is urgent:** Without common benchmarks, we can't objectively compare approaches.

6. **Process Intelligence as AI Foundation:** The claim that process mining grounds generative AI is speculative but intriguing. If true, it elevates the field's strategic importance.

---

**Last Updated:** 2026-03-29
**Collection Size:** 34 papers
**Frontier Status:** Actively evolving (2023–2025)
