# Wil M. P. van der Aalst Papers Collection

This document provides comprehensive summaries of 166 papers by Wil M. P. van der Aalst and colleagues, focusing on Process Mining, Petri Nets, and Business Process Management.

**Collection Overview:**
- 121 arXiv preprints (2012-2026)
- 45 classic papers (1996-2023)
- Total: 166 papers

---


## Papers from Unknown (Pre-2000)

### **Designing Workflows with Coloured Petri Nets**

- **Authors:** Wil M. P. van der Aalst
- **Year:** ~1997
- **File:** `designing_workflow_coloured_petri_nets.pdf`
- **Venue:** CPN '96 Group

**Problem**: Traditional workflow modeling lacks formal semantics and the ability to handle complex data attributes and color sets.

**Key Insights**:
- Colored Petri Nets (CPNs) enable data-aware workflow modeling
- CPN Tools provide simulation and verification capabilities
- Color sets represent data types and attributes in workflows
- Hierarchical CPNs support modular workflow design

**CRE Relevance**: Data-aware workflow patterns - Foundation for gen_pnet with token attributes and colored Petri net extensions. CRE's `gen_pnet` can leverage CPN concepts for enhanced workflow modeling.

**Implementation Status**: Implemented - `src/pnet/pnet.erl` supports token attributes and color sets

**Related Modules**: `src/pnet/pnet.erl`, `src/pnet/pnet_marking.erl`

---

### **Effectiveness of Workflow Management Systems**

- **Authors:** Wil M. P. van der Aalst
- **Year:** ~1999
- **File:** `effectiveness_workflow_management_systems.pdf`
- **Venue:** BPTrends/CAiSE

**Problem**: Organizations need guidance on selecting and implementing workflow management systems (WfMS) effectively.

**Key Insights**:
- WfMS success depends on proper process modeling and analysis
- Workflow patterns provide a framework for system evaluation
- Soundness verification is critical for workflow correctness
- Simulation capabilities enable process optimization before deployment

**CRE Relevance**: YAWL engine evaluation and workflow pattern implementation - CRE's gen_yawl implements the 43 workflow patterns and soundness verification.

**Implementation Status**: Implemented - Full YAWL engine with all 43 patterns

**Related Modules**: `src/core/gen_yawl.erl`, `src/patterns/*.erl`

---

### **Mining Process Models for Non-Free Choice**

- **Authors:** Wil M. P. van der Aalst
- **Year:** ~2010
- **File:** `mining_process_models_non_free_choice.pdf`
- **Venue:** BPM Conference

**Problem**: Process discovery for non-free-choice nets is computationally complex and lacks practical algorithms.

**Key Insights**:
- Non-free-choice structures represent real-world decision patterns
- Advanced place computation enables non-free-choice discovery
- Region-based approaches for complex process models
- Trade-offs between model precision and computational complexity

**CRE Relevance**: Advanced process discovery - CRE's heuristic miner and alpha algorithm handle non-free-choice structures. Ongoing work on choice graph mining for complex decisions.

**Implementation Status**: Partially Implemented - Heuristic miner handles some non-free-choice patterns

**Related Modules**: `src/mining/process_discovery.erl`, `src/rust_implementations/paper_algorithms/algorithms/choice_graph_miner/`

---

### **YAWL Technical Manual**

- **Authors:** Wil M. P. van der Aalst, Arthur H. M. ter Hofstede
- **Year:** 2008
- **File:** `YAWL_Technical_Manual.pdf`
- **Venue**: YAWL Foundation

**Problem**: Comprehensive reference for implementing and using the YAWL workflow system.

**Key Insights**:
- Complete specification of YAWL language semantics
- 43 workflow control-flow patterns formalized
- YAWL services architecture and execution engine
- Worklet handling for exception management
- Decomposition and cancellation sets for complex workflows

**CRE Relevance**: Core YAWL implementation - CRE's gen_yawl is based on YAWL specifications with full pattern support.

**Implementation Status**: Implemented - Complete YAWL engine

**Related Modules**: `src/core/gen_yawl.erl`, `src/yawl/*.erl`, `src/patterns/*.erl`

---

### **YAWL: Design and Implementation (QUT Version)**

- **Authors:** Wil M. P. van der Aalst, Arthur H. M. ter Hofstede
- **Year:** 2004
- **File:** `yawl_design_implementation_qut_2004.pdf`
- **Venue:** Queensland University of Technology

**Problem**: Need for a workflow language that supports all workflow patterns without limitations.

**Key Insights**:
- YAWL supports all 43 workflow patterns
- Formal semantics based on Petri nets and reset nets
- Direct mapping to XML-based YAWL language
- Separation of concerns between control flow and data
- Extensible architecture for custom services

**CRE Relevance**: YAWL design principles - CRE implements YAWL semantics in Erlang with pattern-based execution.

**Implementation Status**: Implemented - Full YAWL engine

**Related Modules**: `src/core/gen_yawl.erl`, `src/yawl/*.erl`

---


## Papers from 2026

### **Deciding Reachability and the Covering Problem with Diagnostics for Sound Acyclic Free-Choice Workflow Nets**

- **Authors:** Thomas M. Prinz, Christopher T. Schwanen, Wil M. P. van der Aalst
- **Year:** 2026
- **arXiv:** [2602.02447](https://arxiv.org/abs/2602.02447)
- **Venue:** arXiv preprint (Submitted Feb 2, 2026)
- **Pages:** 38 pages, 18 figures

**Problem**: Reachability and covering problems are central to Petri net theory, but existing solutions lack efficiency and explainability for sound acyclic free-choice workflow nets.

**Key Insights**:
- **Quadratic Complexity**: Refines reachability to O(P^2 + T^2) for sound acyclic free-choice workflow nets
- **Admissibility**: New concept for concurrent token placement - all places must be pairwise concurrent
- **Maximum Admissibility**: Adding any place would break admissibility
- **Diverging Transitions**: Transitions that produce concurrent tokens leading to a marking
- **Post-Dominance Frontiers**: Compiler construction concept applied to Petri net analysis
- **Explainable Diagnostics**: Clear explanations for why a marking is reachable or not

**CRE Relevance**: State space exploration optimization - CRE's soundness verification in `src/verification/soundness.erl` implements efficient reachability analysis for workflow nets. The admissibility concepts improve concurrent token handling.

**Implementation Status**: Implemented - Core soundness verification with reachability analysis

**Related Modules**: `src/verification/soundness.erl`, `src/pnet/pnet_marking.erl`

**Algorithm Name**: Acyclic Free-Choice Reachability with Diagnostics

---


## Papers from 2025

### **Revealing Inherent Concurrency in Event Data: A Partial Order Approach to Process Discovery**

- **Authors:** Humam Kourani, Gyunam Park, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2509.15346](https://arxiv.org/abs/2509.15346)
- **Submission:** September 22, 2025

**Problem**: Process discovery algorithms traditionally linearize events, failing to capture inherent concurrency. Existing partial-order techniques struggle with scalability.

**Key Insights**:
- **Direct Partial Order Leverage**: Algorithm preserves inherent concurrency without linearization
- **Partially Ordered Traces**: Derives partial orders directly from event data
- **Sound-by-Construction**: Perfectly fitting process model output
- **Hierarchical Abstraction**: Systematically abstracts exclusive choices
- **Scalable Design**: Handles large event logs efficiently
- **Concurrency Preservation**: Maintains true concurrent relationships

**CRE Relevance**: Advanced process discovery - Partial order discovery enhances CRE's process mining capabilities for complex concurrent processes.

**Implementation Status**: Planned - Partial order process discovery module

**Related Modules**: `src/mining/process_discovery.erl`, `src/pnet/pnet.erl`

**Algorithm Name**: Partial Order Process Discovery

---

### **Computing Alignments for Partially-ordered Traces Through Petri Net Unfoldings**

- **Authors:** Ariba Siddiqui, Wil M. P. van der Aalst, Daniel Schuster
- **Year:** 2025
- **arXiv:** [2504.00550](https://arxiv.org/abs/2504.00550)
- **Submission:** April 1, 2025

**Problem**: Conventional alignment techniques assume strict total ordering, leading to inaccuracies with overlapping/missing timestamps. Reachability graphs suffer from state space explosion.

**Key Insights**:
- **Partial Order Alignments**: Efficient alignment for partially ordered traces
- **Petri Net Unfoldings**: Avoids state space explosion problem
- **Directed Net Unfoldings**: FoldA algorithm for efficient computation
- **Trace Unification**: Merges multiple trace orders correctly
- **Complex Conformance**: Handles complex trace structures accurately
- **Natural Partial Orders**: Avoids artificial interleaving creation

**CRE Relevance**: Advanced conformance checking - CRE's alignment computation can be enhanced with partial order support and Petri net unfolding techniques.

**Implementation Status**: Partially Implemented - Basic alignments in `src/mining/conformance.erl`

**Related Modules**: `src/mining/conformance.erl`, `src/pnet/pnet.erl`

**Algorithm Name**: Partial Order Alignment with Petri Net Unfolding

---

### **Knowledge-Driven Hallucination in Large Language Models: An Empirical Study on Process Modeling**

- **Authors:** Humam Kourani, Anton Antonov, Alessandro Berti, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2509.15336](https://arxiv.org/abs/2509.15336)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The utility of Large Language Models (LLMs) in analytical tasks is rooted in their vast pre-trained knowledge, which allows them to interpret ambiguous inputs and infer missing information. However, this same capability introduces a critical risk of what we term knowledge-driven hallucination: a phenomenon where the model's output contradicts explicit source evidence because it is overridden by the model's generalized internal knowledge. This paper investigates this phenomenon by evaluating LLMs on the task of automated process modeling, where the goal is to generate a formal business process ...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs

**CRE Relevance:** LLM/AI integration - Future enhancement for CRE workflow automation

---

### **No AI Without PI! Object-Centric Process Mining as the Enabler for Generative, Predictive, and Prescriptive Artificial Intelligence**

- **Authors:** Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2508.00116](https://arxiv.org/abs/2508.00116)
- **Venue:** INFUS 2025 Keynote (7th International Conference on Intelligent and Fuzzy Systems)
- **Pages:** 10 pages, 4 figures
- **Submission:** July 31, 2025

**Problem**: Organizations struggle to apply AI (generative, predictive, prescriptive) successfully in industrial settings where focus is on end-to-end operational processes.

**Key Insights**:
- **Process Intelligence (PI)**: The missing link between AI and operational processes
- **Object-Centric Process Mining (OCPM)**: Foundation for grounding AI in process context
- **Data vs. Text**: Process data is structured, organization-specific, and dynamic (unlike text)
- **Three AI Types for Processes**:
  - **Generative AI**: Creating process models, simulations, and scenarios
  - **Predictive AI**: Forecasting future behavior, remaining time, bottlenecks
  - **Prescriptive AI**: Actionable recommendations for process improvement
- **OCPM as Enabler**: Handles multiple objects, dynamic behavior, and organizational context

**CRE Relevance**: AI-enhanced process mining foundation - CRE's predictive mining, object-centric support, and LLM integration align with this framework. CRE is positioned to implement all three AI types.

**Implementation Status**: Partially Implemented - Predictive mining in `src/mining/predictive_mining.erl`, object-centric in Rust, LLM integration in progress

**Related Modules**: `src/mining/predictive_mining.erl`, `src/mining/anomaly_detection.erl`, `src/rust_implementations/paper_algorithms/algorithms/llm_process_modeling/`, `src/rust_implementations/paper_algorithms/algorithms/generative_ai/`

**Algorithm Names**: Generative Process Mining, Predictive Process Mining Enhancement, Prescriptive Process Mining

---

### **CPN-Py: A Python-Based Tool for Modeling and Analyzing Colored Petri Nets**

- **Authors:** Alessandro Berti, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2506.12238](https://arxiv.org/abs/2506.12238)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Colored Petri Nets (CPNs) are an established formalism for modeling processes where tokens carry data. Although tools like CPN Tools and CPN IDE excel at CPN-based simulation, they are often separate from modern data science ecosystems. Meanwhile, Python has become the de facto language for process mining, machine learning, and data analytics. In this paper, we introduce CPN-Py, a Python library that faithfully preserves the core concepts of Colored Petri Nets -- including color sets, timed tokens, guard logic, and hierarchical structures -- while providing seamless integration with the Python...

**Key Contributions:**
- Provides a practical tool implementation
- Provides theoretical foundations with proofs

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **OCPQ: Object-Centric Process Querying & Constraints**

- **Authors:** Aaron Küsters, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2506.11541](https://arxiv.org/abs/2506.11541)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process querying is used to extract information and insights from process execution data. Similarly, process constraints can be checked against input data, yielding information on which process instances violate them. Traditionally, such process mining techniques use case-centric event data as input. However, with the uptake of Object-Centric Process Mining (OCPM), existing querying and constraint checking techniques are no longer applicable. Object-Centric Event Data (OCED) removes the requirement to pick a single case notion (i.e., requiring that events belong to exactly one case) and can th...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Unlocking Non-Block-Structured Decisions: Inductive Mining with Choice Graphs**

- **Authors:** Humam Kourani, Gyunam Park, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2505.07052](https://arxiv.org/abs/2505.07052)
- **Venue:** BPM 2025 (23rd International Conference on Business Process Management)
- **Submission:** May 11, 2025

**Problem**: Inductive mining algorithms impose strict block-structured representations, limiting their ability to capture real-world decision patterns. POWL addressed concurrency but not non-block-structured decisions.

**Key Insights**:
- **Choice Graphs**: Extension to POWL for modeling non-block-structured decision points
- **Structured Flexibility**: Choice graphs provide structured yet flexible decision modeling
- **Inductive Discovery Algorithm**: Preserves quality guarantees of inductive mining framework
- **Hierarchical Framework**: Integrates choice graphs into POWL's hierarchical structure
- **Experimental Results**: Discovered models more precisely represent complex decision-making behavior
- **Scalability**: Maintains high scalability of inductive mining techniques

**CRE Relevance**: Advanced process discovery - CRE's choice graph miner in `src/rust_implementations/paper_algorithms/algorithms/choice_graph_miner/` implements this approach. Enables discovery of complex real-world decision patterns.

**Implementation Status**: In Progress - Choice graph miner implementation in Rust

**Related Modules**: `src/rust_implementations/paper_algorithms/algorithms/choice_graph_miner/`, `src/mining/process_discovery.erl`

**Algorithm Name**: Choice Graph Inductive Miner

---

### **Releasing Differentially Private Event Logs Using Generative Models**

- **Authors:** Frederik Wangelik, Majid Rafiei, Mahsa Pourbafrani, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2504.06418](https://arxiv.org/abs/2504.06418)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In recent years, the industry has been witnessing an extended usage of process mining and automated event data analysis. Consequently, there is a rising significance in addressing privacy apprehensions related to the inclusion of sensitive and private information within event data utilized by process mining algorithms. State-of-the-art research mainly focuses on providing quantifiable privacy guarantees, e.g., via differential privacy, for trace variants that are used by the main process mining techniques, e.g., process discovery. However, privacy preservation techniques designed for the relea...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets
- Addresses privacy preservation in event data

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Privacy preservation - Important for event data handling | Uncertain event data handling - Robust process mining techniques

---

### **Computing Alignments for Partially-ordered Traces Through Petri Net Unfoldings**

- **Authors:** Ariba Siddiqui, Wil M. P. van der Aalst, Daniel Schuster
- **Year:** 2025
- **arXiv:** [2504.00550](https://arxiv.org/abs/2504.00550)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Conformance checking techniques aim to provide diagnostics on the conformity between process models and event data. Conventional methods, such as trace alignments, assume strict total ordering of events, leading to inaccuracies when timestamps are overlapping, coarse, or missing. In contrast, existing methods that support partially ordered events rely upon the interleaving semantics of Petri nets, the reachability graphs, which suffer from the state space explosion problem. %Besides, this view also forces the solution to create a partially ordered alignment almost artificially. This paper prop...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Conformance checking and alignments - Validates workflow execution against models | Stream processing - Relevant for real-time event processing in CRE

---

### **ProReco: A Process Discovery Recommender System**

- **Authors:** Tsung-Hao Huang, Tarek Junied, Marco Pegoraro, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2502.10230](https://arxiv.org/abs/2502.10230)
- **Submission:** February 14, 2025

**Problem**: Selecting the most suitable process discovery algorithm from dozens of options is time-consuming and error-prone due to competing quality measures and diverse user requirements.

**Key Insights**:
- **Automated Algorithm Recommendation**: Recommends best discovery algorithm based on log characteristics
- **Multi-Criteria Decision Making**: Balances fitness, precision, generalization, and simplicity
- **Performance Prediction**: Estimates algorithm performance before execution
- **Meta-Learning**: Learns optimal algorithm selections from historical data
- **User Preferences**: Incorporates domain-specific requirements
- **Explainable Recommendations**: Provides rationale for algorithm choices

**CRE Relevance**: Intelligent algorithm selection - CRE's process recommender in `src/rust_implementations/paper_algorithms/algorithms/process_recommender/` implements automated algorithm selection.

**Implementation Status**: In Progress - Process discovery recommender system

**Related Modules**: `src/rust_implementations/paper_algorithms/algorithms/process_recommender/`

**Algorithm Name**: ProReco - Process Discovery Recommender

---

### **Control-flow anomaly detection by process mining-based feature extraction and dimensionality reduction**

- **Authors:** Francesco Vitale, Marco Pegoraro, Wil M. P. van der Aalst, Nicola Mazzocca
- **Year:** 2025
- **arXiv:** [2502.10211](https://arxiv.org/abs/2502.10211)
- **Submission:** February 14, 2025

**Problem**: Conformance checking effectiveness is negatively affected by noisy event data and low-quality process models. Need for more robust anomaly detection.

**Key Insights**:
- **Feature Extraction**: Process mining-based features for anomaly detection
- **Dimensionality Reduction**: Techniques to handle high-dimensional feature spaces
- **Control-Flow Anomalies**: Unknown, skipped, and wrongly-ordered activities
- **ML-Based Detection**: Machine learning for pattern recognition
- **Robust Framework**: Handles noisy data and low-quality models
- **Explainable Results**: Links anomalies to specific process elements

**CRE Relevance**: Enhanced anomaly detection - CRE's anomaly detection in `src/mining/anomaly_detection.erl` can be enhanced with ML-based feature extraction and dimensionality reduction.

**Implementation Status**: Implemented - Basic anomaly detection, ML enhancement planned

**Related Modules**: `src/mining/anomaly_detection.erl`, `src/mining/anomaly_statistics.erl`

**Algorithm Name**: Feature-Based Anomaly Detection

---

### **Federated Conformance Checking**

- **Authors:** Majid Rafiei, Mahsa Pourbafrani, Wil M. P. van der Aalst
- **Year:** 2025
- **arXiv:** [2501.13576](https://arxiv.org/abs/2501.13576)
- **Submission:** January 24, 2025

**Problem**: Organizations need to validate process compliance across organizational boundaries without sharing sensitive event data.

**Key Insights**:
- **Privacy-Preserving Conformance**: Cross-organizational validation without data sharing
- **Federated Learning**: Distributed model training for conformance checking
- **Secure Aggregation**: Privacy-aware fitness and precision computation
- **Multi-Party Validation**: Multiple organizations verify compliance collaboratively
- **Differential Privacy**: Mathematical privacy guarantees for sensitive data
- **Distributed Architecture**: Fault-tolerant federated computation

**CRE Relevance**: Cross-organizational validation - CRE's conformance checking can be extended with federated learning for privacy-preserving cross-organizational validation.

**Implementation Status**: Planned - Federated conformance checking module

**Related Modules**: `src/mining/conformance.erl`, planned: `src/mining/federated_conformance.erl`

**Algorithm Name**: Federated Conformance Checking with Differential Privacy

---

### **No AI Without PI! Object-Centric Process Mining as the Enabler for AI**

- **Authors:** van der Aalst
- **Year:** 2025
- **File:** `van_der_aalst_2025_no_ai_without_pi.pdf`

**Key Contributions:**
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---


## Papers from 2024

### **Evaluating Large Language Models on Business Process Modeling: Framework, Benchmark, and Self-Improvement Analysis**

- **Authors:** Humam Kourani, Alessandro Berti, Daniel Schuster, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2412.00023](https://arxiv.org/abs/2412.00023)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Large Language Models (LLMs) are rapidly transforming various fields, and their potential in Business Process Management (BPM) is substantial. This paper assesses the capabilities of LLMs on business process modeling using a framework for automating this task, a comprehensive benchmark, and an analysis of LLM self-improvement strategies. We present a comprehensive evaluation of 16 state-of-the-art LLMs from major AI vendors using a custom-designed benchmark of 20 diverse business processes. Our analysis highlights significant performance variations across LLMs and reveals a positive correlatio...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | LLM/AI integration - Future enhancement for CRE workflow automation

---

### **Object-Centric Local Process Models**

- **Authors:** Viki Peeva, Marvin Porsil, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2411.10468](https://arxiv.org/abs/2411.10468)
- **Venue:** BPM (to appear)
- **Pages:** 12 pages, 5 figures
- **Submission:** November 4, 2024

**Problem**: Traditional local process model discovery assumes a single case notion. Complex processes involve multiple objects with no single case identifier.

**Key Insights**:
- **OCLPMs (Object-Centric Local Process Models)**: Behavioral patterns for multi-object processes
- **Object-Centric Petri Nets**: Used as the modeling formalism for OCLPMs
- **No Case Notion Required**: Handles processes where multiple objects interact
- **Discovery Algorithm**: Starts from object-centric event logs (OCEL format)
- **ProM Implementation**: Implemented in the open-source ProM framework
- **Case Studies**: Two case studies demonstrating applicability
- **Multi-Perspective Analysis**: Combines different process perspectives

**CRE Relevance**: Advanced object-centric mining - CRE's object-centric local process model implementation in `src/rust_implementations/paper_algorithms/algorithms/object_centric_local/` directly implements this approach.

**Implementation Status**: In Progress - OC local process models in Rust

**Related Modules**: `src/rust_implementations/paper_algorithms/algorithms/object_centric_local/`, `src/rust_implementations/object_centric.rs`

**Algorithm Name**: Object-Centric Local Process Model Discovery (OCLPM)

---

### **Towards a Simple and Extensible Standard for Object-Centric Event Data (OCED) -- Core Model, Design Space, and Lessons Learned**

- **Authors:** Dirk Fahland, Marco Montali, Julian Lebherz, Wil M. P. van der Aalst, Maarten van Asseldonk, et al.
- **Year:** 2024
- **arXiv:** [2410.14495](https://arxiv.org/abs/2410.14495)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining is shifting towards use cases that explicitly leverage the relations between data objects and events under the term of object-centric process mining. Realizing this shift and generally simplifying the exchange and transformation of data between source systems and process mining solutions requires a standardized data format for such object-centric event data (OCED). This report summarizes the activities and results for identifying requirements and challenges for a community-supported standard for OCED. (1) We present a proposal for a core model for object-centric event data that ...

**Key Contributions:**
- Provides a practical tool implementation
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---

### **Leveraging Large Language Models for Enhanced Process Model Comprehension**

- **Authors:** Humam Kourani, Alessandro Berti, Jasmin Hennrich, Wolfgang Kratsch, Robin Weidlich, et al.
- **Year:** 2024
- **arXiv:** [2408.08892](https://arxiv.org/abs/2408.08892)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In Business Process Management (BPM), effectively comprehending process models is crucial yet poses significant challenges, particularly as organizations scale and processes become more complex. This paper introduces a novel framework utilizing the advanced capabilities of Large Language Models (LLMs) to enhance the interpretability of complex process models. We present different methods for abstracting business process models into a format accessible to LLMs, and we implement advanced prompting strategies specifically designed to optimize LLM performance within our framework. Additionally, we...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | LLM/AI integration - Future enhancement for CRE workflow automation

---

### **PM-LLM-Benchmark: Evaluating Large Language Models on Process Mining Tasks**

- **Authors:** Alessandro Berti, Humam Kourani, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2407.13244](https://arxiv.org/abs/2407.13244)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Large Language Models (LLMs) have the potential to semi-automate some process mining (PM) analyses. While commercial models are already adequate for many analytics tasks, the competitive level of open-source LLMs in PM tasks is unknown. In this paper, we propose PM-LLM-Benchmark, the first comprehensive benchmark for PM focusing on domain knowledge (process-mining-specific and process-specific) and on different implementation strategies. We focus also on the challenges in creating such a benchmark, related to the public availability of the data and on evaluation biases by the LLMs. Overall, we...

**Key Contributions:**
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** LLM/AI integration - Future enhancement for CRE workflow automation

---

### **Challenges of Anomaly Detection in the Object-Centric Setting: Dimensions and the Role of Domain Knowledge**

- **Authors:** Alessandro Berti, Urszula Jessen, Wil M. P. van der Aalst, Dirk Fahland
- **Year:** 2024
- **arXiv:** [2407.09023](https://arxiv.org/abs/2407.09023)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Object-centric event logs, allowing events related to different objects of different object types, represent naturally the execution of business processes, such as ERP (O2C and P2P) and CRM. However, modeling such complex information requires novel process mining techniques and might result in complex sets of constraints. Object-centric anomaly detection exploits both the lifecycle and the interactions between the different objects. Therefore, anomalous patterns are proposed to the user without requiring the definition of object-centric process models. This paper proposes different methodologi...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | LLM/AI integration - Future enhancement for CRE workflow automation

---

### **High-Level Event Mining: Overview and Future Work**

- **Authors:** Bianka Bakullari, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2405.14435](https://arxiv.org/abs/2405.14435)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining traditionally relies on input consisting of low-level events that capture individual activities, such as filling out a form or processing a product. However, many of the complex problems inherent in processes, such as bottlenecks and compliance issues, extend beyond the scope of individual events and process instances. Consider congestion, for instance, it can involve and impact numerous cases, much like how a traffic jam affects many cars simultaneously. High-level event mining seeks to address such phenomena using the regular event data available. This report offers an extensi...

**Key Contributions:**
- Comprehensive survey of the domain

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Process-Aware Analysis of Treatment Paths in Heart Failure Patients: A Case Study**

- **Authors:** Harry H. Beyel, Marlo Verket, Viki Peeva, Christian Rennert, Marco Pegoraro, et al.
- **Year:** 2024
- **arXiv:** [2403.10544](https://arxiv.org/abs/2403.10544)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining in healthcare presents a range of challenges when working with different types of data within the healthcare domain. There is high diversity considering the variety of data collected from healthcare processes: operational processes given by claims data, a collection of events during surgery, data related to pre-operative and post-operative care, and high-level data collections based on regular ambulant visits with no apparent events. In this case study, a data set from the last category is analyzed. We apply process-mining techniques on sparse patient heart failure data and inve...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **Process Modeling With Large Language Models**

- **Authors:** Humam Kourani, Alessandro Berti, Daniel Schuster, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2403.07541](https://arxiv.org/abs/2403.07541)
- **Venue:** BPM 2024 (International Conference on Business Process Management) - DOI: 10.1007/978-3-031-61007-3_18
- **Submission:** March 12, 2024 (Revised April 8, 2024)

**Problem**: Traditional process modeling requires extensive expertise and is time-consuming. LLMs offer potential for automation but need proper frameworks.

**Key Insights**:
- **LLM Framework for Process Modeling**: Automated generation and iterative refinement from text
- **Innovative Prompting Strategies**: Effective LLM utilization for process modeling
- **Secure Model Generation Protocol**: Error handling and quality assurance
- **Standard Notation Export**: BPMN and Petri net format support
- **Quality Guarantees**: Robust validation of generated models
- **Accessibility Enhancement**: Intuitive entry point for non-experts
- **Expert Efficiency**: Augments efficiency for experienced modelers

**CRE Relevance**: LLM-enhanced process modeling - CRE's LLM process modeling module in `src/rust_implementations/paper_algorithms/algorithms/llm_process_modeling/` implements this framework for automated model generation.

**Implementation Status**: In Progress - LLM process modeling with text-to-process conversion

**Related Modules**: `src/rust_implementations/paper_algorithms/algorithms/llm_process_modeling/`, `src/rust_implementations/paper_algorithms/algorithms/generative_ai/`

**Algorithm Name**: LLM-Based Process Model Generation and Refinement

---

### **ProMoAI: Process Modeling with Generative AI**

- **Authors:** Humam Kourani, Alessandro Berti, Daniel Schuster, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2403.04327](https://arxiv.org/abs/2403.04327)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** ProMoAI is a novel tool that leverages Large Language Models (LLMs) to automatically generate process models from textual descriptions, incorporating advanced prompt engineering, error handling, and code generation techniques. Beyond automating the generation of complex process models, ProMoAI also supports process model optimization. Users can interact with the tool by providing feedback on the generated model, which is then used for refining the process model. ProMoAI utilizes the capabilities LLMs to offer a novel, AI-driven approach to process modeling, significantly reducing the barrier t...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation

**CRE Relevance:** LLM/AI integration - Future enhancement for CRE workflow automation

---

### **OCEL (Object-Centric Event Log) 2.0 Specification**

- **Authors:** Alessandro Berti, Istvan Koren, Jan Niklas Adams, Gyunam Park, Benedikt Knopp, et al.
- **Year:** 2024
- **arXiv:** [2403.01975](https://arxiv.org/abs/2403.01975)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Object-Centric Event Logs (OCELs) form the basis for Object-Centric Process Mining (OCPM). OCEL 1.0 was first released in 2020 and triggered the development of a range of OCPM techniques. OCEL 2.0 forms the new, more expressive standard, allowing for more extensive process analyses while remaining in an easily exchangeable format. In contrast to the first OCEL standard, it can depict changes in objects, provide information on object relationships, and qualify these relationships to other objects or specific events. Compared to XES, it is more expressive, less complicated, and better readable. ...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---

### **Developing a High-Performance Process Mining Library with Java and Python Bindings in Rust**

- **Authors:** Aaron Küsters, Wil M. P. van der Aalst
- **Year:** 2024
- **arXiv:** [2401.14149](https://arxiv.org/abs/2401.14149)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The most commonly used open-source process mining software tools today are ProM and PM4Py, written in Java and Python, respectively. Such high-level, often interpreted, programming languages trade off performance with memory safety and ease-of-use. In contrast, traditional compiled languages, like C or C++, can achieve top performance but often suffer from instability related to unsafe memory management. Lately, Rust emerged as a highly performant, compiled programming language with inherent memory safety. In this paper, we describe our approach to developing a shared process mining library in...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---


## Papers from 2023

### **Advancements and Challenges in Object-Centric Process Mining: A Systematic Literature Review**

- **Authors:** Alessandro Berti, Marco Montali, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2311.08795](https://arxiv.org/abs/2311.08795)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Recent years have seen the emergence of object-centric process mining techniques. Born as a response to the limitations of traditional process mining in analyzing event data from prevalent information systems like CRM and ERP, these techniques aim to tackle the deficiency, convergence, and divergence issues seen in traditional event logs. Despite the promise, the adoption in real-world process mining analyses remains limited. This paper embarks on a comprehensive literature review of object-centric process mining, providing insights into the current status of the discipline and its historical ...

**Key Contributions:**
- Proposes a new algorithm or method
- Comprehensive survey of the domain
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---

### **Grouping Local Process Models**

- **Authors:** Viki Peeva, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2311.03040](https://arxiv.org/abs/2311.03040)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In recent years, process mining emerged as a proven technology to analyze and improve operational processes. An expanding range of organizations using process mining in their daily operation brings a broader spectrum of processes to be analyzed. Some of these processes are highly unstructured, making it difficult for traditional process discovery approaches to discover a start-to-end model describing the entire process. Therefore, the subdiscipline of Local Process Model (LPM) discovery tries to build a set of LPMs, i.e., smaller models that explain sub-behaviors of the process. However, like ...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Discovering High-Quality Process Models Despite Data Scarcity**

- **Authors:** Jan Niklas Adams, Jari Peeperkorn, Tobias Brockhoff, Isabelle Terrier, Heiko Göhner, et al.
- **Year:** 2023
- **arXiv:** [2310.11332](https://arxiv.org/abs/2310.11332)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process discovery algorithms learn process models from executed activity sequences, describing concurrency, causality, and conflict. Concurrent activities require observing multiple permutations, increasing data requirements, especially for processes with concurrent subprocesses such as hierarchical, composite, or distributed processes. While process discovery algorithms traditionally use sequences of activities as input, recently introduced object-centric process discovery algorithms can use graphs of activities as input, encoding partial orders between activities. As such, they contain the c...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs
- Addresses object-centric process mining challenges

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Object-centric process mining - Advanced feature for multi-object workflows

---

### **Analyzing An After-Sales Service Process Using Object-Centric Process Mining: A Case Study**

- **Authors:** Gyunam Park, Sevde Aydin, Cuneyt Ugur, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2310.10174](https://arxiv.org/abs/2310.10174)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining, a technique turning event data into business process insights, has traditionally operated on the assumption that each event corresponds to a singular case or object. However, many real-world processes are intertwined with multiple objects, making them object-centric. This paper focuses on the emerging domain of object-centric process mining, highlighting its potential yet underexplored benefits in actual operational scenarios. Through an in-depth case study of Borusan Cat's after-sales service process, this study emphasizes the capability of object-centric process mining to cap...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---

### **Extracting Rules from Event Data for Study Planning**

- **Authors:** Majid Rafiei, Duygu Bayrak, Mahsa Pourbafrani, Gyunam Park, Hayyan Helal, et al.
- **Year:** 2023
- **arXiv:** [2310.02735](https://arxiv.org/abs/2310.02735)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In this study, we examine how event data from campus management systems can be used to analyze the study paths of higher education students. The main goal is to offer valuable guidance for their study planning. We employ process and data mining techniques to explore the impact of sequences of taken courses on academic success. Through the use of decision tree models, we generate data-driven recommendations in the form of rules for study planning and compare them to the recommended study plan. The evaluation focuses on RWTH Aachen University computer science bachelor program students and demons...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **The Interplay Between High-Level Problems and The Process Instances That Give Rise To Them**

- **Authors:** Bianka Bakullari, Jules van Thoor, Dirk Fahland, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2309.01571](https://arxiv.org/abs/2309.01571)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Business processes may face a variety of problems due to the number of tasks that need to be handled within short time periods, resources' workload and working patterns, as well as bottlenecks. These problems may arise locally and be short-lived, but as the process is forced to operate outside its standard capacity, the effect on the underlying process instances can be costly. We use the term high-level behavior to cover all process behavior which can not be captured in terms of the individual process instances. %Whenever such behavior emerges, we call the cases which are involved in it partic...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Applying Process Mining on Scientific Workflows: a Case Study on High Performance Computing Data**

- **Authors:** Zahra Sadeghibogar, Alessandro Berti, Marco Pegoraro, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2307.02833](https://arxiv.org/abs/2307.02833)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Computer-based scientific experiments are becoming increasingly data-intensive, necessitating the use of High-Performance Computing (HPC) clusters to handle large scientific workflows. These workflows result in complex data and control flows within the system, making analysis challenging. This paper focuses on the extraction of case IDs from SLURM-based HPC cluster logs, a crucial step for applying mainstream process mining techniques. The core contribution is the development of methods to correlate jobs in the system, whether their interdependencies are explicitly specified or not. We present...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | Stream processing - Relevant for real-time event processing in CRE

---

### **Abstractions, Scenarios, and Prompt Definitions for Process Mining with LLMs: A Case Study**

- **Authors:** Alessandro Berti, Daniel Schuster, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2307.02194](https://arxiv.org/abs/2307.02194)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Large Language Models (LLMs) are capable of answering questions in natural language for various purposes. With recent advancements (such as GPT-4), LLMs perform at a level comparable to humans for many proficient tasks. The analysis of business processes could benefit from a natural process querying language and using the domain knowledge on which LLMs have been trained. However, it is impossible to provide a complete database or event log as an input prompt due to size constraints. In this paper, we apply LLMs in the context of process mining by i) abstracting the information of standard proc...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** LLM/AI integration - Future enhancement for CRE workflow automation

---

### **A Collection of Simulated Event Logs for Fairness Assessment in Process Mining**

- **Authors:** Timo Pohl, Alessandro Berti, Mahnaz Sadat Qafari, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2306.11453](https://arxiv.org/abs/2306.11453)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The analysis of fairness in process mining is a significant aspect of data-driven decision-making, yet the advancement in this field is constrained due to the scarcity of event data that incorporates fairness considerations. To bridge this gap, we present a collection of simulated event logs, spanning four critical domains, which encapsulate a variety of discrimination scenarios. By simulating these event logs with CPN Tools, we ensure data with known ground truth, thereby offering a robust foundation for fairness analysis. These logs are made freely available under the CC-BY-4.0 license and a...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Revisiting the Alpha Algorithm To Enable Real-Life Process Discovery Applications -- Extended Report**

- **Authors:** Aaron Küsters, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2305.17767](https://arxiv.org/abs/2305.17767)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The Alpha algorithm was the first process discovery algorithm that was able to discover process models with concurrency based on incomplete event data while still providing formal guarantees. However, as was stated in the original paper, practical applicability is limited when dealing with exceptional behavior and processes that cannot be described as a structured workflow net without short loops. This paper presents the Alpha+++ algorithm that overcomes many of these limitations, making the algorithm competitive with more recent process mining approaches. The different steps provide insights ...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides theoretical foundations with proofs

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Object-Centric Alignments**

- **Authors:** Lukas Liss, Jan Niklas Adams, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2305.05113](https://arxiv.org/abs/2305.05113)
- **Venue:** BPM 2023

**Problem**: Traditional conformance checking assumes a single case identifier. Object-centric processes involve multiple interacting objects, requiring flattening which causes information loss.

**Key Insights**:
- **Object-Centric Alignment**: Operates directly on object-centric event logs
- **No Flattening Required**: Preserves information about object interactions
- **Multi-Object Conformance**: Validates compliance across object types
- **Efficient Computation**: Optimized for complex object-centric structures
- **ProM Implementation**: Available in open-source framework

**CRE Relevance**: Object-centric conformance checking - CRE's object-centric conformance validation implements these alignment techniques.

**Implementation Status**: Partially Implemented - Object-centric basic support

**Related Modules**: `src/rust_implementations/object_centric.rs`, `src/mining/conformance.erl`

**Algorithm Name**: Object-Centric Alignment Computation

---

### **TraVaG: Differentially Private Trace Variant Generation Using GANs**

- **Authors:** Majid Rafiei, Frederik Wangelik, Mahsa Pourbafrani, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2303.16704](https://arxiv.org/abs/2303.16704)
- **Venue:** CAiSE 2023

**Problem**: Privacy preservation techniques for releasing trace variants don't fulfill industry-scale requirements. Need for differential privacy with quality preservation.

**Key Insights**:
- **GAN-Based Generation**: Generative Adversarial Networks for trace variant generation
- **Differential Privacy**: Mathematically proven privacy guarantees (epsilon-delta)
- **Quality Preservation**: Maintains utility while ensuring privacy
- **Multi-Dimensional Protection**: Protects case, activity, and timing information
- **Industry-Ready**: Scales to real-world event logs

**CRE Relevance**: Privacy-preserving event log generation - CRE's differential privacy module can leverage GAN-based generation for safe event log sharing.

**Implementation Status**: Planned - GAN-based differential privacy for event logs

**Related Modules**: Planned: `src/mining/differential_privacy.erl`

**Algorithm Name**: TraVaG - GAN-Based Differential Privacy

---

### **Performance-Preserving Event Log Sampling for Predictive Monitoring**

- **Authors:** Mohammadreza Fani Sani, Mozhgan Vazifehdoostirani, Gyunam Park, Marco Pegoraro, Sebastiaan J. van Zelst, et al.
- **Year:** 2023
- **arXiv:** [2301.07624](https://arxiv.org/abs/2301.07624)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Predictive process monitoring is a subfield of process mining that aims to estimate case or event features for running process instances. Such predictions are of significant interest to the process stakeholders. However, most of the state-of-the-art methods for predictive monitoring require the training of complex machine learning models, which is often inefficient. Moreover, most of these methods require a hyper-parameter optimization that requires several repetitions of the training process which is not feasible in many real-life applications. In this paper, we propose an instance selection ...

**Key Contributions:**
- Proposes a new algorithm or method
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Discovering Sound Free-choice Workflow Nets With Non-block Structures**

- **Authors:** Tsung-Hao Huang, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2301.02185](https://arxiv.org/abs/2301.02185)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process discovery aims to discover models that can explain the behaviors of event logs extracted from information systems. While various approaches have been proposed, only a few guarantee desirable properties such as soundness and free-choice. State-of-the-art approaches that exploit the representational bias of process trees to provide the guarantees are constrained to be block-structured.Such constructs limit the expressive power of the discovered models, i.e., only a subset of sound free-choice workflow nets can be discovered. To support a more flexible structural representation, we aim to...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Stream processing - Relevant for real-time event processing in CRE

---

### **Comparing Ordering Strategies For Process Discovery Using Synthesis Rules**

- **Authors:** Tsung-Hao Huang, Wil M. P. van der Aalst
- **Year:** 2023
- **arXiv:** [2301.02182](https://arxiv.org/abs/2301.02182)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process discovery aims to learn process models from observed behaviors, i.e., event logs, in the information systems.The discovered models serve as the starting point for process mining techniques that are used to address performance and compliance problems. Compared to the state-of-the-art Inductive Miner, the algorithm applying synthesis rules from the free-choice net theory discovers process models with more flexible (non-block) structures while ensuring the same desirable soundness and free-choiceness properties. Moreover, recent development in this line of work shows that the discovered m...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **Advancements and Challenges in Object-Centric Process Mining**

- **Authors:** van der Aalst
- **Year:** 2023
- **File:** `van_der_Aalst_2023_Advancements_and_Challenges_in_Object_Centric_Process_Mining.pdf`

**Key Contributions:**
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---


## Papers from 2022

### **Discovering Process Models With Long-Term Dependencies While Providing Guarantees and Filtering Infrequent Behavior Patterns**

- **Authors:** Lisa Luise Mannel, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2212.11047](https://arxiv.org/abs/2212.11047)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In process discovery, the goal is to find, for a given event log, the model describing the underlying process. While process models can be represented in a variety of ways, Petri nets form a theoretically well-explored description language and are therefore often used. In this paper, we extend the eST-Miner process discovery algorithm. The eST-Miner computes a set of Petri net places which are considered to be fitting with respect to a certain fraction of the behavior described by the given event log as indicated by a given noise threshold. It evaluates all possible candidate places using toke...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **Resolving Uncertain Case Identifiers in Interaction Logs: A User Study**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Tom-Hendrik Hülsmann, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2212.00009](https://arxiv.org/abs/2212.00009)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Modern software systems are able to record vast amounts of user actions, stored for later analysis. One of the main types of such user interaction data is click data: the digital trace of the actions of a user through the graphical elements of an application, website or software. While readily available, click data is often missing a case notion: an attribute linking events from user interactions to a specific process instance in the software. In this paper, we propose a neural network-based technique to determine a case notion for click data, thus enabling process mining and other process ana...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** Uncertain event data handling - Robust process mining techniques

---

### **Control-Flow-Based Querying of Process Executions from Partially Ordered Event Data**

- **Authors:** Daniel Schuster, Michael Martini, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2211.04146](https://arxiv.org/abs/2211.04146)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Event logs, as viewed in process mining, contain event data describing the execution of operational processes. Most process mining techniques take an event log as input and generate insights about the underlying process by analyzing the data provided. Consequently, handling large volumes of event data is essential to apply process mining successfully. Traditionally, individual process executions are considered sequentially ordered process activities. However, process executions are increasingly viewed as partially ordered activities to more accurately reflect process behavior observed in reali...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **High-Level Event Mining: A Framework**

- **Authors:** Bianka Bakullari, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2211.00006](https://arxiv.org/abs/2211.00006)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining methods often analyze processes in terms of the individual end-to-end process runs. Process behavior, however, may materialize as a general state of many involved process components, which can not be captured by looking at the individual process instances. A more holistic state of the process can be determined by looking at the events that occur close in time and share common process capacities. In this work, we conceptualize such behavior using high-level events and propose a new framework for detecting and logging such high-level events. The output of our method is a new high-...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Explainable Predictive Decision Mining for Operational Support**

- **Authors:** Gyunam Park, Aaron Küsters, Mara Tews, Cameron Pitsch, Jonathan Schneider, et al.
- **Year:** 2022
- **arXiv:** [2210.16786](https://arxiv.org/abs/2210.16786)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Several decision points exist in business processes (e.g., whether a purchase order needs a manager's approval or not), and different decisions are made for different process instances based on their characteristics (e.g., a purchase order higher than $500 needs a manager approval). Decision mining in process mining aims to describe/predict the routing of a process instance at a decision point of the process. By predicting the decision, one can take proactive actions to improve the process. For instance, when a bottleneck is developing in one of the possible decisions, one can predict the deci...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **TraVaS: Differentially Private Trace Variant Selection for Process Mining**

- **Authors:** Majid Rafiei, Frederik Wangelik, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2210.14951](https://arxiv.org/abs/2210.14951)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In the area of industrial process mining, privacy-preserving event data publication is becoming increasingly relevant. Consequently, the trade-off between high data utility and quantifiable privacy poses new challenges. State-of-the-art research mainly focuses on differentially private trace variant construction based on prefix expansion methods. However, these algorithms face several practical limitations such as high computational complexity, introducing fake variants, removing frequent variants, and a bounded variant length. In this paper, we introduce a new approach for direct differential...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **Monitoring Constraints in Business Processes Using Object-Centric Constraint Graphs**

- **Authors:** Gyunam Park, Wil. M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2210.12080](https://arxiv.org/abs/2210.12080)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Constraint monitoring aims to monitor the violation of constraints in business processes, e.g., an invoice should be cleared within 48 hours after the corresponding goods receipt, by analyzing event data. Existing techniques for constraint monitoring assume that a single case notion exists in a business process, e.g., a patient in a healthcare process, and each event is associated with the case notion. However, in reality, business processes are object-centric, i.e., multiple case notions (objects) exist, and an event may be associated with multiple objects. For instance, an Order-To-Cash (O2C...

**Key Contributions:**
- Proposes a new algorithm or method
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Process Modeling and Conformance Checking in Healthcare: A COVID-19 Case Study**

- **Authors:** Elisabetta Benevento, Marco Pegoraro, Mattia Antoniazzi, Harry H. Beyel, Viki Peeva, et al.
- **Year:** 2022
- **arXiv:** [2209.10897](https://arxiv.org/abs/2209.10897)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The discipline of process mining has a solid track record of successful applications to the healthcare domain. Within such research space, we conducted a case study related to the Intensive Care Unit (ICU) ward of the Uniklinik Aachen hospital in Germany. The aim of this work is twofold: developing a normative model representing the clinical guidelines for the treatment of COVID-19 patients, and analyzing the adherence of the observed behavior (recorded in the information system of the hospital) to such guidelines. We show that, through conformance checking techniques, it is possible to analyz...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models

---

### **Conformance Checking for Trace Fragments Using Infix and Postfix Alignments**

- **Authors:** Daniel Schuster, Niklas Föcking, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2209.04290](https://arxiv.org/abs/2209.04290)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Conformance checking deals with collating modeled process behavior with observed process behavior recorded in event data. Alignments are a state-of-the-art technique to detect, localize, and quantify deviations in process executions, i.e., traces, compared to reference process models. Alignments, however, assume complete process executions covering the entire process from start to finish or prefixes of process executions. This paper defines infix/postfix alignments, proposes approaches to their computation, and evaluates them using real-life event data.

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models

---

### **A Framework for Extracting and Encoding Features from Object-Centric Event Data**

- **Authors:** Jan Niklas Adams, Gyunam Park, Sergej Levich, Daniel Schuster, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2209.01219](https://arxiv.org/abs/2209.01219)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Traditional process mining techniques take event data as input where each event is associated with exactly one object. An object represents the instantiation of a process. Object-centric event data contain events associated with multiple objects expressing the interaction of multiple processes. As traditional process mining techniques assume events associated with exactly one object, these techniques cannot be applied to object-centric event data. To use traditional process mining techniques, the object-centric event data are flattened by removing all object references but one. The flattening ...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Detecting Surprising Situations in Event Data**

- **Authors:** Christian Kohlschmidt, Mahnaz Sadat Qafari, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2208.13515](https://arxiv.org/abs/2208.13515)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining is a set of techniques that are used by organizations to understand and improve their operational processes. The first essential step in designing any process reengineering procedure is to find process improvement opportunities. In existing work, it is usually assumed that the set of problematic process instances in which an undesirable outcome occurs is known prior or is easily detectable. So the process enhancement procedure involves finding the root causes and the treatments for the problem in those process instances. For example, the set of problematic instances is considere...

**Key Contributions:**
- Proposes a new algorithm or method
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Defining Cases and Variants for Object-Centric Event Data**

- **Authors:** Jan Niklas Adams, Daniel Schuster, Seth Schmitz, Günther Schuh, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2208.03235](https://arxiv.org/abs/2208.03235)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The execution of processes leaves traces of event data in information systems. These event data can be analyzed through process mining techniques. For traditional process mining techniques, one has to associate each event with exactly one object, e.g., the company's customer. Events related to one object form an event sequence called a case. A case describes an end-to-end run through a process. The cases contained in event data can be used to discover a process model, detect frequent bottlenecks, or learn predictive models. However, events encountered in real-life information systems, e.g., ER...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Quantifying Temporal Privacy Leakage in Continuous Event Data Publishing**

- **Authors:** Majid Rafiei, Gamal Elkoumy, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2208.01886](https://arxiv.org/abs/2208.01886)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining employs event data extracted from different types of information systems to discover and analyze actual processes. Event data often contain highly sensitive information about the people who carry out activities or the people for whom activities are performed. Therefore, privacy concerns in process mining are receiving increasing attention. To alleviate privacy-related risks, several privacy preservation techniques have been proposed. Differential privacy is one of these techniques which provides strong privacy guarantees. However, the proposed techniques presume that event data ...

**Key Contributions:**
- Proposes a new algorithm or method
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **Clustering Object-Centric Event Logs**

- **Authors:** Anahita Farhang Ghahfarokhi, Fatemeh Akoochekian, Fareed Zandkarimi, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2207.12764](https://arxiv.org/abs/2207.12764)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining provides various algorithms to analyze process executions based on event data. Process discovery, the most prominent category of process mining techniques, aims to discover process models from event logs, however, it leads to spaghetti models when working with real-life data. Therefore, several clustering techniques have been proposed on top of traditional event logs (i.e., event logs with a single case notion) to reduce the complexity of process models and discover homogeneous subsets of cases. Nevertheless, in real-life processes, particularly in the context of Business-to-Bus...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs
- Addresses object-centric process mining challenges

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Object-centric process mining - Advanced feature for multi-object workflows

---

### **Predictive Object-Centric Process Monitoring**

- **Authors:** Timo Rohrer, Anahita Farhang Ghahfarokhi, Mohamed Behery, Gerhard Lakemeyer, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2207.10017](https://arxiv.org/abs/2207.10017)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The automation and digitalization of business processes has resulted in large amounts of data captured in information systems, which can aid businesses in understanding their processes better, improve workflows, or provide operational support. By making predictions about ongoing processes, bottlenecks can be identified and resources reallocated, as well as insights gained into the state of a process instance (case). Traditionally, data is extracted from systems in the form of an event log with a single identifying case notion, such as an order id for an Order to Cash (O2C) process. However, re...

**Key Contributions:**
- Proposes a new algorithm or method
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Detecting Context-Aware Deviations in Process Executions**

- **Authors:** Gyunam Park, Janik-Vasily Benzin, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2206.05532](https://arxiv.org/abs/2206.05532)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** A deviation detection aims to detect deviating process instances, e.g., patients in the healthcare process and products in the manufacturing process. A business process of an organization is executed in various contextual situations, e.g., a COVID-19 pandemic in the case of hospitals and a lack of semiconductor chip shortage in the case of automobile companies. Thus, context-aware deviation detection is essential to provide relevant insights. However, existing work 1) does not provide a systematic way of incorporating various contexts, 2) is tailored to a specific approach without using an ext...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets
- Comprehensive survey of the domain

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models

---

### **OPerA: Object-Centric Performance Analysis**

- **Authors:** Gyunam Park, Jan Niklas Adams, Wil. M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2204.10662](https://arxiv.org/abs/2204.10662)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Performance analysis in process mining aims to provide insights on the performance of a business process by using a process model as a formal representation of the process. Such insights are reliably interpreted by process analysts in the context of a model with formal semantics. Existing techniques for performance analysis assume that a single case notion exists in a business process (e.g., a patient in healthcare process). However, in reality, different objects might interact (e.g., order, item, delivery, and invoice in an O2C process). In such a setting, traditional techniques may yield mis...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs
- Addresses object-centric process mining challenges

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **PM4Py-GPU: a High-Performance General-Purpose Library for Process Mining**

- **Authors:** Alessandro Berti, Minh Phan Nghia, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2204.04898](https://arxiv.org/abs/2204.04898)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Open-source process mining provides many algorithms for the analysis of event data which could be used to analyze mainstream processes (e.g., O2C, P2P, CRM). However, compared to commercial tools, they lack the performance and struggle to analyze large amounts of data. This paper presents PM4Py-GPU, a Python process mining library based on the NVIDIA RAPIDS framework. Thanks to the dataframe columnar storage and the high level of parallelism, a significant speed-up is achieved on classic process mining computations and processing activities.

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Provides a practical tool implementation

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | Stream processing - Relevant for real-time event processing in CRE

---

### **Uncertain Case Identifiers in Process Mining: A User Study of the Event-Case Correlation Problem on Click Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Tom-Hendrik Hülsmann, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2204.04164](https://arxiv.org/abs/2204.04164)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Among the many sources of event data available today, a prominent one is user interaction data. User activity may be recorded during the use of an application or website, resulting in a type of user interaction data often called click data. An obstacle to the analysis of click data using process mining is the lack of a case identifier in the data. In this paper, we show a case and user study for event-case correlation on click data, in the context of user interaction events from a mobility sharing company. To reconstruct the case notion of the process, we apply a novel method to aggregate user...

**Key Contributions:**
- Introduces a novel algorithmic approach

**CRE Relevance:** Uncertain event data handling - Robust process mining techniques

---

### **An XES Extension for Uncertain Event Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2204.04135](https://arxiv.org/abs/2204.04135)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Event data, often stored in the form of event logs, serve as the starting point for process mining and other evidence-based process improvements. However, event data in logs are often tainted by noise, errors, and missing data. Recently, a novel body of research has emerged, with the aim to address and analyze a class of anomalies known as uncertainty-imprecisions quantified with meta-information in the event log. This paper illustrates an extension of the XES data standard capable of representing uncertain event data. Such an extension enables input, output, and manipulation of uncertain data...

**Key Contributions:**
- Introduces a novel algorithmic approach

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models | Uncertain event data handling - Robust process mining techniques

---

### **Event Log Sampling for Predictive Monitoring**

- **Authors:** Mohammadreza Fani Sani, Mozhgan Vazifehdoostirani, Gyunam Park, Marco Pegoraro, Sebastiaan J. van Zelst, et al.
- **Year:** 2022
- **arXiv:** [2204.01470](https://arxiv.org/abs/2204.01470)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Predictive process monitoring is a subfield of process mining that aims to estimate case or event features for running process instances. Such predictions are of significant interest to the process stakeholders. However, state-of-the-art methods for predictive monitoring require the training of complex machine learning models, which is often inefficient. This paper proposes an instance selection procedure that allows sampling training process instances for prediction models. We show that our sampling method allows for a significant increase of training speed for next activity prediction method...

**Key Contributions:**
- Proposes a new algorithm or method
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **A Web-Based Tool for Comparative Process Mining**

- **Authors:** Madhavi Bangalore Shankara Narayana, Elisabetta Benevento, Marco Pegoraro, Muhammad Abdullah, Rahim Bin Shahid, et al.
- **Year:** 2022
- **arXiv:** [2204.00547](https://arxiv.org/abs/2204.00547)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques enable the analysis of a wide variety of processes using event data. Among the available process mining techniques, most consider a single process perspective at a time-in the shape of a model or log. In this paper, we have developed a tool that can compare and visualize the same process under different constraints, allowing to analyze multiple aspects of the process. We describe the architecture, structure and use of the tool, and we provide an open-source full implementation.

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Analyzing Process-Aware Information System Updates Using Digital Twins of Organizations**

- **Authors:** Gyunam Park, Marco Comuzzi, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2203.12969](https://arxiv.org/abs/2203.12969)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Digital transformation often entails small-scale changes to information systems supporting the execution of business processes. These changes may increase the operational frictions in process execution, which decreases the process performance. The contributions in the literature providing support to the tracking and impact analysis of small-scale changes are limited in scope and functionality. In this paper, we use the recently developed Digital Twins of Organizations (DTOs) to assess the impact of (process-aware) information systems updates. More in detail, we model the updates using the conf...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **How to Write Beautiful Process-and-Data-Science Papers?**

- **Authors:** Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2203.09286](https://arxiv.org/abs/2203.09286)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** After 25 years of PhD supervision, the author noted typical recurring problems that make papers look sloppy, difficult to read, and incoherent. The goal is not to write a paper for the sake of writing a paper, but to convey a valuable message that is clear and precise. The goal is to write papers that have an impact and are still understandable a couple of decades later. Our mission should be to create papers of high quality that people want to read and that can stand the test of time. We use Dijkstra's adagium "Beauty Is Our Business" to stress the importance of simplicity, correctness, and c...

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **A Python Tool for Object-Centric Process Mining Comparison**

- **Authors:** Anahita Farhang Ghahfarokhi, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2202.05709](https://arxiv.org/abs/2202.05709)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Object-centric process mining provides a more holistic view of processes where we analyze processes with multiple case notions. However, most object-centric process mining techniques consider the whole event log rather than the comparison of existing behaviors in the log. In this paper, we introduce a stand-alone object-centric process cube tool built on the PM4PY-MDL process mining framework. Our infrastructure uses both object and event attributes to build the process cube which leads to different types of materialization. Furthermore, our tool is equipped with the state of the art object-ce...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Provides a practical tool implementation
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---

### **A Scalable Database for the Storage of Object-Centric Event Logs**

- **Authors:** Alessandro Berti, Anahita Farhang Ghahfarokhi, Gyunam Park, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2202.05639](https://arxiv.org/abs/2202.05639)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Object-centric process mining provides a set of techniques for the analysis of event data where events are associated to several objects. To store Object-centric Event Logs (OCELs), the JSON-OCEL and JSON-XML formats have been recently proposed. However, the proposed implementations of the OCEL are file-based. This means that the entire file needs to be parsed in order to apply process mining techniques, such as the discovery of object-centric process models. In this paper, we propose a database storage for the OCEL format using the MongoDB document database. Since documents in MongoDB are equ...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows

---

### **Analyzing Medical Data with Process Mining: a COVID-19 Case Study**

- **Authors:** Marco Pegoraro, Madhavi Bangalore Shankara Narayana, Elisabetta Benevento, Wil M. P. van der Aalst, Lukas Martin, et al.
- **Year:** 2022
- **arXiv:** [2202.04625](https://arxiv.org/abs/2202.04625)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The recent increase in the availability of medical data, possible through automation and digitization of medical equipment, has enabled more accurate and complete analysis on patients' medical data through many branches of data science. In particular, medical records that include timestamps showing the history of a patient have enabled the representation of medical information as sequences of events, effectively allowing to perform process mining analyses. In this paper, we will present some preliminary findings obtained with established process mining techniques in regard of the medical data ...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Interactive Process Improvement using Simulation of Enriched Process Trees**

- **Authors:** Mahsa Pourbafrani, Wil M. P. van der Aalst
- **Year:** 2022
- **arXiv:** [2201.07755](https://arxiv.org/abs/2201.07755)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Event data provide the main source of information for analyzing and improving processes in organizations. Process mining techniques capture the state of running processes w.r.t. various aspects, such as activity-flow and performance metrics. The next step for process owners is to take the provided insights and turn them into actions in order to improve their processes. These actions may be taken in different aspects of a process. However, simply being aware of the process aspects that need to be improved as well as potential actions is insufficient. The key step in between is to assess the out...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Provides a practical tool implementation
- Comprehensive survey of the domain

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **How to Write Beautiful Process and Data Science Papers**

- **Authors:** van der Aalst
- **Year:** 2022
- **File:** `van_der_Aalst_2022_How_to_Write_Beautiful_Process_and_Data_Science_Papers.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2021

### **Precision and Fitness in Object-Centric Process Mining**

- **Authors:** Jan Niklas Adams, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2110.05375](https://arxiv.org/abs/2110.05375)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Traditional process mining considers only one single case notion and discovers and analyzes models based on this. However, a single case notion is often not a realistic assumption in practice. Multiple case notions might interact and influence each other in a process. Object-centric process mining introduces the techniques and concepts to handle multiple case notions. So far, such event logs have been standardized and novel process model discovery techniques were proposed. However, notions for evaluating the quality of a model are missing. These are necessary to enable future research on impro...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs
- Addresses object-centric process mining challenges

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Object-centric process mining - Advanced feature for multi-object workflows | Conformance checking and alignments - Validates workflow execution against models

---

### **Trustworthy Artificial Intelligence and Process Mining: Challenges and Opportunities**

- **Authors:** Andrew Pery, Majid Rafiei, Michael Simon, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2110.02707](https://arxiv.org/abs/2110.02707)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The premise of this paper is that compliance with Trustworthy AI governance best practices and regulatory frameworks is an inherently fragmented process spanning across diverse organizational units, external stakeholders, and systems of record, resulting in process uncertainties and in compliance gaps that may expose organizations to reputational and regulatory risks. Moreover, there are complexities associated with meeting the specific dimensions of Trustworthy AI best practices such as data governance, conformance testing, quality assurance of AI model behaviors, transparency, accountability...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Addresses privacy preservation in event data

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models | Privacy preservation - Important for event data handling | Uncertain event data handling - Robust process mining techniques

---

### **Visualizing Trace Variants From Partially Ordered Event Data**

- **Authors:** Daniel Schuster, Lukas Schade, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2110.02060](https://arxiv.org/abs/2110.02060)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Executing operational processes generates event data, which contain information on the executed process activities. Process mining techniques allow to systematically analyze event data to gain insights that are then used to optimize processes. Visual analytics for event data are essential for the application of process mining. Visualizing unique process executions -- also called trace variants, i.e., unique sequences of executed process activities -- is a common technique implemented in many scientific and industrial process mining applications. Most existing visualizations assume a total orde...

**Key Contributions:**
- Proposes a new algorithm or method
- Comprehensive survey of the domain

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Probability Estimation of Uncertain Process Trace Realizations**

- **Authors:** Marco Pegoraro, Bianka Bakullari, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2108.08615](https://arxiv.org/abs/2108.08615)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining is a scientific discipline that analyzes event data, often collected in databases called event logs. Recently, uncertain event logs have become of interest, which contain non-deterministic and stochastic event attributes that may represent many possible real-life scenarios. In this paper, we present a method to reliably estimate the probability of each of such scenarios, allowing their analysis. Experiments show that the probabilities calculated with our method closely match the true chances of occurrence of specific outcomes, enabling more trustworthy analyses on uncertain data...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Uncertain event data handling - Robust process mining techniques

---

### **SIMPT: Process Improvement Using Interactive Simulation of Time-aware Process Trees**

- **Authors:** Mahsa Pourbafrani, Shuai Jiao, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2108.02052](https://arxiv.org/abs/2108.02052)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques including process discovery, conformance checking, and process enhancement provide extensive knowledge about processes. Discovering running processes and deviations as well as detecting performance problems and bottlenecks are well-supported by process mining tools. However, all the provided techniques represent the past/current state of the process. The improvement in a process requires insights into the future states of the process w.r.t. the possible actions/changes. In this paper, we present a new tool that enables process owners to extract all the process aspects...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Freezing Sub-Models During Incremental Process Discovery: Extended Version**

- **Authors:** Daniel Schuster, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2108.00215](https://arxiv.org/abs/2108.00215)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process discovery aims to learn a process model from observed process behavior. From a user's perspective, most discovery algorithms work like a black box. Besides parameter tuning, there is no interaction between the user and the algorithm. Interactive process discovery allows the user to exploit domain knowledge and to guide the discovery process. Previously, an incremental discovery approach has been introduced where a model, considered to be under construction, gets incrementally extended by user-selected process behavior. This paper introduces a novel approach that additionally allows the...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Stream processing - Relevant for real-time event processing in CRE

---

### **PC4PM: A Tool for Privacy/Confidentiality Preservation in Process Mining**

- **Authors:** Majid Rafiei, Alexander Schnitzler, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2107.14499](https://arxiv.org/abs/2107.14499)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining enables business owners to discover and analyze their actual processes using event data that are widely available in information systems. Event data contain detailed information which is incredibly valuable for providing insights. However, such detailed data often include highly confidential and private information. Thus, concerns of privacy and confidentiality in process mining are becoming increasingly relevant and new techniques are being introduced. To make the techniques easily accessible, new tools need to be developed to integrate the introduced techniques and direct user...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **May I Take Your Order? On the Interplay Between Time and Order in Process Mining**

- **Authors:** Wil M. P. van der Aalst, Luis Santos
- **Year:** 2021
- **arXiv:** [2107.03937](https://arxiv.org/abs/2107.03937)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining starts from event data. The ordering of events is vital for the discovery of process models. However, the timestamps of events may be unreliable or imprecise. To further complicate matters, also causally unrelated events may be ordered in time. The fact that one event is followed by another does not imply that the former causes the latter. This paper explores the relationship between time and order. Moreover, it describes an approach to preprocess event data having timestamp-related problems. This approach avoids using accidental or unreliable orders and timestamps, creates part...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Uncertain event data handling - Robust process mining techniques

---

### **Reduction Using Induced Subnets To Systematically Prove Properties For Free-Choice Nets**

- **Authors:** Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2106.03658](https://arxiv.org/abs/2106.03658)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** We use sequences of t-induced T-nets and p-induced P-nets to convert free-choice nets into T-nets and P-nets while preserving properties such as well-formedness, liveness, lucency, pc-safety, and perpetuality. The approach is general and can be applied to different properties. This allows for more systematic proofs that "peel off" non-trivial parts while retaining the essence of the problem (e.g., lifting properties from T-net and P-net to free-choice nets).

**Key Contributions:**
- Proposes a new algorithm or method
- Provides theoretical foundations with proofs
- Comprehensive survey of the domain

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---

### **Free-Choice Nets With Home Clusters Are Lucent**

- **Authors:** Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2106.03554](https://arxiv.org/abs/2106.03554)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** A marked Petri net is lucent if there are no two different reachable markings enabling the same set of transitions, i.e., states are fully characterized by the transitions they enable. Characterizing the class of systems that are lucent is a foundational and also challenging question. However, little research has been done on the topic. In this paper, it is shown that all free-choice nets having a home cluster are lucent. These nets have a so-called home marking such that it is always possible to reach this marking again. Such a home marking can serve as a regeneration point or as an end-point...

**Key Contributions:**
- Introduces a novel algorithmic approach

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---

### **A Framework for Explainable Concept Drift Detection in Process Mining**

- **Authors:** Jan Niklas Adams, Sebastiaan J. van Zelst, Lara Quack, Kathrin Hausmann, Wil M. P. van der Aalst, et al.
- **Year:** 2021
- **arXiv:** [2105.13155](https://arxiv.org/abs/2105.13155)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Rapidly changing business environments expose companies to high levels of uncertainty. This uncertainty manifests itself in significant changes that tend to occur over the lifetime of a process and possibly affect its performance. It is important to understand the root causes of such changes since this allows us to react to change or anticipate future changes. Research in process mining has so far only focused on detecting, locating and characterizing significant changes in a process and not on finding root causes of such changes. In this paper, we aim to close this gap. We propose a framework...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | Uncertain event data handling - Robust process mining techniques

---

### **Privacy-Preserving Continuous Event Data Publishing**

- **Authors:** Majid Rafiei, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2105.11991](https://arxiv.org/abs/2105.11991)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining enables organizations to discover and analyze their actual processes using event data. Event data can be extracted from any information system supporting operational processes, e.g., SAP. Whereas the data inside such systems is protected using access control mechanisms, the extracted event data contain sensitive information that needs to be protected. This creates a new risk and a possible inhibitor for applying process mining. Therefore, privacy issues in process mining become increasingly important. Several privacy preservation techniques have been introduced to mitigate possi...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **Group-Based Privacy Preservation Techniques for Process Mining**

- **Authors:** Majid Rafiei, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2105.11983](https://arxiv.org/abs/2105.11983)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques help to improve processes using event data. Such data are widely available in information systems. However, they often contain highly sensitive information. For example, healthcare information systems record event data that can be utilized by process mining techniques to improve the treatment process, reduce patient's waiting times, improve resource productivity, etc. However, the recorded event data include highly sensitive information related to treatment activities. Responsible process mining should provide insights about the underlying processes, yet, at the same ...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides theoretical foundations with proofs
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **Cortado---An Interactive Tool for Data-Driven Process Discovery and Modeling**

- **Authors:** Daniel Schuster, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2105.07666](https://arxiv.org/abs/2105.07666)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining aims to diagnose and improve operational processes. Process mining techniques allow analyzing the event data generated and recorded during the execution of (business) processes to gain valuable insights. Process discovery is a key discipline in process mining that comprises the discovery of process models on the basis of the recorded event data. Most process discovery algorithms work in a fully automated fashion. Apart from adjusting their configuration parameters, conventional process discovery algorithms offer limited to no user interaction, i.e., we either edit the discovered...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Stream processing - Relevant for real-time event processing in CRE

---

### **Text-Aware Predictive Monitoring of Business Processes**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, David Benedikt Georgi, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2104.09962](https://arxiv.org/abs/2104.09962)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The real-time prediction of business processes using historical event data is an important capability of modern business process monitoring systems. Existing process prediction methods are able to also exploit the data perspective of recorded events, in addition to the control-flow perspective. However, while well-structured numerical or categorical attributes are considered in many prediction techniques, almost no technique is able to utilize text documents written in natural language, which can hold information critical to the prediction task. In this paper, we illustrate the design, impleme...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | Stream processing - Relevant for real-time event processing in CRE

---

### **Model Independent Error Bound Estimation for Conformance Checking Approximation**

- **Authors:** Mohammadreza Fani Sani, Martin Kabierski, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2103.13315](https://arxiv.org/abs/2103.13315)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Conformance checking techniques allow us to quantify the correspondence of a process's execution, captured in event data, w.r.t., a reference process model. In this context, alignments have proven to be useful for calculating conformance statistics. However, for extensive event data and complex process models, the computation time of alignments is considerably high, hampering their practical use. Simultaneously, it suffices to approximate either alignments or their corresponding conformance value(s) for many applications. Recent work has shown that using subsets of the process model behavior l...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models

---

### **Process Comparison Using Object-Centric Process Cubes**

- **Authors:** Anahita Farhang Ghahfarokhi, Alessandro Berti, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2103.07184](https://arxiv.org/abs/2103.07184)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining provides ways to analyze business processes. Common process mining techniques consider the process as a whole. However, in real-life business processes, different behaviors exist that make the overall process too complex to interpret. Process comparison is a branch of process mining that isolates different behaviors of the process from each other by using process cubes. Process cubes organize event data using different dimensions. Each cell contains a set of events that can be used as an input to apply process mining techniques. Existing work on process cubes assume single case ...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Addresses object-centric process mining challenges

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Object-centric process mining - Advanced feature for multi-object workflows

---

### **PROVED: A Tool for Graph Representation and Analysis of Uncertain Event Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2103.05564](https://arxiv.org/abs/2103.05564)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The discipline of process mining aims to study processes in a data-driven manner by analyzing historical process executions, often employing Petri nets. Event data, extracted from information systems (e.g. SAP), serve as the starting point for process mining. Recently, novel types of event data have gathered interest among the process mining community, including uncertain event data. Uncertain events, process traces and logs contain attributes that are characterized by quantified imprecisions, e.g., a set of possible attribute values. The PROVED tool helps to explore, navigate and analyze such...

**Key Contributions:**
- Provides a practical tool implementation

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Conformance checking and alignments - Validates workflow execution against models | Uncertain event data handling - Robust process mining techniques

---

### **Inferring Unobserved Events in Systems With Shared Resources and Queues**

- **Authors:** Dirk Fahland, Vadim Denisov, Wil. M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2103.00167](https://arxiv.org/abs/2103.00167)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** To identify the causes of performance problems or to predict process behavior, it is essential to have correct and complete event data. This is particularly important for distributed systems with shared resources, e.g., one case can block another case competing for the same machine, leading to inter-case dependencies in performance. However, due to a variety of reasons, real-life systems often record only a subset of all events taking place. To understand and analyze the behavior and performance of processes with shared resources, we aim to reconstruct bounds for timestamps of events in a case...

**Key Contributions:**
- Proposes a new algorithm or method
- Comprehensive survey of the domain

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **A Python Extension to Simulate Petri nets in Process Mining**

- **Authors:** M. Pourbafrani, Sandhya Vasudevan, Faizan Zafar, Yuan Xingran, Ravikumar Singh, et al.
- **Year:** 2021
- **arXiv:** [2102.08774](https://arxiv.org/abs/2102.08774)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The capability of process mining techniques in providing extensive knowledge and insights into business processes has been widely acknowledged. Process mining techniques support discovering process models as well as analyzing process performance and bottlenecks in the past executions of processes. However, process mining tends to be "backward-looking" rather than "forward-looking" techniques like simulation. For example, process improvement also requires "what-if" analyses. In this paper, we present a Python library that uses an event log to directly generate a simulated event log, with additi...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Privacy-Preserving Data Publishing in Process Mining**

- **Authors:** Majid Rafiei, Wil M. P. van der Aalst
- **Year:** 2021
- **arXiv:** [2101.02627](https://arxiv.org/abs/2101.02627)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining aims to provide insights into the actual processes based on event data. These data are often recorded by information systems and are widely available. However, they often contain sensitive private information that should be analyzed responsibly. Therefore, privacy issues in process mining are recently receiving more attention. Privacy preservation techniques obviously need to modify the original data, yet, at the same time, they are supposed to preserve the data utility. Privacy-preserving transformations of the data may lead to incorrect or misleading analysis results. Hence, n...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides theoretical foundations with proofs
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **Precision and Fitness in Object-Centric Process Mining**

- **Authors:** van der Aalst
- **Year:** 2021
- **File:** `van_der_Aalst_2021_Precision_and_Fitness_in_Object_Centric_Process_Mining.pdf`

**Key Contributions:**
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Conformance checking and alignments - Validates workflow execution against models

---


## Papers from 2020

### **Towards Quantifying Privacy in Process Mining**

- **Authors:** Majid Rafiei, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2012.12031](https://arxiv.org/abs/2012.12031)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining employs event logs to provide insights into the actual processes. Event logs are recorded by information systems and contain valuable information helping organizations to improve their processes. However, these data also include highly sensitive private information which is a major concern when applying process mining. Therefore, privacy preservation in process mining is growing in importance, and new techniques are being introduced. The effectiveness of the proposed privacy preservation techniques needs to be evaluated. It is important to measure both sensitive data protection ...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Addresses privacy preservation in event data

**CRE Relevance:** Privacy preservation - Important for event data handling

---

### **OrgMining 2.0: A Novel Framework for Organizational Model Mining from Event Logs**

- **Authors:** Jing Yang, Chun Ouyang, Wil M. P. van der Aalst, Arthur H. M. ter Hofstede, Yang Yu
- **Year:** 2020
- **arXiv:** [2011.12445](https://arxiv.org/abs/2011.12445)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Providing appropriate structures around human resources can streamline operations and thus facilitate the competitiveness of an organization. To achieve this goal, modern organizations need to acquire an accurate and timely understanding of human resource grouping while faced with an ever-changing environment. The use of process mining offers a promising way to help address the need through utilizing event log data stored in information systems. By extracting knowledge about the actual behavior of resources participating in business processes from event logs, organizational models can be const...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models | Stream processing - Relevant for real-time event processing in CRE

---

### **Discovering Object-Centric Petri Nets**

- **Authors:** Wil M. P. van der Aalst, Alessandro Berti
- **Year:** 2020
- **arXiv:** [2010.02047](https://arxiv.org/abs/2010.02047)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Techniques to discover Petri nets from event data assume precisely one case identifier per event. These case identifiers are used to correlate events, and the resulting discovered Petri net aims to describe the life-cycle of individual cases. In reality, there is not one possible case notion, but multiple intertwined case notions. For example, events may refer to mixtures of orders, items, packages, customers, and products. A package may refer to multiple items, multiple products, one order, and one customer. Therefore, we need to assume that each event refers to a collection of objects, each ...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Addresses object-centric process mining challenges

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Object-centric process mining - Advanced feature for multi-object workflows

---

### **PMSD: Data-Driven Simulation Using System Dynamics and Process Mining**

- **Authors:** Mahsa Pourbafrani, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2010.00943](https://arxiv.org/abs/2010.00943)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining extends far beyond process discovery and conformance checking, and also provides techniques for bottleneck analysis and organizational mining. However, these techniques are mostly backward-looking. PMSD is a web application tool that supports forward-looking simulation techniques. It transforms the event data and process mining results into a simulation model which can be executed and validated. PMSD includes log transformation, time window selection, relation detection, interactive model generation, simulating and validating the models in the form of system dynamics, i.e., a te...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **Efficient Time and Space Representation of Uncertain Event Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2010.00334](https://arxiv.org/abs/2010.00334)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining is a discipline which concerns the analysis of execution data of operational processes, the extraction of models from event data, the measurement of the conformance between event data and normative models, and the enhancement of all aspects of processes. Most approaches assume that event data is accurately capture behavior. However, this is not realistic in many applications: data can contain uncertainty, generated from errors in recording, imprecise measurements, and other factors. Recently, new methods have been developed to analyze event data containing uncertainty; these tec...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | Uncertain event data handling - Robust process mining techniques

---

### **Conformance Checking over Uncertain Event Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2009.14452](https://arxiv.org/abs/2009.14452)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The strong impulse to digitize processes and operations in companies and enterprises have resulted in the creation and automatic recording of an increasingly large amount of process data in information systems. These are made available in the form of event logs. Process mining techniques enable the process-centric analysis of data, including automatically discovering process models and checking if event data conform to a given model. In this paper, we analyze the previously unexplored setting of uncertain event logs. In such event logs uncertainty is recorded explicitly, i.e., the time, activi...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models | Uncertain event data handling - Robust process mining techniques

---

### **Alignment Approximation for Process Trees**

- **Authors:** Daniel Schuster, Sebastiaan van Zelst, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2009.14094](https://arxiv.org/abs/2009.14094)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Comparing observed behavior (event data generated during process executions) with modeled behavior (process models), is an essential step in process mining analyses. Alignments are the de-facto standard technique for calculating conformance checking statistics. However, the calculation of alignments is computationally complex since a shortest path problem must be solved on a state space which grows non-linearly with the size of the model and the observed behavior, leading to the well-known state space explosion problem. In this paper, we present a novel framework to approximate alignments on p...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **Practical Aspect of Privacy-Preserving Data Publishing in Process Mining**

- **Authors:** Majid Rafiei, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2009.11542](https://arxiv.org/abs/2009.11542)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques such as process discovery and conformance checking provide insights into actual processes by analyzing event data that are widely available in information systems. These data are very valuable, but often contain sensitive information, and process analysts need to balance confidentiality and utility. Privacy issues in process mining are recently receiving more attention from researchers which should be complemented by a tool to integrate the solutions and make them available in the real world. In this paper, we introduce a Python-based infrastructure implementing state...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation
- Addresses privacy preservation in event data

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models | Privacy preservation - Important for event data handling

---

### **Efficient Construction of Behavior Graphs for Uncertain Event Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2020
- **arXiv:** [2002.08225](https://arxiv.org/abs/2002.08225)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The discipline of process mining deals with analyzing execution data of operational processes, extracting models from event data, checking the conformance between event data and normative models, and enhancing all aspects of processes. Recently, new techniques have been developed to analyze event data containing uncertainty; these techniques strongly rely on representing uncertain event data through graph-based models capturing uncertainty. In this paper we present a novel approach to efficiently compute a graph representation of the behavior contained in an uncertain process trace. We present...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets
- Provides theoretical foundations with proofs

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules | Uncertain event data handling - Robust process mining techniques

---

### **Discovering Object-Centric Petri Nets**

- **Authors:** van der Aalst
- **Year:** 2020
- **File:** `van_der_aalst_2020_discovering_object_centric_petri_nets.pdf`

**Key Contributions:**
- Addresses object-centric process mining challenges

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Object-centric process mining - Advanced feature for multi-object workflows

---

### **Conformance Checking in the Presence of Uncertain Event Data**

- **Authors:** van der Aalst
- **Year:** 2020
- **File:** `conformance_checking_uncertain_event_data_2020.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models | Uncertain event data handling - Robust process mining techniques

---


## Papers from 2019

### **Conformance Checking Approximation using Subset Selection and Edit Distance**

- **Authors:** Mohammadreza Fani Sani, Sebastiaan J. van Zelst, Wil M. P. van der Aalst
- **Year:** 2019
- **arXiv:** [1912.05022](https://arxiv.org/abs/1912.05022)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Conformance checking techniques let us find out to what degree a process model and real execution data correspond to each other. In recent years, alignments have proven extremely useful in calculating conformance statistics. Most techniques to compute alignments provide an exact solution. However, in many applications, it is enough to have an approximation of the conformance value. Specifically, for large event data, the computing time for alignments is considerably long using current techniques which makes them inapplicable in reality. Also, it is no longer feasible to use standard hardware f...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Mining Uncertain Event Data in Process Mining**

- **Authors:** Marco Pegoraro, Wil M. P. van der Aalst
- **Year:** 2019
- **arXiv:** [1910.00089](https://arxiv.org/abs/1910.00089)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Nowadays, more and more process data are automatically recorded by information systems, and made available in the form of event logs. Process mining techniques enable process-centric analysis of data, including automatically discovering process models and checking if event data conform to a certain model. In this paper we analyze the previously unexplored setting of uncertain event logs: logs where quantified uncertainty is recorded together with the corresponding data. We define a taxonomy of uncertain event logs and models, and we examine the challenges that uncertainty poses on process disc...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models | Uncertain event data handling - Robust process mining techniques

---

### **Discovering Process Models from Uncertain Event Data**

- **Authors:** Marco Pegoraro, Merih Seran Uysal, Wil M. P. van der Aalst
- **Year:** 2019
- **arXiv:** [1909.11567](https://arxiv.org/abs/1909.11567)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Modern information systems are able to collect event data in the form of event logs. Process mining techniques allow to discover a model from event data, to check the conformance of an event log against a reference model, and to perform further process-centric analyses. In this paper, we consider uncertain event logs, where data is recorded together with explicit uncertainty information. We describe a technique to discover a directly-follows graph from such event data which retains information about the uncertainty in the process. We then present experimental results of performing inductive mi...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models | Uncertain event data handling - Robust process mining techniques

---

### **Evaluating Conformance Measures in Process Mining using Conformance Propositions (Extended version)**

- **Authors:** Anja F. Syring, Niek Tax, Wil M. P. van der Aalst
- **Year:** 2019
- **arXiv:** [1909.02393](https://arxiv.org/abs/1909.02393)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining sheds new light on the relationship between process models and real-life processes. Process discovery can be used to learn process models from event logs. Conformance checking is concerned with quantifying the quality of a business process model in relation to event data that was logged during the execution of the business process. There exist different categories of conformance measures. Recall, also called fitness, is concerned with quantifying how much of the behavior that was observed in the event log fits the process model. Precision is concerned with quantifying how much b...

**Key Contributions:**
- Provides theoretical foundations with proofs
- Comprehensive survey of the domain

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **What if Process Predictions are not followed by Good Recommendations? (Technical Report)**

- **Authors:** Marcus Dees, Massimiliano de Leoni, Wil M. P. van der Aalst, Hajo A. Reijers
- **Year:** 2019
- **arXiv:** [1905.10173](https://arxiv.org/abs/1905.10173)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process-aware Recommender systems (PAR systems) are information systems that aim to monitor process executions, predict their outcome, and recommend effective interventions to reduce the risk of failure. This paper discusses monitoring, predicting, and recommending using a PAR system within a financial institute in the Netherlands to avoid faulty executions. While predictions were based on the analysis of historical data, the most opportune intervention was selected on the basis of human judgment and subjective opinions. The results showed that, while the predictions of risky cases were relati...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Includes empirical evaluation on real datasets

**CRE Relevance:** Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Fairness-Aware Process Mining**

- **Authors:** van der Aalst
- **Year:** 2019
- **File:** `van_der_aalst_2019_fairness_aware_process_mining.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Foundations of Process Discovery**

- **Authors:** van der Aalst
- **Year:** 2019
- **File:** `foundations_of_process_discovery_2019.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---


## Papers from 2018

### **An Integrated Framework for Process Discovery Algorithm Evaluation**

- **Authors:** Toon Jouck, Alfredo Bolt, Benoît Depaire, Massimiliano de Leoni, Wil M. P. van der Aalst
- **Year:** 2018
- **arXiv:** [1806.07222](https://arxiv.org/abs/1806.07222)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining offers techniques to exploit event data by providing insights and recommendations to improve business processes. The growing amount of algorithms for process discovery has raised the question of which algorithms perform best on a given event log. Current evaluation frameworks for empirically evaluating discovery techniques depend on the notation used (behavioral identical models may give different results) and cannot provide more general statements about populations of models. Therefore, this paper proposes a new integrated evaluation framework that uses a classification approac...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Markings in Perpetual Free-Choice Nets Are Fully Characterized by Their Enabled Transitions**

- **Authors:** Wil M. P. van der Aalst
- **Year:** 2018
- **arXiv:** [1801.04315](https://arxiv.org/abs/1801.04315)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** A marked Petri net is lucent if there are no two different reachable markings enabling the same set of transitions, i.e., states are fully characterized by the transitions they enable. This paper explores the class of marked Petri nets that are lucent and proves that perpetual marked free-choice nets are lucent. Perpetual free-choice nets are free-choice Petri nets that are live and bounded and have a home cluster, i.e., there is a cluster such that from any reachable state there is a reachable state marking the places of this cluster. A home cluster in a perpetual net serves as a "regeneratio...

**Key Contributions:**
- Introduces a novel algorithmic approach

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---


## Papers from 2017

### **Discovering More Precise Process Models from Event Logs by Filtering Out Chaotic Activities**

- **Authors:** Niek Tax, Natalia Sidorova, Wil M. P. van der Aalst
- **Year:** 2017
- **arXiv:** [1711.01287](https://arxiv.org/abs/1711.01287)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process Discovery is concerned with the automatic generation of a process model that describes a business process from execution data of that business process. Real life event logs can contain chaotic activities. These activities are independent of the state of the process and can, therefore, happen at rather arbitrary points in time. We show that the presence of such chaotic activities in an event log heavily impacts the quality of the process models that can be discovered with process discovery techniques. The current modus operandi for filtering activities from event logs is to simply filte...

**Key Contributions:**
- Introduces a novel algorithmic approach

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Recursion Aware Modeling and Discovery For Hierarchical Software Event Log Analysis (Extended)**

- **Authors:** Maikel Leemans, Wil M. P. van der Aalst, Mark G. J. van den Brand
- **Year:** 2017
- **arXiv:** [1710.09323](https://arxiv.org/abs/1710.09323)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** This extended paper presents 1) a novel hierarchy and recursion extension to the process tree model; and 2) the first, recursion aware process model discovery technique that leverages hierarchical information in event logs, typically available for software systems. This technique allows us to analyze the operational processes of software systems under real-life conditions at multiple levels of granularity. The work can be positioned in-between reverse engineering and process mining. An implementation of the proposed approach is available as a ProM plugin. Experimental results based on real-lif...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Guided Interaction Exploration in Artifact-centric Process Models**

- **Authors:** Maikel L. van Eck, Natalia Sidorova, Wil M. P. van der Aalst
- **Year:** 2017
- **arXiv:** [1706.02109](https://arxiv.org/abs/1706.02109)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Artifact-centric process models aim to describe complex processes as a collection of interacting artifacts. Recent development in process mining allow for the discovery of such models. However, the focus is often on the representation of the individual artifacts rather than their interactions. Based on event data we can automatically discover composite state machines representing artifact-centric processes. Moreover, we provide ways of visualizing and quantifying interactions among different artifacts. For example, we are able to highlight strongly correlated behaviours in different artifacts....

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Mining Process Model Descriptions of Daily Life through Event Abstraction**

- **Authors:** Niek Tax, Natalia Sidorova, Reinder Haakma, Wil M. P. van der Aalst
- **Year:** 2017
- **arXiv:** [1705.10202](https://arxiv.org/abs/1705.10202)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques focus on extracting insight in processes from event logs. Process mining has the potential to provide valuable insights in (un)healthy habits and to contribute to ambient assisted living solutions when applied on data from smart home environments. However, events recorded in smart home environments are on the level of sensor triggers, at which process discovery algorithms produce overgeneralizing process models that allow for too much behavior and that are difficult to interpret for human experts. We show that abstracting the events to a higher-level interpretation ca...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Generating Time-Based Label Refinements to Discover More Precise Process Models**

- **Authors:** Niek Tax, Emin Alasgarov, Natalia Sidorova, Wil M. P. van der Aalst, Reinder Haakma
- **Year:** 2017
- **arXiv:** [1705.09359](https://arxiv.org/abs/1705.09359)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining is a research field focused on the analysis of event data with the aim of extracting insights related to dynamic behavior. Applying process mining techniques on data from smart home environments has the potential to provide valuable insights in (un)healthy habits and to contribute to ambient assisted living solutions. Finding the right event labels to enable the application of process mining techniques is however far from trivial, as simply using the triggering sensor as the label for sensor events results in uninformative models that allow for too much behavior (overgeneralizin...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---

### **The Imprecisions of Precision Measures in Process Mining**

- **Authors:** Niek Tax, Xixi Lu, Natalia Sidorova, Dirk Fahland, Wil M. P. van der Aalst
- **Year:** 2017
- **arXiv:** [1705.03303](https://arxiv.org/abs/1705.03303)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In process mining, precision measures are used to quantify how much a process model overapproximates the behavior seen in an event log. Although several measures have been proposed throughout the years, no research has been done to validate whether these measures achieve the intended aim of quantifying over-approximation in a consistent way for all models and logs. This paper fills this gap by postulating a number of axioms for quantifying precision consistently for any log and any model. Further, we show through counter-examples that none of the existing measures consistently quantifies preci...

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models

---

### **Event Stream-Based Process Discovery using Abstract Representations**

- **Authors:** Sebastiaan J. van Zelst, Boudewijn F. van Dongen, Wil M. P. van der Aalst
- **Year:** 2017
- **arXiv:** [1704.08101](https://arxiv.org/abs/1704.08101)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The aim of process discovery, originating from the area of process mining, is to discover a process model based on business process execution data. A majority of process discovery techniques relies on an event log as an input. An event log is a static source of historical data capturing the execution of a business process. In this paper we focus on process discovery relying on online streams of business process execution events. Learning process models from event streams poses both challenges and opportunities, i.e. we need to handle unlimited amounts of data using finite memory and, preferabl...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Stream processing - Relevant for real-time event processing in CRE

---

### **Interest-Driven Discovery of Local Process Models**

- **Authors:** Niek Tax, Benjamin Dalmas, Natalia Sidorova, Wil M P van der Aalst, Sylvie Norre
- **Year:** 2017
- **arXiv:** [1703.07116](https://arxiv.org/abs/1703.07116)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Local Process Models (LPM) describe structured fragments of process behavior occurring in the context of less structured business processes. Traditional LPM discovery aims to generate a collection of process models that describe highly frequent behavior, but these models do not always provide useful answers for questions posed by process analysts aiming at business process improvement. We propose a framework for goal-driven LPM discovery, based on utility functions and constraints. We describe four scopes on which these utility functions and constrains can be defined, and show that utility fun...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Discovering Relaxed Sound Workflow Nets using Integer Linear Programming**

- **Authors:** S. J. van Zelst, B. F. van Dongen, W. M. P. van der Aalst, H. M. W. Verbeek
- **Year:** 2017
- **arXiv:** [1703.06733](https://arxiv.org/abs/1703.06733)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining is concerned with the analysis, understanding and improvement of business processes. Process discovery, i.e. discovering a process model based on an event log, is considered the most challenging process mining task. State-of-the-art process discovery algorithms only discover local control-flow patterns and are unable to discover complex, non-local patterns. Region theory based techniques, i.e. an established class of process discovery techniques, do allow for discovering such patterns. However, applying region theory directly results in complex, over-fitting models, which is les...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation
- Includes empirical evaluation on real datasets

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis

---

### **Learning Hybrid Process Models From Events: Process Discovery Without Faking Confidence**

- **Authors:** Wil M. P. van der Aalst, Riccardo De Masellis, Chiara Di Francescomarino, Chiara Ghidini
- **Year:** 2017
- **arXiv:** [1703.06125](https://arxiv.org/abs/1703.06125)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process discovery techniques return process models that are either formal (precisely describing the possible behaviors) or informal (merely a "picture" not allowing for any form of formal reasoning). Formal models are able to classify traces (i.e., sequences of events) as fitting or non-fitting. Most process mining approaches described in the literature produce such models. This is in stark contrast with the over 25 available commercial process mining tools that only discover informal process models that remain deliberately vague on the precise set of possible traces. There are two main reason...

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides a practical tool implementation
- Provides theoretical foundations with proofs

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Process discovery algorithms - Relevant for workflow pattern analysis | Stream processing - Relevant for real-time event processing in CRE

---

### **Object-Centric Behavioral Constraints**

- **Authors:** Wil M. P. van der Aalst, Guangming Li, Marco Montali
- **Year:** 2017
- **arXiv:** [1703.05740](https://arxiv.org/abs/1703.05740)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Today's process modeling languages often force the analyst or modeler to straightjacket real-life processes into simplistic or incomplete models that fail to capture the essential features of the domain under study. Conventional business process models only describe the lifecycles of individual instances (cases) in isolation. Although process models may include data elements (cf. BPMN), explicit connections to real data models (e.g., an entity relationship model or a UML class model) are rarely made. Therefore, we propose a novel approach that extends data models with a behavioral perspective....

**Key Contributions:**
- Introduces a novel algorithmic approach
- Provides theoretical foundations with proofs
- Addresses object-centric process mining challenges

**CRE Relevance:** Object-centric process mining - Advanced feature for multi-object workflows | Conformance checking and alignments - Validates workflow execution against models

---

### **RapidProM: Mine Your Processes and Not Just Your Data**

- **Authors:** Wil M. P. van der Aalst, Alfredo Bolt, Sebastiaan J. van Zelst
- **Year:** 2017
- **arXiv:** [1703.03740](https://arxiv.org/abs/1703.03740)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** The number of events recorded for operational processes is growing every year. This applies to all domains: from health care and e-government to production and maintenance. Event data are a valuable source of information for organizations that need to meet requirements related to compliance, efficiency, and customer service. Process mining helps to turn these data into real value: by discovering the real processes, by automatically identifying bottlenecks, by analyzing deviations and sources of non-compliance, by revealing the actual behavior of people, etc. Process mining is very different fr...

**Key Contributions:**
- Proposes a new algorithm or method
- Provides a practical tool implementation

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation | Conformance checking and alignments - Validates workflow execution against models

---

### **RapidProM: Mine Your Processes and Not Just Your Data**

- **Authors:** van der Aalst
- **Year:** 2017
- **File:** `van_der_Aalst_2017_RapidProM_Mine_Your_Processes.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2016

### **Heuristic Approaches for Generating Local Process Models through Log Projections**

- **Authors:** Niek Tax, Natalia Sidorova, Wil M. P. van der Aalst, Reinder Haakma
- **Year:** 2016
- **arXiv:** [1610.02876](https://arxiv.org/abs/1610.02876)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Local Process Model (LPM) discovery is focused on the mining of a set of process models where each model describes the behavior represented in the event log only partially, i.e. subsets of possible events are taken into account to create so-called local process models. Often such smaller models provide valuable insights into the behavior of the process, especially when no adequate and comprehensible single overall process model exists that is able to describe the traces of the process from start to end. The practical application of LPM discovery is however hindered by computational issues in t...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Predictive monitoring and anomaly detection - Directly applies to CRE mining modules

---

### **Event Abstraction for Process Mining using Supervised Learning Techniques**

- **Authors:** Niek Tax, Natalia Sidorova, Reinder Haakma, Wil M. P. van der Aalst
- **Year:** 2016
- **arXiv:** [1606.07283](https://arxiv.org/abs/1606.07283)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques focus on extracting insight in processes from event logs. In many cases, events recorded in the event log are too fine-grained, causing process discovery algorithms to discover incomprehensible process models or process models that are not representative of the event log. We show that when process discovery algorithms are only able to discover an unrepresentative process model from a low-level event log, structure in the process can in some cases still be discovered by first abstracting the event log to a higher level of granularity. This gives rise to the challenge t...

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Conformance checking and alignments - Validates workflow execution against models

---

### **Log-based Evaluation of Label Splits for Process Models**

- **Authors:** Niek Tax, Natalia Sidorova, Reinder Haakma, Wil M. P. van der Aalst
- **Year:** 2016
- **arXiv:** [1606.07259](https://arxiv.org/abs/1606.07259)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** Process mining techniques aim to extract insights in processes from event logs. One of the challenges in process mining is identifying interesting and meaningful event labels that contribute to a better understanding of the process. Our application area is mining data from smart homes for elderly, where the ultimate goal is to signal deviations from usual behavior and provide timely recommendations in order to extend the period of independent living. Extracting individual process models showing user behavior is an important instrument in achieving this goal. However, the interpretation of sens...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets

**CRE Relevance:** Conformance checking and alignments - Validates workflow execution against models

---

### **Mining Local Process Models**

- **Authors:** Niek Tax, Natalia Sidorova, Reinder Haakma, Wil M. P. van der Aalst
- **Year:** 2016
- **arXiv:** [1606.06066](https://arxiv.org/abs/1606.06066)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** In this paper we describe a method to discover frequent behavioral patterns in event logs. We express these patterns as \emph{local process models}. Local process model mining can be positioned in-between process discovery and episode / sequential pattern mining. The technique presented in this paper is able to learn behavioral patterns involving sequential composition, concurrency, choice and loop, like in process mining. However, we do not look at start-to-end models, which distinguishes our approach from process discovery and creates a link to episode / sequential pattern mining. We propose...

**Key Contributions:**
- Proposes a new algorithm or method
- Includes empirical evaluation on real datasets
- Addresses object-centric process mining challenges

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Object-centric process mining - Advanced feature for multi-object workflows | Stream processing - Relevant for real-time event processing in CRE

---

### **Heuristic Approaches for Generating Local Process Models**

- **Authors:** van der Aalst
- **Year:** 2016
- **File:** `van_der_Aalst_2016_Heuristic_Approaches_for_Generating_Local_Process_Models.pdf`

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis

---


## Papers from 2013

### **Business Process Management: A Comprehensive Survey**

- **Authors:** van der Aalst
- **Year:** 2013
- **File:** `business_process_management_survey_2013.pdf`

**Key Contributions:**
- Comprehensive survey of the domain

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Business Process Simulation: A Survey**

- **Authors:** van der Aalst
- **Year:** 2013
- **File:** `business_process_simulation_2013.pdf`

**Key Contributions:**
- Comprehensive survey of the domain

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2012

### **Heuristics Miners for Streaming Event Data**

- **Authors:** Andrea Burattin, Alessandro Sperduti, Wil M. P. van der Aalst
- **Year:** 2012
- **arXiv:** [1212.6383](https://arxiv.org/abs/1212.6383)

**Problem Addressed:** This paper addresses key challenges in process mining and workflow management.

**Abstract:** More and more business activities are performed using information systems. These systems produce such huge amounts of event data that existing systems are unable to store and process them. Moreover, few processes are in steady-state and due to changing circumstances processes evolve and systems need to adapt continuously. Since conventional process discovery algorithms have been defined for batch processing, it is difficult to apply them in such evolving environments. Existing algorithms cannot cope with streaming event data and tend to generate unreliable and obsolete results.   In this paper...

**Key Contributions:**
- Proposes a new algorithm or method
- Presents a comprehensive framework
- Includes empirical evaluation on real datasets

**CRE Relevance:** Process discovery algorithms - Relevant for workflow pattern analysis | Stream processing - Relevant for real-time event processing in CRE

---

### **Process Mining Tutorial**

- **Authors:** van der Aalst
- **Year:** 2012
- **File:** `process_mining_tutorial_2012.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2011

### **Process Mining Manifesto**

- **Authors:** van der Aalst, et al.
- **Year:** 2011
- **File:** `process_mining_manifesto_2011.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Process Mining Manifesto (TUe Version)**

- **Authors:** van der Aalst, et al.
- **Year:** 2011
- **File:** `process_mining_manifesto_tue_2011.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Process Mining: Overview and Opportunities**

- **Authors:** van der Aalst
- **Year:** 2011
- **File:** `process_mining_overview_and_opportunities_2011.pdf`

**Key Contributions:**
- Comprehensive survey of the domain

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Soundness of Workflow Nets**

- **Authors:** van der Aalst
- **Year:** 2011
- **File:** `soundness_of_workflow_nets_2011.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---

### **Repairing Process Models**

- **Authors:** van der Aalst
- **Year:** 2011
- **File:** `repairing_process_models_2011.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2010

### **A Decade of BPM Conferences**

- **Authors:** van der Aalst
- **Year:** 2010
- **File:** `decade_bpm_conferences_2010.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2009

### **Supporting the Full BPM Lifecycle**

- **Authors:** van der Aalst
- **Year:** 2009
- **File:** `supporting_full_bpm_lifecycle_2009.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2008

### **Workflow Exception Patterns**

- **Authors:** Russell, van der Aalst, et al.
- **Year:** 2008
- **File:** `workflow_exception_patterns_2008.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2007

### **Business Process Mining: An Industrial Application**

- **Authors:** van der Aalst
- **Year:** 2007
- **File:** `business_process_mining_industrial_application_2007.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2005

### **YAWL: Yet Another Workflow Language**

- **Authors:** van der Aalst, ter Hofstede
- **Year:** 2005
- **File:** `yawl_yet_another_workflow_language_2005.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Process Mining in Case Handling**

- **Authors:** van der Aalst
- **Year:** 2005
- **File:** `process_mining_case_handling_2005.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2004

### **YAWL: Design and Implementation**

- **Authors:** van der Aalst, ter Hofstede
- **Year:** 2004
- **File:** `yawl_design_implementation_2004.pdf`

**Key Contributions:**
- Provides a practical tool implementation

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Tutorial: Models, Systems, and Standards for Workflow**

- **Authors:** van der Aalst
- **Year:** 2004
- **File:** `tutorial_models_systems_standards_workflow_2004.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2003

### **Workflow Patterns**

- **Authors:** van der Aalst, ter Hofstede, et al.
- **Year:** 2003
- **File:** `workflow_patterns_2003.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Discovering Process Models from Empirical Data**

- **Authors:** van der Aalst
- **Year:** 2003
- **File:** `discovering_process_models_empirical_data_2003.pdf`

**Key Contributions:**
- Includes empirical evaluation on real datasets

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Process Mining: A Research Agenda**

- **Authors:** van der Aalst
- **Year:** 2003
- **File:** `process_mining_research_agenda_2003.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2001

### **Workflow Mining: Discovering Process Models from Event Logs**

- **Authors:** van der Aalst
- **Year:** 2001
- **File:** `workflow_mining_discovering_process_models_2001.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Process Mining: Discovering Workflow Models from Event Logs**

- **Authors:** van der Aalst
- **Year:** 2001
- **File:** `process_mining_discovering_workflow_models_2001.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Rediscovering Workflow Models from Event-Based Data**

- **Authors:** van der Aalst
- **Year:** 2001
- **File:** `rediscovering_workflow_models_2001.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Diagnosing Workflow Processes Using Woflan**

- **Authors:** van der Aalst
- **Year:** 2001
- **File:** `diagnosing_workflow_processes_woflan_2001.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 2000

### **Fundamentals of Control Flow in Workflows**

- **Authors:** van der Aalst
- **Year:** 2000
- **File:** `fundamentals_of_control_flow_in_workflows_2000.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **A P2P Approach to Interorganizational Workflows**

- **Authors:** van der Aalst
- **Year:** 2000
- **File:** `p2p_approach_interorganizational_workflows_2000.pdf`

**Key Contributions:**
- Proposes a new algorithm or method

**CRE Relevance:** General process mining reference - Foundational knowledge

---

### **Verification of Business Processes Using Petri Nets**

- **Authors:** van der Aalst
- **Year:** 2000
- **File:** `verification_business_processes_petri_nets_2000.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---

### **Workflow Modeling Using Proclets**

- **Authors:** van der Aalst
- **Year:** 2000
- **File:** `workflow_modeling_using_proclets_2000.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** General process mining reference - Foundational knowledge

---


## Papers from 1998

### **Application of Petri Nets to Workflow Management**

- **Authors:** van der Aalst
- **Year:** 1998
- **File:** `application_petri_nets_workflow_management_1998.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---


## Papers from 1997

### **Verification of Workflow Nets**

- **Authors:** van der Aalst
- **Year:** 1997
- **File:** `verification_of_workflow_nets_1997.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---


## Papers from 1996

### **Structural Characterizations of Sound Workflow Nets**

- **Authors:** van der Aalst
- **Year:** 1996
- **File:** `structural_characterizations_sound_workflow_nets_1996.pdf`

**Key Contributions:**
- Addresses key challenges in the domain
- Demonstrates practical applicability

**CRE Relevance:** Petri net theory and soundness verification - Core to gen_pnet implementation

---


---

## Index by Year

- **Unknown:** 5 papers
- **2026:** 1 papers
- **2025:** 12 papers
- **2024:** 12 papers
- **2023:** 16 papers
- **2022:** 29 papers
- **2021:** 22 papers
- **2020:** 11 papers
- **2019:** 7 papers
- **2018:** 2 papers
- **2017:** 13 papers
- **2016:** 5 papers
- **2013:** 2 papers
- **2012:** 2 papers
- **2011:** 5 papers
- **2010:** 1 papers
- **2009:** 1 papers
- **2008:** 1 papers
- **2007:** 1 papers
- **2005:** 2 papers
- **2004:** 2 papers
- **2003:** 3 papers
- **2001:** 4 papers
- **2000:** 4 papers
- **1998:** 1 papers
- **1997:** 1 papers
- **1996:** 1 papers
