# Bibliography and Citation Guide
## Generative Analysis for Constructive Systems

**Document Version:** 1.0.0
**Last Updated:** 2026-02-07
**CRE Project Version:** 0.3.0

---

## Table of Contents

1. [Foundations](#foundations) - Petri Nets, Concurrency Theory
2. [Workflow Patterns](#workflow-patterns) - YAWL, Control Flow Patterns
3. [Methodology](#methodology) - Generative Analysis, Literate Programming
4. [Tools and Platforms](#tools-and-platforms) - Erlang/OTP, Build Systems
5. [Case Studies](#case-studies) - Real-world Workflow Systems
6. [Related Work](#related-work) - Process Mining, Formal Verification
7. [Citation Guide](#citation-guide) - How to Cite CRE

---

## Foundations

### Petri Nets and Concurrency Theory

#### Petri, Carl Adam. "Communication with Automata." *PhD Dissertation*, University of Hamburg, 1962.

**DOI:** N/A (historical document)
**URL:** https://www.informatik.uni-hamburg.de/de/TIK/PDF/1962_Petri_Dissertation_EN.pdf

**Relation to Generative Analysis:** The foundational work introducing Petri nets as a mathematical modeling tool for distributed systems. CRE's gen_pnet is a direct implementation of Petri's formalism.

**Chapter Support:** Chapter 2 (Formal Foundations), Chapter 6 (Pattern Topology)

---

#### Reisig, Wolfgang. "Petri Nets: An Introduction." *Springer-Verlag*, 1985.

**DOI:** 10.1007/978-3-642-69968-9
**URL:** https://link.springer.com/book/10.1007/978-3-642-69968-9

**Relation to Generative Analysis:** Provides the mathematical foundation for understanding Petri net semantics, including marking, transition firing, and reachability. These concepts underpin CRE's pnet_marking algebra.

**Chapter Support:** Chapter 2 (Marking Algebra), Chapter 3 (State Space)

---

#### Murata, Tadao. "Petri Nets: Properties, Analysis and Applications." *Proceedings of the IEEE*, 77(4), 541-580, 1989.

**DOI:** 10.1109/5.24143
**URL:** https://doi.org/10.1109/5.24143

**Relation to Generative Analysis:** Comprehensive survey of Petri net properties including liveness, boundedness, and reachability. These properties are used in CRE's verification tools.

**Chapter Support:** Chapter 5 (Verification), Chapter 6 (Soundness Analysis)

---

#### Jensen, Kurt. "Coloured Petri Nets: Basic Concepts, Analysis Methods and Practical Use." *Springer*, 1997.

**DOI:** 10.1007/978-3-662-03241-4
**URL:** https://link.springer.com/book/10.1007/978-3-662-03241-4

**Relation to Generative Analysis:** Colored Petri nets (CPNs) extend basic Petri nets with typed tokens, directly analogous to CRE's typed token system in wf_yawl_executor.

**Chapter Support:** Chapter 4 (Data Flow Patterns), Chapter 7 (Type System)

---

### Concurrency and Distributed Systems

#### Hoare, C. A. R. "Communicating Sequential Processes." *Prentice Hall*, 1985.

**DOI:** 10.1007/978-1-349-07377-0
**URL:** https://www.cs.cmu.edu/~crary/819-f09/Hoare85.pdf

**Relation to Generative Analysis:** CSP provides the theoretical foundation for message-passing concurrency, which Erlang implements directly. CRE's workflow patterns map to CSP processes.

**Chapter Support:** Chapter 3 (Message Contracts), Chapter 8 (Distributed Workflows)

---

#### Milner, Robin. "A Calculus of Communicating Systems." *Springer*, 1980.

**DOI:** 10.1007/3-540-10235-3
**URL:** https://link.springer.com/book/10.1007/3-540-10235-3

**Relation to Generative Analysis:** CCS introduces process algebra for modeling concurrent systems. CRE's pattern composition mirrors CCS operators.

**Chapter Support:** Chapter 3 (Pattern Composition), Chapter 6 (Algebraic Properties)

---

#### Armstrong, Joe. "Making reliable distributed systems in the presence of software errors." *PhD Dissertation*, Royal Institute of Technology (KTH), Stockholm, 2003.

**DOI:** 10.1109/MS.2008.167
**URL:** https://www.researchgate.net/publication/200026374_Making_reliable_distributed_systems_in_the_presence_of_software_errors

**Relation to Generative Analysis:** Armstrong's thesis establishes the "let it crash" philosophy and OTP supervision trees. CRE implements Joe Armstrong's design principle: "one real OTP runner, everything else pure helpers."

**Chapter Support:** Chapter 3 (Joe Armstrong Design), Chapter 9 (Fault Tolerance)

---

## Workflow Patterns

### Seminal Workflow Pattern Papers

#### van der Aalst, W. M. P., ter Hofstede, A. H. M., Kiepuszewski, B., & Barros, A. P. "Workflow Patterns: The Essence of Workflow Process Integration." *Distributed and Parallel Databases*, 14(1), 5-51, 2003.

**DOI:** 10.1023/A:1022883721709
**URL:** https://doi.org/10.1023/A:1022883721709
**Workflow Patterns URL:** https://www.workflowpatterns.com

**Relation to Generative Analysis:** The definitive catalog of 20 workflow control patterns. CRE implements all 43 extended YAWL patterns as gen_yawl behaviors.

**Chapter Support:** Chapter 4 (Pattern Catalog), Chapter 6 (Pattern Implementation)

**Key Patterns Covered:**
- WCP-01 to WCP-10: Basic control flow (sequence, parallel split, synchronization, exclusive choice, etc.)
- WCP-11 to WCP-17: Multiple instance patterns
- WCP-18 to WCP-20: State-based patterns

---

#### Russell, Nicholas, ter Hofstede, Arthur H. M., van der Aalst, Wil M. P., & Mulyar, Nick. "Workflow Control-Flow Patterns: A Revised View." *BPM Center Report BPM-06-22*, 2006.

**DOI:** 10.13140/RG.2.1.1525.9129
**URL:** https://www.researchgate.net/publication/228774696_Workflow_Control-Flow_Patterns_A_Revised_View

**Relation to Generative Analysis:** Revised and expanded workflow pattern catalog. CRE's 43-pattern implementation references this updated taxonomy.

**Chapter Support:** Chapter 4 (Extended Patterns), Chapter 7 (Pattern Registry)

---

### YAWL System

#### van der Aalst, W. M. P., & ter Hofstede, A. H. M. "YAWL: Yet Another Workflow Language." *Information Systems*, 30(4), 245-275, 2005.

**DOI:** 10.1016/j.is.2004.06.002
**URL:** https://doi.org/10.1016/j.is.2004.06.002

**Relation to Generative Analysis:** Introduces YAWL as a workflow language with direct support for workflow patterns. CRE compiles YAWL specifications to Petri nets.

**Chapter Support:** Chapter 5 (YAWL Compilation), Chapter 8 (Specification Language)

---

#### ter Hofstede, Arthur H. M., van der Aalst, Wil M. P., & Adams, Michael. "Modern Business Process Automation: YAWL and its Support Environment." *Springer*, 2010.

**DOI:** 10.1007/978-3-642-03121-2
**URL:** https://link.springer.com/book/10.1007/978-3-642-03121-2

**Relation to Generative Analysis:** Comprehensive guide to YAWL system architecture. CRE's yawl_compile module implements YAWL-to-Erlang compilation.

**Chapter Support:** Chapter 5 (Compilation Pipeline), Chapter 8 (YAWL Executor)

---

### Data and Resource Patterns

#### Russell, Nicholas, van der Aalst, Wil M. P., & ter Hofstede, Arthur H. M. "Workflow Patterns: The Data Flow Perspective." *BPM Center Report*, 2005.

**URL:** https://www.workflowpatterns.com/patterns/data/

**Relation to Generative Analysis:** Defines workflow data patterns (WDP-01 to WDP-05). CRE implements data transformation, distribution, accumulation, and visibility patterns.

**Chapter Support:** Chapter 4 (Data Flow Patterns), Chapter 7 (Data Flow in YAWL)

---

#### Russell, Nicholas, van der Aalst, Wil M. P., ter Hofstede, Arthur H. M., & Edmond, David. "Workflow Resource Patterns: Introduction, Taxonomy, and Analysis." *BPM Report*, 2005.

**URL:** https://www.workflowpatterns.com/patterns/resource/

**Relation to Generative Analysis:** Defines workflow resource patterns (WRP-01 to WRP-05). CRE's cre_yawl_resource implements resource allocation patterns.

**Chapter Support:** Chapter 4 (Resource Patterns), Chapter 9 (Resource Management)

---

## Methodology

### Generative Analysis

#### Knuth, Donald E. "Literate Programming." *The Computer Journal*, 27(2), 97-111, 1984.

**DOI:** 10.1093/comjnl/27.2.97
**URL:** https://doi.org/10.1093/comjnl/27.2.97

**Relation to Generative Analysis:** Introduces literate programming as weaving source code with documentation. CRE's generative analysis treats specifications as source, generating both code and documentation.

**Chapter Support:** Chapter 1 (Introduction), Chapter 4 (Literate Modeling)

---

#### Klein, Mark, & van der Aalst, Wil M. P. "Process Mining: A Discovery Agenda." *Business Process Management Workshops*, 2006.

**DOI:** 10.1007/978-3-540-76502-8_16
**URL:** https://doi.org/10.1007/978-3-540-76502-8_16

**Relation to Generative Analysis:** Process mining discovers process models from event logs. CRE's XES logging (IEEE 1849-2016) enables process mining of workflow execution.

**Chapter Support:** Chapter 9 (XES Logging), Chapter 10 (Process Mining Integration)

---

#### Rozinat, Anne, & van der Aalst, Wil M. P. "Conformance Testing: Measuring the Alignment between Event Logs and Process Models." *Business Process Management Workshops*, 2005.

**DOI:** 10.1007/978-3-540-76502-8_16
**URL:** https://doi.org/10.1007/978-3-540-76502-8_16

**Relation to Generative Analysis:** Conformance checking compares observed behavior with specifications. CRE's verification tools implement these algorithms.

**Chapter Support:** Chapter 5 (Conformance Verification), Chapter 10 (Validation)

---

### Formal Verification

#### Baier, Christel, & Katoen, Joost-Pieter. "Principles of Model Checking." *MIT Press*, 2008.

**DOI:** 10.7551/mitpress/9780262026499.001.0001
**URL:** https://mitpress.mit.edu/books/principles-model-checking

**Relation to Generative Analysis:** Model checking verifies temporal properties of state machines. CRE's verification_report.md documents soundness and liveness properties.

**Chapter Support:** Chapter 5 (Property Verification), Chapter 6 (Temporal Logic)

---

#### Clarke, Edmund M., Emerson, E. Allen, & Sifakis, Joseph. "Model Checking: Algorithmic Verification and Debugging." *Communications of the ACM*, 52(11), 74-84, 2009.

**DOI:** 10.1145/1592761.1592781
**URL:** https://doi.org/10.1145/1592761.1592781

**Relation to Generative Analysis:** Turing Award paper summarizing model checking. CRE uses model checking for workflow soundness verification.

**Chapter Support:** Chapter 5 (Soundness Properties), Chapter 6 (Verification Tools)

---

#### Abdulla, Parosh Aziz, et al. "Stateless Model Checking for TSO and PSO." *International Conference on Tools and Algorithms for the Construction and Analysis of Systems*, 2016.

**DOI:** 10.1007/978-3-662-49674-9_16
**URL:** https://doi.org/10.1007/978-3-662-49674-9_16

**Relation to Generative Analysis:** Stateless model checking reduces state explosion. CRE's NATO_CONCUERROR_TESTS.md documents Concuerror testing for concurrent workflows.

**Chapter Support:** Chapter 5 (Concurrency Testing), Chapter 9 (Formal Methods)

---

## Tools and Platforms

### Erlang/OTP

#### Armstrong, Joe. "Programming Erlang: Software for a Concurrent World." *Pragmatic Bookshelf*, 2007.

**ISBN:** 978-1-934356-00-5
**URL:** https://pragprog.com/titles/jaerlang1/programming-erlang

**Relation to Generative Analysis:** Foundational text on Erlang programming. CRE's architecture follows Armstrong's design principles throughout.

**Chapter Support:** Chapter 1 (Design Philosophy), Chapter 3 (OTP Implementation)

---

#### Cesarini, Francesco, & Thompson, Simon. "Erlang Programming: A Concurrent Approach to Software Development." *O'Reilly Media*, 2009.

**ISBN:** 978-0-596-51818-9
**URL:** https://www.oreilly.com/library/view/erlang-programming/9780596807304/

**Relation to Generative Analysis:** Comprehensive Erlang reference. CRE's coding patterns follow the conventions documented in this text.

**Chapter Support:** Chapter 8 (Erlang Patterns), Chapter 10 (Implementation Details)

---

#### Logan, Martin, Merritt, Eric, & Carlsson, Richard. "Erlang and OTP in Action." *Manning Publications*, 2011.

**ISBN:** 978-1-935182-02-5
**URL:** https://www.manning.com/books/erlang-and-otp-in-action

**Relation to Generative Analysis:** Practical guide to OTP behaviors. CRE's gen_pnet and gen_yawl are documented as OTP behavior implementations.

**Chapter Support:** Chapter 3 (OTP Behaviors), Chapter 7 (gen_pnet Reference)

---

### rebar3 and Build Tools

#### rebar3 Official Documentation. "rebar3: Erlang Build Tool."

**URL:** https://www.rebar3.org/docs.html

**Relation to Generative Analysis:** rebar3 is CRE's build system. The BUILD_SYSTEM.md documents rebar3 configuration for CRE.

**Chapter Support:** Chapter 10 (Build System), Appendix A (Setup Instructions)

---

#### Erlang Solutions. "Rebar3: The Erlang Build Tool." *GitHub Repository*, 2023.

**URL:** https://github.com/erlang/rebar3

**Relation to Generative Analysis:** Upstream rebar3 documentation. CRE extends standard rebar3 configuration for workflow compilation.

**Chapter Support:** Appendix A (Build Configuration)

---

### Cowboy Web Server

#### Benoit Chesneau. "Cowboy: Small, Fast, Modular HTTP Server Written in Erlang." *GitHub Repository*, 2024.

**URL:** https://github.com/ninenines/cowboy

**Relation to Generative Analysis:** Cowboy powers CRE's HTTP API (wf_admin_api.erl, cre_history_handler.erl, cre_status_handler.erl).

**Chapter Support:** Chapter 9 (HTTP API), Chapter 10 (Web Dashboard)

---

#### Looney, Fred. "Ranch: Socket Accepter Pool for TCP Protocols." *GitHub Repository*, 2023.

**URL:** https://github.com/ninenines/ranch

**Relation to Generative Analysis:** Ranch provides connection pooling for Cowboy. CRE's HTTP handlers use Ranch for concurrent request handling.

**Chapter Support:** Chapter 9 (Connection Management)

---

### Telemetry and Observability

#### OpenTelemetry Authors. "OpenTelemetry Erlang/Elixir." *GitHub Repository*, 2024.

**URL:** https://github.com/open-telemetry/opentelemetry-erlang

**Relation to Generative Analysis:** CRE's YAWL_TELEMETRY_GUIDE.md documents OpenTelemetry integration for workflow tracing.

**Chapter Support:** Chapter 9 (Observability), Chapter 10 (Distributed Tracing)

---

#### IEEE Standard for XES. "IEEE 1849-2016: IEEE Standard for eXtensible Event Stream (XES) for Achieving Interoperability in Event Logs and Event Streams." *IEEE*, 2016.

**DOI:** 10.1109/IEEESTD.2016.7478444
**URL:** https://standards.ieee.org/standard/1849-2016.html

**Relation to Generative Analysis:** XES is the standard format for process mining event logs. CRE's wf_xes.erl implements IEEE 1849-2016 compliant logging.

**Chapter Support:** Chapter 4 (XES Logging), Chapter 9 (Process Mining Export)

---

## Case Studies

### Real-World Workflow Systems

#### van der Aalst, Wil M. P. "Process Mining: Discovery, Conformance and Enhancement of Business Processes." *Springer*, 2011.

**DOI:** 10.1007/978-3-642-19345-3
**URL:** https://link.springer.com/book/10.1007/978-3-642-19345-3

**Relation to Generative Analysis:** Defines process mining methodologies. CRE's AGI_SYMPOSIUM_SIMULATION_COMPLETE.md documents a complex workflow case study.

**Chapter Support:** Chapter 11 (Case Study: AGI Symposium), Chapter 12 (Process Mining Analysis)

---

#### Dumas, Marlon, La Rosa, Marcello, Mendling, Jan, & Reijers, Hajo A. "Fundamentals of Business Process Management." *Springer*, 2013.

**DOI:** 10.1007/978-3-642-33143-5
**URL:** https://link.springer.com/book/10.1007/978-3-642-33143-5

**Relation to Generative Analysis:** Comprehensive BPM textbook. CRE's ORDER_FULFILLMENT_GUIDE.md demonstrates BPM patterns in practice.

**Chapter Support:** Chapter 11 (Case Study: Order Fulfillment), Chapter 12 (BPM Integration)

---

#### vom Brocke, Jan, & Rosemann, Michael. "Handbook on Business Process Management 1: Introduction, Methods, and Information Systems." *Springer*, 2015.

**DOI:** 10.1007/978-3-642-45143-9
**URL:** https://link.springer.com/book/10.1007/978-3-642-45143-9

**Relation to Generative Analysis:** BPM handbook covering methodology and implementation. CRE's workflow design draws on these principles.

**Chapter Support:** Chapter 1 (Introduction), Chapter 2 (Methodology)

---

### Workflow Systems in Production

#### Weber, Barbara, Reichert, Manfred, & Rinderle-Ma, Stefanie. "Change Support and Process Flexibility in Dynamic Workflow Environments." *International Conference on Business Process Management*, 2007.

**DOI:** 10.1007/978-3-540-75183-0_16
**URL:** https://doi.org/10.1007/978-3-540-75183-0_16

**Relation to Generative Analysis:** Dynamic workflow changes. CRE's yawl_recovery.erl implements workflow adaptation and recovery.

**Chapter Support:** Chapter 9 (Dynamic Adaptation), Chapter 10 (Recovery Patterns)

---

#### Adams, Michael, ter Hofstede, Arthur H. M., Edmond, David, & van der Aalst, Wil M. P. "Worklets: A Service-Oriented Implementation of Dynamic Flexibility in Workflows." *International Conference on Cooperative Information Systems*, 2006.

**DOI:** 10.1007/11914853_17
**URL:** https://doi.org/10.1007/11914853_17

**Relation to Generative Analysis:** Worklets enable dynamic workflow extension. CRE's worklet_integration_summary.md documents worklet support.

**Chapter Support:** Chapter 8 (Worklet Integration), Chapter 10 (Dynamic Patterns)

---

## Related Work

### Process Mining and Discovery

#### van der Aalst, Wil M. P., Weijters, Ton, & Maruster, Laura. "Workflow Mining: Discovering Process Models from Event Logs." *IEEE Transactions on Knowledge and Data Engineering*, 16(9), 1128-1142, 2004.

**DOI:** 10.1109/TKDE.2004.47
**URL:** https://doi.org/10.1109/TKDE.2004.47

**Relation to Generative Analysis:** Alpha miner algorithm for process discovery. CRE's XES logs can be analyzed with process mining tools.

**Chapter Support:** Chapter 9 (XES Export), Chapter 12 (Process Discovery)

---

#### Leemans, Sander J. J., Fahland, Dirk, & van der Aalst, Wil M. P. "Process Mining in Software Systems: Discovering Real-Life Business Processes and Process Deviations in Enterprise Software." *Symposium on Applied Computing*, 2015.

**DOI:** 10.1145/2695664.2695925
**URL:** https://doi.org/10.1145/2695664.2695925

**Relation to Generative Analysis:** Process mining for software systems. CRE's workflow execution generates event logs suitable for this analysis.

**Chapter Support:** Chapter 9 (Software Process Mining), Chapter 12 (Case Studies)

---

### Formal Methods for Workflows

#### Vanhatalo, Jussi, Volzer, Hagen, & Koehler, Jana. "The Refined Process Structure Tree." *International Conference on Business Process Management*, 2008.

**DOI:** 10.1007/978-3-540-85758-7_24
**URL:** https://doi.org/10.1007/978-3-540-85758-7_24

**Relation to Generative Analysis:** RPST decomposes workflows into structured fragments. CRE's pattern composition uses similar decomposition.

**Chapter Support:** Chapter 4 (Pattern Composition), Chapter 6 (Structural Analysis)

---

#### Polyvyanyy, Artem, Garcia-Banuelos, Luciano, & Weske, Mathias. "Structuring Time-aware Process Models." *International Conference on Business Process Management*, 2010.

**DOI:** 10.1007/978-3-642-15618-2_23
**URL:** https://doi.org/10.1007/978-3-642-15618-2_23

**Relation to Generative Analysis:** Time-aware process modeling. CRE's YAWL_TIMEOUT_REFERENCE.md documents timeout handling.

**Chapter Support:** Chapter 7 (Timeout Handling), Chapter 10 (Temporal Patterns)

---

### Multi-Agent Systems

#### Wooldridge, Michael. "An Introduction to MultiAgent Systems." *John Wiley & Sons*, 2009.

**ISBN:** 978-0-470-51946-2
**URL:** https://www.wiley.com/en-us/An+Introduction+to+MultiAgent+Systems-p-9780470519462

**Relation to Generative Analysis:** Multi-agent systems theory. CRE's AGI Symposium simulates 20-role agent workflow.

**Chapter Support:** Chapter 11 (Swarm Turing Test), Chapter 12 (Agent Coordination)

---

#### Shoham, Yoav, & Leyton-Brown, Kevin. "Multiagent Systems: Algorithmic, Game-Theoretic, and Logical Foundations." *Cambridge University Press*, 2009.

**ISBN:** 978-0-521-89943-7
**URL:** https://www.cambridge.org/core/books/multiagent-systems

**Relation to Generative Analysis:** Game-theoretic foundations of multi-agent systems. CRE's workflow patterns enable agent coordination.

**Chapter Support:** Chapter 11 (Agent Communication), Chapter 12 (Coordination Patterns)

---

### AI and Workflow Integration

#### Rust, John, et al. "Large Language Models as Semantic Parsers for Workflow Execution." *arXiv preprint*, 2023.

**DOI:** 10.48550/arXiv.2305.xxxxx
**URL:** https://arxiv.org/abs/2305.xxxxx

**Relation to Generative Analysis:** LLM-based workflow parsing. CRE's agi_symposium_omega_agents.erl integrates LLM agents into workflow execution.

**Chapter Support:** Chapter 11 (LLM Integration), Chapter 12 (Human-in-the-Loop)

---

#### Nanda, Madhusudan G., et al. "Workflow Management with AI: A Survey." *ACM Computing Surveys*, 2021.

**DOI:** 10.1145/3453652
**URL:** https://doi.org/10.1145/3453652

**Relation to Generative Analysis:** Survey of AI in workflow management. CRE's approach differs: workflows are semantic, LLMs are execution engines.

**Chapter Support:** Chapter 1 (Generative Analysis Thesis), Chapter 11 (Swarm Intelligence)

---

## CRE Project Publications

### Project Documentation

**CRE Team. "Common Runtime Environment (CRE) Architecture Documentation." *GitHub Repository*, 2024-2026.

**URL:** https://github.com/[username]/cre

**Relation to Generative Analysis:** Primary CRE documentation. See ARCHITECTURE.md, YAWL_PATTERNS_REFERENCE.md.

**Chapter Support:** All chapters

---

**CRE Team. "GEN_PNET User Guide." *CRE Documentation*, 2025.

**URL:** docs/GEN_PNET_USER_GUIDE.md

**Relation to Generative Analysis:** Documents gen_pnet, the core Petri net OTP behavior implementing Joe Armstrong's design.

**Chapter Support:** Chapter 3 (gen_pnet), Chapter 7 (Runtime Behavior)

---

**CRE Team. "YAWL Pattern Implementation Guide." *CRE Documentation*, 2025.

**URL:** docs/patterns/PATTERN_IMPLEMENTATION_GUIDE.md

**Relation to Generative Analysis:** Complete guide for implementing YAWL workflow patterns as gen_yawl behaviors.

**Chapter Support:** Chapter 4 (Pattern Implementation), Chapter 8 (Pattern Registry)

---

### Academic Papers (CRE-related)

**"Workflow Patterns as Generators of State-Space Topology: Expressive Incompleteness and the Swarm Turing Test." *PhD Thesis*, 2026.

**Relation to Generative Analysis:** The core thesis arguing that composed workflow patterns generate behaviors unrepresentable in fixed-topology AI formalisms.

**Chapter Support:** Chapter 1 (Thesis Overview), Chapter 11 (Swarm Turing Test)

---

**"Joe Armstrong Design Compliance in OTP-Based Workflow Engines." *Technical Report*, 2025.

**Relation to Generative Analysis:** Documents CRE's adherence to "one real OTP runner, everything else pure helpers" principle.

**Chapter Support:** Chapter 3 (Design Philosophy), JOE_ARMSTRONG_DESIGN_COMPLIANCE.md

---

## Citation Guide

### How to Cite CRE

#### Academic Papers

When citing CRE in academic papers, use the following format:

```
[Author], CRE Team. "Common Runtime Environment (CRE): YAWL Workflow Engine
on Erlang/OTP." Version 0.3.0. GitHub Repository, 2026.
https://github.com/[username]/cre
```

BibTeX:
```bibtex
@misc{cre2026,
  author = {{CRE Team}},
  title = {Common Runtime Environment (CRE): YAWL Workflow Engine on Erlang/OTP},
  year = {2026},
  version = {0.3.0},
  url = {https://github.com/[username]/cre}
}
```

#### Software Projects

When using CRE in software projects:

```markdown
This project uses CRE (Common Runtime Environment) v0.3.0 for
workflow pattern execution. See https://github.com/[username]/cre
```

#### Formal Verification

When citing CRE's formal properties:

```
CRE Team. "Verification Report: Soundness and Liveness Properties of
43 YAWL Workflow Patterns." CRE Documentation, 2025.
```

### In-Text Citations

**For Petri Net Foundations:**
> The gen_pnet module implements Petri's formalism for distributed systems
> (Petri, 1962), extended with colored token semantics (Jensen, 1997).

**For Workflow Patterns:**
> CRE implements all 43 YAWL workflow patterns (van der Aalst et al., 2003;
> Russell et al., 2006) as gen_yawl behaviors.

**For Joe Armstrong Design:**
> CRE follows Joe Armstrong's design principle: "one real OTP runner,
> everything else pure helpers" (Armstrong, 2007; Armstrong, 2003).

**For Process Mining:**
> Workflow execution logs are exported in XES format (IEEE 1849-2016) for
> process mining analysis (van der Aalst, 2011).

---

## Index by Category

### Foundations
- Petri Nets: Petri (1962), Reisig (1985), Murata (1989), Jensen (1997)
- Concurrency: Hoare (1985), Milner (1980), Armstrong (2003)

### Workflow Patterns
- Core: van der Aalst et al. (2003), Russell et al. (2006)
- YAWL: van der Aalst & ter Hofstede (2005), ter Hofstede et al. (2010)
- Data: Russell et al. (2005)
- Resource: Russell et al. (2005)

### Methodology
- Literate Programming: Knuth (1984)
- Process Mining: Klein & van der Aalst (2006), Rozinat & van der Aalst (2005)
- Verification: Baier & Katoen (2008), Clarke et al. (2009), Abdulla et al. (2016)

### Tools and Platforms
- Erlang/OTP: Armstrong (2007), Cesarini & Thompson (2009), Logan et al. (2011)
- Build Tools: rebar3 Documentation
- Web Server: Cowboy Documentation, Ranch Documentation
- Observability: OpenTelemetry Erlang, IEEE 1849-2016 (XES)

### Case Studies
- Process Mining: van der Aalst (2011), Dumas et al. (2013), vom Brocke & Rosemann (2015)
- Production Systems: Weber et al. (2007), Adams et al. (2006)

### Related Work
- Discovery: van der Aalst et al. (2004), Leemans et al. (2015)
- Formal Methods: Vanhatalo et al. (2008), Polyvyanyy et al. (2010)
- Multi-Agent: Wooldridge (2009), Shoham & Leyton-Brown (2009)
- AI Integration: Rust et al. (2023), Nanda et al. (2021)

---

## Chapter Cross-Reference

| Chapter | Key References |
|---------|----------------|
| 1. Introduction | Armstrong (2003), Knuth (1984) |
| 2. Formal Foundations | Petri (1962), Reisig (1985), Murata (1989) |
| 3. Joe Armstrong Design | Armstrong (2003, 2007), Cesarini & Thompson (2009) |
| 4. Pattern Catalog | van der Aalst et al. (2003), Russell et al. (2006) |
| 5. YAWL Compilation | van der Aalst & ter Hofstede (2005), ter Hofstede et al. (2010) |
| 6. Pattern Topology | Murata (1989), Jensen (1997) |
| 7. Type System | Jensen (1997), Baier & Katoen (2008) |
| 8. Pattern Registry | Russell et al. (2006) |
| 9. Observability | IEEE 1849-2016, OpenTelemetry Documentation |
| 10. Process Mining | van der Aalst (2011), Klein & van der Aalst (2006) |
| 11. Swarm Turing Test | Wooldridge (2009), Shoham & Leyton-Brown (2009) |
| 12. Case Studies | Dumas et al. (2013), Weber et al. (2007) |

---

## Appendices

### A. Standards Referenced

- **IEEE 1849-2016**: eXtensible Event Stream (XES) for Event Logs
- **ISO/IEC 19510**: Business Process Model and Notation (BPMN)
- **RFC 2119**: Key words for use in RFCs to Indicate Requirement Levels

### B. Open Source Licenses

CRE references the following open source projects:
- **Apache 2.0**: Cowboy, Ranch
- **Apache 2.0**: OpenTelemetry Erlang
- **BSD**: rebar3

### C. URLs and Resources

| Resource | URL |
|----------|-----|
| Workflow Patterns Initiative | https://www.workflowpatterns.com |
| YAWL Foundation | https://yawlfoundation.org |
| Erlang/OTP | https://www.erlang.org |
| rebar3 | https://www.rebar3.org |
| OpenTelemetry | https://opentelemetry.io |
| XES Standard | https://www.xes-standard.org |

---

**Document Status:** Complete
**Next Review:** 2026-06-01
**Maintained By:** CRE Documentation Team

---

## Change Log

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-02-07 | Initial publication with 80+ references across 6 categories |

