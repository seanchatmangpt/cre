# CRE Documentation Index

**CRE (Common Runtime Environment)** - YAWL Workflow Engine on Erlang/OTP
**Version:** 0.3.0 | **OTP Support:** 25.0 - 28.x | **Last Updated:** 2026-02-08

---

## Quick Navigation

| You Are... | Start Here |
|------------|------------|
| **New to CRE** | [Quick Start](#quick-start) |
| **Learning YAWL** | [Tutorials](#tutorials) |
| **Developing** | [API Reference](#api-reference) |
| **Deploying** | [Operations](#operations) |
| **Troubleshooting** | [Support](#support) |

---

## Quick Start

| Document | Description | Time |
|----------|-------------|------|
| [README.md](README.md) | Project overview, features, and introduction | 5 min |
| [QUICK_START.md](QUICK_START.md) | Get running in 5 minutes | 5 min |
| [QUICK_REFERENCE_CARD.md](QUICK_REFERENCE_CARD.md) | Essential commands and patterns | 5 min |
| [EXAMPLES.md](EXAMPLES.md) | Working code examples | 15 min |

---

## API Reference

### Core API Documentation

| Document | Description | Scope |
|----------|-------------|-------|
| [api/core/COMPLETE_API_REFERENCE.md](api/core/COMPLETE_API_REFERENCE.md) | Full API documentation | All modules |
| [api/core/CLIENT_API_COMPLETE_REFERENCE.md](api/core/CLIENT_API_COMPLETE_REFERENCE.md) | Client API comprehensive guide | Client APIs |
| [reference/api_reference.md](reference/api_reference.md) | Reference section documentation | Core APIs |

### Pattern APIs

| Document | Description |
|----------|-------------|
| [api/patterns/patterns_api.md](api/patterns/patterns_api.md) | Patterns API reference |

### Mining APIs

| Document | Description |
|----------|-------------|
| [api/mining/MINING_MODULES_API_REFERENCE.md](api/mining/MINING_MODULES_API_REFERENCE.md) | Process mining modules API |

### Core Behavior APIs

| Document | Description |
|----------|-------------|
| [yawl_patterns/GEN_PNET_API_SPECIFICATION.md](yawl_patterns/GEN_PNET_API_SPECIFICATION.md) | gen_pnet behavior API |
| [yawl_patterns/GEN_YAWL_API_SPECIFICATION.md](yawl_patterns/GEN_YAWL_API_SPECIFICATION.md) | gen_yawl behavior API |
| [yawl_patterns/GEN_PNET_INTEGRATION_ARCHITECTURE.md](yawl_patterns/GEN_PNET_INTEGRATION_ARCHITECTURE.md) | Integration architecture |

---

## Guides

### Feature Guides

| Document | Description | Level |
|----------|-------------|-------|
| [guides/human_in_the_loop.md](guides/human_in_the_loop.md) | Approval workflows with LLM integration | Intermediate |
| [guides/telemetry.md](guides/telemetry.md) | Monitoring and telemetry setup | Intermediate |
| [guides/timeout_configuration.md](guides/timeout_configuration.md) | Timeout configuration | Beginner |
| [guides/order_fulfillment_example.md](guides/order_fulfillment_example.md) | Real-world workflow example | Intermediate |
| [guides/tool_configuration.md](guides/tool_configuration.md) | Tool configuration guide | Intermediate |

### Migration Guides

| Document | Description | Level |
|----------|-------------|-------|
| [guides/migration/migration_guide.md](guides/migration/migration_guide.md) | General migration guide | Advanced |
| [guides/migration/otp_25_28.md](guides/migration/otp_25_28.md) | Erlang/OTP version migration | Advanced |

### Integration & Deployment

| Document | Description | Level |
|----------|-------------|-------|
| [DEPLOYMENT.md](DEPLOYMENT.md) | Production deployment guide | Advanced |
| [INTEGRATION.md](INTEGRATION.md) | System integration overview | Intermediate |
| [HELPER_INTEGRATION_GUIDE.md](HELPER_INTEGRATION_GUIDE.md) | Helper module integration | Intermediate |
| [WF_MODULES_INTEGRATION_GUIDE.md](WF_MODULES_INTEGRATION_GUIDE.md) | Workflow utilities integration | Intermediate |
| [UTILITY_MODULES_GUIDE.md](UTILITY_MODULES_GUIDE.md) | Utility modules usage | Intermediate |

---

## Reference

| Document | Description |
|----------|-------------|
| [reference/glossary.md](reference/glossary.md) | Terminology and definitions |
| [reference/faq.md](reference/faq.md) | Frequently asked questions |
| [reference/bibliography.md](reference/bibliography.md) | References and citations |
| [reference/EXCEPTION_HANDLING.md](reference/EXCEPTION_HANDLING.md) | Exception handling reference |
| [reference/QUICK_REFERENCE_CHEATSHEET.md](reference/QUICK_REFERENCE_CHEATSHEET.md) | Comprehensive cheatsheet |
| [reference/quick_reference_cards.md](reference/quick_reference_cards.md) | Quick reference collection |

---

## Tutorials

### Tutorial Series

| Tutorial | Duration | Prerequisites | Description |
|----------|----------|---------------|-------------|
| [Getting Started](tutorials/getting_started.md) | 30 min | None | Your first YAWL workflow |
| [Basic Patterns](tutorials/basic_patterns_tutorial.md) | 60 min | Getting Started | WCP-01 to WCP-06 patterns |
| [Advanced Patterns](tutorials/advanced_patterns_tutorial.md) | 90 min | Basic Patterns | Complex pattern composition |
| [Colored Tokens](tutorials/colored_tokens_tutorial.md) | 45 min | Getting Started | Data-carrying tokens |
| [Handler Development](tutorials/HANDLER_DEVELOPMENT.md) | 60 min | Basic Patterns | Business logic integration |
| [Migration](tutorials/workflow_migration_tutorial.md) | 60 min | Getting Started | Migrate from legacy systems |

### Learning Paths

| Document | Description | Level |
|----------|-------------|-------|
| [TUTORIALS_INDEX.md](TUTORIALS_INDEX.md) | Complete tutorial roadmap | All |
| [WORKFLOW_PATTERNS_LEARNING_PATH.md](WORKFLOW_PATTERNS_LEARNING_PATH.md) | Structured pattern learning path | All |

---

## Architecture & Design

| Document | Description | Level |
|----------|-------------|-------|
| [ARCHITECTURE.md](ARCHITECTURE.md) | Joe Armstrong design philosophy, system architecture | Advanced |
| [architecture/system-overview.md](architecture/system-overview.md) | Detailed system architecture components | Advanced |
| [architecture/hybrid_proposals.md](architecture/hybrid_proposals.md) | Hybrid architecture proposals | Advanced |
| [development/build_system.md](development/build_system.md) | Build automation and tools | Intermediate |
| [GEN_PNET_USER_GUIDE.md](GEN_PNET_USER_GUIDE.md) | Core Petri net runtime behavior guide | Intermediate |
| [DIAGRAMS_REFERENCE.md](DIAGRAMS_REFERENCE.md) | Architecture and diagram index | All |

### Design Principles

| Document | Description |
|----------|-------------|
| [architecture/design_principles.md](architecture/design_principles.md) | System design principles |
| [architecture/joe_armstrong_compliance.md](architecture/joe_armstrong_compliance.md) | Joe Armstrong philosophy compliance |

### Architecture Diagrams

| Document | Description |
|----------|-------------|
| [architecture/diagrams/compilation_pipeline.md](architecture/diagrams/compilation_pipeline.md) | Compilation pipeline diagrams |
| [architecture/diagrams/state_machines.md](architecture/diagrams/state_machines.md) | State machine diagrams |
| [architecture/diagrams/plantuml.md](architecture/diagrams/plantuml.md) | PlantUML diagrams |
| [architecture/diagrams/decision_trees.md](architecture/diagrams/decision_trees.md) | Decision tree documentation |

---

## YAWL Workflow Patterns

### Core Pattern Documentation

| Document | Description | Level |
|----------|-------------|-------|
| [YAWL_PATTERNS_REFERENCE.md](YAWL_PATTERNS_REFERENCE.md) | All 43 YAWL patterns catalog | Intermediate |
| [CORE_YAWL_PATTERNS_GUIDE.md](CORE_YAWL_PATTERNS_GUIDE.md) | Essential patterns deep dive | Intermediate |
| [YAWL_PATTERN_REFERENCE.md](YAWL_PATTERN_REFERENCE.md) | Pattern semantics and use cases | Intermediate |
| [YAWL_PATTERNS_WORKBOOK.md](YAWL_PATTERNS_WORKBOOK.md) | Practice exercises | Beginner |
| [YAWL_PATTERN_EXAMPLES.md](YAWL_PATTERN_EXAMPLES.md) | Pattern implementation examples | All |
| [43_PATTERNS_COMPLETE.md](43_PATTERNS_COMPLETE.md) | All 43 patterns summary | All |

### Pattern Categories

| Document | Description |
|----------|-------------|
| [patterns/PATTERN_IMPLEMENTATION_GUIDE.md](patterns/PATTERN_IMPLEMENTATION_GUIDE.md) | Implementation guide |
| [patterns/ADVANCED_PATTERNS.md](patterns/ADVANCED_PATTERNS.md) | Advanced patterns |
| [patterns/WDP_PATTERNS.md](patterns/WDP_PATTERNS.md) | Data patterns (WDP) |
| [patterns/WRP_PATTERNS.md](patterns/WRP_PATTERNS.md) | Resource patterns (WRP) |
| [patterns/anti_patterns_guide.md](patterns/anti_patterns_guide.md) | Anti-patterns guide |
| [patterns/reference_card.md](patterns/reference_card.md) | Pattern reference card |

### YAWL Compilation

| Document | Description |
|----------|-------------|
| [YAWL_COMPILE_COMPLETE_GUIDE.md](YAWL_COMPILE_COMPLETE_GUIDE.md) | YAWL compilation to Petri nets |
| [yawl_patterns/YAWL_ARCHITECTURE.md](yawl_patterns/YAWL_ARCHITECTURE.md) | YAWL pattern system architecture |

---

## Petri Net Core

### Type System

| Document | Description | Level |
|----------|-------------|-------|
| [pnet/types/guide.md](pnet/types/guide.md) | Type system concepts | Intermediate |
| [pnet/types/tutorial.md](pnet/types/tutorial.md) | Type system tutorial | Beginner |
| [pnet/types/api_reference.md](pnet/types/api_reference.md) | Type definitions and validation | Advanced |
| [pnet/types/quick_reference.md](pnet/types/quick_reference.md) | Type system quick reference | All |

### Marking Algebra

| Document | Description | Level |
|----------|-------------|-------|
| [pnet/marking/tutorial.md](pnet/marking/tutorial.md) | Marking algebra fundamentals | Intermediate |
| [pnet/marking/algebra.md](pnet/marking/algebra.md) | Multiset marking theory | Advanced |
| [pnet/marking/implementation.md](pnet/marking/implementation.md) | Technical implementation details | Advanced |
| [pnet/marking/api_reference.md](pnet/marking/api_reference.md) | State management API | Intermediate |
| [pnet/marking/quick_reference.md](pnet/marking/quick_reference.md) | Quick operations lookup | All |
| [pnet/marking/tests.md](pnet/marking/tests.md) | Test documentation | All |

### Mode Enumeration

| Document | Description | Level |
|----------|-------------|-------|
| [pnet/mode/guide.md](pnet/mode/guide.md) | Mode enumeration concepts | Intermediate |
| [pnet/mode/tutorial.md](pnet/mode/tutorial.md) | Mode handling tutorial | Intermediate |
| [pnet/mode/quick_reference.md](pnet/mode/quick_reference.md) | Mode enumeration quick ref | All |

### Core Reference

| Document | Description |
|----------|-------------|
| [PNET_CORE_COMPREHENSIVE_REFERENCE.md](PNET_CORE_COMPREHENSIVE_REFERENCE.md) | Core net behaviors |

---

## Operations

### Testing & Quality

| Document | Description |
|----------|-------------|
| [operations/testing/testing.md](operations/testing/testing.md) | Comprehensive testing documentation |
| [operations/testing/test_organization.md](operations/testing/test_organization.md) | Test structure and organization |
| [operations/testing/test_status.md](operations/testing/test_status.md) | Current test results |
| [operations/testing/verification_report.md](operations/testing/verification_report.md) | System verification results |
| [operations/testing/verification_checklist.md](operations/testing/verification_checklist.md) | Verification checklist |
| [operations/testing/nato_concuerror_tests.md](operations/testing/nato_concuerror_tests.md) | Concurrency testing |

### Performance & Tuning

| Document | Description |
|----------|-------------|
| [operations/performance/performance.md](operations/performance/performance.md) | Performance tuning guide |

### Troubleshooting

| Document | Description |
|----------|-------------|
| [operations/troubleshooting/troubleshooting.md](operations/troubleshooting/troubleshooting.md) | Common problems and solutions |
| [operations/troubleshooting/known_issues.md](operations/troubleshooting/known_issues.md) | Current known issues |
| [operations/troubleshooting/debugging.md](operations/troubleshooting/debugging.md) | Debugging guide |
| [operations/troubleshooting/debugging_quick_reference.md](operations/troubleshooting/debugging_quick_reference.md) | Debugging quick reference |

---

## Papers

Research papers on process mining and workflow management:

| Document | Description |
|----------|-------------|
| [papers/README.md](papers/README.md) | Process mining papers index |
| [papers/PAPER_SUMMARIES.md](papers/PAPER_SUMMARIES.md) | Paper summaries |
| [papers/algorithm_mapping.csv](papers/algorithm_mapping.csv) | Algorithm mapping |
| [papers/analysis_readme.md](papers/analysis_readme.md) | Analysis documentation |
| [papers/analysis_summary.md](papers/analysis_summary.md) | Analysis summary |

The papers directory contains PDFs covering:
- Workflow pattern research
- Process mining fundamentals
- Petri net theory
- YAWL language specifications
- Conformance checking
- Object-centric process mining

---

## Rust Modules

High-performance Rust implementations:

| Document | Description | Level |
|----------|-------------|-------|
| [rust/implementation_guide.md](rust/implementation_guide.md) | Complete Rust modules guide | Intermediate |
| [rust/quick_reference.md](rust/quick_reference.md) | Rust modules quick reference | All |
| [rust/erlang_integration.md](rust/erlang_integration.md) | Erlang-Rust NIF integration | Advanced |
| [rust/implementation_plan.md](rust/implementation_plan.md) | Rust implementation plan | Advanced |

---

## Advanced Topics

### Genetic Algorithms

| Document | Description |
|----------|-------------|
| [features/genetic-algorithms/constitution_schema.md](features/genetic-algorithms/constitution_schema.md) | Genetic algorithm schema |
| [features/genetic-algorithms/examples.md](features/genetic-algorithms/examples.md) | GA examples |
| [features/genetic-algorithms/validation.md](features/genetic-algorithms/validation.md) | GA validation |

### Strategy Plugins

| Document | Description |
|----------|-------------|
| [features/strategies/plugin_system.md](features/strategies/plugin_system.md) | Strategy plugin system |
| [features/strategies/rl_strategies.md](features/strategies/rl_strategies.md) | RL strategies |

### Specialized Analysis

#### YAWL Analysis

| Document | Description |
|----------|-------------|
| [analysis/yawl/yengine.md](analysis/yawl/yengine.md) | YAWL engine analysis |
| [analysis/yawl/dataflow.md](analysis/yawl/dataflow.md) | Data flow analysis |
| [analysis/yawl/interfaces.md](analysis/yawl/interfaces.md) | Interface analysis |
| [analysis/yawl/logging.md](analysis/yawl/logging.md) | Logging analysis |
| [analysis/yawl/persistence.md](analysis/yawl/persistence.md) | Persistence analysis |
| [analysis/yawl/resourcing.md](analysis/yawl/resourcing.md) | Resource analysis |
| [analysis/yawl/specification.md](analysis/yawl/specification.md) | Specification analysis |
| [analysis/yawl/marking.md](analysis/yawl/marking.md) | Marking analysis |
| [analysis/yawl/multi_instance.md](analysis/yawl/multi_instance.md) | Multi-instance analysis |
| [analysis/yawl/exception.md](analysis/yawl/exception.md) | Exception analysis |
| [analysis/yawl/java.md](analysis/yawl/java.md) | Java comparison analysis |
| [analysis/yawl/timer.md](analysis/yawl/timer.md) | Timer analysis |
| [analysis/yawl/architecture_comparison.md](analysis/yawl/architecture_comparison.md) | Architecture comparison |
| [analysis/yawl/recommendations.md](analysis/yawl/recommendations.md) | YAWL recommendations |
| [analysis/yawl/net.md](analysis/yawl/net.md) | YNet analysis |
| [analysis/yawl/netrunner.md](analysis/yawl/netrunner.md) | YNetRunner analysis |
| [analysis/yawl/ytask.md](analysis/yawl/ytask.md) | YTask analysis |
| [analysis/yawl/pattern_comparison.md](analysis/yawl/pattern_comparison.md) | Pattern comparison |
| [analysis/yawl/verification_checklist.md](analysis/yawl/verification_checklist.md) | Verification checklist |
| [analysis/yawl/resetnet.md](analysis/yawl/resetnet.md) | Reset/Net analysis |
| [analysis/yawl/workitem.md](analysis/yawl/workitem.md) | WorkItem analysis |

#### Other Analysis

| Document | Description |
|----------|-------------|
| [analysis/generative/chapter_6_2_6_3.md](analysis/generative/chapter_6_2_6_3.md) | Generative analysis chapter |
| [analysis/generative/chapters_3_6_and_4_1.md](analysis/generative/chapters_3_6_and_4_1.md) | Generative analysis chapters |
| [analysis/generative/diagrams.md](analysis/generative/diagrams.md) | Generative analysis diagrams |
| [analysis/other/executor_pattern.md](analysis/other/executor_pattern.md) | Executor pattern analysis |
| [analysis/other/pattern_implementation.md](analysis/other/pattern_implementation.md) | Pattern implementation analysis |
| [analysis/other/pattern_enhancements.md](analysis/other/pattern_enhancements.md) | Pattern enhancements |
| [analysis/other/token_protocol.md](analysis/other/token_protocol.md) | Token communication protocol |

---

## Development

### Contributing

| Document | Description |
|----------|-------------|
| [development/contributing.md](development/contributing.md) | Developer contribution guidelines |
| [development/release_notes/0.3.0.md](development/release_notes/0.3.0.md) | Version 0.3.0 release notes |
| [development/release_notes/0.3.0_summary.md](development/release_notes/0.3.0_summary.md) | Release summary |
| [development/schema_validation.md](development/schema_validation.md) | Schema validation documentation |
| [development/build_system.md](development/build_system.md) | Build system documentation |

### Planning & Roadmap

| Document | Description |
|----------|-------------|
| [planning/roadmap.md](planning/roadmap.md) | Development roadmap |
| [planning/gap_analysis.md](planning/gap_analysis.md) | Planning gap analysis |
| [planning/executive_summary.md](planning/executive_summary.md) | Roadmap summary |
| [planning/innovation_opportunities.md](planning/innovation_opportunities.md) | Innovation opportunities |
| [planning/innovation_synthesis.md](planning/innovation_synthesis.md) | Innovation synthesis |
| [planning/tooling_roadmap.md](planning/tooling_roadmap.md) | Tooling roadmap |

---

## Examples & Training

### Examples

| Document | Description |
|----------|-------------|
| [examples/README.md](examples/README.md) | Examples directory guide |
| [examples/basic_workflow.erl](examples/basic_workflow.erl) | Basic workflow example |
| [examples/approval_workflow.erl](examples/approval_workflow.erl) | Approval workflow example |

### Example Workflows

| Document | Description |
|----------|-------------|
| [example_workflows/01_simple_approval.yaml](example_workflows/01_simple_approval.yaml) | Simple approval YAML |
| [example_workflows/02_parallel_approval.yaml](example_workflows/02_parallel_approval.yaml) | Parallel approval YAML |
| [example_workflows/03_majority_approval.yaml](example_workflows/03_majority_approval.yaml) | Majority approval YAML |
| [example_workflows/04_multi_stage_review.yaml](example_workflows/04_multi_stage_review.yaml) | Multi-stage review YAML |
| [example_workflows/05_cancellation_region.yaml](example_workflows/05_cancellation_region.yaml) | Cancellation region YAML |

### Training Materials

| Document | Description |
|----------|-------------|
| [training/materials.md](training/materials.md) | Training materials collection |
| [training/workshops.md](training/workshops.md) | Workshop materials |
| [training/exercises.md](training/exercises.md) | Tutorial exercises |
| [training/rubrics.md](training/rubrics.md) | Evaluation criteria |
| [training/slides.md](training/slides.md) | Presentation slides |

---

## Diagrams

### Diagram Collections

| Directory | Description |
|-----------|-------------|
| [diagrams/](diagrams/) | Architecture diagrams directory |
| [diagrams/c4/](diagrams/c4/) | C4 model diagrams |
| [diagrams/mermaid/](diagrams/mermaid/) | Mermaid diagram source files |
| [mermaid-diagrams/](mermaid-diagrams/) | Additional Mermaid diagrams |

### Specific Diagrams

| Document | Description |
|----------|-------------|
| [mermaid-diagrams/README.md](mermaid-diagrams/README.md) | Mermaid diagrams index |
| [mermaid-diagrams/workflow-state-diagram.md](mermaid-diagrams/workflow-state-diagram.md) | Workflow state diagram |
| [mermaid-diagrams/sequence-diagram.md](mermaid-diagrams/sequence-diagram.md) | Sequence diagram |
| [mermaid-diagrams/system-architecture-diagram.md](mermaid-diagrams/system-architecture-diagram.md) | System architecture |
| [mermaid-diagrams/yawl-patterns-diagram.md](mermaid-diagrams/yawl-patterns-diagram.md) | YAWL patterns diagram |
| [diagrams/c4/C4_ARCHITECTURE.md](diagrams/c4/C4_ARCHITECTURE.md) | C4 architecture documentation |

---

## Case Studies & Research

### Case Studies

| Document | Description |
|----------|-------------|
| [case-studies/case_study_agi_symposium.md](case-studies/case_study_agi_symposium.md) | AGI Symposium case study |
| [case-studies/AGI_SYMPOSIUM_SIMULATION_COMPLETE.md](case-studies/AGI_SYMPOSIUM_SIMULATION_COMPLETE.md) | Simulation completion report |
| [case-studies/AGI_SYMPOSIUM_ISSUES.md](case-studies/AGI_SYMPOSIUM_ISSUES.md) | Issues and lessons learned |
| [case-studies/bcd_retrospective.md](case-studies/bcd_retrospective.md) | BCD retrospective |
| [case-studies/FINAL_MERGE_SUMMARY.md](case-studies/FINAL_MERGE_SUMMARY.md) | Merge summary |

### Research & Papers

| Document | Description |
|----------|-------------|
| [papers/](papers/) | Process mining papers collection |
| [generative_analysis_book/](generative_analysis_book/) | Generative analysis chapters |
| [book/](book/) | Book chapters and drafts |
| [thesis_workflow_swarm.md](thesis_workflow_swarm.md) | PhD thesis workflow |

---

## Documentation by Task

| Task | Documents |
|------|-----------|
| **Install & Setup** | [QUICK_START.md](QUICK_START.md), [DEPLOYMENT.md](DEPLOYMENT.md) |
| **First Workflow** | [tutorials/getting_started.md](tutorials/getting_started.md), [examples/](examples/) |
| **Learn Patterns** | [WORKFLOW_PATTERNS_LEARNING_PATH.md](WORKFLOW_PATTERNS_LEARNING_PATH.md), [CORE_YAWL_PATTERNS_GUIDE.md](CORE_YAWL_PATTERNS_GUIDE.md) |
| **Build Workflows** | [YAWL_PATTERNS_REFERENCE.md](YAWL_PATTERNS_REFERENCE.md), [YAWL_COMPILE_COMPLETE_GUIDE.md](YAWL_COMPILE_COMPLETE_GUIDE.md) |
| **API Integration** | [api/core/COMPLETE_API_REFERENCE.md](api/core/COMPLETE_API_REFERENCE.md), [api/core/CLIENT_API_COMPLETE_REFERENCE.md](api/core/CLIENT_API_COMPLETE_REFERENCE.md) |
| **Production** | [DEPLOYMENT.md](DEPLOYMENT.md), [operations/testing/testing.md](operations/testing/testing.md), [guides/telemetry.md](guides/telemetry.md) |
| **Troubleshooting** | [operations/troubleshooting/troubleshooting.md](operations/troubleshooting/troubleshooting.md), [operations/troubleshooting/known_issues.md](operations/troubleshooting/known_issues.md), [reference/faq.md](reference/faq.md) |

---

## Learning Paths

### Path 1: Complete Beginner (3-5 days)

```
1. README.md (5 min)
2. QUICK_START.md (5 min)
3. tutorials/getting_started.md (30 min)
4. tutorials/basic_patterns_tutorial.md (60 min)
5. CORE_YAWL_PATTERNS_GUIDE.md (90 min)
6. api/core/CLIENT_API_COMPLETE_REFERENCE.md (60 min)
7. YAWL_PATTERNS_WORKBOOK.md (60 min)
8. operations/testing/testing.md (45 min)
9. DEPLOYMENT.md (60 min)
```

### Path 2: Pattern Expert (2-3 days)

```
1. WORKFLOW_PATTERNS_LEARNING_PATH.md (15 min)
2. CORE_YAWL_PATTERNS_GUIDE.md (90 min)
3. YAWL_PATTERNS_REFERENCE.md (120 min)
4. tutorials/advanced_patterns_tutorial.md (90 min)
5. tutorials/colored_tokens_tutorial.md (45 min)
6. patterns/ADVANCED_PATTERNS.md (60 min)
7. YAWL_COMPILE_COMPLETE_GUIDE.md (60 min)
```

### Path 3: API Developer (1-2 days)

```
1. api/core/COMPLETE_API_REFERENCE.md (90 min)
2. api/core/CLIENT_API_COMPLETE_REFERENCE.md (60 min)
3. PNET_CORE_COMPREHENSIVE_REFERENCE.md (60 min)
4. GEN_PNET_USER_GUIDE.md (45 min)
5. HELPER_INTEGRATION_GUIDE.md (45 min)
```

### Path 4: Operations Engineer (1 day)

```
1. ARCHITECTURE.md (60 min)
2. DEPLOYMENT.md (60 min)
3. guides/telemetry.md (45 min)
4. operations/testing/testing.md (45 min)
5. operations/troubleshooting/troubleshooting.md (30 min)
6. operations/performance/performance.md (30 min)
```

---

## Support

| Resource | Description |
|----------|-------------|
| [README.md](README.md) | Project overview |
| [reference/faq.md](reference/faq.md) | Frequently asked questions |
| [reference/glossary.md](reference/glossary.md) | Terminology |
| [operations/troubleshooting/troubleshooting.md](operations/troubleshooting/troubleshooting.md) | Problem solving |
| [development/contributing.md](development/contributing.md) | How to contribute |

---

**Last Updated:** 2026-02-08 | **Version:** 0.3.0 | **Pattern Count:** 43 YAWL patterns
