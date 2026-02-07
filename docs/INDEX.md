# CRE Documentation Index

**CRE (Common Runtime Environment)** - A YAWL workflow engine built on Erlang/OTP, implementing Joe Armstrong's design philosophy: one real OTP runner (`gen_pnet`), everything else pure helpers/utilities.

**Version:** 0.2.1 | **OTP Support:** 25.0 - 28.x

---

## Quick Start

| Document | Description |
|----------|-------------|
| [Quick Start Guide](QUICK_START.md) | Get running in 5 minutes - installation, first workflow, basic configuration |
| [README](README.md) | Project overview, features, and introduction |

---

## Getting Started

| Document | Description |
|----------|-------------|
| [Tutorials Index](TUTORIALS_INDEX.md) | Complete tutorial roadmap and learning paths |
| [tutorials/README.md](tutorials/README.md) | Step-by-step tutorial series overview |
| [tutorials/getting_started.md](tutorials/getting_started.md) | Your first YAWL workflow (30 min) |
| [tutorials/basic_patterns_tutorial.md](tutorials/basic_patterns_tutorial.md) | Learn WCP-01 to WCP-06 patterns (60 min) |
| [tutorials/advanced_patterns_tutorial.md](tutorials/advanced_patterns_tutorial.md) | Complex pattern composition (90 min) |
| [tutorials/colored_tokens_tutorial.md](tutorials/colored_tokens_tutorial.md) | Data-carrying tokens (45 min) |
| [tutorials/workflow_migration_tutorial.md](tutorials/workflow_migration_tutorial.md) | Migrate from legacy systems (60 min) |

---

## Architecture & Design

| Document | Description |
|----------|-------------|
| [Architecture Overview](ARCHITECTURE.md) | Joe Armstrong design philosophy, system architecture, module organization |
| [Diataxis Architecture](DIATAXIS_ARCHITECTURE.md) | Documentation architecture principles and design patterns |
| [gen_pnet User Guide](GEN_PNET_USER_GUIDE.md) | Core Petri net runtime behavior guide |
| [yawl_patterns/Architecture](yawl_patterns/YAWL_ARCHITECTURE.md) | YAWL pattern system architecture |
| [yawl_patterns/gen_pnet Integration](yawl_patterns/GEN_PNET_INTEGRATION_ARCHITECTURE.md) | Integration architecture for patterns |

---

## YAWL Workflow Patterns

### Pattern Guides

| Document | Description |
|----------|-------------|
| [Core YAWL Patterns Guide](CORE_YAWL_PATTERNS_GUIDE.md) | Essential patterns deep dive (6 core patterns) |
| [YAWL Pattern Reference](YAWL_PATTERN_REFERENCE.md) | Detailed pattern specifications |
| [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE.md) | All 43 YAWL patterns catalog |

### Quick References

| Document | Description |
|----------|-------------|
| [Core Patterns Quick Reference](CORE_YAWL_PATTERNS_QUICK_REFERENCE.md) | Essential patterns cheat sheet |
| [YAWL Patterns Reference Card](YAWL_PATTERNS_REFERENCE_CARD.md) | Pattern reference card |
| [YAWL Pattern Examples](YAWL_PATTERN_EXAMPLES.md) | Pattern implementation examples |
| [YAWL Patterns Workbook](YAWL_PATTERNS_WORKBOOK.md) | Practice exercises |

### Learning Resources

| Document | Description |
|----------|-------------|
| [Workflow Patterns Learning Path](WORKFLOW_PATTERNS_LEARNING_PATH.md) | Structured learning path for patterns |
| [YAWL Compile Complete Guide](YAWL_COMPILE_COMPLETE_GUIDE.md) | YAWL compilation to Petri nets |

---

## API Reference

### Complete API Documentation

| Document | Description |
|----------|-------------|
| [Complete API Reference](COMPLETE_API_REFERENCE.md) | Full API documentation for all modules |
| [reference/api_reference.md](reference/api_reference.md) | Reference section API documentation |

### Client API

| Document | Description |
|----------|-------------|
| [Client API Complete Reference](CLIENT_API_COMPLETE_REFERENCE.md) | Comprehensive client API guide |
| [Client API Quick Reference](CLIENT_API_QUICK_REFERENCE.md) | Quick API lookup |
| [CRE Client API Guide](CRE_CLIENT_API_GUIDE.md) | Client library guide |

### Core APIs

| Document | Description |
|----------|-------------|
| [gen_pnet API Specification](yawl_patterns/GEN_PNET_API_SPECIFICATION.md) | gen_pnet behavior API |
| [gen_yawl API Specification](yawl_patterns/GEN_YAWL_API_SPECIFICATION.md) | gen_yawl behavior API |
| [PNET Core Comprehensive Reference](PNET_CORE_COMPREHENSIVE_REFERENCE.md) | Core net behaviors |

---

## Petri Net Core

### Type System

| Document | Description |
|----------|-------------|
| [PNET Types Guide](PNET_TYPES_GUIDE.md) | Type system concepts |
| [PNET Types Tutorial](PNET_TYPES_TUTORIAL.md) | Beginner-Intermediate type system |
| [PNET Types API Reference](PNET_TYPES_API_REFERENCE.md) | Type definitions and validation |
| [PNET Types Quick Reference](PNET_TYPES_QUICK_REFERENCE.md) | Type system quick ref |
| [PNET Types Doctest Reference](PNET_TYPES_DOCTEST_REFERENCE.md) | Examples with tests |

### Marking Algebra

| Document | Description |
|----------|-------------|
| [PNET Marking Tutorial](PNET_MARKING_TUTORIAL.md) | Marking algebra fundamentals |
| [pnet_marking Algebra](pnet_marking_algebra.md) | Multiset marking theory |
| [pnet_marking Implementation](pnet_marking_implementation.md) | Technical implementation details |
| [pnet_marking API Reference](pnet_marking_api_reference.md) | State management API |
| [pnet_marking Quick Reference](pnet_marking_quick_reference.md) | Quick operations lookup |
| [PNET Marking Tests](PNET_MARKING_TESTS.md) | Test documentation |
| [PNET Marking Doctest Reference](PNET_MARKING_DOCTEST_REFERENCE.md) | Examples with tests |

### Mode Enumeration

| Document | Description |
|----------|-------------|
| [PNET Mode Guide](PNET_MODE_GUIDE.md) | Mode enumeration concepts |
| [PNET Mode Tutorial](PNET_MODE_TUTORIAL.md) | Beginner-Intermediate mode handling |
| [PNET Mode Quick Reference](PNET_MODE_QUICK_REFERENCE.md) | Mode enumeration quick ref |
| [PNET Mode Doctest Reference](PNET_MODE_DOCTEST_REFERENCE.md) | Examples with tests |

### Patterns Doctest

| Document | Description |
|----------|-------------|
| [Patterns Doctest Reference](PATTERNS_DOCTEST_REFERENCE.md) | Pattern tests and examples |

---

## Guides

### Integration & Deployment

| Document | Description |
|----------|-------------|
| [Deployment Guide](DEPLOYMENT.md) | Production setup, configuration, monitoring |
| [Helper Integration Guide](HELPER_INTEGRATION_GUIDE.md) | Helper module integration |
| [WF Modules Integration Guide](WF_MODULES_INTEGRATION_GUIDE.md) | Workflow utilities integration |
| [Integration](INTEGRATION.md) | System integration overview |

### Feature Guides

| Document | Description |
|----------|-------------|
| [Human-in-the-Loop](HUMAN_IN_THE_LOOP.md) | Approval workflows with LLM integration |
| [YAWL Telemetry Guide](YAWL_TELEMETRY_GUIDE.md) | Monitoring and telemetry setup |
| [YAWL Timeout Reference](YAWL_TIMEOUT_REFERENCE.md) | Timeout configuration |

### Example Workflows

| Document | Description |
|----------|-------------|
| [Examples Guide](EXAMPLES_GUIDE.md) | Working code examples |
| [Order Fulfillment Guide](ORDER_FULFILLMENT_GUIDE.md) | Real-world workflow example |

### Migration

| Document | Description |
|----------|-------------|
| [Migration Guide](MIGRATION_GUIDE.md) | Upgrade instructions and migration |
| [OTP 25-28 Migration](OTP_25_28_MIGRATION.md) | Erlang/OTP version migration |

---

## Testing & Quality

### Testing

| Document | Description |
|----------|-------------|
| [Testing Guide](TESTING.md) | Comprehensive testing documentation |
| [Test Organization](TEST_ORGANIZATION.md) | Test structure and organization |
| [Test Status](TEST_STATUS.md) | Current test results and coverage |
| [Coverage Report](COVERAGE_REPORT.md) | Test coverage analysis |
| [NATO Concuerror Tests](NATO_CONCUERROR_TESTS.md) | Concurrency testing documentation |

### Verification

| Document | Description |
|----------|-------------|
| [Verification Report](VERIFICATION_REPORT.md) | System verification results |
| [Schema Validation Integration](SCHEMA_VALIDATION_INTEGRATION.md) | Schema validation documentation |

---

## Reference

### Quick References

| Document | Description |
|----------|-------------|
| [Quick Reference Card](QUICK_REFERENCE_CARD.md) | General quick reference |
| [Quick Reference Cheatsheet](QUICK_REFERENCE_CHEATSHEET.md) | Comprehensive cheatsheet |
| [Diagrams Reference](DIAGRAMS_REFERENCE.md) | Architecture and diagram index |

### Utility Modules

| Document | Description |
|----------|-------------|
| [Utility Modules Guide](UTILITY_MODULES_GUIDE.md) | Utility modules usage |

### Release Notes

| Document | Description |
|----------|-------------|
| [Release Notes 2.1.0](RELEASE_NOTES_2.1.0.md) | Version 2.1.0 release notes |

### Development

| Document | Description |
|----------|-------------|
| [Contributing](CONTRIBUTING.md) | Developer contribution guidelines |
| [Build System](BUILD_SYSTEM.md) | Build automation and tools |

### Troubleshooting

| Document | Description |
|----------|-------------|
| [Known Issues](KNOWN_ISSUES.md) | Current known issues and workarounds |
| [Troubleshooting](TROUBLESHOOTING.md) | Common problems and solutions |

---

## Specialized Topics

### Exception Handling

| Document | Description |
|----------|-------------|
| [reference/Exception Handling](reference/EXCEPTION_HANDLING.md) | Exception handling reference |

### Worklet Integration

| Document | Description |
|----------|-------------|
| [worklet_integration_summary.md](worklet_integration_summary.md) | Worklet integration overview |

---

## Subdirectories

| Directory | Description |
|-----------|-------------|
| [tutorials/](tutorials/) | Step-by-step tutorial series |
| [reference/](reference/) | Reference documentation |
| [yawl_patterns/](yawl_patterns/) | YAWL pattern architecture and specs |
| [mermaid-diagrams/](mermaid-diagrams/) | Mermaid diagram source files |
| [examples/](examples/) | Working code examples |
| [patterns/](patterns/) | Workflow pattern documentation (WRP, WDP) |
| [old/](old/) | Archived documentation |

---

## Documentation by Category

### By Experience Level

**Beginner:**
- [Quick Start](QUICK_START.md)
- [tutorials/getting_started.md](tutorials/getting_started.md)

**Intermediate:**
- [tutorials/basic_patterns_tutorial.md](tutorials/basic_patterns_tutorial.md)
- [Core YAWL Patterns Guide](CORE_YAWL_PATTERNS_GUIDE.md)
- [Client API Tutorial](CLIENT_API_TUTORIAL.md)

**Advanced:**
- [Architecture Overview](ARCHITECTURE.md)
- [tutorials/advanced_patterns_tutorial.md](tutorials/advanced_patterns_tutorial.md)
- [Complete API Reference](COMPLETE_API_REFERENCE.md)

### By Task

**Install & Setup:**
- [Quick Start](QUICK_START.md) | [Deployment Guide](DEPLOYMENT.md)

**Learn Patterns:**
- [Workflow Patterns Learning Path](WORKFLOW_PATTERNS_LEARNING_PATH.md) | [Tutorials Index](TUTORIALS_INDEX.md)

**Build Workflows:**
- [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE.md) | [YAWL Compile Guide](YAWL_COMPILE_COMPLETE_GUIDE.md)

**API Integration:**
- [Client API Complete Reference](CLIENT_API_COMPLETE_REFERENCE.md) | [API Reference](API_REFERENCE.md)

**Production:**
- [Deployment Guide](DEPLOYMENT.md) | [Testing Guide](TESTING.md) | [YAWL Telemetry Guide](YAWL_TELEMETRY_GUIDE.md)

---

**Last Updated:** February 2026
