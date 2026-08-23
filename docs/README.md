# CRE - YAWL Workflow Engine Documentation

![Version](https://img.shields.io/badge/v0.3.0-blue)
![OTP](https://img.shields.io/badge/OTP%2025%2B-green)
![License](https://img.shields.io/badge/APACHE%202.0-orange)

Welcome to the official documentation for **CRE (Common Runtime Environment)** - a powerful YAWL workflow engine with human-in-the-loop capabilities, OpenTelemetry observability, and a web-based dashboard.

---

## Table of Contents

- [What is CRE?](#what-is-cre)
- [Quick Start](#quick-start)
- [Documentation Structure](#documentation-structure)
- [Key Resources](#key-resources)
- [Version Compatibility](#version-compatibility)

---

## What is CRE?

CRE is a **YAWL (Yet Another Workflow Language)** workflow engine implemented in Erlang/OTP. Starting from its origins as a Cuneiform runtime environment, CRE has evolved into a comprehensive workflow management system that combines:

- **43 YAWL patterns** for complex workflow modeling
- **Human-in-the-loop approval flows** with LLM integration
- **OpenTelemetry observability** for comprehensive monitoring
- **Web-based dashboard** for workflow visualization
- **XES logging** for audit and compliance
- **Distributed execution** across Erlang clusters

---

## Quick Start

| Resource | Time | Description |
|----------|------|-------------|
| [Quick Start Guide](QUICK_START.md) | 5 min | Get running immediately |
| [Quick Reference](reference/QUICK_REF.md) | 5 min | Essential commands & API |
| [INDEX.md](INDEX.md) | - | Complete documentation catalog |

---

## Documentation Structure

```
docs/
|-- README.md                           # This file - Project overview
|-- INDEX.md                            # Complete documentation index
|
|-- api/                                # API References
|   |-- core/                           # Core API documentation
|   |   |-- COMPLETE_API_REFERENCE.md   # Full API reference
|   |   |-- CLIENT_API_COMPLETE_REFERENCE.md  # Client API
|   |-- patterns/                       # Pattern APIs
|   |   |-- patterns_api.md             # Pattern module APIs
|   |-- mining/                         # Process Mining APIs
|       |-- MINING_MODULES_API_REFERENCE.md  # Mining modules
|
|-- guides/                             # How-to Guides
|   |-- migration/                      # Migration guides
|   |   |-- migration_guide.md          # General migration
|   |   |-- otp_25_28.md                # Erlang/OTP migration
|   |-- human_in_the_loop.md            # Approval workflows
|   |-- telemetry.md                    # Monitoring setup
|   |-- timeout_configuration.md        # Timeout configuration
|   |-- order_fulfillment_example.md    # Real-world example
|   |-- tool_configuration.md           # Tool configuration
|
|-- reference/                          # Reference Materials
|   |-- QUICK_REF.md                    # Consolidated quick reference
|   |-- api_reference.md                # API reference section
|   |-- bibliography.md                 # References and citations
|   |-- EXCEPTION_HANDLING.md           # Exception handling
|   |-- faq.md                          # Frequently asked questions
|   |-- glossary.md                     # Terminology and definitions
|
|-- papers/                             # Research Papers
|   |-- README.md                       # Papers index
|   |-- PAPER_SUMMARIES.md              # Paper summaries
|   |-- paper_algorithm_mapping.csv    # Algorithm mapping
|   |-- *.pdf                           # Process mining papers
|
|-- patterns/                           # YAWL Pattern Documentation
|   |-- PATTERN_IMPLEMENTATION_GUIDE.md # Implementation guide
|   |-- ADVANCED_PATTERNS.md            # Advanced patterns
|   |-- WDP_PATTERNS.md                 # Data patterns
|   |-- WRP_PATTERNS.md                 # Resource patterns
|   |-- anti_patterns_guide.md          # Anti-patterns
|
|-- pnet/                               # Petri Net Core
|   |-- types/                          # Type system docs
|   |-- marking/                        # Marking algebra docs
|   |-- mode/                           # Mode enumeration docs
|
|-- tutorials/                          # Tutorial Series
|   |-- getting_started.md              # First workflow
|   |-- basic_patterns_tutorial.md      # Basic patterns
|   |-- advanced_patterns_tutorial.md   # Advanced patterns
|   |-- HANDLER_DEVELOPMENT.md          # Handler development
|
|-- operations/                         # Operations & Support
|   |-- testing/                        # Testing documentation
|   |-- performance/                    # Performance tuning
|   |-- troubleshooting/                # Troubleshooting guides
|
|-- architecture/                       # Architecture Documentation
|   |-- system-overview.md              # System architecture
|   |-- design_principles.md            # Design principles
|   |-- diagrams/                       # Architecture diagrams
|
|-- diagrams/                           # Diagram Collections
|   |-- c4/                             # C4 model diagrams
|   |-- mermaid/                        # Mermaid diagram sources
|
|-- examples/                           # Code Examples
|-- example_workflows/                  # Workflow YAML examples
|-- training/                           # Training Materials
|-- case-studies/                       # Case Study Documentation
|-- development/                        # Developer Resources
|-- planning/                           # Planning & Roadmap
|-- rust/                               # Rust Modules
|-- features/                           # Feature Documentation
|-- analysis/                           # Technical Analysis
|-- yawl_patterns/                      # YAWL Architecture
|-- verification_scripts/               # Verification Scripts
|-- archive/                            # Archived Documentation
|   |-- tutorials/                      # Superseded tutorials
|   |-- papers/                         # Old paper analysis
|   |-- architecture/                   # Duplicate architecture docs
|   |-- reference/                      # Consolidated references
|   |-- patterns/                       # Old pattern docs
|   `-- yawl/                           # Superseded YAWL docs
|-- old/                                # Legacy archived files
```

---

## Key Resources

### New Users
- [Quick Start Guide](QUICK_START.md) - Get running in 5 minutes
- [Quick Reference](reference/QUICK_REF.md) - Essential commands & API
- [Basic Examples](../examples/) - Working code examples

### Developers
- [Complete API Reference](api/core/COMPLETE_API_REFERENCE.md) - Full API documentation
- [Architecture Overview](ARCHITECTURE.md) - System design and internals
- [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE.md) - 43 workflow patterns

### Operations
- [Deployment Guide](DEPLOYMENT.md) - Production setup
- [Testing Guide](operations/testing/testing.md) - Testing documentation
- [Troubleshooting](operations/troubleshooting/troubleshooting.md) - Common issues

### Research
- [Process Mining Papers](papers/) - Research paper collection
- [Pattern Catalog](YAWL_PATTERNS_REFERENCE.md) - Complete pattern catalog
- [Archived Documentation](archive/) - Superseded materials

---

## Version Compatibility

| Component | Supported Versions |
|-----------|-------------------|
| **Erlang/OTP** | 25.0 - 28.x |
| **CRE Client API** | Compatible with all versions |
| **Worker Nodes** | Compatible with all versions |
| **Web Dashboard** | Requires modern browser (ES6+) |
| **OpenTelemetry** | OTLP 1.0+ compatible |

---

## Key Features

| Feature | Description | Release |
|---------|-------------|---------|
| **YAWL Patterns** | Complete set of 43 workflow patterns | v0.3.0 |
| **Human-in-the-Loop** | Approval workflows with LLM integration | v0.3.0 |
| **OpenTelemetry** | Structured logging and metrics | v0.3.0 |
| **Web Dashboard** | Real-time visualization | v0.3.0 |
| **XES Logging** | Event log standard | v0.3.0 |
| **OTP 25+ Support** | Modern Erlang/OTP | v0.3.0 |

---

## Support

- **Documentation Index**: See [INDEX.md](INDEX.md) for complete catalog
- **FAQ**: [reference/faq.md](reference/faq.md)
- **Glossary**: [reference/glossary.md](reference/glossary.md)
- **Contributing**: [development/contributing.md](development/contributing.md)

---

**Last Updated:** 2026-02-08 | **Version:** 0.3.0 | **Pattern Count:** 43 YAWL patterns
