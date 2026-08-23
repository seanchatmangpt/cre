# START HERE - CRE Documentation Navigation Hub

**CRE (Common Runtime Environment)** - YAWL Workflow Engine on Erlang/OTP
**Version:** 0.3.0 | **OTP Support:** 25.0 - 28.x | **Last Updated:** 2026-02-09

---

## Welcome to CRE!

CRE is a **YAWL (Yet Another Workflow Language)** workflow engine implemented in Erlang/OTP. It provides:

- **36 of 43 YAWL Patterns** for complex workflow modeling
- **Human-in-the-loop approval flows** with LLM integration
- **OpenTelemetry observability** for comprehensive monitoring
- **Web-based dashboard** for real-time workflow visualization
- **Distributed execution** across Erlang clusters
- **Process mining capabilities** with AI/ML integration

---

## Find Your Path

Choose your role below to get personalized guidance:

---

### "I'm a Developer..."

> I want to build workflows, integrate CRE into my applications, and understand the API.

**Your Quick Start Path:**

| Step | Document | Time | What You'll Learn |
|------|----------|------|-------------------|
| 1 | [Quick Start Guide](QUICK_START.md) | 5 min | Install CRE and run your first workflow |
| 2 | [Getting Started Tutorial](tutorials/getting_started.md) | 30 min | Create your first YAWL workflow |
| 3 | [Basic Patterns Tutorial](tutorials/basic_patterns_tutorial.md) | 60 min | Learn essential workflow patterns |
| 4 | [Client API Reference](api/core/CLIENT_API_COMPLETE_REFERENCE.md) | 45 min | Complete API documentation |
| 5 | [Examples](../examples/) | - | Working code examples |

**Essential Developer Resources:**

- [Complete API Reference](api/core/COMPLETE_API_REFERENCE.md) - Full API documentation
- [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE.md) - All 43 workflow patterns
- [Pattern Implementation Guide](patterns/PATTERN_IMPLEMENTATION_GUIDE.md) - How to implement patterns
- [Human-in-the-Loop Guide](guides/human_in_the_loop.md) - Approval workflows
- [Telemetry Integration](guides/telemetry.md) - Monitoring and observability

**Common Developer Tasks:**

| Task | Document |
|------|----------|
| Integrate CRE into my app | [Utility Modules Guide](UTILITY_MODULES_GUIDE.md) |
| Add approval workflows | [Human-in-the-Loop](guides/human_in_the_loop.md) |
| Configure timeouts | [Timeout Configuration](guides/timeout_configuration.md) |
| Process mining | [Mining API Reference](MINING_API_REFERENCE.md) |
| Debug workflows | [Debugging Guide](operations/troubleshooting/debugging.md) |

---

### "I'm a Researcher..."

> I'm interested in workflow patterns, process mining algorithms, Petri nets, and the theoretical foundations.

**Your Research Path:**

| Step | Document | Time | What You'll Learn |
|------|----------|------|-------------------|
| 1 | [Paper Index](papers/README.md) | 10 min | Overview of research collection |
| 2 | [Pattern Catalog](YAWL_PATTERNS_REFERENCE.md) | 30 min | Complete pattern implementations |
| 3 | [Petri Net Core](GEN_PNET_USER_GUIDE.md) | 45 min | Core Petri net runtime behavior |
| 4 | [Algorithm Mapping](papers/paper_algorithm_mapping.csv) | - | Paper to implementation mapping |

**Research Resources:**

- [Process Mining Papers](papers/README.md) - 166+ research papers (van der Aalst collection)
- [Paper Summaries](papers/PAPER_SUMMARIES.md) - Summarized key papers
- [YAWL Pattern Reference](YAWL_PATTERNS_REFERENCE.md) - Complete pattern semantics
- [Petri Net Theory](PNET_CORE_COMPREHENSIVE_REFERENCE.md) - Theoretical foundations
- [Pattern Analysis](analysis/) - Technical analysis documents

**Key Research Topics:**

| Topic | Document |
|-------|----------|
| Workflow patterns | [YAWL Patterns Reference](YAWL_PATTERNS_REFERENCE.md) |
| Process discovery | [papers/analysis_summary.md](papers/analysis_summary.md) |
| Petri net algebra | [pnet/marking/algebra.md](pnet/marking/algebra.md) |
| Conformance checking | [papers/conformance_checking_uncertain_event_data_2020.pdf](papers/conformance_checking_uncertain_event_data_2020.pdf) |
| Object-centric mining | [papers/analysis_summary.md](papers/analysis_summary.md) |

---

### "I'm Deploying to Production..."

> I need to set up CRE for production, configure clustering, monitoring, and ensure reliability.

**Your Production Path:**

| Step | Document | Time | What You'll Learn |
|------|----------|------|-------------------|
| 1 | [Deployment Guide](DEPLOYMENT.md) | 30 min | Production setup and configuration |
| 2 | [Architecture Overview](ARCHITECTURE.md) | 45 min | System design and components |
| 3 | [Monitoring Guide](monitoring.md) | 20 min | OpenTelemetry and metrics |
| 4 | [Testing Documentation](operations/testing/testing.md) | 30 min | Verification and testing |
| 5 | [Troubleshooting](operations/troubleshooting/troubleshooting.md) | 20 min | Common issues and solutions |

**Production Resources:**

- [Deployment Guide](DEPLOYMENT.md) - Complete production setup
- [Monitoring & Observability](monitoring.md) - Metrics and logging
- [Performance Tuning](operations/performance/performance.md) - Optimization guide
- [Testing Documentation](operations/testing/testing.md) - Verification procedures
- [Known Issues](operations/troubleshooting/known_issues.md) - Current limitations

**System Requirements:**

| Resource | Minimum | Recommended |
|----------|---------|-------------|
| CPU | 2 cores | 8+ cores |
| Memory | 4 GB | 16+ GB |
| Erlang/OTP | 25.0 | 27.x or 28.x |
| Disk | 20 GB SSD | 100+ GB SSD |

**Production Checklist:**

- [ ] OTP version compatibility verified (25-28)
- [ ] System requirements met
- [ ] OpenTelemetry configured
- [ ] Monitoring dashboards set up
- [ ] Backup procedures documented
- [ ] High availability configured
- [ ] Security hardening applied

---

### "I Want to Contribute..."

> I want to contribute code, improve documentation, or fix bugs in CRE.

**Your Contributor Path:**

| Step | Document | Time | What You'll Learn |
|------|----------|------|-------------------|
| 1 | [Contributing Guide](development/contributing.md) | 15 min | Contribution guidelines |
| 2 | [Build System](development/build_system.md) | 20 min | Compilation and testing |
| 3 | [Architecture Overview](ARCHITECTURE.md) | 45 min | System design principles |
| 4 | [Testing Guide](operations/testing/testing.md) | 30 min | How to write tests |
| 5 | [WIP Items](WIP_ITEMS.md) | 10 min | Current work items |

**Contributor Resources:**

- [Contributing Guide](development/contributing.md) - Guidelines and workflows
- [Build System](development/build_system.md) - rebar3 and compilation
- [Testing Documentation](operations/testing/testing.md) - Test structure and procedures
- [Code Style](.claude/rules/erlang.md) - Erlang conventions
- [WIP Items](WIP_ITEMS.md) - Current work-in-progress items

**Quick Contribution Tasks:**

| Task | Document |
|------|----------|
| Fix a bug | [WIP Items](WIP_ITEMS.md), [GitHub Issues](https://github.com/joergen7/cre/issues) |
| Add documentation | [INDEX.md](INDEX.md), [README.md](README.md) |
| Write tests | [Test Gaps](TEST_GAPS.md), [testing.md](operations/testing/testing.md) |
| Implement pattern | [Pattern Status](PATTERN_STATUS.md), [43 Patterns](43_PATTERNS_COMPLETE.md) |
| Improve coverage | [Type Gaps](TYPE_GAPS.md) |

**Development Commands:**

```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit     # Unit tests
rebar3 ct        # Integration tests

# Type analysis
rebar3 dialyzer

# Format check
rebar3 efmt -c

# Generate coverage
rebar3 cover
```

---

## Quick Reference

### Essential Commands

| Command | Description |
|---------|-------------|
| `rebar3 compile` | Compile the project |
| `rebar3 eunit` | Run unit tests |
| `rebar3 ct` | Run integration tests |
| `rebar3 shell` | Start interactive shell |
| `rebar3 dialyzer` | Type analysis |

### Key Documents by Topic

| Topic | Document |
|-------|----------|
| **Getting Started** | [Quick Start](QUICK_START.md) |
| **API Reference** | [Complete API](api/core/COMPLETE_API_REFERENCE.md) |
| **Patterns** | [YAWL Patterns](YAWL_PATTERNS_REFERENCE.md) |
| **Deployment** | [Deployment Guide](DEPLOYMENT.md) |
| **Research** | [Papers Index](papers/README.md) |
| **Contributing** | [Contributing Guide](development/contributing.md) |
| **Full Index** | [INDEX.md](INDEX.md) |

---

## Learning Paths

### Path 1: Complete Beginner (3-5 days)

```
Day 1:   Quick Start + Getting Started Tutorial
Day 2:   Basic Patterns Tutorial + Core Patterns Guide
Day 3:   API Reference + Pattern Workbook
Day 4:   Human-in-the-Loop + Examples
Day 5:   Deployment + Testing
```

### Path 2: Pattern Expert (2-3 days)

```
Day 1:   YAWL Patterns Reference + Pattern Catalog
Day 2:   Advanced Patterns Tutorial + Pattern Implementation Guide
Day 3:   YAWL Compilation Guide + Petri Net Core
```

### Path 3: Production Engineer (1-2 days)

```
Day 1:   Deployment Guide + Architecture Overview + Monitoring
Day 2:   Testing + Troubleshooting + Performance Tuning
```

---

## Where to Get Help

| Resource | Description |
|----------|-------------|
| [FAQ](reference/faq.md) | Frequently asked questions |
| [Glossary](reference/glossary.md) | Terminology and definitions |
| [Troubleshooting](operations/troubleshooting/troubleshooting.md) | Common problems and solutions |
| [GitHub Issues](https://github.com/joergen7/cre/issues) | Bug reports and feature requests |
| [Documentation Index](INDEX.md) | Complete documentation catalog |

---

## Version Information

| Component | Version |
|-----------|---------|
| **CRE** | 0.3.0 |
| **Erlang/OTP** | 25.0 - 28.x |
| **YAWL Patterns** | 36 of 43 implemented |
| **Test Pass Rate** | 90.7% (689/760 tests) |

---

## Next Steps

1. Choose your path above based on your role
2. Follow the recommended documents in order
3. Try the examples in the [`examples/`](../examples/) directory
4. Refer to [INDEX.md](INDEX.md) for complete documentation catalog
5. Check the [README.md](../README.md) for project overview

---

**Need help?** Start with the [FAQ](reference/faq.md) or [Troubleshooting Guide](operations/troubleshooting/troubleshooting.md).

**Enjoy using CRE!** For the latest updates, visit the [GitHub repository](https://github.com/joergen7/cre).
