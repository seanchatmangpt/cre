# CRE API Documentation

This directory contains consolidated API documentation for all CRE modules, organized by domain.

---

## Quick Navigation

| Document | Description |
|----------|-------------|
| **[patterns.md](patterns.md)** | All 43+ workflow pattern modules (control flow, branching, cancellation, data, resource, ML) |
| **[mining.md](mining.md)** | Process mining APIs (discovery, conformance, anomaly detection, prediction) |
| **[workflow.md](workflow.md)** | Core APIs (gen_pnet, gen_yawl, YAWL compilation) and workflow utilities |

---

## Pattern APIs

**[patterns.md](patterns.md)** - Complete API reference for all workflow pattern modules implementing the Workflow Patterns Initiative catalog.

### Pattern Categories

- **Control Flow Patterns** - Sequence, parallel split, synchronization, exclusive choice, simple merge, multiple choice, N-of-M join
- **Advanced Branching Patterns** - Structured partial join, generalized AND-join, blocking/cancelling partial joins
- **Cancellation Patterns** - Cancel activity, cancel case, cancel region, discriminator patterns
- **Data Patterns** - Data transformation, distribution, accumulation, visibility
- **Resource Patterns** - Resource creation, allocation, deallocation, role-based allocation
- **Multiple Instance Patterns** - WCP12-WCP14 multiple instances with and without synchronization
- **RL Strategy Patterns** - Q-learning, UCB, Thompson Sampling, contextual strategies
- **Utility Patterns** - Critical section, milestone, circuit breaker, arbitrary cycles

### Key Modules

| Module | Pattern | Description |
|--------|---------|-------------|
| `sequence` | WCP-01 | Sequential execution |
| `parallel_split` | WCP-02 | Parallel branch execution |
| `synchronization` | WCP-03 | AND-join for parallel branches |
| `exclusive_choice` | WCP-04 | Conditional branch selection |
| `multi_instance` | WCP12-14 | Multiple instance patterns |
| `circuit_breaker` | Fault Tolerance | Prevent cascading failures |
| `milestone` | WCP-18 | Enable activity on milestone |
| `critical_section` | WCP-26 | Mutual exclusion |
| `strategy_thompson_sampling` | RL | Bayesian branch selection |

---

## Mining APIs

**[mining.md](mining.md)** - Process mining module APIs for anomaly detection and predictive monitoring.

### Mining Categories

- **Process Discovery** - Alpha algorithm, heuristic miner, frequency-based discovery
- **Conformance Checking** - Token replay, fitness/precision scores, alignment
- **Anomaly Detection** - Real-time and batch detection with classification and alerting
- **Predictive Mining** - Next activity prediction, remaining time estimation, outcome prediction

### Key Modules

| Module | Category | Description |
|--------|----------|-------------|
| `alpha_algorithm` | Discovery | Alpha algorithm for WF-net discovery |
| `process_discovery` | Discovery | Enhanced discovery with noise handling |
| `conformance` | Checking | Token replay and alignment |
| `anomaly_detection` | Detection | Real-time/batch anomaly detection |
| `predictive_mining` | Prediction | Main prediction API |
| `pred_rnn` | Prediction | Pure Erlang RNN implementation |
| `pred_stats` | Prediction | Markov, EMA, linear regression |
| `pred_training` | Training | Training data collection |
| `anomaly_statistics` | Analysis | Statistical calculations |

---

## Workflow APIs

**[workflow.md](workflow.md)** - Core APIs for workflow execution and client interaction.

### Core Categories

- **Core OTP Behaviors** - gen_pnet and gen_yawl behavior modules
- **YAWL Compilation** - YAWL spec compiler and validators
- **PNET Pure Helpers** - Type definitions, marking algebra, mode enumeration
- **Workflow Utilities** - Timer, scope, task, audit, pool utilities
- **Client API** - cre_client and cre_yawl_client for workflow execution

### Key Modules

| Module | Category | Description |
|--------|----------|-------------|
| `gen_pnet` | Core Behavior | Generic Petri net behavior |
| `gen_yawl` | Core Behavior | YAWL wrapper with enhanced fire/3 |
| `yawl_compile` | Compilation | YAWL spec compiler |
| `yawl_validate` | Validation | YAWL spec validator |
| `pnet_marking` | PNET Helper | Marking algebra operations |
| `pnet_mode` | PNET Helper | Mode enumeration |
| `cre_client` | Client API | Generic client gen_server |
| `cre_yawl_client` | Client API | YAWL-specific client |

---

## Related Documentation

- [GEN_PNET_USER_GUIDE.md](../GEN_PNET_USER_GUIDE.md) - Core Petri net runtime guide
- [GEN_YAWL_API_SPECIFICATION.md](../yawl_patterns/GEN_YAWL_API_SPECIFICATION.md) - gen_yawl behavior API
- [GEN_PNET_API_SPECIFICATION.md](../yawl_patterns/GEN_PNET_API_SPECIFICATION.md) - gen_pnet behavior API
- [reference/api_reference.md](../reference/api_reference.md) - Reference section documentation

---

## Directory Structure

```
api/
|-- README.md                            # This file (consolidated index)
|-- patterns.md                          # All 43+ workflow patterns
|-- mining.md                            # Process mining APIs
|-- workflow.md                          # Core APIs and utilities
|-- core/                                # (Legacy - content consolidated)
|-- mining/                              # (Legacy - content consolidated)
|-- patterns/                            # (Legacy - content consolidated)
```

---

**Last Updated:** 2026-02-09
