# YAWL Workflow Tutorials

**Learn YAWL and gen_pnet step-by-step**

---

## Tutorial Overview

This tutorial series provides comprehensive, hands-on learning for the YAWL (Yet Another Workflow Language) system built on gen_pnet. The tutorials follow the Diataxis framework, combining learning-oriented content with practical examples.

## Tutorial Series

| Tutorial | Duration | Prerequisites | Description |
|----------|----------|---------------|-------------|
| [1. Getting Started](getting_started.md) | 30 min | None | Your first YAWL workflow |
| [2. Basic Patterns](basic_patterns_tutorial.md) | 60 min | Tutorial 1 | WCP-01 to WCP-06 patterns |
| [3. Advanced Patterns](advanced_patterns_tutorial.md) | 90 min | Tutorial 2 | WCP-07 to WCP-20 patterns |
| [4. Colored Tokens](colored_tokens_tutorial.md) | 45 min | Tutorial 1 | Data-carrying tokens |
| [5. Pattern Examples](PATTERN_EXAMPLES.md) | 45 min | Tutorial 2 | Real-world pattern implementations |
| [6. Handler Development](HANDLER_DEVELOPMENT.md) | 60 min | Tutorial 2 | Business logic integration |
| [7. Migration Guide](workflow_migration_tutorial.md) | 60 min | Tutorial 1 | Migrate from gen_statem |
| [8. Study Plan Template](YAWL_PATTERNS_STUDY_PLAN_TEMPLATE.md) | 3-6 hours | None | Personalized YAWL learning path |

---

## Learning Path

```
Beginner                     Intermediate                  Advanced
    |                            |                            |
    v                            v                            v
┌─────────────┐           ┌─────────────┐           ┌─────────────┐
│  Getting   │─────────>│  Basic      │─────────>│  Advanced   │
│  Started   │           │  Patterns   │           │  Patterns   │
└─────────────┘           └─────────────┘           └─────────────┘
       |                                                        |
       v                                                        v
┌─────────────┐                                         ┌─────────────┐
│  Colored    │                                         │  Migration  │
│  Tokens     │                                         │  Guide      │
└─────────────┘                                         └─────────────┘
```

---

## Quick Start

If you're new to YAWL and gen_pnet, start here:

1. **Install CRE** (2 minutes)
   ```bash
   git clone https://github.com/your-org/cre.git
   cd cre
   rebar3 compile
   ```

2. **Run Tutorial 1** (15 minutes)
   ```erlang
   c(hello_yawl).
   hello_yawl:run().
   ```

3. **Build Your First Workflow** (15 minutes)
   ```erlang
   c(approval_workflow).
   approval_workflow:run().
   ```

---

## By Learning Objective

### Learn the Basics
- Start with [Getting Started](getting_started.md)
- Learn Petri net fundamentals
- Build your first workflow

### Master Patterns
- Complete [Basic Patterns Tutorial](basic_patterns_tutorial.md)
- Learn sequence, parallel split, synchronization, choice
- Practice with exercises

### Handle Complex Workflows
- Work through [Advanced Patterns Tutorial](advanced_patterns_tutorial.md)
- Implement multiple instance patterns
- Use state-based patterns

### Work with Data
- Study [Colored Tokens Tutorial](colored_tokens_tutorial.md)
- Implement data flow patterns
- Transform tokens between tasks

### Migrate Existing Code
- Follow [Migration Guide](workflow_migration_tutorial.md)
- Convert gen_statem to gen_pnet
- Run dual-mode for testing

---

## Tutorial Structure

Each tutorial follows this structure:

1. **Learning Objectives** - What you'll accomplish
2. **Concepts** - Key terminology and ideas
3. **Step-by-Step Instructions** - Hands-on learning
4. **Code Examples** - Complete, runnable code
5. **Exercises** - Practice what you learned
6. **Summary** - Key takeaways

---

## Prerequisites

### Before Starting

- **Erlang/OTP** 25.0 or later
- **rebar3** 3.18.0 or later
- **Basic Erlang knowledge** (modules, functions, pattern matching)
- **Terminal access** for running commands

### Helpful but Not Required

- Knowledge of Petri nets
- Experience with workflow systems
- Understanding of OTP behaviors

---

## Getting Help

### During Tutorials

If you get stuck:

1. **Check the examples** - Each tutorial has complete code examples
2. **Review the error** - Error messages often point to the issue
3. **Verify prerequisites** - Ensure Erlang/OTP and rebar3 are correct
4. **Consult references** - See sidebar documentation links

### Additional Resources

- [YAWL Patterns Reference](../YAWL_PATTERNS_REFERENCE.md) - Complete pattern catalog
- [gen_pnet API Specification](../yawl_patterns/GEN_PNET_API_SPECIFICATION.md) - API details
- [gen_pnet Architecture](../yawl_patterns/GEN_PNET_INTEGRATION_ARCHITECTURE.md) - System design
- [Examples](../examples/) - Working code examples

---

## Tutorial Files

The tutorial files are located in `/docs/tutorials/`:

```
docs/tutorials/
├── README.md                        (this file)
├── getting_started.md               (beginner tutorial)
├── basic_patterns_tutorial.md       (basic patterns)
├── advanced_patterns_tutorial.md    (advanced patterns)
├── colored_tokens_tutorial.md       (data flow)
├── workflow_migration_tutorial.md   (migration guide)
└── YAWL_PATTERNS_STUDY_PLAN_TEMPLATE.md (personalized learning path)
```

---

## Exercise Solutions

Solutions to tutorial exercises are available in `/docs/examples/tutorial_solutions/`:

```
docs/examples/tutorial_solutions/
├── ex01_sequence.erl
├── ex02_parallel.erl
├── ex03_sync_merge.erl
├── ex04_multi_instance.erl
└── ...
```

---

## Contributing

Found an issue or improvement? The tutorials are part of the CRE project:

1. Check existing issues
2. Create a detailed bug report
3. Submit a pull request

---

## What You'll Learn Across All Tutorials

After completing all tutorials, you will be able to:

- **Design workflows** using YAWL patterns
- **Implement patterns** with gen_pnet
- **Handle data flow** with colored tokens
- **Debug workflows** using token inspection
- **Migrate code** from legacy YAWL
- **Optimize performance** for your use case

---

## Teaching with These Tutorials

If you're teaching YAWL to others:

- Each tutorial is self-contained
- Exercises can be done independently
- Solutions are provided for verification
- Estimated times help with planning

---

## Changelog

### Version 1.0.0 (2026-02-05)
- Initial tutorial release
- 5 complete tutorials
- Diataxis framework structure
- 20+ exercises with solutions

---

**Ready to start?** Begin with [Getting Started](getting_started.md)
