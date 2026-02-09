# CRE - YAWL Workflow Engine Documentation

![Version](https://img.shields.io/badge/v0.3.0-blue)
![OTP](https://img.shields.io/badge/OTP%2025%2B-green)
![License](https://img.shields.io/badge/APACHE%202.0-orange)

Welcome to the official documentation for CRE (Cuneiform Runtime Environment) - a powerful YAWL workflow engine with human-in-the-loop capabilities, OpenTelemetry observability, and a web-based dashboard.

## 🎯 What is CRE?

CRE is a **YAWL (Yet Another Workflow Language)** workflow engine implemented in Erlang/OTP. Starting from its origins as a Cuneiform runtime environment, CRE has evolved into a comprehensive workflow management system that combines:

- **43 YAWL patterns** for complex workflow modeling
- **Human-in-the-loop approval flows** with LLM integration
- **OpenTelemetry observability** for comprehensive monitoring
- **Web-based dashboard** for workflow visualization
- **XES logging** for audit and compliance
- **Distributed execution** across Erlang clusters

## ✨ Key Features

| Feature | Description | Release |
|---------|-------------|---------|
| **YAWL Patterns** | Complete set of 43 workflow patterns for process modeling | v0.3.0 |
| **Human-in-the-Loop** | Approval workflows with LLM integration and checkpointing | v0.3.0 |
| **OpenTelemetry** | Structured logging, metrics, and distributed tracing | v0.3.0 |
| **Web Dashboard** | Real-time workflow visualization and monitoring | v0.3.0 |
| **XES Logging** | Event log standard for process mining and analysis | v0.3.0 |
| **OTP 25+ Support** | Modern Erlang/OTP with improved performance | v0.3.0 |

## 📚 Where to Start?

See [INDEX.md](./INDEX.md) for the complete documentation index with all resources organized by category and learning path.

### 🚀 New Users (5 minutes)
- **Start here**: [Quick Start Guide](./QUICK_START.md) - Get running immediately
- **Documentation Index**: [INDEX.md](./INDEX.md) - Complete documentation catalog
- **Try the examples**: [Basic Examples](../examples/)

### 🏗️ Developers & Architects
- **API Reference**: [Complete API Documentation](./COMPLETE_API_REFERENCE.md)
- **System Design**: [Architecture Overview](./ARCHITECTURE.md)
- **Patterns Guide**: [YAWL Patterns Reference](./YAWL_PATTERNS_REFERENCE.md)
- **Deployment**: [Production Guide](./DEPLOYMENT.md)

### 📖 Specific Topics
- **Human-in-the-Loop**: [Approval Workflows](./HUMAN_IN_THE_LOOP.md)
- **Telemetry**: [OpenTelemetry Integration](./YAWL_TELEMETRY_GUIDE.md)
- **Migration**: [OTP 25-28 Migration](./OTP_25_28_MIGRATION.md)

## 🔗 Documentation Map

For a complete documentation catalog, see [INDEX.md](./INDEX.md).

```
docs/
├── README.md                           # This file - Project overview
├── INDEX.md                            # 📑 Complete documentation index
├── QUICK_START.md                     # 🚀 5-minute getting started
├── COMPLETE_API_REFERENCE.md          # 📖 Complete API reference
├── ARCHITECTURE.md                    # 🏗️ System architecture
├── DEPLOYMENT.md                      # 🚀 Production deployment
├── CONTRIBUTING.md                    # 🤝 Developer guide
├── YAWL_PATTERNS_REFERENCE.md         # 🔍 Pattern details
├── YAWL_PATTERN_EXAMPLES.md          # 💯 Pattern examples
├── YAWL_TELEMETRY_GUIDE.md          # 📊 Monitoring setup
├── HUMAN_IN_THE_LOOP.md               # 👥 Human approval flows
├── tutorials/                         # 📚 Step-by-step tutorials
├── patterns/                          # 🔧 Advanced pattern documentation
├── reference/                         # 📖 Reference documentation
├── yawl_patterns/                     # 🎯 YAWL pattern architecture
├── diagrams/                          # 📊 Architecture diagrams
├── examples/                          # 💻 Working code examples
└── old/                               # 📦 Archived docs
```

## 🎯 Version Compatibility

| Component | Supported Versions |
|-----------|-------------------|
| **Erlang/OTP** | 25.0 - 28.x |
| **CRE Client API** | Compatible with all versions |
| **Worker Nodes** | Compatible with all versions |
| **Web Dashboard** | Requires modern browser (ES6+) |
| **OpenTelemetry** | OTLP 1.0+ compatible |

## 🚀 Quick Links

### Core Features
- [43 YAWL Patterns](./YAWL_PATTERNS_REFERENCE.md) - Complete workflow pattern library
- [Human-in-the-Loop Workflows](./HUMAN_IN_THE_LOOP.md) - Approval flows with LLM integration
- [OpenTelemetry Integration](./YAWL_TELEMETRY_GUIDE.md) - Observability and monitoring
- [Web Dashboard](https://github.com/your-org/cre-dashboard) - Real-time visualization

### Getting Started
- [Quick Start Tutorial](./QUICK_START.md) - Your first workflow in 5 minutes
- [Basic Examples](../examples/basic_workflow.erl) - Simple workflow implementation
- [API Reference](./API_REFERENCE.md) - Complete function documentation
- [Architecture Overview](./ARCHITECTURE.md) - System design and internals

### Production
- [Deployment Guide](./DEPLOYMENT.md) - Production setup and scaling
- [Performance Tuning](./DEPLOYMENT.md#performance-tuning) - Optimization guide
- [Security Considerations](./DEPLOYMENT.md#security) - Hardening guidelines

### Community & Support
- [GitHub Issues](https://github.com/your-org/cre/issues) - Bug reports and features
- [Contributing Guide](./CONTRIBUTING.md) - How to contribute code
- [Migration Guide](./MIGRATION.md) - Upgrade instructions

## 📝 What Happened to Cuneiform?

CRE was originally developed as the **Cuneiform runtime environment** for distributed programming. Starting with **v0.3.0**, CRE has evolved into a YAWL workflow engine while maintaining backward compatibility with the original CRE client/worker APIs. The core execution engine remains compatible, but the focus has shifted to workflow modeling and human-in-the-loop processes.

> **Note**: All existing CRE applications continue to work without changes. The evolution adds YAWL patterns on top of the existing foundation.

## 🎯 Questions?

- **Documentation Issues**: [Report on GitHub](https://github.com/your-org/cre/issues)
- **Technical Questions**: [Stack Overflow](https://stackoverflow.com/questions/tagged/cre-workflow)
- **Discussions**: [GitHub Discussions](https://github.com/your-org/cre/discussions)

---

## 📊 CRE v0.3.0 Highlights

- ✅ **43 YAWL patterns** - Complete pattern library for complex workflows
- ✅ **Human-in-the-loop** - Approval workflows with LLM integration
- ✅ **OpenTelemetry** - Structured logging and metrics
- ✅ **Web dashboard** - Real-time workflow visualization
- ✅ **XES logging** - Standard event logging for process mining
- ✅ **OTP 28 support** - Latest Erlang/OTP compatibility
- ✅ **Improved timeouts** - Better resource management

**Ready to start?** Head over to the [Quick Start Guide](./QUICK_START.md) and build your first workflow!