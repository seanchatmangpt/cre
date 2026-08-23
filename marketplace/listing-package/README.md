# CRE Documentation for Google Cloud Marketplace

Welcome to **CRE (Common Runtime Environment)** on Google Cloud Marketplace!

CRE is a production-grade **YAWL workflow engine** built on Erlang/OTP, implementing **36 of 43 YAWL workflow patterns** with human-in-the-loop approvals, OpenTelemetry observability, and fault-tolerant distributed execution.

## Quick Links

- **[Product Overview](overview.md)** - What is CRE and why use it
- **[Architecture](architecture.md)** - System design and components
- **[Deployment Guide](deployment-guide.md)** - Step-by-step deployment instructions
- **[Operations Guide](operations-guide.md)** - Scaling, backup, troubleshooting
- **[Security Model](security-model.md)** - Security architecture and compliance
- **[Cost Model](cost-model.md)** - Pricing and cost optimization

## What is CRE?

CRE (Common Runtime Environment) is a comprehensive workflow management system that enables:

- **Complex Workflow Orchestration** - 36 YAWL patterns for sequence, parallel, choice, loop, and multi-instance workflows
- **Human-in-the-Loop Approvals** - Integrate LLM-powered human decisions directly into workflows
- **Fault-Tolerant Execution** - Automatic failure recovery and task rescheduling
- **Distributed Scaling** - Scale horizontally across Erlang clusters
- **Full Observability** - OpenTelemetry integration for metrics, traces, and logs
- **Web Dashboard** - Real-time workflow visualization and monitoring

### Key Features

| Feature | Description |
|---------|-------------|
| **36 YAWL Patterns** | Extensive workflow pattern library (sequence, parallel split, synchronization, exclusive choice, multi-choice, loops, milestones, critical sections, and more) |
| **Human Approvals** | Built-in approval workflows with LLM integration |
| **OpenTelemetry** | Comprehensive observability with Cloud Logging, Cloud Monitoring, and Cloud Trace |
| **Fault Tolerance** | Automatic failure recovery and task rescheduling |
| **Distributed Execution** | Scale across Erlang clusters with automatic load balancing |
| **XES Logging** | Standardized event logging for process mining |
| **OTP 25-28 Support** | Modern Erlang/OTP with improved performance |

### Use Cases

- **Business Process Automation** - Automate complex business processes with human approvals
- **Data Processing Pipelines** - Build fault-tolerant data workflows with automatic retry
- **Workflow Orchestration** - Coordinate microservices and distributed systems
- **Process Mining** - XES logging for compliance and process optimization
- **Approval Workflows** - Human-in-the-loop decisions with audit trails

## Deployment Model

CRE is available on Google Cloud Marketplace as a **BYOL (Bring Your Own License)** solution:

- **License**: Apache 2.0 (open source, no additional licensing fees)
- **Infrastructure Costs**: You pay only for GCP resources consumed (GKE, Cloud Spanner, etc.)
- **Support**: Community support via GitHub Issues (48-hour response time)

## Documentation Structure

This directory contains all documentation for deploying and operating CRE on Google Cloud Marketplace:

### Customer Documentation

- **[overview.md](overview.md)** - Product overview, features, and use cases
- **[architecture.md](architecture.md)** - System architecture, components, and data flow
- **[deployment-guide.md](deployment-guide.md)** - Complete deployment guide for GKE
- **[operations-guide.md](operations-guide.md)** - Scaling, backup, restore, and troubleshooting
- **[security-model.md](security-model.md)** - Security architecture, compliance, and best practices
- **[cost-model.md](cost-model.md)** - Pricing, infrastructure costs, and optimization

### Diagrams

- **[diagrams/](diagrams/)** - Architecture and deployment diagrams (PNG format)
  - Architecture overview
  - Deployment architecture
  - Component details

## Getting Started

1. **Review the Overview** - Start with [overview.md](overview.md) to understand CRE
2. **Understand the Architecture** - Read [architecture.md](architecture.md) for system design
3. **Deploy CRE** - Follow the [deployment-guide.md](deployment-guide.md) step-by-step
4. **Operate CRE** - Use [operations-guide.md](operations-guide.md) for day-to-day operations

## Additional Resources

### Main CRE Documentation

- **[Main Project README](https://github.com/joergen7/cre/blob/main/README.md)** - Complete feature list and quick start
- **[API Reference](https://github.com/joergen7/cre/blob/main/docs/API_REFERENCE.md)** - Complete API documentation
- **[YAWL Patterns Reference](https://github.com/joergen7/cre/blob/main/docs/YAWL_PATTERNS_REFERENCE.md)** - All 43 YAWL patterns
- **[Deployment Guide](https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md)** - Comprehensive deployment documentation

### GCP Marketplace Documentation

- **[GCP Marketplace Readiness](../docs/gcp/GCP_MARKETPLACE_READINESS.md)** - Technical assessment
- **[Support Terms](../docs/gcp/marketplace/SUPPORT.md)** - Support scope, SLA, contacts
- **[Privacy Policy](../docs/gcp/marketplace/PRIVACY.md)** - Data handling and privacy
- **[Service Level Agreement](../docs/gcp/marketplace/SLA.md)** - Uptime commitment and credits
- **[Upgrade Guide](../docs/gcp/marketplace/UPGRADE.md)** - Version upgrade procedures

### Operational Runbooks

- **[Deployment Runbook](../docs/gcp/runbooks/deployment.md)** - Initial deployment procedures
- **[Scaling Runbook](../docs/gcp/runbooks/scaling.md)** - Horizontal and vertical autoscaling
- **[Backup Runbook](../docs/gcp/runbooks/backup.md)** - Backup, restore, and disaster recovery
- **[Rollback Runbook](../docs/gcp/runbooks/rollback.md)** - Rollback procedures for failed deployments
- **[Troubleshooting Runbook](../docs/gcp/runbooks/troubleshooting.md)** - Common issues and debugging

## Support

For GCP Marketplace deployments, see:

- **[Support Terms](../docs/gcp/marketplace/SUPPORT.md)** - Support scope, SLA, escalation
- **[GitHub Issues](https://github.com/joergen7/cre/issues)** - Bug reports and questions
- **[Documentation](https://github.com/joergen7/cre/blob/main/docs/)** - Complete documentation index

## Version Information

- **CRE Version**: 0.3.0
- **OTP Support**: 25, 26, 27, 28
- **Documentation Last Updated**: 2025-01-10

---

**License**: Apache 2.0 | **Project**: https://github.com/joergen7/cre
