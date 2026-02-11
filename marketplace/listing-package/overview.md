# CRE Product Overview

## What is CRE?

**CRE (Common Runtime Environment)** is a production-grade **YAWL workflow engine** built on Erlang/OTP. Starting from its origins as a Cuneiform runtime environment, CRE has evolved into a comprehensive workflow management system with **36 of 43 YAWL workflow patterns** implemented, human-in-the-loop approval flows, OpenTelemetry observability, and a web-based dashboard.

CRE is designed for **complex workflow orchestration** requiring fault tolerance, distributed execution, and human decision points. It is particularly well-suited for business process automation, data processing pipelines, and workflow orchestration in cloud-native environments.

## Key Features

### Core Workflow Capabilities

CRE provides a comprehensive workflow management platform:

- **36 of 43 YAWL Patterns** - Extensive workflow pattern library including:
  - **Sequence** - Linear task execution
  - **Parallel Split** - Execute tasks concurrently
  - **Synchronization** - Wait for all parallel tasks to complete
  - **Exclusive Choice** - Branch based on conditions
  - **Simple Merge** - Rejoin exclusive branches
  - **Multi-Choice** - Multiple conditional branches
  - **Synchronizing Merge** - Rejoin multiple branches
  - **Discriminator** - Wait for first of N parallel tasks
  - **N-out-of-M Join** - Wait for N of M parallel tasks
  - **Structured Loop** - Repeat tasks with conditions
  - **Milestone** - Enable tasks based on process state
  - **Critical Section** - Mutual exclusion for shared resources
  - **Multi-Instance Patterns** - Parallel task execution
  - **Deferred Choice** - Runtime branch selection
  - **Interleaved Routing** - Alternate task execution
  - **Data Transform Patterns** - Parameter passing, data distribution/accumulation
  - **Resource Patterns** - Role-based allocation, creation, initialization, deallocation

### Technical Features

- **Distributed Execution** - Scale across Erlang clusters with automatic load balancing
- **Fault Tolerance** - Automatic failure recovery and task rescheduling
- **Human-in-the-Loop Approvals** - Integrate LLM-powered human decisions into workflows
- **OpenTelemetry Integration** - Comprehensive observability with structured logging and metrics
- **Web Dashboard** - Real-time workflow visualization and monitoring interface
- **XES Logging** - Standardized event logging for process mining and compliance
- **Caching System** - Memoize results to avoid redundant computations
- **Timeout Management** - Configurable timeouts for all workflow components
- **Pattern Validation** - Ensure workflows follow YAWL standards

### Platform Features

- **OTP 25-28 Support** - Modern Erlang/OTP with improved performance and reliability
- **Cloud Native** - Designed for Kubernetes and cloud environments
- **96% Test Pass Rate** - 689 of 760 tests passing (0.3.0)
- **Apache 2.0 License** - Open source with no licensing fees

## Use Cases

### 1. Business Process Automation

Automate complex business processes with human approval gates:

- Document approval workflows
- Procurement processes
- Compliance workflows
- Multi-stage approvals with conditional routing
- Audit trails with XES logging

### 2. Data Processing Pipelines

Build fault-tolerant data workflows:

- ETL (Extract, Transform, Load) pipelines
- Data quality validation
- Multi-stage data processing
- Automatic retry on failure
- Distributed data processing

### 3. Workflow Orchestration

Coordinate microservices and distributed systems:

- Microservice orchestration
- Service choreography
- Distributed transaction coordination
- Event-driven workflows
- API composition

### 4. Process Mining and Compliance

Use XES logging for process analysis:

- Process optimization
- Compliance reporting
- Audit trail generation
- Performance analysis
- Bottleneck identification

### 5. Approval Workflows

Human-in-the-loop decisions with LLM integration:

- Document approvals
- Change management
- Access requests
- Budget approvals
- Risk assessments

## System Requirements

### Minimum Requirements

- **Kubernetes**: GKE 1.25+ (or compatible Kubernetes distribution)
- **CPU**: 2 cores minimum (4 cores recommended for production)
- **Memory**: 4 GiB minimum (8 GiB recommended for production)
- **Storage**: 10 GiB persistent volume for Mnesia data
- **Network**: Pod-to-pod communication, load balancer for external access

### Recommended Production Configuration

- **Kubernetes**: GKE 1.27+ with 3+ nodes
- **CRE Pods**: 3 replicas for high availability
- **CPU**: 4 cores per pod (autoscaling up to 8 cores)
- **Memory**: 8 GiB per pod (autoscaling up to 16 GiB)
- **Storage**: 50 GiB persistent volume with backup
- **Database**: Cloud Spanner for distributed state (optional, for multi-region deployments)
- **Monitoring**: Cloud Monitoring, Cloud Logging, Cloud Trace
- **Backup**: Daily automated backups with 30-day retention

## Integration Capabilities

### Google Cloud Integration

CRE integrates seamlessly with Google Cloud services:

- **Cloud Spanner** - Distributed database for workflow state
- **Cloud Operations Suite** - Logging, monitoring, tracing
- **Cloud Storage** - Backup and restore
- **Artifact Registry** - Container image storage
- **Cloud Deploy** - Continuous delivery pipelines

### API and Protocol Support

- **REST API** - HTTP/JSON API for workflow submission and monitoring
- **Web Dashboard** - Web UI for workflow visualization
- **OpenTelemetry** - Standards-based observability
- **XES Format** - Standard event logging for process mining

## Deployment Options

### Google Cloud Marketplace

CRE is available on Google Cloud Marketplace as a **BYOL (Bring Your Own License)** solution:

- **License**: Apache 2.0 (open source, no additional fees)
- **Infrastructure Costs**: Pay only for GCP resources consumed
- **Deployment**: One-click deployment to GKE
- **Support**: Community support via GitHub Issues

### Self-Managed Deployment

Deploy CRE on your own Kubernetes cluster:

- Helm charts for easy installation
- Terraform modules for infrastructure as code
- Support for GKE, EKS, AKS, and vanilla Kubernetes
- Custom configurations via environment variables

## Performance Characteristics

### Scalability

- **Horizontal Scaling**: Add CRE pods to increase throughput
- **Vertical Scaling**: Increase CPU/memory per pod for complex workflows
- **Cluster Scaling**: Scale GKE node pools based on load
- **Distributed Execution**: Distribute workflows across Erlang nodes

### Fault Tolerance

- **Automatic Retry**: Failed tasks are automatically retried with exponential backoff
- **State Recovery**: Mnesia cluster maintains workflow state across pod failures
- **Pod Rescheduling**: Kubernetes automatically reschedules failed pods
- **Data Backup**: Automated backups with point-in-time recovery

### Observability

- **Metrics**: CPU, memory, workflow queue length, execution time, error rate
- **Logging**: Structured JSON logs exported to Cloud Logging
- **Tracing**: Distributed tracing with OpenTelemetry and Cloud Trace
- **Health Checks**: `/health` and `/ready` endpoints for Kubernetes probes

## Licensing and Support

### License

CRE is released under the **Apache License 2.0**:

- Open source, no licensing fees
- Freedom to modify and distribute
- Includes third-party software attributions (see [THIRD_PARTY_NOTICES.md](../../THIRD_PARTY_NOTICES.md))

### Support

For GCP Marketplace deployments:

- **Community Support**: GitHub Issues (48-hour response time)
- **Documentation**: Comprehensive guides and runbooks
- **Issue Tracking**: GitHub Issues for bug reports and feature requests

For enterprise support requirements, contact the CRE team via GitHub Issues.

## Next Steps

1. **[Architecture](architecture.md)** - Learn about CRE's system design
2. **[Deployment Guide](deployment-guide.md)** - Deploy CRE on GKE
3. **[Operations Guide](operations-guide.md)** - Run CRE in production
4. **[Security Model](security-model.md)** - Understand security and compliance
5. **[Cost Model](cost-model.md)** - Estimate infrastructure costs

## Additional Resources

- **[Main CRE Documentation](https://github.com/joergen7/cre/blob/main/docs/)** - Complete documentation index
- **[API Reference](https://github.com/joergen7/cre/blob/main/docs/API_REFERENCE.md)** - Complete API documentation
- **[YAWL Patterns Reference](https://github.com/joergen7/cre/blob/main/docs/YAWL_PATTERNS_REFERENCE.md)** - All 43 YAWL patterns
- **[GitHub Repository](https://github.com/joergen7/cre)** - Source code and contributions

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
