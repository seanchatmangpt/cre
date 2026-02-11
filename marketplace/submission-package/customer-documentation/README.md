# CRE Customer Documentation

This directory contains links to all customer-facing documentation for CRE on Google Cloud Marketplace.

**Note**: Documentation is stored in `../listing-package/`. This file provides an index for the submission package.

---

## Documentation Index

### Getting Started

1. **[Overview](../listing-package/overview.md)**
   What is CRE and why use it. Features, use cases, and system requirements.

2. **[Architecture](../listing-package/architecture.md)**
   System design, components, and data flow. Understand how CRE works.

3. **[Deployment Guide](../listing-package/deployment-guide.md)**
   Step-by-step deployment instructions for GKE. Prerequisites, configuration, verification.

### Operating CRE

4. **[Operations Guide](../listing-package/operations-guide.md)**
   Running CRE in production. Scaling, backup, troubleshooting, monitoring.

5. **[Security Model](../listing-package/security-model.md)**
   Security architecture and compliance. Pod Security Standards, encryption, IAM, audit logging.

6. **[Cost Model](../listing-package/cost-model.md)**
   Pricing and cost optimization. Infrastructure costs, cost saving tips, total cost of ownership.

### Additional Resources

- **[Main CRE Documentation](https://github.com/joergen7/cre/blob/main/docs/)** - Complete documentation index
- **[API Reference](https://github.com/joergen7/cre/blob/main/docs/API_REFERENCE.md)** - REST API documentation
- **[YAWL Patterns Reference](https://github.com/joergen7/cre/blob/main/docs/YAWL_PATTERNS_REFERENCE.md)** - All 43 workflow patterns
- **[Quick Start Guide](https://github.com/joergen7/cre/blob/main/docs/QUICK_START.md)** - 5-minute tutorial

### Operational Runbooks

Detailed runbooks for common operational tasks:

- **[Deployment Runbook](../../docs/gcp/runbooks/deployment.md)** - Initial deployment procedures
- **[Scaling Runbook](../../docs/gcp/runbooks/scaling.md)** - Autoscaling operations
- **[Backup Runbook](../../docs/gcp/runbooks/backup.md)** - Backup and restore
- **[Rollback Runbook](../../docs/gcp/runbooks/rollback.md)** - Rollback procedures
- **[Troubleshooting Runbook](../../docs/gcp/runbooks/troubleshooting.md)** - Debugging procedures

### Legal and Support

- **[Support Terms](../../docs/gcp/marketplace/SUPPORT.md)** - Support scope, SLA, contacts
- **[Privacy Policy](../../docs/gcp/marketplace/PRIVACY.md)** - Data handling and privacy
- **[Service Level Agreement](../../docs/gcp/marketplace/SLA.md)** - Uptime commitment and credits
- **[Upgrade Guide](../../docs/gcp/marketplace/UPGRADE.md)** - Version upgrade procedures

---

## Documentation Navigation

### For New Users

1. Start with [Overview](../listing-package/overview.md) to understand CRE
2. Read [Architecture](../listing-package/architecture.md) for system design
3. Follow [Deployment Guide](../listing-package/deployment-guide.md) to deploy CRE
4. Use [Operations Guide](../listing-package/operations-guide.md) for day-to-day operations

### For Operators

1. Review [Security Model](../listing-package/security-model.md) for security best practices
2. Configure monitoring per [Operations Guide](../listing-package/operations-guide.md#monitoring-and-observability)
3. Set up automated backups using [Backup Runbook](../../docs/gcp/runbooks/backup.md)
4. Configure autoscaling using [Scaling Runbook](../../docs/gcp/runbooks/scaling.md)

### For Security and Compliance

1. Review [Security Model](../listing-package/security-model.md) for complete security architecture
2. Review [Compliance Matrix](../../docs/gcp/COMPLIANCE_MATRIX.md) for SOC 2, HIPAA, PCI-DSS, GDPR mapping
3. Configure CMEK encryption per [Security Model](../listing-package/security-model.md#data-protection)
4. Enable Cloud Audit Logs per [Security Model](../listing-package/security-model.md#audit-and-compliance)

---

## Documentation Format

All documentation is provided in **Markdown format** for easy reading and contribution:

- **GitHub**: View directly on GitHub
- **GitBook**: Import to GitBook for PDF export
- **Hugo/Jekyll**: Use with static site generators
- **Markdown Editors**: View with any Markdown editor

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
**Documentation Owner**: CRE Team (docs@common-runtime.org)
