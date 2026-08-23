# CRE GCP Marketplace Documentation

This directory contains documentation specific to CRE's deployment on Google Cloud Marketplace.

## Marketplace Documentation

### Customer-Facing

- **[Overview](../../README.md)** - Main project README with feature overview
- **[Architecture](../../ARCHITECTURE.md)** - System architecture and design
- **[Deployment Guide](../../DEPLOYMENT.md)** - Comprehensive deployment guide

### Legal & Business

- **[Support Terms](SUPPORT.md)** - Support scope, SLA, contacts (48-hour response time)
- **[Pricing Model](PRICING.md)** - BYOL pricing, infrastructure costs, cost optimization
- **[Privacy Policy](PRIVACY.md)** - Data handling, data residency, Cloud Operations integration
- **[Service Level Agreement](SLA.md)** - 99.5% uptime commitment, exclusions, credits

### Operational

- **[Upgrade Guide](UPGRADE.md)** - Version upgrade procedures (rolling, blue-green, rollback)
- **[Troubleshooting](TROUBLESHOOTING.md)** - Common issues and solutions

## Runbooks

See `../runbooks/` for operational procedures:

- **[Runbooks Index](../runbooks/README.md)** - Index of all runbooks
- **[Deployment Runbook](../runbooks/deployment.md)** - Initial deployment, validation, configuration
- **[Scaling Runbook](../runbooks/scaling.md)** - HPA, VPA, cluster scaling, custom metrics
- **[Backup Runbook](../runbooks/backup.md)** - Automated backups, manual backups, restore procedures, disaster recovery
- **[Rollback Runbook](../runbooks/rollback.md)** - Rollback procedures for failed deployments
- **[Troubleshooting Runbook](../runbooks/troubleshooting.md)** - Common issues, debugging, escalation

## Marketplace Readiness

- **[GCP Marketplace Readiness](../GCP_MARKETPLACE_READINESS.md)** - Technical assessment, infrastructure, security, compliance

## Marketplace Deployment

- **[Marketplace README](../../../marketplace/README.md)** - Quick start for Marketplace deployment
- **[Deployer Spec](../../../marketplace/deployer.yaml)** - Marketplace deployment specification
- **[Submission Checklist](../../../marketplace/SUBMISSION_CHECKLIST.md)** - Marketplace submission checklist

## Marketplace Listing Package

Customer-facing documentation for the Marketplace listing:

- **[Listing Package](../../../marketplace/listing-package/README.md)** - Consolidated customer documentation
- **[Overview](../../../marketplace/listing-package/overview.md)** - Product features and use cases
- **[Architecture](../../../marketplace/listing-package/architecture.md)** - System design and components
- **[Deployment Guide](../../../marketplace/listing-package/deployment-guide.md)** - GKE deployment instructions
- **[Operations Guide](../../../marketplace/listing-package/operations-guide.md)** - Scaling, backup, troubleshooting
- **[Security Model](../../../marketplace/listing-package/security-model.md)** - Security and compliance
- **[Cost Model](../../../marketplace/listing-package/cost-model.md)** - Pricing and cost optimization

## Marketplace Submission Package

For Google's technical review team:

- **[Submission Package](../../../marketplace/submission-package/README.md)** - Complete submission package
- **[Technical Assessment](../../../marketplace/submission-package/technical-assessment/README.md)** - Technical documentation for review
- **[Legal Documents](../../../marketplace/submission-package/legal/README.md)** - License, third-party notices, privacy policy, SLA, support terms
- **[Customer Documentation](../../../marketplace/submission-package/customer-documentation/README.md)** - Links to customer docs
- **[Listing Artifacts](../../../marketplace/submission-package/listing/README.md)** - Logo, screenshots, descriptions

## Additional Resources

### Main CRE Documentation

- **[Documentation Index](../../README.md)** - Complete documentation index
- **[API Reference](../../API_REFERENCE.md)** - Complete function documentation
- **[YAWL Patterns Reference](../../YAWL_PATTERNS_REFERENCE.md)** - All 43 YAWL patterns
- **[Quick Start Guide](../../QUICK_START.md)** - 5-minute tutorial

### Security and Compliance

- **[Security Whitepaper](../SECURITY_WHITEPAPER.md)** - Security architecture and controls
- **[Compliance Matrix](../COMPLIANCE_MATRIX.md)** - SOC 2, HIPAA, PCI-DSS, GDPR mapping
- **[Security Guide](../SECURITY_GUIDE.md)** - Security configuration guide

### Infrastructure as Code

- **[Terraform Modules](../../../terraform/gcp/)** - GCP infrastructure modules
- **[Kubernetes Manifests](../../../k8s/gcp/)** - GKE deployment manifests
- **[Helm Chart](../../../k8s/charts/cre/)** - Helm chart for CRE

## Quick Links

### For New Users

1. **[Product Overview](../../../marketplace/listing-package/overview.md)** - What is CRE?
2. **[Deployment Guide](../../../marketplace/listing-package/deployment-guide.md)** - Deploy CRE on GKE
3. **[Operations Guide](../../../marketplace/listing-package/operations-guide.md)** - Run CRE in production

### For Operators

1. **[Scaling Runbook](../runbooks/scaling.md)** - Autoscaling CRE
2. **[Backup Runbook](../runbooks/backup.md)** - Backup and restore
3. **[Troubleshooting Runbook](../runbooks/troubleshooting.md)** - Debug issues

### For Security and Compliance

1. **[Security Model](../../../marketplace/listing-package/security-model.md)** - Security architecture
2. **[Compliance Matrix](../COMPLIANCE_MATRIX.md)** - Compliance mapping
3. **[Privacy Policy](PRIVACY.md)** - Data handling and privacy

## Support

- **[Support Terms](SUPPORT.md)** - Support scope, SLA, escalation
- **[GitHub Issues](https://github.com/joergen7/cre/issues)** - Bug reports and questions
- **[Documentation](https://github.com/joergen7/cre/blob/main/docs/)** - Full documentation

---

**CRE Version**: 0.3.0
**Last Updated**: 2025-01-10
**Documentation Owner**: CRE Team (docs@common-runtime.org)
