# CRE Service Level Agreement

## Overview

This SLA applies to CRE (Common Runtime Environment) when deployed on Google Cloud Platform via GCP Marketplace.

**Important**: This SLA covers the CRE software itself. For GCP infrastructure SLAs, please refer to [Google Cloud SLAs](https://cloud.google.com/terms/sla).

## Service Commitment

### CRE Software Availability

**Community Edition Target**: 99% uptime

The CRE software, when properly deployed according to our documentation, is designed to achieve 99% availability on a monthly basis.

**Exclusions from SLA**:
- Downtime caused by GCP infrastructure issues (covered by Google Cloud SLA)
- Downtime resulting from customer modifications to CRE
- Downtime due to custom workflow code errors
- Downtime during scheduled maintenance windows
- Issues caused by exceeding resource limits

### Infrastructure Availability

For infrastructure SLAs, refer to Google Cloud's commitments:

| Service | SLA | Documentation |
|---------|-----|---------------|
| GKE | 99.95% (regional) | [GKE SLA](https://cloud.google.com/kubernetes-engine/docs/sla) |
| Cloud Spanner | 99.999% | [Spanner SLA](https://cloud.google.com/spanner/sla) |
| Cloud Load Balancing | 99.99% | [LB SLA](https://cloud.google.com/load-balancing/docs/sla) |

## Service Credits

### CRE Software SLA

Since CRE is provided as open-source software under the Apache-2.0 license, **no service credits** are provided for CRE software downtime.

### GCP Infrastructure Credits

Service credits for GCP infrastructure are handled according to Google Cloud's SLA policies. Please refer to [Google Cloud Credits](https://cloud.google.com/terms/sla).

## Scheduled Maintenance

### Maintenance Windows

- **Scheduled Maintenance**: Up to 4 hours per month
- **Notification**: 7 days advance notice via GitHub releases
- **Impact**: Possible downtime during upgrades

### Maintenance Process

1. **Release Announcement**: Posted on GitHub releases
2. **Documentation Update**: Migration guides provided
3. **Rolling Upgrade**: Designed for zero-downtime upgrades
4. **Rollback Plan**: Documented in rollback runbook

## Uptime Calculation

### Monthly Uptime Percentage

```
Monthly Uptime % = (Total Minutes in Month - Downtime Minutes) / Total Minutes in Month × 100
```

**Downtime** is defined as:
- CRE pods not accepting workflow requests
- Inability to execute workflows due to CRE software errors
- Complete loss of workflow state persistence (CRE-specific)

**NOT counted as downtime**:
- Individual workflow failures (workflow-specific errors)
- Maintenance windows with advance notice
- Customer-initiated restarts/upgrades
- GCP infrastructure issues

## Incident Management

### Severity Levels

| Severity | Description | Response Time (Community) |
|----------|-------------|---------------------------|
| S1 | Complete service failure | 48 hours |
| S2 | Major feature degradation | 72 hours |
| S3 | Minor feature issues | 1 week |
| S4 | General inquiries | Best effort |

### Incident Reporting

Report incidents via:
- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Label**: Add `bug` or `critical` label
- **Required Info**: Version, deployment, logs, reproduction steps

### Resolution Process

1. **Acknowledgment**: Initial response within SLA timeframe
2. **Investigation**: Root cause analysis
3. **Fix**: Patch or workaround provided
4. **Release**: Published via GitHub releases
5. **Verification**: Customer confirms resolution

## Exclusions

This SLA does not apply to:

1. **Beta or Preview Features**: Marked as experimental in documentation
2. **Custom Workflows**: Third-party workflow code errors
3. **Resource Limits**: Issues caused by insufficient GCP resources
4. **Customer Actions**: Downtime from customer configuration changes
5. **Force Majeure**: Events beyond reasonable control
6. **Deprecated Versions**: Versions beyond end-of-life

## Version Support

### Supported Versions

- **Current Release**: Full support (latest version)
- **Previous Release**: Security fixes only (N-1)
- **Older Versions**: No support

### End of Life

Versions are supported for:
- **6 months** after release (or until next major release)
- Security patches for additional **3 months**

## SLA Modifications

This SLA may be updated:
- **Effective**: Immediately upon posting to GitHub
- **Notification**: Via GitHub release notes
- **Existing Customers**: 30 days notice for material changes

## Contact

For SLA-related questions:
- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Email**: cre-support@common-runtime.org

## Related Documentation
- [GCP Marketplace Readiness](/docs/gcp/GCP_MARKETPLACE_READINESS.md)
- [Deployment Runbook](/docs/gcp/runbooks/deployment.md)
- [Troubleshooting Runbook](/docs/gcp/runbooks/troubleshooting.md)
- [Rollback Runbook](/docs/gcp/runbooks/rollback.md)
