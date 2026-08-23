# CRE Privacy Policy

## Data Collection

### CRE Does Not Collect User Data

CRE (Common Runtime Environment) is an open-source workflow engine that:

- **DOES NOT** collect usage telemetry
- **DOES NOT** send data to external services
- **DOES NOT** track workflow content
- **DOES NOT** monitor user behavior

All workflow data remains:
- **Within your GCP project**
- **Under your control**
- **Subject to your privacy policies**

### Cloud Operations Integration

CRE optionally integrates with GCP Cloud Operations for monitoring:

| Integration | Data Sent | Control |
|-------------|-----------|---------|
| Cloud Logging | Application logs | Configurable via logger backend |
| Cloud Trace | Distributed traces | Configurable sampling rate |
| Cloud Monitoring | Metrics (CPU, memory) | Standard GKE metrics |

**You control** what is sent to Cloud Operations through CRE configuration.

## Data Storage

### Where Your Data Lives

All CRE workflow data is stored in GCP resources within **your project**:

| Data Type | Storage Location | Data Residency |
|-----------|------------------|----------------|
| Workflow State | Mnesia (local) or Cloud Spanner | Your chosen region |
| Application Logs | Cloud Logging | Your chosen region |
| Backup Artifacts | Cloud Storage | Your chosen bucket location |
| Monitoring Data | Cloud Monitoring | Google-managed (global) |

### Cloud Spanner (Optional)

If you choose to use Cloud Spanner for workflow state:
- Data stored in your GCP project's Spanner instance
- Subject to Cloud Spanner's data residency
- See [Cloud Spanner data residency](https://cloud.google.com/spanner/docs/data-residency)

## Google Cloud Privacy

### Google's Use of Data

For Google Cloud infrastructure data (not CRE software):

- **Infrastructure Logs**: Google collects logs for GCP operations
- **Support Access**: Only with your explicit permission
- **No Data Mining**: Google does not mine your data for advertising

See [Google Cloud Privacy Notice](https://cloud.google.com/terms/cloud-privacy-notice)

## Access Control

### Your Data, Your Control

You maintain full control over CRE data:

- **IAM Policies**: Control who can access GCP resources
- **Workload Identity**: No service account keys required
- **Private Cluster**: Network-isolated GKE deployment
- **Encryption**: Google-managed or customer-managed encryption keys (CMEK)

### Audit Logging

All access to CRE resources is logged via Cloud Audit Logs:
- **Admin Activity**: Always logged
- **Data Access**: Optional, configurable by you

## Third-Party Services

### CRE Has No Third-Party Dependencies

CRE does not integrate with:
- Analytics services
- Advertising services
- Data brokers
- Social media platforms

The only external services are Google Cloud infrastructure components.

## Security

### Data Protection

CRE uses industry-standard security practices:

- **Transport Encryption**: TLS for all network communication
- **At-Rest Encryption**: GCP default encryption
- **Network Isolation**: Private GKE clusters
- **Access Control**: Role-based IAM policies
- **Vulnerability Scanning**: CI/CD pipeline with Trivy

## Compliance

### Applicable Regulations

When deployed appropriately, CRE can help with:

- **GDPR**: EU data protection (data stays in EU regions)
- **SOC 2**: Security controls via GCP compliance
- **HIPAA**: Healthcare data (with BAA from Google Cloud)

**Note**: You are responsible for ensuring your deployment meets applicable regulations.

### GCP Compliance

Google Cloud maintains certifications for:
- ISO 27001
- SOC 2 Type II
- HIPAA (with BAA)
- GDPR compliance

See [Google Cloud Compliance](https://cloud.google.com/compliance)

## Children's Privacy

CRE is not directed to children under 13. We do not knowingly collect information from children.

## Privacy Policy Changes

### Updates

- **Effective**: When posted to GitHub repository
- **Notification**: Via GitHub release notes
- **Review Date**: This policy was last updated February 2025

## Contact

### Privacy Inquiries

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Email**: cre-support@common-runtime.org

### GCP Privacy Resources

- [Google Cloud Privacy Notice](https://cloud.google.com/terms/cloud-privacy-notice)
- [Google Privacy Policy](https://policies.google.com/privacy)
- [Google Cloud Security](https://cloud.google.com/security)

## Related Documentation
- [GCP Marketplace Readiness](/docs/gcp/GCP_MARKETPLACE_READINESS.md)
- [Security Module](/terraform/gcp/modules/security/)
- [IAM and Workload Identity](/k8s/gcp/serviceaccount.yaml)
