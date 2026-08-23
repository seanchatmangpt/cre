# CRE Marketplace Submission Checklist

## Technical Assessment

### Container Images
- [ ] Multi-arch images (linux/amd64, linux/arm64) in Artifact Registry
- [ ] Images use immutable version tags (vX.Y.Z)
- [ ] Images signed with cosign
- [ ] SBOM generated in SPDX format
- [ ] Trivy scan shows no CRITICAL vulnerabilities
- [ ] Non-root user execution
- [ ] Read-only root filesystem support
- [ ] All capabilities dropped

### Kubernetes Deployment
- [ ] GKE Application schema (application.yaml) present
- [ ] Helm chart passes lint: `helm lint k8s/charts/cre`
- [ ] No secrets in values.yaml
- [ ] Health checks functional (/health, /ready, /startup)
- [ ] Pod Security Standards compliance
- [ ] Network policies defined
- [ ] Resource limits configured
- [ ] Pod Disruption Budget configured

### Security & Compliance
- [ ] No hardcoded secrets
- [ ] External Secrets Operator integration documented
- [ ] Workload Identity Federation (no service account keys)
- [ ] Private GKE cluster configuration
- [ ] Shielded nodes enabled
- [ ] Binary Authorization policy documented
- [ ] Audit logging enabled

### Documentation
- [ ] Marketplace README complete
- [ ] License terms documented (BYOL)
- [ ] Architecture diagram provided
- [ ] Support process documented
- [ ] Known limitations documented
- [ ] Quick start guide verified

### Testing
- [ ] Fresh GKE project deployment tested
- [ ] Health checks verified
- [ ] Scaling tested (manual and HPA)
- [ ] Persistence verified
- [ ] Backup/restore tested
- [ ] Pod disruption tested
- [ ] Network policies verified

## Marketplace Listing

### Metadata
- [ ] Title: "CRE - Common Runtime Environment"
- [ ] Category: Development Tools
- [ ] Tags: workflow, automation, integration, yawl, erlang
- [ ] Logo uploaded (PNG, 128x128px)
- [ ] Short description (80 chars max)
- [ ] Long description (2000 chars max)
- [ ] Documentation URL
- [ ] Support URL

### Pricing
- [ ] BYOL model selected
- [ ] License agreement link provided
- [ ] No usage metering required

### Deployment UI
- [ ] application.yaml parameters tested
- [ ] Default values appropriate
- [ ] Constraints working correctly
- [ ] Output variables displaying correctly

## Post-Submission

### Monitoring
- [ ] Cloud Monitoring dashboard created
- [ ] Alert policies configured
- [ ] Log queries documented

### Support
- [ ] Issue triage process defined
- [ ] SLA documented (if applicable)
- [ ] Escalation path defined

### Maintenance
- [ ] Update process documented
- [ ] Rollback procedure tested
- [ ] Versioning strategy defined
