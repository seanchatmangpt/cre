# CRE Support

## Support Contact

### Community Support
- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Documentation**: https://github.com/joergen7/cre/blob/main/README.md
- **License**: Apache-2.0 (https://github.com/joergen7/cre/blob/main/LICENSE)

### Enterprise Support
For GCP Marketplace deployments requiring enterprise support:
- **Email**: cre-support@common-runtime.org
- **Response Time**: 48 hours for initial response

## Support Scope

### Covered
- Bug fixes for defects in CRE software
- Installation and deployment issues
- Workflow pattern questions
- API usage guidance
- GCP Marketplace deployment assistance

### Not Covered
- Custom workflow development
- Erlang/OTP training
- GCP infrastructure configuration beyond CRE deployment
- Third-party integration development

## Support SLA

| Severity Level | Description | Response Time | Availability |
|----------------|-------------|---------------|--------------|
| **P1 - Critical** | Production system down, data loss | 48 hours | Community |
| **P2 - High** | Major feature broken, workaround available | 72 hours | Community |
| **P3 - Medium** | Minor feature broken, workaround available | 1 week | Community |
| **P4 - Low** | Enhancement request, documentation | Best effort | Community |

### Severity Definitions

**P1 - Critical**
- Complete loss of service
- Data corruption or loss
- Security vulnerability requiring immediate action

**P2 - High**
- Major feature non-functional
- Significant performance degradation
- No reasonable workaround available

**P3 - Medium**
- Minor feature non-functional
- Performance issue with workaround
- Feature works differently than documented

**P4 - Low**
- Cosmetic issues
- Documentation improvements
- Feature requests

## Getting Help

### Before Opening an Issue
1. Check the documentation: https://github.com/joergen7/cre#readme
2. Search existing issues: https://github.com/joergen7/cre/issues
3. Review runbooks: `/docs/gcp/runbooks/`

### Opening a Support Request
Include the following information:
- CRE version (e.g., 0.3.0)
- GCP deployment region
- Kubernetes version
- Steps to reproduce
- Expected vs actual behavior
- Logs from Cloud Logging

## Emergency Contact

For critical production issues affecting GCP Marketplace deployments:
- Create a GitHub issue with the **critical** label
- Include "EMERGENCY" in the issue title
- Provide all diagnostic information listed above

## Related Documentation
- [Deployment Runbook](/docs/gcp/runbooks/deployment.md)
- [Troubleshooting Runbook](/docs/gcp/runbooks/troubleshooting.md)
- [GCP Marketplace Readiness](/docs/gcp/GCP_MARKETPLACE_READINESS.md)
