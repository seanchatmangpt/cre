# Compliance Framework - Quick Start Guide

Get up and running with compliance validation in 5 minutes.

## Installation

```bash
# Navigate to compliance directory
cd scripts/compliance

# Install dependencies
npm install

# Optional: Link CLI globally
npm link
```

## First Audit (5 minutes)

### Step 1: List Available Standards
```bash
npm run list
```

This shows all available compliance frameworks.

### Step 2: Run a Single Standard Validation
```bash
npm run validate:soc2
```

Output will show:
- Number of checks passed/failed
- Pass rate percentage
- Report location

### Step 3: View the Report
```bash
# Open the generated HTML report
open compliance-reports/soc2-report-*.html

# Or view markdown
cat compliance-reports/soc2-report-*.md
```

## Generate Compliance Certificate

```bash
npm run certificate -- \
  --standard SOC2 \
  --organization "My Company" \
  --auditor "John Doe"
```

This generates:
- HTML certificate (printable)
- JSON certificate (for records)
- Digital signature for verification

## Full Compliance Assessment

Run validation for all standards with one command:

```bash
npm run assess -- \
  --organization "My Company" \
  --auditor "John Doe"
```

Output includes:
- 4 compliance reports (SOC 2, ISO 27001, GDPR, HIPAA)
- 4 compliance certificates
- Comprehensive compliance summary

## Understanding the Results

### Pass Rate Interpretation

| Pass Rate | Status | Action Required |
|-----------|--------|-----------------|
| 100% | ✓ Compliant | Maintain current practices |
| 80-99% | ⚠ Partial | Remediate high-severity issues within 30 days |
| <80% | ✗ Non-compliant | Urgent remediation required |

### Critical vs High Issues

- **Critical**: Address within 7 days
- **High**: Address within 30 days
- **Medium/Low**: Address within 90 days

## Common Use Cases

### Development Team
```bash
# Quick pre-commit check
npm run validate:iso27001
```

### Security Team
```bash
# Full assessment for annual audit
npm run assess
```

### Operations Team
```bash
# Generate updated certificate
npm run certificate -- --standard SOC2
```

### Compliance Officer
```bash
# Generate all reports in different formats
npm run report -- --standard GDPR --format markdown
npm run report -- --standard GDPR --format html
npm run report -- --standard GDPR --format json
```

## Report Formats Quick Reference

| Format | Use Case | Example |
|--------|----------|---------|
| **Markdown** | Documentation, version control | `npm run report -- --format markdown` |
| **HTML** | Presentations, executives | `npm run report -- --format html` |
| **JSON** | Integration, automation | `npm run report -- --format json` |
| **CSV** | Excel, data analysis | `npm run report -- --format csv` |

## Interpreting Recommendations

Each report provides actionable recommendations:

```
# High Priority
1. Implement role-based access control (RBAC) in authentication system
2. Enable encryption at rest for database

# Medium Priority
3. Document data retention policies
4. Establish incident response procedures

# Low Priority
5. Update security awareness training materials
```

## Next Steps

1. **Review Failed Checks**: Open the HTML report and identify issues
2. **Prioritize Remediation**: Address critical issues first
3. **Schedule Fixes**: Plan remediation over 30-90 days
4. **Re-audit**: Run assessment again after fixes
5. **Maintain Compliance**: Run quarterly assessments

## Troubleshooting

### "Module not found" errors
```bash
npm install
npm link
```

### Reports not generating
```bash
# Check permissions
ls -la compliance-reports/

# Create output directory
mkdir -p compliance-reports
```

### Certificate verification fails
```bash
# Regenerate certificate
npm run certificate -- --standard SOC2
```

## Environment Setup

### For Production Audits

Create a configuration file:

```json
{
  "organization": "My Company",
  "auditor": "Jane Smith",
  "environment": "production",
  "standards": ["SOC2", "ISO27001", "GDPR", "HIPAA"]
}
```

### For Continuous Integration

```bash
# Add to CI pipeline
npm run assess > audit-results.log 2>&1
```

## Getting Help

### Show all available commands
```bash
npm run
```

### Get command help
```bash
npm run validate -- --help
npm run assess -- --help
npm run certificate -- --help
```

### Review specific standard details
```bash
npm run list
```

## Key Takeaways

- **Standards**: SOC2, ISO27001, GDPR, HIPAA (4 frameworks)
- **Checks**: 80+ automated compliance checks
- **Reports**: Multiple formats (MD, HTML, JSON, CSV)
- **Certificates**: Digital certificates with verification
- **Frequency**: Audit quarterly (90 days)

## What's Next?

1. Run your first full assessment
2. Review the compliance summary
3. Address critical findings
4. Schedule quarterly audits
5. Integrate into CI/CD pipeline

---

**For detailed documentation, see README.md**
