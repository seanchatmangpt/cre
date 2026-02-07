# Compliance Framework

A comprehensive compliance validation framework supporting SOC 2, ISO 27001, GDPR, and HIPAA compliance audits. Automatically validates compliance controls, generates detailed reports, and produces certification documents.

## Features

- **Multi-Standard Support**: SOC 2 Type II, ISO 27001:2022, GDPR, and HIPAA compliance validation
- **Automated Validation**: 80+ compliance checks across all standards
- **Comprehensive Reports**: Generate markdown, HTML, JSON, and CSV reports
- **Certification**: Create compliance certificates with digital signatures
- **Risk Assessment**: Identify critical compliance gaps and remediation steps
- **Audit Trail**: Maintain detailed compliance audit logs
- **Easy Integration**: Simple CLI and programmatic APIs

## Installation

```bash
npm install @cre/compliance-framework
# or
yarn add @cre/compliance-framework
```

## Quick Start

### Run Full Compliance Assessment

```bash
npx compliance assess \
  --project /path/to/project \
  --organization "My Company" \
  --auditor "John Doe" \
  --output ./compliance-reports
```

### Validate Specific Standard

```bash
# SOC 2 Validation
npx compliance validate --standard SOC2

# ISO 27001 Validation
npx compliance validate --standard ISO27001

# GDPR Validation
npx compliance validate --standard GDPR

# HIPAA Validation
npx compliance validate --standard HIPAA
```

### Generate Reports

```bash
# Markdown report
npx compliance report --standard SOC2 --format markdown

# HTML report
npx compliance report --standard ISO27001 --format html

# JSON report
npx compliance report --standard GDPR --format json

# CSV report
npx compliance report --standard HIPAA --format csv
```

### Generate Certificates

```bash
npx compliance certificate \
  --standard SOC2 \
  --organization "My Company" \
  --auditor "John Doe"
```

### Verify Certificates

```bash
npx compliance verify /path/to/certificate.json
```

### List Available Frameworks

```bash
npx compliance list
```

## Compliance Standards

### SOC 2 Type II

Validates Trust Service Criteria covering:
- **Security**: Access control, encryption, vulnerability management
- **Availability**: System uptime, disaster recovery
- **Processing Integrity**: Data validation, error handling
- **Confidentiality**: Data classification and protection
- **Privacy**: Privacy controls and data handling

**Key Controls**: 11 critical checks
**Audit Frequency**: Annual
**Applicable To**: Cloud services, SaaS, service providers

### ISO 27001:2022

Comprehensive Information Security Management System validation:
- **Organizational Controls**: Policies, roles, responsibilities (A.5)
- **People Controls**: Personnel screening, training (A.6)
- **Physical Controls**: Facility access, workstation security (A.7)
- **Technological Controls**: Access, encryption, logging (A.8)
- **Communication Controls**: Network security, transit encryption

**Key Controls**: 20+ technical controls
**Audit Frequency**: Annual
**Applicable To**: All organizations

### GDPR

General Data Protection Regulation compliance for EU data:
- **Principles**: Lawfulness, fairness, transparency, accuracy
- **Data Subject Rights**: Right to access, erasure, portability
- **Technical Measures**: Privacy by design, encryption, DPA
- **Incident Management**: Breach notification procedures
- **Governance**: DPO designation, data transfer mechanisms

**Key Controls**: 18 GDPR articles
**Audit Frequency**: Semi-annual (180 days)
**Applicable To**: Organizations processing EU citizen data

### HIPAA

Health Insurance Portability and Accountability Act:
- **Administrative Safeguards**: Security management, access controls, training
- **Physical Safeguards**: Facility access, workstation security, media controls
- **Technical Safeguards**: User authentication, audit controls, encryption
- **Privacy Rule**: Privacy policies, patient notices, patient rights
- **Breach Notification**: Incident response and notification procedures

**Key Controls**: 23 compliance requirements
**Audit Frequency**: Annual
**Applicable To**: Healthcare providers, health plans, clearinghouses

## Usage Examples

### Programmatic API

```typescript
import ComplianceManager from '@cre/compliance-framework';

const manager = new ComplianceManager();

// Create context
const context = {
  projectRoot: '/path/to/project',
  environment: 'production'
};

// Validate single standard
const soc2Results = await manager.validateStandard('SOC2', context);
const soc2Report = manager.generateReport('SOC2', soc2Results);

// Generate certificate
const certificate = manager.generateCertificate(
  'SOC2',
  soc2Report,
  'My Company',
  'John Doe',
  'Full assessment'
);

// Save outputs
await manager.saveReport(soc2Report, './reports', 'html');
await manager.saveCertificate(certificate, './certs');

// Verify certificate
const isValid = manager.verifyCertificate(certificate);
const status = manager.getCertificateStatus(certificate);
```

### Full Assessment

```typescript
const result = await manager.runFullCompliance(
  '/path/to/project',
  'My Company',
  'John Doe',
  './compliance-reports'
);

console.log('Reports generated:', result.reports.size);
console.log('Certificates created:', result.certificates.size);
console.log('Summary file:', result.summaryFile);
```

## Report Formats

### Markdown Report

Human-readable compliance report with:
- Executive summary with pass rate
- Detailed results table
- Failed checks with remediation steps
- Recommendations by priority

### HTML Report

Professional compliance report with:
- Interactive dashboard
- Color-coded compliance status
- Responsive design
- Printable format

### JSON Report

Structured data for integration:
- Complete check results
- Metadata and timestamps
- Detailed findings
- Recommendation data

### CSV Report

Spreadsheet-compatible format:
- All checks in tabular format
- Easy filtering and sorting
- Compatible with Excel/Google Sheets

## Certificate Features

Generated compliance certificates include:

- **Verification**: Digital signature with SHA-256 hashing
- **Professional Design**: Formal certificate layout with seal
- **Compliance Level**: Full, Partial, or Conditional
- **Validity Period**: 12-month validity with expiry tracking
- **Digital Signature**: Cryptographic verification
- **Control Mapping**: All 80+ controls documented

```json
{
  "id": "CERT-SOC2-1234567890",
  "standard": "SOC2",
  "organization": "My Company",
  "certificateNumber": "SOC2-1234567890-XXXXX-YYYYY",
  "issuedDate": "2024-01-01T00:00:00Z",
  "expiryDate": "2025-01-01T00:00:00Z",
  "complianceLevel": "full",
  "digitalSignature": "a1b2c3d4...",
  "auditor": "John Doe"
}
```

## Compliance Checks

### SOC 2 (11 Checks)
- Access Control & Authentication
- Data Encryption in Transit
- Data Encryption at Rest
- Logging & Monitoring
- Vulnerability Management
- System Availability & Uptime
- Disaster Recovery Planning
- Confidentiality & Data Classification
- Data Integrity & Validation
- Error Handling & Recovery
- Security Management Process

### ISO 27001 (20 Checks)
- Security Policies
- Information Security Roles
- Segregation of Duties
- Management Responsibility
- Personnel Screening
- Security Awareness Training
- Incident Reporting
- Physical Security
- User Access Management
- Privileged Access Rights
- Password Management
- Cryptography Controls
- Logging & Monitoring
- Vulnerability Management
- Malware Protection
- Network Security
- Encryption in Transit

### GDPR (18 Checks)
- Lawfulness, Fairness & Transparency
- Purpose Limitation
- Data Minimization
- Accuracy
- Storage Limitation
- Integrity & Confidentiality
- Accountability
- Privacy Notice
- Right to Erasure
- Data Portability
- Privacy by Design
- DPIA
- Security Measures
- Data Processing Agreements
- Breach Notification
- Data Protection Officer
- International Transfers
- Consent & Cookie Management

### HIPAA (23 Checks)
- Security Management Process
- Security Officer Assignment
- Workforce Security
- Access Management
- Security Training
- Incident Response
- Contingency Planning
- Business Associate Agreements
- Facility Access Controls
- Workstation Security
- Media Controls
- User Access Controls
- Audit Logging
- Data Integrity
- Transmission Security
- Encryption & Decryption
- Privacy Policies
- Patient Notices
- Patient Rights
- Breach Notification
- Risk Assessment
- Portable Media Controls
- Physical Security

## Output Structure

```
compliance-reports/
├── soc2-report-report-xxx.md
├── soc2-report-report-xxx.html
├── soc2-report-report-xxx.json
├── soc2-report-report-xxx.csv
├── soc2-certificate-CERT-xxx.html
├── soc2-certificate-CERT-xxx.json
├── iso27001-report-report-xxx.md
├── iso27001-certificate-CERT-xxx.html
├── gdpr-report-report-xxx.md
├── gdpr-certificate-CERT-xxx.html
├── hipaa-report-report-xxx.md
├── hipaa-certificate-CERT-xxx.html
└── compliance-summary.md
```

## Configuration

### Environment Variables

```bash
COMPLIANCE_PROJECT_ROOT=/path/to/project
COMPLIANCE_ORGANIZATION="My Company"
COMPLIANCE_AUDITOR="John Doe"
COMPLIANCE_OUTPUT_DIR=./compliance-reports
COMPLIANCE_ENVIRONMENT=production
```

### Configuration File

Create `.compliance.json`:

```json
{
  "projectRoot": "/path/to/project",
  "organization": "My Company",
  "auditor": "John Doe",
  "outputDir": "./compliance-reports",
  "environment": "production",
  "standards": ["SOC2", "ISO27001", "GDPR", "HIPAA"],
  "auditFrequency": 90,
  "includeFormats": ["markdown", "html", "json"]
}
```

## Integration

### CI/CD Pipeline

```yaml
# GitHub Actions Example
- name: Run Compliance Assessment
  run: |
    npx compliance assess \
      --project . \
      --organization "My Org" \
      --auditor "CI/CD Bot"

- name: Upload Reports
  uses: actions/upload-artifact@v2
  with:
    name: compliance-reports
    path: compliance-reports/
```

### Git Hooks

```bash
#!/bin/bash
# .git/hooks/pre-commit
npx compliance validate --standard SOC2 || exit 1
```

## Troubleshooting

### Missing Dependencies

```bash
npm install commander
npm install --save-dev typescript ts-node
```

### Permission Errors

Ensure proper file system permissions for reading project files and writing reports.

### Certificate Verification Fails

Certificates are valid for 12 months from issue date. Generate new certificates after expiry.

## Best Practices

1. **Regular Audits**: Run assessments every 90 days
2. **Documentation**: Maintain records of all compliance activities
3. **Remediation**: Address critical findings within 30 days
4. **Training**: Ensure team understands compliance requirements
5. **Review**: Regularly review and update compliance policies
6. **Monitoring**: Continuously monitor compliance status
7. **Testing**: Test disaster recovery and incident procedures

## API Reference

### ComplianceManager

```typescript
class ComplianceManager {
  // Validation
  validateStandard(standard, context): Promise<CheckResult[]>
  validateAllStandards(context): Promise<Map<Standard, CheckResult[]>>

  // Reporting
  generateReport(standard, results): ComplianceReport
  generateAllReports(context): Promise<Map<Standard, ComplianceReport>>
  saveReport(report, outputDir, format): Promise<string>
  saveAllReports(reports, outputDir, formats): Promise<string[]>

  // Certification
  generateCertificate(standard, report, org, auditor, scope): ComplianceCertificate
  saveCertificate(certificate, outputDir): Promise<string>
  verifyCertificate(certificate): boolean
  getCertificateStatus(certificate): string

  // Framework
  getFramework(standard): ComplianceFramework
  getAllFrameworks(): ComplianceFramework[]

  // Full Assessment
  runFullCompliance(projectRoot, org, auditor, outputDir): Promise<Result>
}
```

## Support

For issues, questions, or feature requests, please contact the compliance team or open an issue in the repository.

## License

MIT License - See LICENSE file for details

## Version History

### v1.0.0 (2024-01-XX)
- Initial release
- Support for SOC 2, ISO 27001, GDPR, HIPAA
- 80+ compliance checks
- Report generation (MD, HTML, JSON, CSV)
- Digital certificates with verification
- CLI tool with full feature set

---

**Last Updated**: January 2024
**Maintained By**: CRE Compliance Team
