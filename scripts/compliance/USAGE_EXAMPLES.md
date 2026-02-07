# Compliance Framework - Usage Examples

Comprehensive examples for using the compliance framework.

## CLI Usage Examples

### 1. Validate SOC 2 Compliance

```bash
npx compliance validate --standard SOC2 --project /path/to/project
```

**Output:**
```
Validating SOC2...

SOC 2 Compliance Report:
Status: PARTIAL
Pass Rate: 81.82%
Passed: 9/11

Reports saved to ./compliance-reports
```

### 2. Validate All Standards

```bash
npx compliance validate --project /path/to/project --environment production
```

**Output:**
```
Validating all standards...

Compliance Summary:

SOC2: PARTIAL (81.82%)
ISO27001: COMPLIANT (88.89%)
GDPR: PARTIAL (83.33%)
HIPAA: COMPLIANT (100.00%)

Reports saved to ./compliance-reports
```

### 3. Generate HTML Report

```bash
npx compliance report \
  --standard GDPR \
  --format html \
  --output ./reports
```

**Output:**
```
Report generated: ./reports/gdpr-report-report-GDPR-xxx.html
```

### 4. Generate Multiple Report Formats

```bash
for fmt in markdown json html csv; do
  npx compliance report \
    --standard ISO27001 \
    --format $fmt \
    --output ./reports
done
```

**Output:**
```
Report generated: ./reports/iso27001-report-report-ISO27001-xxx.md
Report generated: ./reports/iso27001-report-report-ISO27001-xxx.json
Report generated: ./reports/iso27001-report-report-ISO27001-xxx.html
Report generated: ./reports/iso27001-report-report-ISO27001-xxx.csv
```

### 5. Generate Compliance Certificate

```bash
npx compliance certificate \
  --standard HIPAA \
  --organization "Healthcare Provider Inc." \
  --auditor "Dr. John Smith" \
  --output ./certs
```

**Output:**
```
Generating HIPAA certificate...

Certificate generated:
Certificate Number: HIPAA-1704067200000-A1B2C3D4-E5F6G7H8
Compliance Level: full
Issued: Mon Jan 01 2024
Expires: Mon Jan 01 2025
Status: VALID

Files saved:
- ./certs/hipaa-certificate-CERT-xxx.html
- ./certs/hipaa-certificate-CERT-xxx.json
```

### 6. Full Compliance Assessment

```bash
npx compliance assess \
  --project /path/to/project \
  --organization "Acme Corporation" \
  --auditor "Jane Compliance Officer" \
  --output ./compliance-audit-2024
```

**Output:**
```
Starting full compliance assessment...
Project: /path/to/project
Organization: Acme Corporation
Output: ./compliance-audit-2024

Running compliance validators...
Saving compliance reports...
Generating compliance certificates...
Generating compliance summary...

Compliance Assessment Complete!

Generated:
- 4 compliance reports
- 4 compliance certificates
- Summary document: ./compliance-audit-2024/compliance-summary.md
```

### 7. Verify Certificate

```bash
npx compliance verify ./certs/soc2-certificate-CERT-xxx.json
```

**Output:**
```
Certificate Verification Results:

Certificate Number: SOC2-1704067200000-A1B2C3D4-E5F6G7H8
Standard: SOC2
Organization: My Company
Issued: Mon Jan 01 2024
Expires: Mon Jan 01 2025

Verification: VALID
Status: VALID
```

### 8. List Available Standards

```bash
npx compliance list
```

**Output:**
```
Available Compliance Frameworks:

SOC2
  Description: Service Organization Control Type II
  Total Checks: 11
  Audit Frequency: 365 days
  Applicable to: Cloud services, SaaS, Service Providers
  Regions: US, Global

ISO27001
  Description: Information Security Management System
  Total Checks: 20
  Audit Frequency: 365 days
  Applicable to: All Organizations
  Regions: Global

GDPR
  Description: General Data Protection Regulation
  Total Checks: 18
  Audit Frequency: 180 days
  Applicable to: Organizations processing EU citizen data
  Regions: EU, EEA

HIPAA
  Description: Health Insurance Portability and Accountability Act
  Total Checks: 23
  Audit Frequency: 365 days
  Applicable to: Healthcare providers, Health plans, Healthcare clearinghouses
  Regions: US
```

## Programmatic API Examples

### 1. Basic Validation

```typescript
import { ComplianceManager } from '@cre/compliance-framework';

const manager = new ComplianceManager();

// Create compliance context
const context = {
  projectRoot: '/path/to/project',
  environment: 'production',
  fileSystem: {
    exists: async (path) => { /* ... */ },
    read: async (path) => { /* ... */ },
    list: async (path) => { /* ... */ }
  }
};

// Validate SOC2
const results = await manager.validateStandard('SOC2', context);
console.log(`SOC2 validation: ${results.length} checks completed`);
```

### 2. Generate Report and Save

```typescript
const manager = new ComplianceManager();

// Validate
const results = await manager.validateStandard('SOC2', context);

// Generate report
const report = manager.generateReport('SOC2', results);

// Save in multiple formats
await manager.saveReport(report, './reports', 'markdown');
await manager.saveReport(report, './reports', 'html');
await manager.saveReport(report, './reports', 'json');

console.log(`Report pass rate: ${report.summary.passRate.toFixed(2)}%`);
```

### 3. Generate Certificate

```typescript
const manager = new ComplianceManager();

// Validate and generate report
const results = await manager.validateStandard('ISO27001', context);
const report = manager.generateReport('ISO27001', results);

// Generate certificate
const certificate = manager.generateCertificate(
  'ISO27001',
  report,
  'My Organization',
  'John Auditor',
  'Full information security assessment'
);

// Save certificate
await manager.saveCertificate(certificate, './certs');

// Verify certificate
const isValid = manager.verifyCertificate(certificate);
const status = manager.getCertificateStatus(certificate);

console.log(`Certificate valid: ${isValid}`);
console.log(`Certificate status: ${status}`);
```

### 4. Full Assessment with All Standards

```typescript
const manager = new ComplianceManager();

// Run all validations
const allReports = await manager.generateAllReports(context);

// Save all reports
const savedFiles = await manager.saveAllReports(
  allReports,
  './compliance-reports',
  ['markdown', 'html', 'json']
);

console.log(`Generated ${savedFiles.length} report files`);

// Generate summary
const summary = await manager.generateComplianceSummary(allReports);
console.log(summary);
```

### 5. Access Framework Information

```typescript
const manager = new ComplianceManager();

// Get specific framework
const soc2Framework = manager.getFramework('SOC2');
console.log(`${soc2Framework.standard}: ${soc2Framework.checks.length} checks`);

// Get all frameworks
const frameworks = manager.getAllFrameworks();
frameworks.forEach(fw => {
  console.log(`${fw.standard}: ${fw.description}`);
});
```

### 6. Custom Validator Integration

```typescript
import { SOC2Validator } from '@cre/compliance-framework';

const validator = new SOC2Validator();
const checks = validator.getChecks();

// Filter critical checks
const criticalChecks = checks.filter(c => c.severity === 'critical');
console.log(`Critical SOC2 checks: ${criticalChecks.length}`);

// Run validation
const context = { /* ... */ };
const results = await validator.runValidation(context);

// Process results
results.forEach(result => {
  console.log(`${result.checkId}: ${result.passed ? 'PASS' : 'FAIL'}`);
  if (!result.passed && result.remediation) {
    console.log(`  Remediation: ${result.remediation}`);
  }
});
```

### 7. Real-time Monitoring

```typescript
import { ComplianceManager } from '@cre/compliance-framework';

const manager = new ComplianceManager();

// Run assessments periodically
setInterval(async () => {
  const results = await manager.validateStandard('SOC2', context);
  const report = manager.generateReport('SOC2', results);

  // Alert on compliance drop
  if (report.summary.passRate < 80) {
    console.error(`ALERT: SOC2 compliance dropped to ${report.summary.passRate.toFixed(2)}%`);
    // Send notification, create issue, etc.
  }

  console.log(`SOC2 Status: ${report.overallStatus} (${report.summary.passRate.toFixed(2)}%)`);
}, 24 * 60 * 60 * 1000); // Daily
```

### 8. Integration with Reporting System

```typescript
import { ComplianceManager, ComplianceReport } from '@cre/compliance-framework';

class ComplianceReportingService {
  private manager: ComplianceManager;

  constructor() {
    this.manager = new ComplianceManager();
  }

  async generateExecutiveReport(context) {
    const allReports = await this.manager.generateAllReports(context);

    const summary = {
      timestamp: new Date(),
      overallCompliance: this.calculateOverallCompliance(allReports),
      standards: Array.from(allReports.values()).map(r => ({
        standard: r.standard,
        status: r.overallStatus,
        passRate: r.summary.passRate,
        topIssues: r.results
          .filter(res => !res.passed)
          .slice(0, 3)
          .map(res => res.message)
      }))
    };

    return summary;
  }

  private calculateOverallCompliance(
    reports: Map<string, ComplianceReport>
  ): number {
    let totalChecks = 0;
    let totalPassed = 0;

    reports.forEach(report => {
      totalChecks += report.summary.totalChecks;
      totalPassed += report.summary.passed;
    });

    return (totalPassed / totalChecks) * 100;
  }
}

// Usage
const service = new ComplianceReportingService();
const report = await service.generateExecutiveReport(context);
console.log(`Overall Compliance: ${report.overallCompliance.toFixed(2)}%`);
```

## CI/CD Integration Examples

### GitHub Actions

```yaml
name: Compliance Audit

on:
  schedule:
    - cron: '0 0 * * 0'  # Weekly
  workflow_dispatch:

jobs:
  compliance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: npm install
        working-directory: scripts/compliance

      - name: Run compliance assessment
        run: |
          npx compliance assess \
            --project . \
            --organization "${{ secrets.ORG_NAME }}" \
            --auditor "GitHub Actions Bot" \
            --output ./compliance-reports
        working-directory: scripts/compliance

      - name: Upload reports
        uses: actions/upload-artifact@v3
        with:
          name: compliance-reports
          path: scripts/compliance/compliance-reports/

      - name: Create issue on failure
        if: failure()
        uses: actions/create-issue@v2
        with:
          title: "Compliance Assessment Failed"
          body: "See artifacts for details"
```

### GitLab CI

```yaml
compliance_audit:
  stage: test
  image: node:18
  script:
    - cd scripts/compliance
    - npm install
    - npx compliance assess \
        --project ../.. \
        --organization "$CI_PROJECT_NAME" \
        --auditor "GitLab CI" \
        --output ./compliance-reports
  artifacts:
    paths:
      - scripts/compliance/compliance-reports/
    reports:
      dotenv: compliance-reports/summary.env
  only:
    - schedules
```

### Jenkins

```groovy
pipeline {
  agent any

  triggers {
    cron('H 0 * * 0')  // Weekly
  }

  stages {
    stage('Compliance Assessment') {
      steps {
        dir('scripts/compliance') {
          sh 'npm install'
          sh '''
            npx compliance assess \
              --project ../.. \
              --organization "My Company" \
              --auditor "Jenkins" \
              --output ./compliance-reports
          '''
        }
      }
    }

    stage('Archive Reports') {
      steps {
        archiveArtifacts artifacts: 'scripts/compliance/compliance-reports/**'
      }
    }
  }

  post {
    always {
      publishHTML([
        reportDir: 'scripts/compliance/compliance-reports',
        reportFiles: 'compliance-summary.md',
        reportName: 'Compliance Report'
      ])
    }
  }
}
```

## Advanced Scenarios

### Continuous Compliance Monitoring

```typescript
class ComplianceMonitor {
  private manager: ComplianceManager;
  private previousResults: Map<string, number> = new Map();

  async checkCompliance(context) {
    const allReports = await this.manager.generateAllReports(context);

    for (const [standard, report] of allReports) {
      const currentRate = report.summary.passRate;
      const previousRate = this.previousResults.get(standard) || 100;

      if (currentRate < previousRate) {
        this.alertComplianceDegradation(standard, previousRate, currentRate);
      }

      this.previousResults.set(standard, currentRate);
    }
  }

  private alertComplianceDegradation(standard: string, previous: number, current: number) {
    console.warn(`⚠️  ${standard} compliance degraded: ${previous.toFixed(2)}% → ${current.toFixed(2)}%`);
    // Send Slack notification, create Jira ticket, etc.
  }
}
```

---

For more information, see README.md and QUICK_START.md
