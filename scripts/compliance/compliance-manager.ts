/**
 * Compliance Manager
 * Orchestrates compliance validation, reporting, and certification
 */

import fs from 'fs/promises';
import path from 'path';
import { SOC2Validator } from './validators/soc2-validator';
import { ISO27001Validator } from './validators/iso27001-validator';
import { GDPRValidator } from './validators/gdpr-validator';
import { HIPAAValidator } from './validators/hipaa-validator';
import { ComplianceReportGenerator } from './report-generator';
import { CertificateGenerator } from './certificate-generator';
import {
  ComplianceContext,
  ComplianceStandard,
  ComplianceReport,
  ComplianceCertificate,
  ComplianceFramework,
  CheckResult,
} from './types';

export class ComplianceManager {
  private reportGenerator: ComplianceReportGenerator;
  private certificateGenerator: CertificateGenerator;
  private frameworks: Map<ComplianceStandard, ComplianceFramework>;
  private validators: Map<ComplianceStandard, any>;

  constructor() {
    this.reportGenerator = new ComplianceReportGenerator();
    this.certificateGenerator = new CertificateGenerator();
    this.frameworks = new Map();
    this.validators = new Map();

    this.initializeValidators();
    this.initializeFrameworks();
  }

  private initializeValidators(): void {
    const soc2Validator = new SOC2Validator();
    const iso27001Validator = new ISO27001Validator();
    const gdprValidator = new GDPRValidator();
    const hipaaValidator = new HIPAAValidator();

    this.validators.set('SOC2', soc2Validator);
    this.validators.set('ISO27001', iso27001Validator);
    this.validators.set('GDPR', gdprValidator);
    this.validators.set('HIPAA', hipaaValidator);
  }

  private initializeFrameworks(): void {
    this.frameworks.set('SOC2', {
      standard: 'SOC2',
      description: 'Service Organization Control Type II',
      checks: this.validators.get('SOC2').getChecks(),
      auditFrequency: 365,
      enforcingRegions: ['US', 'Global'],
      applicableTo: ['Cloud Services', 'SaaS', 'Service Providers'],
    });

    this.frameworks.set('ISO27001', {
      standard: 'ISO27001',
      description: 'Information Security Management System',
      checks: this.validators.get('ISO27001').getChecks(),
      auditFrequency: 365,
      enforcingRegions: ['Global'],
      applicableTo: ['All Organizations'],
    });

    this.frameworks.set('GDPR', {
      standard: 'GDPR',
      description: 'General Data Protection Regulation',
      checks: this.validators.get('GDPR').getChecks(),
      auditFrequency: 180,
      enforcingRegions: ['EU', 'EEA'],
      applicableTo: ['Organizations processing EU citizen data'],
    });

    this.frameworks.set('HIPAA', {
      standard: 'HIPAA',
      description: 'Health Insurance Portability and Accountability Act',
      checks: this.validators.get('HIPAA').getChecks(),
      auditFrequency: 365,
      enforcingRegions: ['US'],
      applicableTo: ['Healthcare providers', 'Health plans', 'Healthcare clearinghouses'],
    });
  }

  public async validateStandard(
    standard: ComplianceStandard,
    context: ComplianceContext,
  ): Promise<CheckResult[]> {
    const validator = this.validators.get(standard);

    if (!validator) {
      throw new Error(`Unknown compliance standard: ${standard}`);
    }

    return validator.runValidation(context);
  }

  public async validateAllStandards(
    context: ComplianceContext,
  ): Promise<Map<ComplianceStandard, CheckResult[]>> {
    const results = new Map<ComplianceStandard, CheckResult[]>();

    for (const [standard, validator] of this.validators.entries()) {
      const checks = await validator.runValidation(context);
      results.set(standard as ComplianceStandard, checks);
    }

    return results;
  }

  public generateReport(
    standard: ComplianceStandard,
    results: CheckResult[],
  ): ComplianceReport {
    return this.reportGenerator.generateReport(standard, results);
  }

  public async generateAllReports(
    context: ComplianceContext,
  ): Promise<Map<ComplianceStandard, ComplianceReport>> {
    const validationResults = await this.validateAllStandards(context);
    const reports = new Map<ComplianceStandard, ComplianceReport>();

    for (const [standard, results] of validationResults.entries()) {
      const report = this.reportGenerator.generateReport(standard, results);
      reports.set(standard, report);
    }

    return reports;
  }

  public async saveReport(
    report: ComplianceReport,
    outputDir: string,
    format: 'markdown' | 'json' | 'html' | 'csv' = 'markdown',
  ): Promise<string> {
    await fs.mkdir(outputDir, { recursive: true });

    const filename = `${report.standard.toLowerCase()}-report-${report.id}.${this.getFileExtension(format)}`;
    const filepath = path.join(outputDir, filename);

    let content: string;

    switch (format) {
      case 'json':
        content = this.reportGenerator.generateJsonReport(report);
        break;
      case 'html':
        content = this.reportGenerator.generateHtmlReport(report);
        break;
      case 'csv':
        content = this.reportGenerator.generateCsvReport(report);
        break;
      case 'markdown':
      default:
        content = this.reportGenerator.generateMarkdownReport(report);
        break;
    }

    await fs.writeFile(filepath, content, 'utf-8');

    return filepath;
  }

  public async saveAllReports(
    reports: Map<ComplianceStandard, ComplianceReport>,
    outputDir: string,
    formats: Array<'markdown' | 'json' | 'html' | 'csv'> = ['markdown', 'json', 'html'],
  ): Promise<string[]> {
    const savedFiles: string[] = [];

    for (const [, report] of reports.entries()) {
      for (const format of formats) {
        const filepath = await this.saveReport(report, outputDir, format);
        savedFiles.push(filepath);
      }
    }

    return savedFiles;
  }

  public generateCertificate(
    standard: ComplianceStandard,
    report: ComplianceReport,
    organization: string,
    auditor: string,
    scope: string,
  ): ComplianceCertificate {
    return this.certificateGenerator.generateCertificate(
      standard,
      report,
      organization,
      auditor,
      scope,
    );
  }

  public async saveCertificate(
    certificate: ComplianceCertificate,
    outputDir: string,
  ): Promise<string> {
    await fs.mkdir(outputDir, { recursive: true });

    const filename = `${certificate.standard.toLowerCase()}-certificate-${certificate.id}.html`;
    const filepath = path.join(outputDir, filename);

    const content = this.certificateGenerator.generateCertificateDocument(certificate);

    await fs.writeFile(filepath, content, 'utf-8');

    return filepath;
  }

  public async saveCertificateJson(
    certificate: ComplianceCertificate,
    outputDir: string,
  ): Promise<string> {
    await fs.mkdir(outputDir, { recursive: true });

    const filename = `${certificate.standard.toLowerCase()}-certificate-${certificate.id}.json`;
    const filepath = path.join(outputDir, filename);

    const content = this.certificateGenerator.generateCertificateJson(certificate);

    await fs.writeFile(filepath, content, 'utf-8');

    return filepath;
  }

  public verifyCertificate(certificate: ComplianceCertificate): boolean {
    return this.certificateGenerator.verifyCertificate(certificate);
  }

  public getCertificateStatus(certificate: ComplianceCertificate): string {
    return this.certificateGenerator.getCertificateStatus(certificate);
  }

  public getFramework(standard: ComplianceStandard): ComplianceFramework | undefined {
    return this.frameworks.get(standard);
  }

  public getAllFrameworks(): ComplianceFramework[] {
    return Array.from(this.frameworks.values());
  }

  public async generateComplianceSummary(
    reports: Map<ComplianceStandard, ComplianceReport>,
  ): Promise<string> {
    const summary = `# Compliance Assessment Summary

Generated: ${new Date().toISOString()}

## Overall Compliance Status

| Standard | Status | Pass Rate | Passed | Failed | Total |
|----------|--------|-----------|--------|--------|-------|
${Array.from(reports.entries())
  .map(
    ([, report]) => `| ${report.standard} | ${report.overallStatus.toUpperCase()} | ${report.summary.passRate.toFixed(1)}% | ${report.summary.passed} | ${report.summary.failed} | ${report.summary.totalChecks} |`,
  )
  .join('\n')}

## Detailed Reports

${Array.from(reports.entries())
  .map(([, report]) => this.generateReportSummary(report))
  .join('\n\n')}

---
Generated by Compliance Manager v1.0
`;

    return summary;
  }

  private generateReportSummary(report: ComplianceReport): string {
    const failedChecks = report.results.filter((r) => !r.passed);

    return `### ${report.standard} Report

**ID:** ${report.id}
**Status:** ${report.overallStatus.toUpperCase()}
**Pass Rate:** ${report.summary.passRate.toFixed(1)}%
**Next Audit:** ${report.nextAuditDate?.toISOString().split('T')[0]}

**Failed Controls (${failedChecks.length}):**
${failedChecks.length > 0 ? failedChecks.map((c) => `- ${c.checkId}: ${c.message}`).join('\n') : 'All controls passed!'}

**Key Recommendations:**
${report.recommendations.slice(0, 3).map((r) => `- ${r}`).join('\n')}`;
  }

  private getFileExtension(format: string): string {
    const extensions: Record<string, string> = {
      markdown: 'md',
      json: 'json',
      html: 'html',
      csv: 'csv',
    };

    return extensions[format] || 'txt';
  }

  public async runFullCompliance(
    projectRoot: string,
    organization: string,
    auditor: string,
    outputDir: string = './compliance-reports',
  ): Promise<{
    reports: Map<ComplianceStandard, ComplianceReport>;
    certificates: Map<ComplianceStandard, ComplianceCertificate>;
    summaryFile: string;
  }> {
    console.log(`Starting compliance assessment for ${organization}...`);

    // Create context
    const context: ComplianceContext = {
      projectRoot,
      environment: 'production',
      fileSystem: {
        exists: async (p: string) => {
          try {
            await fs.stat(p);
            return true;
          } catch {
            return false;
          }
        },
        read: async (p: string) => {
          return fs.readFile(p, 'utf-8');
        },
        list: async (p: string) => {
          const entries = await fs.readdir(p);
          return entries;
        },
      },
    };

    // Generate all reports
    console.log('Running compliance validators...');
    const reports = await this.generateAllReports(context);

    // Save reports
    console.log('Saving compliance reports...');
    await this.saveAllReports(reports, outputDir, ['markdown', 'json', 'html', 'csv']);

    // Generate certificates
    console.log('Generating compliance certificates...');
    const certificates = new Map<ComplianceStandard, ComplianceCertificate>();

    for (const [standard, report] of reports.entries()) {
      const certificate = this.generateCertificate(
        standard,
        report,
        organization,
        auditor,
        'Full compliance assessment',
      );

      certificates.set(standard, certificate);

      // Save certificates
      await this.saveCertificate(certificate, outputDir);
      await this.saveCertificateJson(certificate, outputDir);
    }

    // Generate summary
    console.log('Generating compliance summary...');
    const summary = await this.generateComplianceSummary(reports);
    const summaryFile = path.join(outputDir, 'compliance-summary.md');
    await fs.writeFile(summaryFile, summary, 'utf-8');

    console.log(`Compliance assessment completed. Reports saved to ${outputDir}`);

    return {
      reports,
      certificates,
      summaryFile,
    };
  }
}

export default ComplianceManager;
