#!/usr/bin/env node

/**
 * Compliance Validation CLI
 * Command-line tool for running compliance checks and generating reports
 */

import { program } from 'commander';
import fs from 'fs/promises';
import path from 'path';
import ComplianceManager from './compliance-manager';
import { ComplianceStandard, ComplianceContext } from './types';

const manager = new ComplianceManager();

program.name('compliance').description('Compliance validation and reporting tool').version('1.0.0');

// Validate command
program
  .command('validate')
  .description('Run compliance validation')
  .option('-s, --standard <standard>', 'Compliance standard (SOC2, ISO27001, GDPR, HIPAA)')
  .option('-p, --project <path>', 'Project root path', process.cwd())
  .option('-e, --environment <env>', 'Environment (development, staging, production)', 'production')
  .option('-o, --output <dir>', 'Output directory for reports', './compliance-reports')
  .action(async (options) => {
    try {
      const projectRoot = path.resolve(options.project);
      const outputDir = path.resolve(options.output);

      const context: ComplianceContext = {
        projectRoot,
        environment: options.environment,
        fileSystem: {
          exists: async (p: string) => {
            try {
              await fs.stat(p);
              return true;
            } catch {
              return false;
            }
          },
          read: async (p: string) => fs.readFile(p, 'utf-8'),
          list: async (p: string) => fs.readdir(p),
        },
      };

      if (options.standard) {
        // Single standard validation
        const standard = options.standard.toUpperCase() as ComplianceStandard;
        console.log(`\nValidating ${standard}...`);

        const results = await manager.validateStandard(standard, context);
        const report = manager.generateReport(standard, results);

        console.log(`\n${standard} Compliance Report:`);
        console.log(`Status: ${report.overallStatus.toUpperCase()}`);
        console.log(`Pass Rate: ${report.summary.passRate.toFixed(2)}%`);
        console.log(`Passed: ${report.summary.passed}/${report.summary.totalChecks}`);

        await manager.saveReport(report, outputDir, 'markdown');
        await manager.saveReport(report, outputDir, 'json');
        await manager.saveReport(report, outputDir, 'html');

        console.log(`\nReports saved to ${outputDir}`);
      } else {
        // All standards validation
        console.log('\nValidating all standards...');

        const reports = await manager.generateAllReports(context);

        console.log('\nCompliance Summary:');
        for (const [standard, report] of reports.entries()) {
          console.log(
            `\n${standard}: ${report.overallStatus.toUpperCase()} (${report.summary.passRate.toFixed(2)}%)`,
          );
        }

        await manager.saveAllReports(reports, outputDir, [
          'markdown',
          'json',
          'html',
          'csv',
        ]);

        const summary = await manager.generateComplianceSummary(reports);
        const summaryFile = path.join(outputDir, 'compliance-summary.md');
        await fs.writeFile(summaryFile, summary, 'utf-8');

        console.log(`\nReports saved to ${outputDir}`);
      }
    } catch (error) {
      console.error('Validation failed:', error);
      process.exit(1);
    }
  });

// Certificate command
program
  .command('certificate')
  .description('Generate compliance certificates')
  .option('-s, --standard <standard>', 'Compliance standard (SOC2, ISO27001, GDPR, HIPAA)')
  .option('-p, --project <path>', 'Project root path', process.cwd())
  .option('-o, --organization <name>', 'Organization name', 'My Organization')
  .option('-a, --auditor <name>', 'Auditor name', 'Compliance Officer')
  .option('-d, --output <dir>', 'Output directory for certificates', './compliance-reports')
  .action(async (options) => {
    try {
      const projectRoot = path.resolve(options.project);
      const outputDir = path.resolve(options.output);

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
          read: async (p: string) => fs.readFile(p, 'utf-8'),
          list: async (p: string) => fs.readdir(p),
        },
      };

      if (options.standard) {
        const standard = options.standard.toUpperCase() as ComplianceStandard;

        console.log(`\nGenerating ${standard} certificate...`);

        const results = await manager.validateStandard(standard, context);
        const report = manager.generateReport(standard, results);
        const certificate = manager.generateCertificate(
          standard,
          report,
          options.organization,
          options.auditor,
          'Full compliance assessment',
        );

        const htmlFile = await manager.saveCertificate(certificate, outputDir);
        const jsonFile = await manager.saveCertificateJson(certificate, outputDir);

        console.log(`\nCertificate generated:`);
        console.log(`Certificate Number: ${certificate.certificateNumber}`);
        console.log(`Compliance Level: ${certificate.complianceLevel}`);
        console.log(`Issued: ${certificate.issuedDate.toDateString()}`);
        console.log(`Expires: ${certificate.expiryDate.toDateString()}`);
        console.log(`Status: ${manager.getCertificateStatus(certificate)}`);
        console.log(`\nFiles saved:`);
        console.log(`- ${htmlFile}`);
        console.log(`- ${jsonFile}`);
      } else {
        console.error('Please specify a standard with --standard option');
        process.exit(1);
      }
    } catch (error) {
      console.error('Certificate generation failed:', error);
      process.exit(1);
    }
  });

// Report command
program
  .command('report')
  .description('Generate compliance report')
  .option('-s, --standard <standard>', 'Compliance standard (SOC2, ISO27001, GDPR, HIPAA)')
  .option('-p, --project <path>', 'Project root path', process.cwd())
  .option('-f, --format <format>', 'Report format (markdown, json, html, csv)', 'markdown')
  .option('-o, --output <dir>', 'Output directory', './compliance-reports')
  .action(async (options) => {
    try {
      const projectRoot = path.resolve(options.project);
      const outputDir = path.resolve(options.output);

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
          read: async (p: string) => fs.readFile(p, 'utf-8'),
          list: async (p: string) => fs.readdir(p),
        },
      };

      if (options.standard) {
        const standard = options.standard.toUpperCase() as ComplianceStandard;

        console.log(`\nGenerating ${standard} report...`);

        const results = await manager.validateStandard(standard, context);
        const report = manager.generateReport(standard, results);

        const file = await manager.saveReport(
          report,
          outputDir,
          options.format as 'markdown' | 'json' | 'html' | 'csv',
        );

        console.log(`\nReport generated: ${file}`);
      } else {
        console.error('Please specify a standard with --standard option');
        process.exit(1);
      }
    } catch (error) {
      console.error('Report generation failed:', error);
      process.exit(1);
    }
  });

// Full assessment command
program
  .command('assess')
  .description('Run full compliance assessment')
  .option('-p, --project <path>', 'Project root path', process.cwd())
  .option('-o, --organization <name>', 'Organization name', 'My Organization')
  .option('-a, --auditor <name>', 'Auditor name', 'Compliance Officer')
  .option('-d, --output <dir>', 'Output directory', './compliance-reports')
  .action(async (options) => {
    try {
      const projectRoot = path.resolve(options.project);
      const outputDir = path.resolve(options.output);

      console.log(`\nStarting full compliance assessment...`);
      console.log(`Project: ${projectRoot}`);
      console.log(`Organization: ${options.organization}`);
      console.log(`Output: ${outputDir}\n`);

      const result = await manager.runFullCompliance(
        projectRoot,
        options.organization,
        options.auditor,
        outputDir,
      );

      console.log('\nCompliance Assessment Complete!');
      console.log(`\nGenerated:`);
      console.log(`- ${result.reports.size} compliance reports`);
      console.log(`- ${result.certificates.size} compliance certificates`);
      console.log(`- Summary document: ${result.summaryFile}`);
    } catch (error) {
      console.error('Assessment failed:', error);
      process.exit(1);
    }
  });

// List command
program
  .command('list')
  .description('List available compliance frameworks')
  .action(() => {
    const frameworks = manager.getAllFrameworks();

    console.log('\nAvailable Compliance Frameworks:\n');

    frameworks.forEach((framework) => {
      console.log(`${framework.standard}`);
      console.log(`  Description: ${framework.description}`);
      console.log(`  Total Checks: ${framework.checks.length}`);
      console.log(`  Audit Frequency: ${framework.auditFrequency} days`);
      console.log(`  Applicable to: ${framework.applicableTo?.join(', ')}`);
      console.log(`  Regions: ${framework.enforcingRegions?.join(', ')}\n`);
    });
  });

// Verify command
program
  .command('verify <certificateFile>')
  .description('Verify compliance certificate')
  .action(async (certificateFile) => {
    try {
      const content = await fs.readFile(certificateFile, 'utf-8');
      const certificate = JSON.parse(content);

      const isValid = manager.verifyCertificate(certificate);
      const status = manager.getCertificateStatus(certificate);

      console.log('\nCertificate Verification Results:\n');
      console.log(`Certificate Number: ${certificate.certificateNumber}`);
      console.log(`Standard: ${certificate.standard}`);
      console.log(`Organization: ${certificate.organization}`);
      console.log(`Issued: ${certificate.issuedDate}`);
      console.log(`Expires: ${certificate.expiryDate}`);
      console.log(`\nVerification: ${isValid ? 'VALID' : 'INVALID'}`);
      console.log(`Status: ${status}`);
    } catch (error) {
      console.error('Verification failed:', error);
      process.exit(1);
    }
  });

program.parse(process.argv);
