/**
 * Compliance Framework - Main Export Module
 */

// Types
export type {
  ComplianceCheck,
  CheckResult,
  ComplianceContext,
  ComplianceReport,
  ComplianceCertificate,
  ComplianceStandard,
  ComplianceFramework,
  AuditLog,
  ComplianceMetrics,
  DataProcessingRecord,
  RiskAssessment,
} from './types';

// Validators
export { SOC2Validator } from './validators/soc2-validator';
export { ISO27001Validator } from './validators/iso27001-validator';
export { GDPRValidator } from './validators/gdpr-validator';
export { HIPAAValidator } from './validators/hipaa-validator';

// Generators
export { ComplianceReportGenerator } from './report-generator';
export { CertificateGenerator } from './certificate-generator';

// Manager
export { default as ComplianceManager } from './compliance-manager';
export { ComplianceManager as NamedComplianceManager } from './compliance-manager';

// Utilities
export { version } from './package.json';
