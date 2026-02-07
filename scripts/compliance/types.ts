/**
 * Compliance Framework Types and Interfaces
 * Defines types for SOC 2, ISO 27001, GDPR, and HIPAA compliance
 */

export interface ComplianceCheck {
  id: string;
  name: string;
  description: string;
  category: string;
  severity: 'critical' | 'high' | 'medium' | 'low';
  standard: ComplianceStandard;
  control: string;
  validator: (context: ComplianceContext) => Promise<CheckResult>;
}

export interface CheckResult {
  checkId: string;
  passed: boolean;
  message: string;
  evidence?: string;
  findings?: string[];
  remediation?: string;
  timestamp: Date;
  details?: Record<string, unknown>;
}

export interface ComplianceContext {
  projectRoot: string;
  environment: 'development' | 'staging' | 'production';
  config?: Record<string, unknown>;
  fileSystem?: {
    exists: (path: string) => Promise<boolean>;
    read: (path: string) => Promise<string>;
    list: (path: string) => Promise<string[]>;
  };
}

export interface ComplianceReport {
  id: string;
  timestamp: Date;
  standard: ComplianceStandard;
  summary: {
    totalChecks: number;
    passed: number;
    failed: number;
    passRate: number;
  };
  results: CheckResult[];
  overallStatus: 'compliant' | 'non-compliant' | 'partial';
  recommendations: string[];
  nextAuditDate?: Date;
}

export interface ComplianceCertificate {
  id: string;
  standard: ComplianceStandard;
  issuedDate: Date;
  expiryDate: Date;
  organization: string;
  auditor: string;
  scope: string;
  complianceLevel: 'full' | 'partial' | 'conditional';
  controlCertifications: {
    controlId: string;
    status: 'certified' | 'non-certified' | 'conditional';
    evidence: string;
  }[];
  digitalSignature: string;
  certificateNumber: string;
}

export type ComplianceStandard = 'SOC2' | 'ISO27001' | 'GDPR' | 'HIPAA';

export interface ComplianceFramework {
  standard: ComplianceStandard;
  description: string;
  checks: ComplianceCheck[];
  auditFrequency: number; // days
  enforcingRegions?: string[];
  applicableTo?: string[];
}

export interface AuditLog {
  timestamp: Date;
  checkId: string;
  result: 'pass' | 'fail' | 'skip';
  evidence: string;
  auditor?: string;
  notes?: string;
}

export interface ComplianceMetrics {
  standard: ComplianceStandard;
  overallScore: number; // 0-100
  categoryScores: Record<string, number>;
  trendData: {
    date: Date;
    score: number;
  }[];
  failingControls: string[];
  improvementAreas: string[];
}

export interface DataProcessingRecord {
  id: string;
  dataCategory: string;
  purpose: string;
  legalBasis: string;
  processor?: string;
  recipient?: string;
  retentionPeriod: string;
  technicalMeasures: string[];
  organizationalMeasures: string[];
  gdprCompliant: boolean;
}

export interface RiskAssessment {
  id: string;
  standard: ComplianceStandard;
  controlId: string;
  riskLevel: 'critical' | 'high' | 'medium' | 'low';
  threat: string;
  likelihood: number; // 0-1
  impact: number; // 0-1
  mitigationStrategy: string;
  owner: string;
  targetDate: Date;
  status: 'open' | 'in-progress' | 'resolved' | 'accepted';
}
