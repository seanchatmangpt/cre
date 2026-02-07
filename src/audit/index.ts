/**
 * Comprehensive Audit Logging System
 * Exports for Cloud Logging, SIEM integration, compliance exports, and access control
 */

// Type exports
export * from './types';

// Manager exports
export { AuditManager } from './audit-manager';

// Backend exports
export { CloudLoggingBackend } from './backends/cloud-logging';
export { SiemExporter } from './backends/siem-exporter';
export { LocalAuditStore } from './backends/local-store';

// Compliance exports
export { ComplianceExportManager } from './compliance/export-manager';
export { RetentionManager } from './compliance/retention-manager';
export { AccessControlManager } from './compliance/access-control';

// Re-export for convenience
import { AuditManager } from './audit-manager';
import { ComplianceExportManager } from './compliance/export-manager';
import { RetentionManager } from './compliance/retention-manager';
import { AccessControlManager } from './compliance/access-control';
import { AuditLogConfig } from './types';

/**
 * Initialize the complete audit logging system
 */
export async function initializeAuditSystem(config: AuditLogConfig) {
  const auditManager = new AuditManager(config);
  const complianceExportManager = new ComplianceExportManager(auditManager['localStore'], config.projectId);
  const retentionManager = new RetentionManager(auditManager['localStore']);
  const accessControlManager = new AccessControlManager();

  await auditManager.initialize();
  await retentionManager.initialize();

  return {
    audit: auditManager,
    compliance: complianceExportManager,
    retention: retentionManager,
    accessControl: accessControlManager,
  };
}
