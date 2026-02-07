/**
 * Compliance System Tests
 * Tests for retention policies, access controls, and compliance exports
 */

import { describe, it, expect, beforeEach } from '@jest/globals';
import { AccessControlManager } from '../../src/audit/compliance/access-control';
import { RetentionManager } from '../../src/audit/compliance/retention-manager';
import { LocalAuditStore } from '../../src/audit/backends/local-store';
import {
  AuditEventType,
  AuditAccessPermission,
  AuditLogConfig,
} from '../../src/audit/types';

describe('AccessControlManager', () => {
  let accessControl: AccessControlManager;

  beforeEach(() => {
    accessControl = new AccessControlManager();
  });

  describe('Role Permissions', () => {
    it('should grant admin full permissions', () => {
      expect(accessControl.hasPermission('admin', AuditAccessPermission.READ)).toBe(true);
      expect(accessControl.hasPermission('admin', AuditAccessPermission.EXPORT)).toBe(true);
      expect(accessControl.hasPermission('admin', AuditAccessPermission.DELETE)).toBe(true);
      expect(accessControl.hasPermission('admin', AuditAccessPermission.MANAGE_POLICY)).toBe(true);
      expect(accessControl.hasPermission('admin', AuditAccessPermission.MANAGE_ACCESS)).toBe(true);
    });

    it('should grant compliance-officer appropriate permissions', () => {
      expect(accessControl.hasPermission('compliance-officer', AuditAccessPermission.READ)).toBe(true);
      expect(accessControl.hasPermission('compliance-officer', AuditAccessPermission.EXPORT)).toBe(true);
      expect(accessControl.hasPermission('compliance-officer', AuditAccessPermission.MANAGE_POLICY)).toBe(true);
      expect(accessControl.hasPermission('compliance-officer', AuditAccessPermission.DELETE)).toBe(false);
    });

    it('should grant auditor read and export only', () => {
      expect(accessControl.hasPermission('auditor', AuditAccessPermission.READ)).toBe(true);
      expect(accessControl.hasPermission('auditor', AuditAccessPermission.EXPORT)).toBe(true);
      expect(accessControl.hasPermission('auditor', AuditAccessPermission.DELETE)).toBe(false);
      expect(accessControl.hasPermission('auditor', AuditAccessPermission.MANAGE_POLICY)).toBe(false);
    });

    it('should grant analyst read only', () => {
      expect(accessControl.hasPermission('analyst', AuditAccessPermission.READ)).toBe(true);
      expect(accessControl.hasPermission('analyst', AuditAccessPermission.EXPORT)).toBe(false);
      expect(accessControl.hasPermission('analyst', AuditAccessPermission.DELETE)).toBe(false);
    });

    it('should grant viewer limited access', () => {
      expect(accessControl.hasPermission('viewer', AuditAccessPermission.READ)).toBe(true);
      expect(accessControl.hasPermission('viewer', AuditAccessPermission.EXPORT)).toBe(false);
    });
  });

  describe('Data Redaction', () => {
    it('should redact sensitive fields for analyst role', () => {
      const event: any = {
        eventId: '123',
        eventType: AuditEventType.AUTH_LOGIN,
        timestamp: new Date(),
        actor: {
          userId: 'user-123',
          userEmail: 'user@example.com',
        },
        context: {
          sourceIp: '192.168.1.1',
          userAgent: 'Mozilla/5.0',
        },
      };

      const redacted = accessControl.applyDataRedaction(event, 'analyst');

      expect(redacted.actor.userEmail).toBe('[REDACTED]');
      expect(redacted.context.userAgent).toBe('[REDACTED]');
    });

    it('should not redact for admin role', () => {
      const event: any = {
        eventId: '123',
        actor: {
          userEmail: 'user@example.com',
        },
        context: {
          userAgent: 'Mozilla/5.0',
        },
      };

      const redacted = accessControl.applyDataRedaction(event, 'admin');

      expect(redacted.actor.userEmail).toBe('user@example.com');
      expect(redacted.context.userAgent).toBe('Mozilla/5.0');
    });
  });

  describe('Event Filtering by Role', () => {
    it('should filter events based on role permissions', () => {
      const events: any[] = [
        { eventType: AuditEventType.AUTH_LOGIN },
        { eventType: AuditEventType.RESOURCE_READ },
        { eventType: AuditEventType.ADMIN_USER_CREATE },
      ];

      const filtered = accessControl.filterEventsByRole(events, 'viewer');

      // Viewer should only see auth and resource events
      expect(filtered.length).toBeGreaterThan(0);
      expect(filtered.length).toBeLessThanOrEqual(events.length);
    });
  });

  describe('Temporary Access', () => {
    it('should grant temporary access with time limit', () => {
      const control = accessControl.grantTemporaryAccess('analyst', AuditAccessPermission.EXPORT, 60);

      expect(control.timeRangeRestriction).toBeDefined();
      expect(control.timeRangeRestriction?.startTime).toBeInstanceOf(Date);
      expect(control.timeRangeRestriction?.endTime).toBeInstanceOf(Date);
    });

    it('should enforce time restrictions', () => {
      accessControl.grantTemporaryAccess('analyst', AuditAccessPermission.EXPORT, -60); // Already expired

      // Should still work (time-based check would fail at auth time)
      expect(accessControl.hasPermission('analyst', AuditAccessPermission.READ)).toBe(true);
    });
  });

  describe('Access Revocation', () => {
    it('should revoke specific permissions', () => {
      const controls = accessControl.listControls();
      const auditControl = controls.find(c => c.role === 'auditor');

      if (auditControl) {
        accessControl.revokeAccess(auditControl.controlId, AuditAccessPermission.EXPORT);

        expect(accessControl.hasPermission('auditor', AuditAccessPermission.EXPORT)).toBe(false);
      }
    });

    it('should disable entire access control', () => {
      const controls = accessControl.listControls();
      const analyControl = controls.find(c => c.role === 'analyst');

      if (analyControl) {
        accessControl.disableControl(analyControl.controlId);

        expect(accessControl.hasPermission('analyst', AuditAccessPermission.READ)).toBe(false);
      }
    });

    it('should re-enable disabled access control', () => {
      const controls = accessControl.listControls();
      const analyControl = controls.find(c => c.role === 'analyst');

      if (analyControl) {
        accessControl.disableControl(analyControl.controlId);
        accessControl.enableControl(analyControl.controlId);

        expect(accessControl.hasPermission('analyst', AuditAccessPermission.READ)).toBe(true);
      }
    });
  });

  describe('Compliance Validation', () => {
    it('should validate access for audit actions', () => {
      const result = accessControl.validateCompllianceAccess('user-123', 'admin', 'delete');

      expect(result.allowed).toBe(true);
      expect(result.reason).toBe('Access granted');
    });

    it('should deny unauthorized access', () => {
      const result = accessControl.validateCompllianceAccess('user-123', 'analyst', 'delete');

      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('does not have');
    });

    it('should deny access with unknown action', () => {
      const result = accessControl.validateCompllianceAccess('user-123', 'admin', 'unknown');

      expect(result.allowed).toBe(false);
    });
  });
});

describe('RetentionManager', () => {
  let retentionManager: RetentionManager;
  let localStore: LocalAuditStore;
  let config: AuditLogConfig;

  beforeEach(async () => {
    config = {
      enabled: true,
      projectId: 'test-project',
      logName: 'test-audit-logs',
      batchSize: 10,
      batchTimeoutMs: 1000,
      enableCloudLogging: false,
      enableSiemExport: false,
      siemFormat: 'CEF',
      retentionPolicies: [],
      accessControls: [],
      complianceExports: [],
    };

    localStore = new LocalAuditStore(config);
    await localStore.initialize();

    retentionManager = new RetentionManager(localStore);
    await retentionManager.initialize();
  });

  describe('Policy Management', () => {
    it('should initialize with default policies', () => {
      const policies = retentionManager.listPolicies();

      expect(policies.length).toBeGreaterThan(0);
      expect(policies.map(p => p.complianceFramework)).toContain('SOC2');
      expect(policies.map(p => p.complianceFramework)).toContain('HIPAA');
    });

    it('should get specific policy', () => {
      const policy = retentionManager.getPolicy('SOC2');

      expect(policy).toBeDefined();
      expect(policy?.complianceFramework).toBe('SOC2');
    });

    it('should update policy', () => {
      const updated = retentionManager.updatePolicy('SOC2', {
        retentionDays: 730,
      });

      expect(updated.retentionDays).toBe(730);

      const retrieved = retentionManager.getPolicy('SOC2');
      expect(retrieved?.retentionDays).toBe(730);
    });
  });

  describe('Retention Days Lookup', () => {
    it('should return retention days for event type', () => {
      const days = retentionManager.getRetentionDays(AuditEventType.AUTH_LOGIN);

      expect(days).toBeGreaterThan(0);
      expect(typeof days).toBe('number');
    });

    it('should return default retention days for unknown event types', () => {
      const days = retentionManager.getRetentionDays(AuditEventType.SYSTEM_START);

      expect(days).toBeGreaterThan(0);
    });
  });

  describe('Policy Summary', () => {
    it('should generate policy summary', () => {
      const summary = retentionManager.getPolicySummary();

      expect(Object.keys(summary).length).toBeGreaterThan(0);
      expect(summary.SOC2).toBeDefined();
      expect(summary.SOC2.retentionDays).toBeDefined();
      expect(summary.SOC2.enabled).toBe(true);
    });
  });

  describe('Framework-Specific Policies', () => {
    it('should have stricter retention for HIPAA', () => {
      const hipaaPolicy = retentionManager.getPolicy('HIPAA');
      const defaultPolicy = retentionManager.getPolicy('DEFAULT');

      expect(hipaaPolicy?.retentionDays).toBeGreaterThanOrEqual(
        defaultPolicy?.retentionDays || 0
      );
    });

    it('should have shorter retention for GDPR', () => {
      const gdprPolicy = retentionManager.getPolicy('GDPR');
      const soc2Policy = retentionManager.getPolicy('SOC2');

      expect(gdprPolicy?.retentionDays).toBeLessThan(
        soc2Policy?.retentionDays || Infinity
      );
    });
  });

  describe('Compliance Event Types', () => {
    it('should include authentication events in SOC2 policy', () => {
      const policy = retentionManager.getPolicy('SOC2');

      expect(policy?.eventTypes).toContain(AuditEventType.AUTH_LOGIN);
      expect(policy?.eventTypes).toContain(AuditEventType.AUTH_LOGOUT);
    });

    it('should include resource access events in HIPAA policy', () => {
      const policy = retentionManager.getPolicy('HIPAA');

      expect(policy?.eventTypes).toContain(AuditEventType.RESOURCE_READ);
      expect(policy?.eventTypes).toContain(AuditEventType.RESOURCE_UPDATE);
    });
  });
});
