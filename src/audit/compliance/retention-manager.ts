/**
 * Retention Policy Manager
 * Manages data retention policies for compliance frameworks
 */

import { RetentionPolicy, AuditEventType } from '../types';
import { LocalAuditStore } from '../backends/local-store';

/**
 * Predefined retention policies for compliance frameworks
 */
const PREDEFINED_POLICIES: Record<string, Partial<RetentionPolicy>> = {
  SOC2: {
    name: 'SOC 2 Retention',
    retentionDays: 365,
    archiveAfterDays: 90,
    deleteAfterDays: 2555, // ~7 years
    complianceFramework: 'SOC2',
  },
  HIPAA: {
    name: 'HIPAA Retention',
    retentionDays: 1825, // 5 years
    archiveAfterDays: 365,
    deleteAfterDays: 2555, // 7 years minimum
    complianceFramework: 'HIPAA',
  },
  'PCI-DSS': {
    name: 'PCI-DSS Retention',
    retentionDays: 365,
    archiveAfterDays: 90,
    deleteAfterDays: 730, // 2 years
    complianceFramework: 'PCI-DSS',
  },
  GDPR: {
    name: 'GDPR Retention',
    retentionDays: 90, // Shorter retention for GDPR
    archiveAfterDays: 30,
    deleteAfterDays: 365, // Minimum 1 year for legal holds
    complianceFramework: 'GDPR',
  },
  DEFAULT: {
    name: 'Default Retention',
    retentionDays: 180,
    archiveAfterDays: 60,
    deleteAfterDays: 365,
  },
};

export class RetentionManager {
  private policies: Map<string, RetentionPolicy> = new Map();
  private localStore: LocalAuditStore;
  private archivePath: string = '.audit/archive';

  constructor(localStore: LocalAuditStore) {
    this.localStore = localStore;
  }

  /**
   * Initialize with default policies
   */
  async initialize(): Promise<void> {
    // Add default policies
    this.addPolicy(this.createPolicy('DEFAULT', PREDEFINED_POLICIES.DEFAULT as RetentionPolicy));
    this.addPolicy(this.createPolicy('SOC2', PREDEFINED_POLICIES.SOC2 as RetentionPolicy));
    this.addPolicy(this.createPolicy('HIPAA', PREDEFINED_POLICIES.HIPAA as RetentionPolicy));
    this.addPolicy(this.createPolicy('PCI-DSS', PREDEFINED_POLICIES['PCI-DSS'] as RetentionPolicy));
    this.addPolicy(this.createPolicy('GDPR', PREDEFINED_POLICIES.GDPR as RetentionPolicy));

    console.log('Retention policies initialized');
  }

  /**
   * Add a retention policy
   */
  addPolicy(policy: RetentionPolicy): void {
    this.policies.set(policy.policyId, policy);
    console.log(`Added retention policy: ${policy.name} (${policy.policyId})`);
  }

  /**
   * Get a specific policy
   */
  getPolicy(policyId: string): RetentionPolicy | null {
    return this.policies.get(policyId) || null;
  }

  /**
   * List all policies
   */
  listPolicies(): RetentionPolicy[] {
    return Array.from(this.policies.values());
  }

  /**
   * Update a policy
   */
  updatePolicy(policyId: string, updates: Partial<RetentionPolicy>): RetentionPolicy {
    const policy = this.policies.get(policyId);
    if (!policy) {
      throw new Error(`Policy not found: ${policyId}`);
    }

    const updated: RetentionPolicy = {
      ...policy,
      ...updates,
      policyId: policy.policyId, // Prevent changing ID
    };

    this.policies.set(policyId, updated);
    console.log(`Updated retention policy: ${policyId}`);
    return updated;
  }

  /**
   * Enforce all active retention policies
   */
  async enforceAllPolicies(): Promise<Map<string, number>> {
    const results = new Map<string, number>();

    for (const policy of this.listPolicies()) {
      if (!policy.enabled) {
        continue;
      }

      try {
        const deletedCount = await this.enforcePolicy(policy);
        results.set(policy.policyId, deletedCount);
      } catch (error) {
        console.error(`Failed to enforce policy ${policy.policyId}:`, error);
      }
    }

    return results;
  }

  /**
   * Enforce a specific retention policy
   */
  async enforcePolicy(policy: RetentionPolicy): Promise<number> {
    if (!policy.enabled) {
      console.log(`Policy ${policy.policyId} is disabled, skipping`);
      return 0;
    }

    try {
      // Archive events after archiveAfterDays
      if (policy.archiveAfterDays) {
        await this.archiveOldEvents(policy, policy.archiveAfterDays);
      }

      // Delete events after deleteAfterDays
      if (policy.deleteAfterDays) {
        const deletedCount = await this.deleteOldEvents(policy, policy.deleteAfterDays);
        console.log(`Enforced retention policy ${policy.policyId}: deleted ${deletedCount} events`);
        return deletedCount;
      }

      return 0;
    } catch (error) {
      console.error(`Error enforcing policy ${policy.policyId}:`, error);
      throw error;
    }
  }

  /**
   * Archive old events
   */
  private async archiveOldEvents(policy: RetentionPolicy, archiveAfterDays: number): Promise<void> {
    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - archiveAfterDays);

    console.log(`Archiving events older than ${archiveAfterDays} days for policy ${policy.policyId}`);

    // Query events to archive
    const result = await this.localStore.query({
      eventTypes: policy.eventTypes,
      dateRange: {
        startDate: new Date(0),
        endDate: cutoffDate,
      },
      limit: 100000,
    });

    if (result.events.length === 0) {
      console.log('No events to archive');
      return;
    }

    // Archive to GCS (would implement actual archival)
    console.log(`Would archive ${result.events.length} events to ${this.archivePath}`);
  }

  /**
   * Delete old events
   */
  private async deleteOldEvents(policy: RetentionPolicy, deleteAfterDays: number): Promise<number> {
    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - deleteAfterDays);

    console.log(`Deleting events older than ${deleteAfterDays} days for policy ${policy.policyId}`);

    const deletedCount = await this.localStore.enforceRetention(deleteAfterDays);
    return deletedCount;
  }

  /**
   * Get policy for event type
   */
  getPolicyForEventType(eventType: AuditEventType): RetentionPolicy | null {
    // Find first enabled policy that includes this event type
    for (const policy of this.policies.values()) {
      if (policy.enabled && policy.eventTypes.includes(eventType)) {
        return policy;
      }
    }

    // Fall back to default policy
    return this.policies.get('DEFAULT') || null;
  }

  /**
   * Get retention days for event type
   */
  getRetentionDays(eventType: AuditEventType): number {
    const policy = this.getPolicyForEventType(eventType);
    return policy?.retentionDays || PREDEFINED_POLICIES.DEFAULT.retentionDays || 180;
  }

  /**
   * Create a new retention policy
   */
  private createPolicy(id: string, template: RetentionPolicy): RetentionPolicy {
    // Map framework name to event types
    const eventTypesByFramework: Record<string, AuditEventType[]> = {
      DEFAULT: Object.values(AuditEventType),
      SOC2: [
        AuditEventType.AUTH_LOGIN,
        AuditEventType.AUTH_LOGOUT,
        AuditEventType.AUTHZ_GRANT,
        AuditEventType.AUTHZ_REVOKE,
        AuditEventType.ADMIN_USER_CREATE,
        AuditEventType.ADMIN_USER_DELETE,
        AuditEventType.ADMIN_POLICY_CREATE,
        AuditEventType.ADMIN_POLICY_UPDATE,
        AuditEventType.ADMIN_POLICY_DELETE,
        AuditEventType.SECURITY_VIOLATION,
      ],
      HIPAA: [
        AuditEventType.RESOURCE_READ,
        AuditEventType.RESOURCE_UPDATE,
        AuditEventType.RESOURCE_DELETE,
        AuditEventType.AUTH_LOGIN,
        AuditEventType.AUTH_LOGOUT,
        AuditEventType.ADMIN_USER_CREATE,
        AuditEventType.ADMIN_USER_DELETE,
        AuditEventType.SECURITY_VIOLATION,
      ],
      'PCI-DSS': [
        AuditEventType.AUTH_LOGIN,
        AuditEventType.AUTH_FAILURE,
        AuditEventType.RESOURCE_CREATE,
        AuditEventType.RESOURCE_UPDATE,
        AuditEventType.RESOURCE_DELETE,
        AuditEventType.ADMIN_USER_CREATE,
        AuditEventType.ADMIN_USER_DELETE,
        AuditEventType.CONFIG_UPDATE,
        AuditEventType.SECURITY_VIOLATION,
      ],
      GDPR: [
        AuditEventType.RESOURCE_READ,
        AuditEventType.RESOURCE_UPDATE,
        AuditEventType.RESOURCE_DELETE,
        AuditEventType.RESOURCE_EXPORT,
        AuditEventType.AUTH_LOGIN,
        AuditEventType.AUTH_LOGOUT,
        AuditEventType.SECURITY_VIOLATION,
      ],
    };

    return {
      policyId: id,
      name: template.name || `${id} Policy`,
      description: template.description,
      eventTypes: eventTypesByFramework[id] || Object.values(AuditEventType),
      retentionDays: template.retentionDays || 180,
      archiveAfterDays: template.archiveAfterDays,
      deleteAfterDays: template.deleteAfterDays,
      complianceFramework: template.complianceFramework,
      enabled: true,
    };
  }

  /**
   * Get policy summary
   */
  getPolicySummary(): Record<string, any> {
    const summary: Record<string, any> = {};

    for (const policy of this.policies.values()) {
      summary[policy.policyId] = {
        name: policy.name,
        framework: policy.complianceFramework,
        retentionDays: policy.retentionDays,
        enabled: policy.enabled,
        eventTypes: policy.eventTypes.length,
      };
    }

    return summary;
  }
}
