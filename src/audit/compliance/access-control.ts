/**
 * Audit Log Access Control Manager
 * Manages access permissions and audit log visibility for different roles
 */

import { v4 as uuidv4 } from 'uuid';
import {
  AuditLogAccessControl,
  AuditAccessPermission,
  AuditEvent,
  AuditEventType,
} from '../types';

export class AccessControlManager {
  private controls: Map<string, AuditLogAccessControl> = new Map();
  private roleHierarchy: Map<string, string[]> = new Map();

  constructor() {
    this.initializeRoleHierarchy();
    this.initializeDefaultControls();
  }

  /**
   * Initialize role hierarchy
   */
  private initializeRoleHierarchy(): void {
    // Define role inheritance
    this.roleHierarchy.set('admin', ['admin', 'compliance-officer', 'auditor', 'analyst']);
    this.roleHierarchy.set('compliance-officer', ['compliance-officer', 'auditor', 'analyst']);
    this.roleHierarchy.set('auditor', ['auditor', 'analyst']);
    this.roleHierarchy.set('analyst', ['analyst']);
    this.roleHierarchy.set('viewer', ['viewer']);
    this.roleHierarchy.set('system', ['system']);
  }

  /**
   * Initialize default access controls
   */
  private initializeDefaultControls(): void {
    // Admin role - full access
    this.addControl({
      controlId: uuidv4(),
      role: 'admin',
      permissions: [
        AuditAccessPermission.READ,
        AuditAccessPermission.EXPORT,
        AuditAccessPermission.DELETE,
        AuditAccessPermission.MANAGE_POLICY,
        AuditAccessPermission.MANAGE_ACCESS,
      ],
      enabled: true,
    });

    // Compliance Officer - read, export, manage policy
    this.addControl({
      controlId: uuidv4(),
      role: 'compliance-officer',
      permissions: [AuditAccessPermission.READ, AuditAccessPermission.EXPORT, AuditAccessPermission.MANAGE_POLICY],
      dataRedaction: {
        enabled: false,
        fields: [],
      },
      enabled: true,
    });

    // Auditor - read, export
    this.addControl({
      controlId: uuidv4(),
      role: 'auditor',
      permissions: [AuditAccessPermission.READ, AuditAccessPermission.EXPORT],
      dataRedaction: {
        enabled: true,
        fields: ['context.userAgent', 'metadata'],
      },
      enabled: true,
    });

    // Analyst - read only
    this.addControl({
      controlId: uuidv4(),
      role: 'analyst',
      permissions: [AuditAccessPermission.READ],
      dataRedaction: {
        enabled: true,
        fields: ['actor.userEmail', 'context.userAgent', 'context.sourceIp', 'metadata'],
      },
      enabled: true,
    });

    // Viewer - read only with filters
    this.addControl({
      controlId: uuidv4(),
      role: 'viewer',
      permissions: [AuditAccessPermission.READ],
      eventTypeFilter: [AuditEventType.AUTH_LOGIN, AuditEventType.RESOURCE_CREATE, AuditEventType.RESOURCE_DELETE],
      dataRedaction: {
        enabled: true,
        fields: ['actor', 'context', 'metadata'],
      },
      enabled: true,
    });
  }

  /**
   * Add an access control rule
   */
  addControl(control: Omit<AuditLogAccessControl, 'enabled'>): void {
    const fullControl: AuditLogAccessControl = {
      ...control,
      enabled: true,
    };
    this.controls.set(control.controlId, fullControl);
    console.log(`Added access control: ${control.role}`);
  }

  /**
   * Check if a user/role has permission
   */
  hasPermission(role: string, permission: AuditAccessPermission, resource?: string): boolean {
    const control = this.getControl(role);

    if (!control || !control.enabled) {
      return false;
    }

    // Check basic permission
    if (!control.permissions.includes(permission)) {
      return false;
    }

    // Check resource filter if applicable
    if (resource && control.resourceFilter && !control.resourceFilter.includes(resource)) {
      return false;
    }

    // Check time range restriction
    if (control.timeRangeRestriction) {
      const now = new Date();
      if (now < control.timeRangeRestriction.startTime || now > control.timeRangeRestriction.endTime) {
        return false;
      }
    }

    return true;
  }

  /**
   * Get control for role (or inherited role)
   */
  private getControl(role: string): AuditLogAccessControl | null {
    // Direct match
    for (const control of this.controls.values()) {
      if (control.role === role) {
        return control;
      }
    }

    // No control found
    return null;
  }

  /**
   * Apply data redaction based on role
   */
  applyDataRedaction(event: AuditEvent, role: string): AuditEvent {
    const control = this.getControl(role);

    if (!control || !control.dataRedaction?.enabled || !control.dataRedaction.fields.length) {
      return event;
    }

    const redacted = { ...event };
    const fieldsToRedact = control.dataRedaction.fields;

    for (const field of fieldsToRedact) {
      redacted = this.redactField(redacted, field);
    }

    return redacted;
  }

  /**
   * Redact a specific field in an event
   */
  private redactField(event: AuditEvent, fieldPath: string): AuditEvent {
    const redacted = JSON.parse(JSON.stringify(event));
    const parts = fieldPath.split('.');

    let current = redacted;
    for (let i = 0; i < parts.length - 1; i++) {
      if (current[parts[i]] === undefined) {
        return redacted;
      }
      current = current[parts[i]];
    }

    const lastPart = parts[parts.length - 1];
    if (current[lastPart] !== undefined) {
      current[lastPart] = '[REDACTED]';
    }

    return redacted;
  }

  /**
   * Filter events based on role
   */
  filterEventsByRole(events: AuditEvent[], role: string): AuditEvent[] {
    const control = this.getControl(role);

    if (!control) {
      return [];
    }

    let filtered = events;

    // Apply event type filter
    if (control.eventTypeFilter && control.eventTypeFilter.length > 0) {
      filtered = filtered.filter((e) => control.eventTypeFilter?.includes(e.eventType));
    }

    // Apply data redaction
    filtered = filtered.map((e) => this.applyDataRedaction(e, role));

    return filtered;
  }

  /**
   * Audit an access request
   */
  logAccessRequest(userId: string, role: string, permission: AuditAccessPermission, granted: boolean): void {
    console.log(`Access request: user=${userId}, role=${role}, permission=${permission}, granted=${granted}`);
    // In production, this would log to the audit system itself
  }

  /**
   * Grant temporary access
   */
  grantTemporaryAccess(
    role: string,
    permission: AuditAccessPermission,
    durationMinutes: number
  ): AuditLogAccessControl {
    const control = this.getControl(role);
    if (!control) {
      throw new Error(`No control found for role: ${role}`);
    }

    const updated = { ...control };
    const now = new Date();
    const endTime = new Date(now.getTime() + durationMinutes * 60000);

    updated.timeRangeRestriction = {
      startTime: now,
      endTime,
    };

    this.controls.set(control.controlId, updated);
    console.log(`Granted temporary ${permission} access to ${role} for ${durationMinutes} minutes`);

    return updated;
  }

  /**
   * Revoke access
   */
  revokeAccess(controlId: string, permission: AuditAccessPermission): AuditLogAccessControl {
    const control = this.controls.get(controlId);
    if (!control) {
      throw new Error(`Control not found: ${controlId}`);
    }

    const updated = {
      ...control,
      permissions: control.permissions.filter((p) => p !== permission),
    };

    this.controls.set(controlId, updated);
    console.log(`Revoked ${permission} from ${control.role}`);

    return updated;
  }

  /**
   * Disable access control
   */
  disableControl(controlId: string): void {
    const control = this.controls.get(controlId);
    if (!control) {
      throw new Error(`Control not found: ${controlId}`);
    }

    control.enabled = false;
    this.controls.set(controlId, control);
    console.log(`Disabled access control: ${control.role}`);
  }

  /**
   * Enable access control
   */
  enableControl(controlId: string): void {
    const control = this.controls.get(controlId);
    if (!control) {
      throw new Error(`Control not found: ${controlId}`);
    }

    control.enabled = true;
    this.controls.set(controlId, control);
    console.log(`Enabled access control: ${control.role}`);
  }

  /**
   * Get all controls
   */
  listControls(): AuditLogAccessControl[] {
    return Array.from(this.controls.values());
  }

  /**
   * Get control for role
   */
  getControlForRole(role: string): AuditLogAccessControl | null {
    return this.getControl(role);
  }

  /**
   * Validate access for compliance audit
   */
  validateCompllianceAccess(userId: string, role: string, action: string): {
    allowed: boolean;
    reason: string;
  } {
    const control = this.getControl(role);

    if (!control) {
      return { allowed: false, reason: 'No access control defined for role' };
    }

    if (!control.enabled) {
      return { allowed: false, reason: 'Access control is disabled' };
    }

    let permission: AuditAccessPermission;
    if (action === 'read' || action === 'query') {
      permission = AuditAccessPermission.READ;
    } else if (action === 'export') {
      permission = AuditAccessPermission.EXPORT;
    } else if (action === 'delete') {
      permission = AuditAccessPermission.DELETE;
    } else {
      return { allowed: false, reason: 'Unknown action' };
    }

    if (!control.permissions.includes(permission)) {
      return { allowed: false, reason: `Role ${role} does not have ${permission} permission` };
    }

    if (control.timeRangeRestriction) {
      const now = new Date();
      if (now < control.timeRangeRestriction.startTime || now > control.timeRangeRestriction.endTime) {
        return { allowed: false, reason: 'Access outside allowed time range' };
      }
    }

    return { allowed: true, reason: 'Access granted' };
  }

  /**
   * Get role hierarchy
   */
  getRoleHierarchy(): Map<string, string[]> {
    return this.roleHierarchy;
  }
}
