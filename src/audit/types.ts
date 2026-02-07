/**
 * Audit Logging Types and Interfaces
 * Comprehensive type definitions for audit events, compliance exports, and SIEM integration
 */

/**
 * Core audit event types for different operations
 */
export enum AuditEventType {
  // Authentication events
  AUTH_LOGIN = 'auth.login',
  AUTH_LOGOUT = 'auth.logout',
  AUTH_FAILURE = 'auth.failure',
  AUTH_MFA_SUCCESS = 'auth.mfa_success',
  AUTH_MFA_FAILURE = 'auth.mfa_failure',

  // Authorization events
  AUTHZ_GRANT = 'authz.grant',
  AUTHZ_REVOKE = 'authz.revoke',
  AUTHZ_DENY = 'authz.deny',
  AUTHZ_MODIFY = 'authz.modify',

  // Resource access
  RESOURCE_CREATE = 'resource.create',
  RESOURCE_READ = 'resource.read',
  RESOURCE_UPDATE = 'resource.update',
  RESOURCE_DELETE = 'resource.delete',
  RESOURCE_EXPORT = 'resource.export',

  // Configuration changes
  CONFIG_CREATE = 'config.create',
  CONFIG_UPDATE = 'config.update',
  CONFIG_DELETE = 'config.delete',

  // Administrative actions
  ADMIN_USER_CREATE = 'admin.user_create',
  ADMIN_USER_DELETE = 'admin.user_delete',
  ADMIN_USER_MODIFY = 'admin.user_modify',
  ADMIN_ROLE_CREATE = 'admin.role_create',
  ADMIN_ROLE_DELETE = 'admin.role_delete',
  ADMIN_POLICY_CREATE = 'admin.policy_create',
  ADMIN_POLICY_UPDATE = 'admin.policy_update',
  ADMIN_POLICY_DELETE = 'admin.policy_delete',

  // Security events
  SECURITY_VIOLATION = 'security.violation',
  SECURITY_ANOMALY = 'security.anomaly',
  SECURITY_THREAT = 'security.threat',

  // System events
  SYSTEM_START = 'system.start',
  SYSTEM_STOP = 'system.stop',
  SYSTEM_ERROR = 'system.error',

  // Compliance events
  COMPLIANCE_CHECK = 'compliance.check',
  COMPLIANCE_VIOLATION = 'compliance.violation',
}

/**
 * Severity levels for audit events
 */
export enum AuditSeverity {
  INFO = 'INFO',
  WARNING = 'WARNING',
  ERROR = 'ERROR',
  CRITICAL = 'CRITICAL',
}

/**
 * Status of audit actions
 */
export enum AuditStatus {
  SUCCESS = 'SUCCESS',
  FAILURE = 'FAILURE',
  PARTIAL = 'PARTIAL',
  PENDING = 'PENDING',
}

/**
 * Core audit event interface
 */
export interface AuditEvent {
  // Event identification
  eventId: string;
  eventType: AuditEventType;
  timestamp: Date;

  // Actor information
  actor: {
    userId: string;
    userEmail?: string;
    userName?: string;
    serviceAccount?: boolean;
  };

  // Resource information
  resource?: {
    resourceId: string;
    resourceType: string;
    resourceName?: string;
  };

  // Action details
  action: {
    status: AuditStatus;
    severity: AuditSeverity;
    description: string;
    errorMessage?: string;
  };

  // Context
  context: {
    sourceIp: string;
    userAgent?: string;
    requestId?: string;
    correlationId?: string;
    environment?: string;
  };

  // Metadata
  metadata?: Record<string, any>;

  // Changes (for update operations)
  changes?: {
    before?: Record<string, any>;
    after?: Record<string, any>;
  };

  // Compliance tags
  compliance?: {
    framework?: string;
    controlId?: string;
    dataClassification?: 'public' | 'internal' | 'confidential' | 'restricted';
    piiInvolved?: boolean;
  };
}

/**
 * Batch audit events for efficient processing
 */
export interface AuditEventBatch {
  batchId: string;
  events: AuditEvent[];
  batchTimestamp: Date;
}

/**
 * SIEM event format (CEF - Common Event Format)
 */
export interface CEFEvent {
  cefVersion: string;
  deviceVendor: string;
  deviceProduct: string;
  deviceVersion: string;
  signatureId: string;
  name: string;
  severity: string;
  extensions: Record<string, string | number | boolean>;
}

/**
 * LEEF event format (Log Event Extended Format)
 */
export interface LEEFEvent {
  version: string;
  vendor: string;
  product: string;
  version_field: string;
  eventId: string;
  [key: string]: any;
}

/**
 * Retention policy configuration
 */
export interface RetentionPolicy {
  policyId: string;
  name: string;
  description?: string;
  eventTypes: AuditEventType[];
  retentionDays: number;
  archiveAfterDays?: number;
  deleteAfterDays?: number;
  complianceFramework?: string;
  enabled: boolean;
}

/**
 * Access control for audit logs
 */
export interface AuditLogAccessControl {
  controlId: string;
  role: string;
  permissions: AuditAccessPermission[];
  eventTypeFilter?: AuditEventType[];
  resourceFilter?: string[];
  timeRangeRestriction?: {
    startTime: Date;
    endTime: Date;
  };
  dataRedaction?: {
    enabled: boolean;
    fields: string[];
  };
}

/**
 * Audit access permissions
 */
export enum AuditAccessPermission {
  READ = 'read',
  EXPORT = 'export',
  DELETE = 'delete',
  MANAGE_POLICY = 'manage_policy',
  MANAGE_ACCESS = 'manage_access',
}

/**
 * Compliance export configuration
 */
export interface ComplianceExport {
  exportId: string;
  name: string;
  framework: string; // SOC2, HIPAA, PCI-DSS, GDPR, etc.
  format: 'JSON' | 'CSV' | 'XML' | 'PARQUET';
  filters: {
    eventTypes?: AuditEventType[];
    dateRange: {
      startDate: Date;
      endDate: Date;
    };
    severity?: AuditSeverity[];
    status?: AuditStatus[];
  };
  transformations?: {
    redactPii?: boolean;
    anonymize?: boolean;
    aggregateByHour?: boolean;
  };
  destination: {
    type: 'gcs' | 'bigquery' | 'gcs_and_bigquery' | 'siem' | 'sftp';
    bucket?: string;
    dataset?: string;
    table?: string;
    host?: string;
    port?: number;
  };
  schedule?: {
    frequency: 'hourly' | 'daily' | 'weekly' | 'monthly' | 'on_demand';
    nextRun?: Date;
  };
}

/**
 * Cloud Logging entry
 */
export interface CloudLoggingEntry {
  logName: string;
  severity: string;
  timestamp: string;
  jsonPayload?: Record<string, any>;
  textPayload?: string;
  labels?: Record<string, string>;
  sourceLocation?: {
    file: string;
    line: number;
    function: string;
  };
  trace?: string;
  spanId?: string;
}

/**
 * Audit log stream statistics
 */
export interface AuditLogStatistics {
  periodStart: Date;
  periodEnd: Date;
  totalEvents: number;
  eventsByType: Record<AuditEventType, number>;
  eventsBySeverity: Record<AuditSeverity, number>;
  eventsByStatus: Record<AuditStatus, number>;
  failedEvents: number;
  exportedEvents: number;
}

/**
 * Configuration for audit logging system
 */
export interface AuditLogConfig {
  enabled: boolean;
  projectId: string;
  logName: string;
  batchSize: number;
  batchTimeoutMs: number;
  enableCloudLogging: boolean;
  enableSiemExport: boolean;
  siemEndpoint?: string;
  siemFormat: 'CEF' | 'LEEF' | 'JSON';
  retentionPolicies: RetentionPolicy[];
  accessControls: AuditLogAccessControl[];
  complianceExports: ComplianceExport[];
  encryption?: {
    enabled: boolean;
    keyId?: string;
  };
  sampling?: {
    enabled: boolean;
    rate: number; // 0.0 to 1.0
  };
}

/**
 * Audit event filter for querying
 */
export interface AuditEventFilter {
  eventTypes?: AuditEventType[];
  actors?: string[];
  resources?: string[];
  dateRange?: {
    startDate: Date;
    endDate: Date;
  };
  severity?: AuditSeverity[];
  status?: AuditStatus[];
  sourceIp?: string;
  limit?: number;
  offset?: number;
}

/**
 * Query result with pagination
 */
export interface AuditEventQueryResult {
  events: AuditEvent[];
  total: number;
  limit: number;
  offset: number;
  hasMore: boolean;
}
