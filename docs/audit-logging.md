# Comprehensive Audit Logging System

## Overview

This audit logging system provides enterprise-grade audit trails with integration to Google Cloud Logging, SIEM systems, compliance frameworks (SOC2, HIPAA, PCI-DSS, GDPR), and fine-grained access controls.

## Features

- **Cloud Logging Integration**: Real-time export to Google Cloud Logging
- **SIEM Export**: CEF and LEEF format support for SIEM integration
- **Compliance Frameworks**: Built-in support for SOC2, HIPAA, PCI-DSS, and GDPR
- **Retention Policies**: Automated data retention based on compliance requirements
- **Access Controls**: Role-based access with data redaction
- **Compliance Exports**: Multi-format exports (JSON, CSV, XML, PARQUET) to GCS and BigQuery
- **Event Batching**: Efficient event queuing and batch processing
- **Local Persistence**: SQLite-backed local store for queryability
- **Performance**: Event sampling and optimized batch processing

## Installation

```bash
npm install @google-cloud/logging @google-cloud/storage @google-cloud/bigquery uuid
```

## Quick Start

### Basic Configuration

```typescript
import { initializeAuditSystem, AuditEventType, AuditSeverity } from './src/audit';

const config = {
  enabled: true,
  projectId: 'your-gcp-project',
  logName: 'audit-logs',
  batchSize: 50,
  batchTimeoutMs: 5000,
  enableCloudLogging: true,
  enableSiemExport: true,
  siemEndpoint: 'siem-server.example.com:514',
  siemFormat: 'CEF' as const,
  retentionPolicies: [],
  accessControls: [],
  complianceExports: [],
};

const { audit, compliance, retention, accessControl } = await initializeAuditSystem(config);
```

### Logging Events

#### Authentication Events

```typescript
// Successful login
await audit.logAuthEvent(
  userId,
  true,
  sourceIp,
  userAgent,
  { timestamp: Date.now() }
);

// Failed authentication
await audit.logAuthEvent(
  userId,
  false,
  sourceIp,
  userAgent,
  { reason: 'invalid_credentials' }
);
```

#### Resource Access

```typescript
await audit.logResourceEvent(
  AuditEventType.RESOURCE_READ,
  userId,
  resourceId,
  'Document',
  true,
  sourceIp,
  undefined,
  { documentType: 'PDF' }
);
```

#### Authorization Changes

```typescript
await audit.logAuthzEvent(
  userId,
  'grant',          // 'grant', 'revoke', 'deny', 'modify'
  'viewer',         // role
  resourceId,
  sourceIp,
  { scope: 'read-only' }
);
```

#### Security Events

```typescript
await audit.logSecurityEvent(
  AuditEventType.SECURITY_ANOMALY,
  'Multiple failed login attempts detected',
  AuditSeverity.WARNING,
  sourceIp,
  userId,
  { failureCount: 5, timeWindow: '5m' }
);
```

### Querying Events

```typescript
// Query all events
const result = await audit.queryEvents({});

// Query by event type
const authEvents = await audit.queryEvents({
  eventTypes: [AuditEventType.AUTH_LOGIN, AuditEventType.AUTH_LOGOUT],
  limit: 100,
});

// Query by user
const userEvents = await audit.queryEvents({
  actors: ['user-123'],
  dateRange: {
    startDate: new Date('2024-01-01'),
    endDate: new Date('2024-01-31'),
  },
});

// Query with pagination
const page1 = await audit.queryEvents({
  limit: 50,
  offset: 0,
});

const page2 = await audit.queryEvents({
  limit: 50,
  offset: 50,
});
```

### Getting Statistics

```typescript
const startDate = new Date();
startDate.setDate(startDate.getDate() - 7); // Last 7 days
const endDate = new Date();

const stats = await audit.getStatistics(startDate, endDate);

console.log(`Total events: ${stats.totalEvents}`);
console.log(`Failed events: ${stats.failedEvents}`);
console.log(`By severity:`, stats.eventsBySeverity);
console.log(`By status:`, stats.eventsByStatus);
```

## Compliance Frameworks

### SOC2 Compliance

```typescript
const config = {
  // ... other config
  retentionPolicies: [
    {
      policyId: 'SOC2',
      name: 'SOC 2 Retention',
      eventTypes: [
        AuditEventType.AUTH_LOGIN,
        AuditEventType.AUTH_LOGOUT,
        AuditEventType.AUTHZ_GRANT,
        AuditEventType.AUTHZ_REVOKE,
        AuditEventType.ADMIN_USER_CREATE,
        AuditEventType.ADMIN_USER_DELETE,
        AuditEventType.SECURITY_VIOLATION,
      ],
      retentionDays: 365,
      archiveAfterDays: 90,
      deleteAfterDays: 2555,
      complianceFramework: 'SOC2',
      enabled: true,
    },
  ],
};
```

### HIPAA Compliance

```typescript
const hipaaPolicy = {
  policyId: 'HIPAA',
  name: 'HIPAA Retention',
  eventTypes: [
    AuditEventType.RESOURCE_READ,
    AuditEventType.RESOURCE_UPDATE,
    AuditEventType.RESOURCE_DELETE,
    AuditEventType.AUTH_LOGIN,
    AuditEventType.AUTH_LOGOUT,
  ],
  retentionDays: 1825, // 5 years
  archiveAfterDays: 365,
  deleteAfterDays: 2555,
  complianceFramework: 'HIPAA',
  enabled: true,
  compliance: {
    framework: 'HIPAA',
    dataClassification: 'restricted',
    piiInvolved: true,
  },
};
```

### PCI-DSS Compliance

```typescript
const pciPolicy = {
  policyId: 'PCI-DSS',
  name: 'PCI-DSS Retention',
  eventTypes: [
    AuditEventType.AUTH_LOGIN,
    AuditEventType.AUTH_FAILURE,
    AuditEventType.RESOURCE_CREATE,
    AuditEventType.RESOURCE_UPDATE,
    AuditEventType.RESOURCE_DELETE,
    AuditEventType.CONFIG_UPDATE,
    AuditEventType.SECURITY_VIOLATION,
  ],
  retentionDays: 365,
  archiveAfterDays: 90,
  deleteAfterDays: 730,
  complianceFramework: 'PCI-DSS',
  enabled: true,
};
```

### GDPR Compliance

```typescript
const gdprPolicy = {
  policyId: 'GDPR',
  name: 'GDPR Retention',
  eventTypes: [
    AuditEventType.RESOURCE_READ,
    AuditEventType.RESOURCE_UPDATE,
    AuditEventType.RESOURCE_DELETE,
    AuditEventType.RESOURCE_EXPORT,
  ],
  retentionDays: 90,
  archiveAfterDays: 30,
  deleteAfterDays: 365,
  complianceFramework: 'GDPR',
  enabled: true,
  transformations: {
    redactPii: true,
    anonymize: true,
  },
};
```

## Access Control

### Role-Based Access

```typescript
// Check permissions
const canExport = accessControl.hasPermission(
  'auditor',
  AuditAccessPermission.EXPORT
);

// Apply data redaction
const redactedEvent = accessControl.applyDataRedaction(event, 'analyst');

// Filter events by role
const visibleEvents = accessControl.filterEventsByRole(events, 'analyst');

// Validate compliance access
const validation = accessControl.validateCompllianceAccess(
  userId,
  'analyst',
  'read'
);

if (validation.allowed) {
  // Grant access
} else {
  console.log(`Access denied: ${validation.reason}`);
}
```

### Role Hierarchy

- **admin**: Full access (read, export, delete, manage policy, manage access)
- **compliance-officer**: Read, export, manage policy
- **auditor**: Read, export
- **analyst**: Read only (with data redaction)
- **viewer**: Read only (filtered events with data redaction)

### Temporary Access

```typescript
// Grant temporary export access for 60 minutes
accessControl.grantTemporaryAccess(
  'analyst',
  AuditAccessPermission.EXPORT,
  60
);

// Revoke specific permission
accessControl.revokeAccess(controlId, AuditAccessPermission.EXPORT);

// Disable entire role access
accessControl.disableControl(controlId);
```

## Compliance Exports

### Export to Google Cloud Storage

```typescript
const exportConfig = {
  exportId: uuidv4(),
  name: 'Q1 2024 SOC2 Audit Export',
  framework: 'SOC2',
  format: 'JSON' as const,
  filters: {
    eventTypes: [
      AuditEventType.AUTH_LOGIN,
      AuditEventType.AUTHZ_GRANT,
    ],
    dateRange: {
      startDate: new Date('2024-01-01'),
      endDate: new Date('2024-03-31'),
    },
  },
  transformations: {
    redactPii: true,
    anonymize: false,
  },
  destination: {
    type: 'gcs' as const,
    bucket: 'compliance-exports',
  },
};

const exportId = await compliance.executeExport(exportConfig);
console.log(`Export completed: ${exportId}`);
```

### Export to BigQuery

```typescript
const bigQueryExport = {
  exportId: uuidv4(),
  name: 'HIPAA Compliance Report',
  framework: 'HIPAA',
  format: 'PARQUET' as const,
  filters: {
    dateRange: {
      startDate: new Date('2024-01-01'),
      endDate: new Date('2024-12-31'),
    },
  },
  destination: {
    type: 'bigquery' as const,
    dataset: 'audit_logs',
    table: 'hipaa_events',
  },
  schedule: {
    frequency: 'monthly' as const,
  },
};

await compliance.executeExport(bigQueryExport);
```

### Export with Transformations

```typescript
const transformedExport = {
  // ... export config
  transformations: {
    redactPii: true,           // Redact email, IP, userAgent
    anonymize: true,           // Hash user IDs
    aggregateByHour: true,     // Summarize to hourly buckets
  },
};
```

## Retention Policies

### Enforce All Policies

```typescript
const results = await retention.enforceAllPolicies();

for (const [policyId, deletedCount] of results) {
  console.log(`Policy ${policyId}: deleted ${deletedCount} events`);
}
```

### Get Retention for Event Type

```typescript
const days = retention.getRetentionDays(AuditEventType.AUTH_LOGIN);
console.log(`Auth events retained for ${days} days`);
```

### View Policy Summary

```typescript
const summary = retention.getPolicySummary();
console.log(JSON.stringify(summary, null, 2));

// Output:
// {
//   "SOC2": {
//     "name": "SOC 2 Retention",
//     "framework": "SOC2",
//     "retentionDays": 365,
//     "enabled": true,
//     "eventTypes": 10
//   },
//   ...
// }
```

## SIEM Integration

### Supported Formats

#### CEF (Common Event Format)

```
CEF:0|CRE|CRE-AuditLog|1.0|EVT000042|auth.login|2|eventId=abc123 eventType=auth.login eventTimestamp=2024-01-15T10:30:00Z ...
```

#### LEEF (Log Event Extended Format)

```
LEEF:2.0|CRE|CRE-AuditLog|1.0|EVT000042|eventUuid=abc123	eventType=auth.login	eventTimestamp=1705329000000 ...
```

### Configuration

```typescript
const config = {
  enableSiemExport: true,
  siemEndpoint: 'siem-server.example.com:514',
  siemFormat: 'CEF' as const, // or 'LEEF'
};
```

### Event Fields in SIEM Export

- Event ID and Type
- Timestamp
- Actor (user ID, email, name)
- Resource (ID, type, name)
- Action (status, severity, description)
- Context (source IP, user agent, request ID)
- Compliance (framework, classification, PII flag)

## Cloud Logging Integration

### Setup

```typescript
const config = {
  enableCloudLogging: true,
  projectId: 'your-gcp-project',
  logName: 'audit-logs',
};
```

### Log Entry Format

Each event is exported as:

```json
{
  "severity": "INFO",
  "jsonPayload": {
    "eventId": "abc-123",
    "eventType": "auth.login",
    "actor": { "userId": "user-123", "userEmail": "user@example.com" },
    "action": { "status": "SUCCESS", "severity": "INFO" },
    "context": { "sourceIp": "192.168.1.1", "requestId": "req-456" }
  },
  "labels": {
    "event_type": "auth.login",
    "severity": "INFO",
    "actor_id": "user-123"
  }
}
```

### Create Log Sink

```typescript
const cloudLogging = new CloudLoggingBackend(config);
await cloudLogging.initialize();

// Export to BigQuery
await cloudLogging.createLogSink(
  'audit-to-bigquery',
  'bigquery.googleapis.com/projects/my-project/datasets/audit_logs'
);

// Export to GCS
await cloudLogging.createLogSink(
  'audit-to-gcs',
  'storage.googleapis.com/my-audit-bucket'
);
```

## Performance Optimization

### Event Sampling

```typescript
const config = {
  sampling: {
    enabled: true,
    rate: 0.1, // Sample 10% of events
  },
};
```

### Batch Configuration

```typescript
const config = {
  batchSize: 100,          // Number of events before flushing
  batchTimeoutMs: 10000,   // Max time before flushing
};
```

### Memory Management

```typescript
// Get queue size
const queueSize = audit.getQueueSize();

// Force flush
await audit.forceFlush();

// Graceful shutdown
await audit.shutdown();
```

## Best Practices

1. **Use Sampling in High-Volume Scenarios**: Enable sampling to reduce storage costs while maintaining statistical validity.

2. **Set Appropriate Batch Sizes**: Balance between latency and throughput. Larger batches reduce overhead but increase latency.

3. **Enforce Retention Policies**: Regularly run `enforceAllPolicies()` to comply with data retention requirements.

4. **Monitor Access**: Track who accesses audit logs using the access control system.

5. **Export Regularly**: Schedule regular compliance exports for audit trail verification.

6. **Use Correct Severity Levels**: Assign appropriate severity to help with SIEM alerting.

7. **Include Metadata**: Add relevant metadata to events for better analysis.

8. **Test Compliance Exports**: Validate exported data against compliance requirements.

## API Reference

See `/src/audit/types.ts` for complete type definitions.

### Key Enums

- `AuditEventType`: 30+ event types for different operations
- `AuditSeverity`: INFO, WARNING, ERROR, CRITICAL
- `AuditStatus`: SUCCESS, FAILURE, PARTIAL, PENDING
- `AuditAccessPermission`: READ, EXPORT, DELETE, MANAGE_POLICY, MANAGE_ACCESS

### Main Classes

- `AuditManager`: Core event logging and management
- `CloudLoggingBackend`: Google Cloud Logging integration
- `SiemExporter`: SIEM export in CEF/LEEF format
- `LocalAuditStore`: Local event persistence and querying
- `ComplianceExportManager`: Multi-format compliance exports
- `RetentionManager`: Retention policy management
- `AccessControlManager`: Role-based access control

## Troubleshooting

### Events Not Being Exported to Cloud Logging

1. Check GCP project ID configuration
2. Verify service account has `logging.logEntries.create` permission
3. Check `enableCloudLogging` is true
4. Verify network connectivity

### SIEM Events Not Appearing

1. Verify `siemEndpoint` is accessible
2. Check firewall rules allow outbound to SIEM port (typically 514)
3. Verify `enableSiemExport` is true
4. Check SIEM system is accepting connections

### Compliance Export Failures

1. Verify GCS bucket or BigQuery dataset exists
2. Check service account has appropriate permissions
3. Verify data transformations don't cause invalid output
4. Check available disk/memory for large exports

## Security Considerations

- All API calls should require authentication
- Use HTTPS for SIEM export (if supported)
- Rotate Cloud Logging API keys regularly
- Audit access to audit logs themselves
- Store exported compliance reports securely
- Use data redaction for sensitive roles
- Enable data encryption at rest
