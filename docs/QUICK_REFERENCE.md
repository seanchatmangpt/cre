# Audit Logging Quick Reference Guide

## Installation

```bash
npm install @google-cloud/logging @google-cloud/storage @google-cloud/bigquery uuid
```

## Initialize System

```typescript
import { initializeAuditSystem } from './src/audit';

const { audit, compliance, retention, accessControl } = await initializeAuditSystem({
  enabled: true,
  projectId: 'your-gcp-project',
  logName: 'audit-logs',
  batchSize: 50,
  batchTimeoutMs: 5000,
  enableCloudLogging: true,
  enableSiemExport: true,
  siemEndpoint: 'siem:514',
  siemFormat: 'CEF',
  retentionPolicies: [],
  accessControls: [],
  complianceExports: [],
});
```

## Common Tasks

### Log Authentication Events

```typescript
// Successful login
await audit.logAuthEvent(userId, true, sourceIp, userAgent);

// Failed login
await audit.logAuthEvent(userId, false, sourceIp, userAgent, { reason: 'invalid_password' });
```

### Query Events

```typescript
// All events
const all = await audit.queryEvents({});

// By type
const auths = await audit.queryEvents({
  eventTypes: [AuditEventType.AUTH_LOGIN, AuditEventType.AUTH_LOGOUT],
});

// By user
const userEvents = await audit.queryEvents({
  actors: ['user-123', 'user-456'],
});

// Paginated
const page = await audit.queryEvents({
  limit: 50,
  offset: 0,
});
```

### Export for Compliance

```typescript
const soc2Export = await compliance.executeExport({
  exportId: uuidv4(),
  name: 'Q1 2024 SOC2 Report',
  framework: 'SOC2',
  format: 'JSON',
  filters: {
    dateRange: {
      startDate: new Date('2024-01-01'),
      endDate: new Date('2024-03-31'),
    },
  },
  destination: {
    type: 'gcs_and_bigquery',
    bucket: 'compliance-exports',
    dataset: 'audit_logs',
    table: 'soc2_events',
  },
});
```

### Manage Access Control

```typescript
// Check permission
const canExport = accessControl.hasPermission('auditor', AuditAccessPermission.EXPORT);

// Filter events by role
const visibleEvents = accessControl.filterEventsByRole(events, 'analyst');

// Grant temporary access (60 minutes)
accessControl.grantTemporaryAccess('analyst', AuditAccessPermission.EXPORT, 60);
```

## Event Types

- Authentication: `AUTH_LOGIN`, `AUTH_LOGOUT`, `AUTH_FAILURE`
- Resources: `RESOURCE_CREATE`, `RESOURCE_READ`, `RESOURCE_UPDATE`, `RESOURCE_DELETE`
- Authorization: `AUTHZ_GRANT`, `AUTHZ_REVOKE`, `AUTHZ_DENY`
- Admin: `ADMIN_USER_CREATE`, `ADMIN_USER_DELETE`, `ADMIN_POLICY_CREATE`
- Security: `SECURITY_VIOLATION`, `SECURITY_ANOMALY`, `SECURITY_THREAT`

## Roles

- `admin` - Full access
- `compliance-officer` - Read, export, manage policy
- `auditor` - Read, export
- `analyst` - Read only (redacted)
- `viewer` - Limited read access

## Compliance Frameworks

- SOC2: 365-day retention
- HIPAA: 1825-day (5-year) retention
- PCI-DSS: 365-day retention
- GDPR: 90-day retention with PII redaction

## Documentation

- [Full Documentation](./audit-logging.md)
- [Deployment Guide](./DEPLOYMENT_GUIDE.md)
- [Configuration Example](./audit-config-example.json)
