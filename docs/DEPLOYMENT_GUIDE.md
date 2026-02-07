# Audit Logging System - Deployment & Integration Guide

## Prerequisites

- Node.js 16+
- GCP Project with:
  - Cloud Logging API enabled
  - Cloud Storage API enabled
  - BigQuery API enabled
  - Service Account with appropriate IAM roles
- SIEM system (optional, for real-time export)

## GCP Setup

### 1. Enable Required APIs

```bash
gcloud services enable logging.googleapis.com
gcloud services enable storage-api.googleapis.com
gcloud services enable bigquery.googleapis.com
```

### 2. Create Service Account

```bash
gcloud iam service-accounts create audit-logging-sa \
  --display-name="Audit Logging Service Account"

gcloud iam service-accounts keys create key.json \
  --iam-account=audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com
```

### 3. Grant IAM Roles

```bash
# Cloud Logging
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/logging.logWriter"

# Cloud Storage
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/storage.objectCreator"

# BigQuery
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/bigquery.dataEditor"

gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/bigquery.jobUser"
```

### 4. Create GCS Bucket for Compliance Exports

```bash
gsutil mb -p PROJECT_ID gs://compliance-exports
gsutil versioning set on gs://compliance-exports
gsutil lifecycle set - gs://compliance-exports <<EOF
{
  "lifecycle": {
    "rule": [
      {
        "action": {"type": "SetStorageClass", "storageClass": "NEARLINE"},
        "condition": {"age": 90}
      },
      {
        "action": {"type": "Delete"},
        "condition": {"age": 2555}
      }
    ]
  }
}
EOF
```

### 5. Create BigQuery Datasets

```bash
bq mk --dataset \
  --location=US \
  --description="Audit logs dataset" \
  audit_logs

bq mk --dataset \
  --location=US \
  --description="Compliance exports dataset" \
  compliance_exports
```

## Application Integration

### Installation

```bash
npm install
npm install @google-cloud/logging @google-cloud/storage @google-cloud/bigquery uuid
```

### Environment Configuration

Create `.env` file:

```env
# GCP Configuration
GCP_PROJECT_ID=your-gcp-project
GOOGLE_APPLICATION_CREDENTIALS=./key.json

# Audit Logging
AUDIT_ENABLED=true
AUDIT_LOG_NAME=audit-logs
AUDIT_BATCH_SIZE=50
AUDIT_BATCH_TIMEOUT=5000

# Cloud Logging
CLOUD_LOGGING_ENABLED=true

# SIEM Export
SIEM_ENABLED=false
SIEM_ENDPOINT=siem-server.example.com:514
SIEM_FORMAT=CEF

# Sampling
SAMPLING_ENABLED=true
SAMPLING_RATE=0.95
```

### Initialize in Application

```typescript
// src/index.ts
import { initializeAuditSystem, AuditEventType, AuditSeverity } from './audit';
import * as dotenv from 'dotenv';

dotenv.config();

async function initializeApp() {
  const config = {
    enabled: process.env.AUDIT_ENABLED === 'true',
    projectId: process.env.GCP_PROJECT_ID!,
    logName: process.env.AUDIT_LOG_NAME || 'audit-logs',
    batchSize: parseInt(process.env.AUDIT_BATCH_SIZE || '50'),
    batchTimeoutMs: parseInt(process.env.AUDIT_BATCH_TIMEOUT || '5000'),
    enableCloudLogging: process.env.CLOUD_LOGGING_ENABLED === 'true',
    enableSiemExport: process.env.SIEM_ENABLED === 'true',
    siemEndpoint: process.env.SIEM_ENDPOINT,
    siemFormat: (process.env.SIEM_FORMAT || 'CEF') as 'CEF' | 'LEEF',
    sampling: {
      enabled: process.env.SAMPLING_ENABLED === 'true',
      rate: parseFloat(process.env.SAMPLING_RATE || '1.0'),
    },
    retentionPolicies: [],
    accessControls: [],
    complianceExports: [],
  };

  const { audit, compliance, retention, accessControl } = await initializeAuditSystem(config);

  return { audit, compliance, retention, accessControl };
}

// Usage in request handlers
export function auditMiddleware(auditSystem: any) {
  return async (req: any, res: any, next: any) => {
    // Log authentication
    if (req.user) {
      await auditSystem.audit.logAuthEvent(
        req.user.id,
        true,
        req.ip,
        req.get('user-agent')
      );
    }

    // Log resource access
    if (req.method !== 'GET' && req.path.startsWith('/api/')) {
      await auditSystem.audit.logResourceEvent(
        AuditEventType.RESOURCE_UPDATE,
        req.user?.id || 'anonymous',
        req.path,
        'API Endpoint',
        true,
        req.ip
      );
    }

    next();
  };
}

// Graceful shutdown
process.on('SIGTERM', async () => {
  console.log('Shutting down gracefully...');
  const { audit } = await initializeApp();
  await audit.shutdown();
  process.exit(0);
});
```

## Kubernetes Deployment

### ConfigMap

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: audit-logging-config
  namespace: default
data:
  audit-config.json: |
    {
      "enabled": true,
      "projectId": "my-project",
      "logName": "audit-logs",
      "batchSize": 50,
      "batchTimeoutMs": 5000,
      "enableCloudLogging": true,
      "enableSiemExport": false,
      "siemFormat": "CEF"
    }
```

### Secret for Service Account

```bash
kubectl create secret generic gcp-audit-key \
  --from-file=key.json=./key.json \
  --namespace=default
```

### Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: audit-logging
  namespace: default
spec:
  replicas: 3
  selector:
    matchLabels:
      app: audit-logging
  template:
    metadata:
      labels:
        app: audit-logging
    spec:
      serviceAccountName: audit-logging
      containers:
      - name: audit-logging
        image: my-registry/audit-logging:latest
        imagePullPolicy: Always
        env:
        - name: GCP_PROJECT_ID
          valueFrom:
            configMapKeyRef:
              name: audit-logging-config
              key: projectId
        - name: AUDIT_ENABLED
          value: "true"
        - name: GOOGLE_APPLICATION_CREDENTIALS
          value: /var/secrets/gcp/key.json
        volumeMounts:
        - name: gcp-key
          mountPath: /var/secrets/gcp
          readOnly: true
        - name: audit-storage
          mountPath: /data/audit
        resources:
          requests:
            memory: "256Mi"
            cpu: "100m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
      volumes:
      - name: gcp-key
        secret:
          secretName: gcp-audit-key
      - name: audit-storage
        persistentVolumeClaim:
          claimName: audit-storage-pvc
```

### RBAC

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: audit-logging
  namespace: default

---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: audit-logging
  namespace: default
rules:
- apiGroups: [""]
  resources: ["pods", "pods/log"]
  verbs: ["get", "list", "watch"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: audit-logging
  namespace: default
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: audit-logging
subjects:
- kind: ServiceAccount
  name: audit-logging
  namespace: default
```

### Persistent Volume

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: audit-storage-pvc
  namespace: default
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
  storageClassName: fast-ssd
```

## Docker Deployment

### Dockerfile

```dockerfile
FROM node:18-slim

WORKDIR /app

COPY package*.json ./
RUN npm install --production

COPY dist ./dist
COPY key.json ./key.json

ENV NODE_ENV=production
ENV GOOGLE_APPLICATION_CREDENTIALS=/app/key.json

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:8080/health', (r) => {if (r.statusCode !== 200) throw new Error(r.statusCode)})"

CMD ["node", "dist/index.js"]
```

Build and push:

```bash
docker build -t my-registry/audit-logging:latest .
docker push my-registry/audit-logging:latest
```

## SIEM Integration

### Splunk

```bash
# Configure Universal Forwarder to listen on port 514
[tcpout]
defaultGroup = my_indexers
maxQueueSize = 512MB

[tcpout:my_indexers]
server = indexer1.example.com:9997, indexer2.example.com:9997
maxConnectionsPerIndexer = 2
useACK = true

# Configure audit log input
[inputs]
[syslog:514]
connection_host = dns
```

### ELK Stack (Elasticsearch, Logstash, Kibana)

```bash
# Logstash configuration
input {
  syslog {
    port => 514
    codec => cef
  }
}

filter {
  if [cef][cef_version] {
    mutate {
      add_tag => [ "cef" ]
    }
  }
}

output {
  elasticsearch {
    hosts => ["elasticsearch:9200"]
    index => "audit-logs-%{+YYYY.MM.dd}"
  }
}
```

### Datadog

Set up agent to forward syslog:

```yaml
logs:
  - type: syslog
    port: 514
    service: audit-logs
    source: cre-audit
```

## Compliance Export Scheduling

### Using cron (Linux/Mac)

```bash
# Export SOC2 compliance report quarterly
0 0 1 1,4,7,10 * /usr/local/bin/export-compliance-soc2.sh

# Export GDPR monthly
0 2 1 * * /usr/local/bin/export-compliance-gdpr.sh

# Export HIPAA annually
0 0 1 1 * /usr/local/bin/export-compliance-hipaa.sh
```

### Using Cloud Scheduler

```bash
gcloud scheduler jobs create http audit-export-soc2 \
  --schedule="0 0 1 1,4,7,10 *" \
  --uri="https://audit-logging.example.com/api/export/soc2" \
  --http-method=POST \
  --oidc-service-account-email=audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com
```

## Monitoring & Alerting

### CloudWatch Metrics (for AWS deployments)

```typescript
import { CloudWatch } from 'aws-sdk';

const cloudwatch = new CloudWatch();

async function publishMetrics(stats: AuditLogStatistics) {
  await cloudwatch.putMetricData({
    Namespace: 'AuditLogging',
    MetricData: [
      {
        MetricName: 'TotalEvents',
        Value: stats.totalEvents,
        Unit: 'Count',
      },
      {
        MetricName: 'FailedEvents',
        Value: stats.failedEvents,
        Unit: 'Count',
      },
    ],
  }).promise();
}
```

### Cloud Monitoring (for GCP)

```bash
# Create alert policy
gcloud alpha monitoring policies create \
  --notification-channels=CHANNEL_ID \
  --display-name="Audit Log Export Failure" \
  --condition-display-name="Export job failure rate > 10%" \
  --condition-threshold-value=0.1 \
  --condition-threshold-duration=300s
```

## Performance Tuning

### Database Optimization

```typescript
// Index frequently queried fields
CREATE INDEX idx_audit_timestamp ON audit_events(timestamp);
CREATE INDEX idx_audit_actor ON audit_events(actor_user_id);
CREATE INDEX idx_audit_type ON audit_events(event_type);
```

### Memory Management

```typescript
// Configure appropriate batch sizes based on memory
config.batchSize = Math.min(100, Math.floor(availableMemory / 10000));

// Enable sampling for high-volume environments
config.sampling = {
  enabled: true,
  rate: 0.5, // Sample 50% of events
};
```

### Storage Optimization

```bash
# Compress archived audit logs
gsutil iam ch serviceAccount:audit-logging-sa@PROJECT_ID.iam.gserviceaccount.com:objectCreator \
  gs://compliance-exports

# Set up lifecycle policy to compress after 30 days
gsutil lifecycle set - gs://compliance-exports <<EOF
{
  "lifecycle": {
    "rule": [
      {
        "action": {"type": "SetStorageClass", "storageClass": "COLDLINE"},
        "condition": {"age": 30}
      }
    ]
  }
}
EOF
```

## Testing & Validation

### Load Testing

```bash
# Generate test events
npm run test:load -- --events=10000 --concurrency=50

# Monitor performance
watch 'ps aux | grep node | grep -v grep'
```

### Compliance Validation

```bash
# Verify exports match retention policies
npm run test:compliance -- --framework=SOC2

# Validate SIEM event format
npm run test:siem-format -- --format=CEF
```

## Disaster Recovery

### Backup Strategy

```bash
# Daily backup of local audit store
gsutil -m cp -r .audit/events gs://audit-backups/$(date +%Y-%m-%d)/

# Backup BigQuery tables weekly
bq extract audit_logs.* gs://audit-backups/bigquery-export-$(date +%Y-%m-%d)/*
```

### Restore Procedure

```bash
# Restore from GCS backup
gsutil -m cp -r gs://audit-backups/2024-01-15/* .audit/events/

# Restore BigQuery table
bq load --source_format=AVRO audit_logs.restored_events gs://audit-backups/*/audit_logs.*.avro
```

## Support & Troubleshooting

For detailed troubleshooting and FAQ, see `/docs/audit-logging.md`
