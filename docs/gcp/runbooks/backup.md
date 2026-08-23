# CRE GCP Backup and Restore Runbook

**Procedures for backing up and restoring CRE workflow engine on Google Cloud Platform.**

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Backup Architecture](#backup-architecture)
3. [Automated Backups](#automated-backups)
4. [Manual Backups](#manual-backups)
5. [Restore Procedures](#restore-procedures)
6. [Backup Validation](#backup-validation)
7. [Disaster Recovery](#disaster-recovery)
8. [Escalation Contacts](#escalation-contacts)

---

## Prerequisites

### Required Tools

```bash
# Verify installations
gcloud --version          # Google Cloud SDK 400.0.0+
kubectl version --client  # kubectl 1.27.0+
```

### Required Permissions

| IAM Role | Purpose |
|----------|---------|
| `roles/storage.admin` | Cloud Storage backup management |
| `roles/compute.storageAdmin` | Snapshot management |
| `roles/spanner.admin` | Spanner backup (if applicable) |
| `roles/secretmanager.viewer` | Access backup secrets |

---

## Backup Architecture

### CRE Data Components

| Component | Data Type | Backup Method | Retention |
|-----------|-----------|---------------|-----------|
| Mnesia Tables | Workflow state, cases | Daily snapshots + Point-in-time | 30 days |
| Persistent Volumes | Logs, checkpoints | Daily snapshots | 30 days |
| Configuration | ConfigMaps, Secrets | Version control | Permanent |
| Container Images | GCR | Immutable tags | Permanent |
| Terraform State | Infrastructure | State file versioning | 90 days |

### Backup Storage Strategy

```
+-------------------+
|   CRE Pods        |
|                   |
| - Mnesia Data     |-> PVC Snapshots -> Cloud Storage
| - Logs            |
| - Checkpoints     |
+-------------------+
         |
         v
+-------------------+
|   GKE Cluster     |
|                   |
| - PV Snapshots    |-> Regional Cloud Storage
+-------------------+
         |
         v
+-------------------+
|   Cross-Region    |
|   Replication     |
|                   |
| - Cold Storage    |-> Archive ( Glacier equivalent )
+-------------------+
```

---

## Automated Backups

### Configure Automated PVC Snapshots

Create a SnapshotSchedule for Kubernetes PVCs:

```yaml
# k8s/snapshot-schedule.yaml
apiVersion: snapshot.storage.k8s.io/v1
kind: VolumeSnapshotClass
metadata:
  name: csi-gce-pd-snapshot
driver: pd.csi.storage.gke.io
deletionPolicy: Retain

---
apiVersion: velero.io/v1
kind: Schedule
metadata:
  name: cre-daily-backup
  namespace: velero
spec:
  schedule: "0 2 * * *"  # 2 AM daily
  template:
    includedNamespaces:
    - cre
    storageLocation: gcp-daily
    volumeSnapshotLocations:
    - gcp-snapshots
    ttl: 720h  # 30 days
```

### Terraform Snapshot Schedule

```hcl
# terraform/gcp/modules/storage/main.tf
resource "google_compute_disk_resource_policy_attachment" "cre_disks" {
  name = google_compute_resource_policy.backup_schedule.name
  disk = google_compute_disk.cre_data.name
}

resource "google_compute_resource_policy" "backup_schedule" {
  name   = "cre-daily-snapshot"
  region = var.region
  snapshot_schedule_policy {
    schedule {
      daily_schedule {
        days_in_month = ["1", "15"]
        start_time = "02:00"
      }
    }

    retention_policy {
      max_retention_days    = 30
      source_disk_forbidden = false
    }

    snapshot_properties {
      labels = {
        snapshot_group = "cre-persistent-data"
        auto_created  = "true"
      }
      storage_locations = ["${var.region}"]
    }
  }
}
```

### Mnesia Automated Backup

Create an Erlang module for scheduled backups:

```erlang
%%% src/backup_scheduler.erl
-module(backup_scheduler).
-export([start_link/0, init/1, handle_info/2, schedule_backup/0]).

-behaviour(gen_server).

-define(BACKUP_INTERVAL, timer:hours(24)). % Daily backup

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    schedule_backup(),
    {ok, #{}}.

handle_info(backup, State) ->
    do_backup(),
    schedule_backup(),
    {noreply, State}.

schedule_backup() ->
    %% Schedule next backup at 2 AM
    {Time, _} = calendar:universal_time(),
    HoursUntil2AM = case Time of
        {H, _, _} when H < 2 -> 2 - H;
        {H, _, _} -> 26 - H
    end,
    erlang:send_after(HoursUntil2AM * 3600000, self(), backup).

do_backup() ->
    Timestamp = integer_to_binary(os:system_time(seconds)),
    Filename = <<"/opt/cre/backup/cre_backup_", Timestamp/binary, ".">>,
    case mnesia:backup(binary_to_list(Filename)) of
        ok ->
            logger:info("Backup completed: ~s", [Filename]),
            upload_to_gcs(Filename);
        {error, Reason} ->
            logger:error("Backup failed: ~p", [Reason])
    end.

upload_to_gcs(Filename) ->
    Bucket = application:get_env(cre, backup_bucket, "cre-backups"),
    Command = io_lib:format("gsutil cp ~s gs://~s/", [Filename, Bucket]),
    case os:cmd(Command) of
        [] -> ok;
        Error -> logger:error("GCS upload failed: ~s", [Error])
    end.
```

---

## Manual Backups

### On-Demand PVC Snapshot

```bash
# Identify PVC to backup
kubectl get pvc -n cre

# Create snapshot
cat <<EOF | kubectl apply -f -
apiVersion: snapshot.storage.k8s.io/v1
kind: VolumeSnapshot
metadata:
  name: cre-manual-snapshot-$(date +%Y%m%d)
  namespace: cre
spec:
  volumeSnapshotClassName: csi-gce-pd-snapshot
  source:
    persistentVolumeClaimName: cre-data-pvc
EOF

# Verify snapshot creation
kubectl get volumesnapshot -n cre
```

### Manual Mnesia Backup

```bash
# Execute Mnesia backup
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    Timestamp = integer_to_list(os:system_time(seconds)),
    Filename = \"/opt/cre/backup/manual_backup_\" ++ Timestamp ++ \".\",
    case mnesia:backup(Filename) of
      ok -> io:format('Backup: ~s~n', [Filename]);
      {error, Reason} -> io:format('Error: ~p~n', [Reason])
    end.
  "

# Copy backup locally
kubectl cp -n cre deployment/cre:/opt/cre/backup/manual_backup_* \
  ./local_backup/
```

### Manual Configuration Backup

```bash
# Backup all Kubernetes resources
kubectl get all,configmaps,secrets,pvc,pv -n cre -o yaml > cre-config-backup-$(date +%Y%m%d).yaml

# Backup Terraform state
cd /path/to/cre/terraform/gcp
terraform output -json > terraform-output-backup-$(date +%Y%m%d).json
cp terraform.tfstate terraform.tfstate.backup.$(date +%Y%m%d)
```

### Container Image Backup

```bash
# Export current running images
kubectl get deployment cre -n cre -o jsonpath='{.spec.template.spec.containers[0].image}'

# Tag and push to backup registry
docker tag gcr.io/${PROJECT_ID}/cre:latest \
  gcr.io/${PROJECT_ID}/cre:backup-$(date +%Y%m%d)
docker push gcr.io/${PROJECT_ID}/cre:backup-$(date +%Y%m%d)
```

---

## Restore Procedures

### Restore from PVC Snapshot

```bash
# List available snapshots
kubectl get volumesnapshot -n cre

# Create new PVC from snapshot
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: cre-data-restored
  namespace: cre
spec:
  dataSource:
    name: cre-manual-snapshot-YYYYMMDD
    kind: VolumeSnapshot
    apiGroup: snapshot.storage.k8s.io
  accessModes:
    - ReadWriteOnce
  storageClassName: standard-rwo
  resources:
    requests:
      storage: 10Gi
EOF

# Update deployment to use restored PVC
kubectl patch deployment cre -n cre -p '{
  "spec": {
    "template": {
      "spec": {
        "volumes": [{
          "name": "cre-data",
          "persistentVolumeClaim": {"claimName": "cre-data-restored"}
        }]
      }
    }
  }
}'

# Restart pods
kubectl rollout restart deployment/cre -n cre
```

### Restore Mnesia from Backup

```bash
# Copy backup file to pod
kubectl cp ./local_backup/backup_20250209 \
  -n cre deployment/cre:/tmp/restore_backup

# Stop CRE pods (graceful shutdown)
kubectl scale deployment/cre --replicas=0 -n cre

# Wait for termination
kubectl wait --for=delete pod -l app=cre -n cre --timeout=60s

# Start single pod for restore
kubectl scale deployment/cre --replicas=1 -n cre
kubectl wait --for=condition=ready pod -l app=cre -n cre --timeout=120s

# Execute restore
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    case mnesia:restore('/tmp/restore_backup.', []) of
      {atomic, _} -> io:format('Restore successful~n');
      {aborted, Reason} -> io:format('Restore failed: ~p~n', [Reason])
    end.
  "

# Scale back to normal
kubectl scale deployment/cre --replicas=3 -n cre
```

### Restore from GCS Backup

```bash
# List backups in GCS
gsutil ls gs://${BACKUP_BUCKET}/

# Download and restore
gsutil cp gs://${BACKUP_BUCKET}/cre_backup_20250209. /tmp/restore_backup.

# Copy to pod and restore
kubectl cp /tmp/restore_backup. -n cre deployment/cre:/tmp/
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:restore('/tmp/restore_backup.', [])."
```

### Full Disaster Recovery

```bash
# Step 1: Restore infrastructure
cd /path/to/cre/terraform/gcp
terraform apply

# Step 2: Restore GKE cluster from backup
gcloud container clusters restore cre-cluster \
  --backup-url=gs://${BACKUP_BUCKET}/cluster-backup \
  --region=${REGION} \
  --project=${PROJECT_ID}

# Step 3: Restore PVCs
for snapshot in $(kubectl get volumesnapshot -n cre -o name); do
  # Create PVC from each snapshot
  kubectl apply -f - <<EOF
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: ${snapshot}-restored
  namespace: cre
spec:
  dataSource:
    name: ${snapshot}
    kind: VolumeSnapshot
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
EOF
done

# Step 4: Deploy CRE
kubectl apply -f k8s/

# Step 5: Restore Mnesia data
# (See Mnesia restore procedure above)

# Step 6: Verify
kubectl get pods -n cre
kubectl exec -n cre deployment/cre -- /opt/cre/bin/cre_eval "mnesia:system_info()."
```

---

## Backup Validation

### Automated Backup Health Check

```bash
#!/bin/bash
# scripts/runbooks/backup_health_check.sh

set -euo pipefail

PROJECT_ID="${PROJECT_ID:-your-project-id}"
BACKUP_BUCKET="${BACKUP_BUCKET:-cre-backups}"
NAMESPACE="${NAMESPACE:-cre}"
MAX_AGE_HOURS="${MAX_AGE_HOURS:-48}"

echo "=== CRE Backup Health Check ==="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo

# 1. Check recent snapshots
echo "1. Checking PVC snapshots..."
RECENT_SNAPSHETS=$(gcloud compute snapshots list \
  --project=${PROJECT_ID} \
  --filter="creationTimestamp > '-${MAX_AGE_HOURS} hours' AND labels:snapshot_group='cre-persistent-data'" \
  --format="value(name)" | wc -l)

if [ "${RECENT_SNAPSHETS}" -gt 0 ]; then
  echo "   ✓ Recent snapshots found: ${RECENT_SNAPSHETS}"
else
  echo "   ✗ No recent snapshots (within ${MAX_AGE_HOURS} hours)"
  exit 1
fi

# 2. Check GCS backups
echo "2. Checking GCS backups..."
RECENT_GCS=$(gsutil ls -l gs://${BACKUP_BUCKET}/** 2>/dev/null | \
  grep "$(date -u -d '${MAX_AGE_HOURS} hours ago' +%Y-%m-%d)" | wc -l)

if [ "${RECENT_GCS}" -gt 0 ]; then
  echo "   ✓ Recent GCS backups: ${RECENT_GCS}"
else
  echo "   ✗ No recent GCS backups"
  exit 1
fi

# 3. Test Mnesia backup restore (dry run)
echo "3. Testing Mnesia backup integrity..."
TEST_POD=$(kubectl get pod -n ${NAMESPACE} -l app=cre -o jsonpath='{.items[0].metadata.name}')
kubectl exec -n ${NAMESPACE} ${TEST_POD} -- \
  /opt/cre/bin/cre_eval "
    case mnesia:checkpoint_backup([list_to_atom(\"test_checkpoints\"]) of
      ok -> io:format('   Checkpoint test OK~n');
      {error, Reason} -> io:format('   Checkpoint test failed: ~p~n', [Reason]), halt(1)
    end.
  " || echo "   ✗ Mnesia backup test failed"

# 4. Check backup age
echo "4. Checking backup age..."
LATEST_BACKUP=$(gsutil ls gs://${BACKUP_BUCKET}/ 2>/dev/null | sort -r | head -1)
if [ -n "${LATEST_BACKUP}" ]; then
  echo "   ✓ Latest backup: ${LATEST_BACKUP}"
else
  echo "   ✗ No backups found in GCS"
  exit 1
fi

# 5. Verify backup size
echo "5. Verifying backup size..."
BACKUP_SIZE=$(gsutil du -s gs://${BACKUP_BUCKET} | awk '{print $1}')
BACKUP_SIZE_MB=$((BACKUP_SIZE / 1024 / 1024))
if [ "${BACKUP_SIZE_MB}" -gt 10 ]; then
  echo "   ✓ Backup size: ${BACKUP_SIZE_MB} MB"
else
  echo "   ⚠ Warning: Backup size seems small: ${BACKUP_SIZE_MB} MB"
fi

echo
echo "=== Health Check Complete ==="
```

### Manual Backup Verification

```bash
# 1. Verify snapshot exists and is complete
gcloud compute snapshots describe cre-snapshot-YYYYMMDD \
  --project=${PROJECT_ID} \
  --zone=${REGION}-a

# 2. Verify GCS backup is accessible
gsutil stat gs://${BACKUP_BUCKET}/cre_backup_YYYYMMDD

# 3. Test backup restore on non-production
kubectl create namespace cre-restore-test
# ... deploy test pod with restored backup
kubectl delete namespace cre-restore-test
```

---

## Disaster Recovery

### RPO/RTO Targets

| Metric | Target | Current |
|--------|--------|---------|
| RPO (Recovery Point Objective) | 15 minutes | Daily (24h) |
| RTO (Recovery Time Objective) | 1 hour | 2 hours |
| Data Retention | 30 days | 30 days |
| Cross-Region Replication | Async, 1 hour | Not configured |

### Disaster Recovery Runbook

#### Step 1: Assess Impact (0-15 min)

```bash
# Determine scope of disaster
# - Regional outage?
# - Cluster failure?
# - Data corruption?

# Check GCP status
gcloud compute regions describe ${REGION}

# Check cluster status
gcloud container clusters describe cre-cluster --region=${REGION}
```

#### Step 2: Activate DR Plan (15-30 min)

```bash
# If regional outage, activate DR region
export DR_REGION="${DR_REGION:-us-east4}"

# Deploy to DR region
cd /path/to/cre/terraform/gcp
terraform apply -var="region=${DR_REGION}"
```

#### Step 3: Restore Data (30-90 min)

```bash
# Restore from regional backup
# (See restore procedures above)

# Verify data integrity
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:system_info(running_db_nodes)."
```

#### Step 4: Switch Traffic (90-120 min)

```bash
# Update DNS to point to DR region
gcloud dns record-sets transaction start \
  --zone=cre-zone

gcloud dns record-sets transaction remove \
  --name=cre.example.com. \
  --type=A \
  --zone=cre-zone

gcloud dns record-sets transaction add \
  --name=cre.example.com. \
  --type=A \
  --ttl=300 \
  --zone=cre-zone \
  "${DR_LB_IP}"

gcloud dns record-sets transaction execute --zone=cre-zone
```

---

## Escalation Contacts

| Role | Name | Contact | Hours |
|------|------|---------|-------|
| On-Call Engineer | CRE Ops | oncall@company.com | 24/7 |
| Infrastructure Lead | Infra Team | infra@company.com | Business Hours |
| Engineering Manager | CRE Leadership | eng-manager@company.com | Business Hours |
| GCP Support | Google Cloud | gcp-support | 24/7 |

---

## Quick Reference Commands

### Backup Commands

```bash
# Manual snapshot
kubectl apply -f k8s/snapshot.yaml

# Mnesia backup
kubectl exec -n cre deployment/cre -- /opt/cre/bin/cre_eval "mnesia:backup('/tmp/backup.')."

# Configuration backup
kubectl get all -n cre -o yaml > backup.yaml

# GCS upload
gsutil cp /tmp/backup. gs://${BACKUP_BUCKET}/
```

### Restore Commands

```bash
# List snapshots
gcloud compute snapshots list --project=${PROJECT_ID}

# Restore from snapshot
gcloud compute disks create cre-restored \
  --source-snapshot=SNAPSHOT_NAME \
  --zone=${REGION}-a

# Mnesia restore
kubectl exec -n cre deployment/cre -- /opt/cre/bin/cre_eval "mnesia:restore('/tmp/backup.', [])."
```

### Validation Commands

```bash
# Health check
./scripts/runbooks/backup_health_check.sh

# Verify backup
gsutil stat gs://${BACKUP_BUCKET}/backup_name

# Test restore
kubectl exec -n cre deployment/cre -- /opt/cre/bin/cre_eval "mnesia:table_info(Tab, size)."
```

---

*Last Updated: 2025-02-09*
*For CRE version 0.3.0+*
