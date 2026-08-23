# CRE GCP Scaling Operations Runbook

**Procedures for scaling CRE workflow engine on Google Cloud Platform.**

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Scaling Overview](#scaling-overview)
3. [Horizontal Pod Autoscaling](#horizontal-pod-autoscaling)
4. [Vertical Pod Autoscaling](#vertical-pod-autoscaling)
5. [Cluster Node Scaling](#cluster-node-scaling)
6. [Database Scaling](#database-scaling)
7. [Scaling Decision Matrix](#scaling-decision-matrix)
8. [Validation Steps](#validation-steps)
9. [Troubleshooting](#troubleshooting)
10. [Escalation Contacts](#escalation-contacts)

---

## Prerequisites

### Required Tools

```bash
# Verify installations
kubectl version --client  # kubectl 1.27.0+
gcloud --version          # Google Cloud SDK 400.0.0+
```

### Required Permissions

| IAM Role | Purpose |
|----------|---------|
| `roles/container.admin` | GKE cluster and node pool management |
| `roles/monitoring.viewer` | View metrics for scaling decisions |
| `roles/compute.admin` | Compute resource management |

---

## Scaling Overview

### CRE Architecture Components

```
                    +-------------------+
                    |   Cloud Load      |
                    |   Balancer        |
                    +---------+---------+
                              |
                    +---------v---------+
                    |   Ingress         |
                    +---------+---------+
                              |
                    +---------v---------+
                    |   CRE Pods        |<---+
                    |   (Stateless)     |    |
                    +---------+---------+    |
                              |              HPA
                    +---------v---------+    |
                    |   CRE Pods        |----+
                    |   (Stateful)      |
                    +---------+---------+
                              |
                    +---------v---------+
                    |   Mnesia Cluster  |
                    |   (Distributed)   |
                    +-------------------+
```

### Scaling Strategies

| Component | Scaling Type | Method | Constraints |
|-----------|--------------|--------|-------------|
| CRE Web Pods | Horizontal | HPA | CPU, Memory, Custom Metrics |
| CRE Worker Pods | Horizontal | HPA | Queue Depth, Custom Metrics |
| Mnesia Nodes | Vertical | Manual | Requires careful planning |
| GKE Nodes | Cluster Autoscaler | Automatic | Resource requests required |

---

## Horizontal Pod Autoscaling

### HPA Configuration

Create HorizontalPodAutoscaler for CRE:

```yaml
# k8s/hpa.yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: cre-hpa
  namespace: cre
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: cre
  minReplicas: 3
  maxReplicas: 20
  metrics:
  # CPU-based scaling
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  # Memory-based scaling
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  # Custom metric: queue depth
  - type: Pods
    pods:
      metric:
        name: cre_workflow_queue_depth
      target:
        type: AverageValue
        averageValue: "100"
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
      - type: Percent
        value: 100
        periodSeconds: 30
      - type: Pods
        value: 2
        periodSeconds: 30
      selectPolicy: Max
```

### Deploy HPA

```bash
# Apply HPA configuration
kubectl apply -f k8s/hpa.yaml -n cre

# Verify HPA status
kubectl get hpa -n cre

# Watch HPA in action
kubectl get hpa -n cre -w
```

### Manual Scaling

```bash
# Scale to specific replica count
kubectl scale deployment/cre --replicas=10 -n cre

# Verify scaling
kubectl get pods -n cre -l app=cre

# Check autoscaling status
kubectl describe hpa cre-hpa -n cre
```

### Scaling Based on Custom Metrics

```bash
# Install Prometheus Adapter for custom metrics
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm repo update
helm install prometheus-adapter prometheus-community/prometheus-adapter \
  -n monitoring --create-namespace

# Define custom metric rule
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: prometheus-adapter-config
  namespace: monitoring
data:
  config.yaml: |
    rules:
    - seriesQuery: 'cre_workflow_queue_depth'
      resources:
        overrides:
          namespace:
            resource: namespace
          pod:
            resource: pod
      name:
        matches: "^(.*)"
        as: "cre_workflow_queue_depth"
      metricsQuery: 'sum(cre_workflow_queue_depth{<<.LabelMatchers>>}) by (<<.GroupBy>>)'
EOF

# Restart adapter
kubectl rollout restart deployment/prometheus-adapter -n monitoring
```

---

## Vertical Pod Autoscaling

### VPA Configuration

```yaml
# k8s/vpa.yaml
apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: cre-vpa
  namespace: cre
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: cre
  updatePolicy:
    updateMode: "Auto"
  resourcePolicy:
    containerPolicies:
    - containerName: cre
      minAllowed:
        cpu: "100m"
        memory: "128Mi"
      maxAllowed:
        cpu: "4"
        memory: "8Gi"
      controlledResources: ["cpu", "memory"]
```

### Deploy VPA

```bash
# Apply VPA configuration
kubectl apply -f k8s/vpa.yaml -n cre

# Check VPA recommendations
kubectl describe vpa cre-vpa -n cre

# View recommended resources
kubectl get vpa cre-vpa -n cre -o jsonpath='{.status.recommendation}'
```

### Manual Resource Adjustment

```bash
# Update deployment resources
kubectl set resources deployment/cre \
  --limits=cpu=4,memory=8Gi \
  --requests=cpu=2,memory=4Gi \
  -n cre

# Verify
kubectl describe pod -l app=cre -n cre | grep -A 5 "Containers:"
```

---

## Cluster Node Scaling

### Cluster Autoscaler Configuration

Enable Cluster Autoscaler in GKE:

```bash
# Enable autoscaling for node pool
gcloud container node-pools update general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --enable-autoscaling \
  --min-nodes=3 \
  --max-nodes=20 \
  --project=${PROJECT_ID}

# Verify
gcloud container node-pools describe general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

### Node Pool Autoscaling via Terraform

```hcl
# terraform/gcp/modules/gke_cluster/main.tf (update)
resource "google_container_node_pool" "general" {
  name       = "general"
  location   = var.region
  cluster    = google_container_cluster.primary.name

  autoscaling {
    min_node_count = 3
    max_node_count = 20
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    machine_type = "e2-standard-4"
    disk_size_gb = 100
    disk_type    = "pd-ssd"

    # Resource requests for autoscaler
    resource_labels = {
      autoscaler_killed = "false"
    }
  }
}
```

### Provisional Node Pool for Spot Instances

```bash
# Create spot instance node pool
gcloud container node-pools create spot-pool \
  --cluster=cre-cluster \
  --region=${REGION} \
  --machine-type=e2-standard-4 \
  --spot \
  --enable-autoscaling \
  --min-nodes=0 \
  --max-nodes=10 \
  --node-labels=workload=generic,pool=spot \
  --node-taints=spot=true:NoSchedule \
  --project=${PROJECT_ID}

# Deploy non-critical workloads to spot nodes
kubectl patch deployment cre-worker -n cre -p '{"spec":{"template":{"spec":{"tolerations":[{"key":"spot","operator":"Equal","value":"true","effect":"NoSchedule"}]}}}}'
```

### Node Pool Scaling Operations

```bash
# Resize node pool (manual)
gcloud container node-pools resize general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --num-nodes=10 \
  --project=${PROJECT_ID}

# Add new node pool for memory-intensive workloads
gcloud container node-pools create memory-optimized \
  --cluster=cre-cluster \
  --region=${REGION} \
  --machine-type=n2-highmem-16 \
  --num-nodes=2 \
  --enable-autoscaling \
  --min-nodes=2 \
  --max-nodes=10 \
  --node-labels=workload=mnesia \
  --node-taints=workload=mnesia:NoSchedule \
  --project=${PROJECT_ID}
```

---

## Database Scaling

### Mnesia Cluster Scaling

Mnesia scaling requires careful planning due to its distributed nature:

```bash
# Check current Mnesia cluster status
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:system_info(running_db_nodes)."

# Add new Mnesia node
# 1. Deploy new pod with Mnesia replica role
kubectl apply -f k8s/mnesia-replica.yaml -n cre

# 2. Join cluster from new node
kubectl exec -n cre $(kubectl get pod -n cre -l role=mnesia-replica -o jsonpath='{.items[0].metadata.name}') -- \
  /opt/cre/bin/cre_eval "
    case mnesia:change_config(extra_db_nodes, ['cre@cre-primary']) of
      {ok, _} -> ok;
      {error, Reason} -> {error, Reason}
    end.
  "

# 3. Copy tables to new node
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    mnesia:change_table_copy_type(wf_cases, node(), disc_copies).
  "
```

### Mnesia Memory Optimization

```erlang
% In cre_config.erl, optimize for larger datasets

% Increase ETS table size
-define(ETS_TABLE_SIZE, 1048576). % 1M entries

% Configure Mnesia for larger datasets
init_mnesia_config() ->
    application:set_env(mnesia, dump_log_write_threshold, 5000),
    application:set_env(mnesia, dump_log_time_threshold, 300000),
    application:set_env(mnesia, max_wait_for_decision, 60000),
    ok.
```

### Cloud Spanner Scaling (if applicable)

```bash
# Update Spanner instance processing units
gcloud spanner instances update cre-instance \
  --processing-units=1000 \
  --project=${PROJECT_ID}

# Add additional Spanner node
gcloud spanner instances update cre-instance \
  --nodes=5 \
  --project=${PROJECT_ID}

# Check capacity
gcloud spanner instances describe cre-instance \
  --project=${PROJECT_ID}
```

---

## Scaling Decision Matrix

### When to Scale Horizontally

| Condition | Action | Threshold |
|-----------|--------|-----------|
| CPU > 70% sustained | Add pods | 5 minutes |
| Memory > 80% sustained | Add pods | 5 minutes |
| Queue depth > 1000 | Add worker pods | Immediate |
| Response time > P95 500ms | Add pods | 2 minutes |

### When to Scale Vertically

| Condition | Action | Threshold |
|-----------|--------|-----------|
| Single pod CPU > 90% | Increase CPU limit | Sustained |
| Single pod memory > 90% | Increase memory limit | Sustained |
| OOMKilled events | Increase memory limit | Any occurrence |
| High garbage collection | Increase heap size | Sustained |

### When to Scale Nodes

| Condition | Action | Threshold |
|-----------|--------|-----------|
| Pending pods due to insufficient resources | Add nodes | Immediate |
| Node CPU > 80% across cluster | Add nodes | Sustained 10 min |
| Node memory > 80% across cluster | Add nodes | Sustained 10 min |

### Scaling Flowchart

```
Metrics Alert
      |
      v
Check Resource Type
      |
      +-- CPU/Memory --> Check Level
      |                     |
      |                     +-- Pod Level --> HPA Scales Pods
      |                     |
      |                     +-- Node Level --> CA Scales Nodes
      |
      +-- Queue Depth --> Scale Worker Pods
      |
      +-- Database --> Manual Review Required
```

---

## Validation Steps

### Post-Scaling Health Check

```bash
# Run automated validation
./scripts/runbooks/scale_validation.sh

# Expected output:
# ✓ Pod distribution: OK
# ✓ Resource utilization: OK
# ✓ Service connectivity: OK
# ✓ Database replication: OK
```

### Verify New Pods

```bash
# Check all pods are running
kubectl get pods -n cre

# Check pod resource usage
kubectl top pods -n cre

# Check pod distribution across nodes
kubectl get pods -n cre -o wide
```

### Verify Cluster Nodes

```bash
# Check node status
kubectl get nodes

# Check node resource usage
kubectl top nodes

# Verify cluster autoscaler logs
kubectl logs -n kube-system -l k8s-app=cluster-autoscaler --tail=50
```

### Verify Data Replication

```bash
# Check Mnesia replication status
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    lists:foreach(fun(T) ->
      case mnesia:table_info(T, where_to_read) of
        [] -> io:format('~p: WARNING - no replicas~n', [T]);
        Nodes -> io:format('~p: ~p replicas~n', [T, length(Nodes)])
      end
    end, mnesia:system_info(tables)).
  "
```

---

## Troubleshooting

### HPA Not Scaling

```bash
# Check HPA status
kubectl describe hpa cre-hpa -n cre

# Common issues:
# 1. Metrics not available - install metrics-server
kubectl apply -f https://github.com/kubernetes-sigs/metrics-server/releases/latest/download/components.yaml

# 2. Resource requests not set
kubectl describe pod -l app=cre -n cre | grep Requests

# 3. Already at max replicas
kubectl get hpa cre-hpa -n cre -o jsonpath='{.spec.maxReplicas}'
```

### Cluster Autoscaler Not Scaling

```bash
# Check autoscaler logs
kubectl logs -n kube-system -l k8s-app=cluster-autoscaler --tail=100

# Common issues:
# 1. Resource requests not set
kubectl get deployment cre -n cre -o jsonpath='{.spec.template.spec.containers[0].resources}'

# 2. Quota limits
kubectl describe quota -n cre

# 3. Node pool at max size
gcloud container node-pools describe general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

### Pod Eviction During Scale-Down

```bash
# Check PDB status
kubectl get pdb -n cre

# Create/update PDB to prevent excessive scale-down
cat <<EOF | kubectl apply -f -
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: cre-pdb
  namespace: cre
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app: cre
EOF
```

---

## Escalation Contacts

| Role | Name | Contact | Hours |
|------|------|---------|-------|
| On-Call Engineer | CRE Ops | oncall@company.com | 24/7 |
| Infrastructure Lead | Infra Team | infra@company.com | Business Hours |
| Engineering Manager | CRE Leadership | eng-manager@company.com | Business Hours |

---

## Quick Reference Commands

```bash
# Horizontal scaling
kubectl scale deployment/cre --replicas=N -n cre
kubectl get hpa -n cre

# Vertical scaling
kubectl set resources deployment/cre --limits=cpu=X,memory=Y -n cre
kubectl get vpa -n cre

# Node scaling
gcloud container node-pools update general --cluster=cre-cluster \
  --enable-autoscaling --min-nodes=M --max-nodes=N --region=$REGION

# Validation
kubectl get pods -n cre -w
kubectl top pods -n cre
kubectl top nodes
```

---

*Last Updated: 2025-02-09*
*For CRE version 0.3.0+*
